
from urllib import urlencode
import json
import os
import sys
import re
import requests
import multipart

class MZBenchAPIException(Exception):
    pass

def start(host, script_file, script_content,
          node_commit = None, nodes = None, workers_per_node = None, deallocate_after_bench = None,
          provision_nodes = None, exclusive_node_usage = None, benchmark_name = None,
          cloud = None, emails=[], includes=[], env={}
        ):
    """Starts a bench

    :param host: MZBench API server host with port
    :type host: str
    :param script_file: Scenario filename for dashboard
    :type script_file: str or unicode
    :param script_content: Scenario content to execute
    :type script_content: str or unicode
    :param node_commit: Commit or branch name for MZBench node, default is "master"
    :type node_commit: str
    :param nodes: Number of nodes to allocate or node list, 1 by default
    :type nodes: int or list of strings
    :param workers_per_node: Number of workers to start on one node
    :type workers_per_node: int
    :param deallocate_after_bench: Deallocate nodes after bench is over
    :type deallocate_after_bench: "true" or "false"
    :param provision_nodes: Install required software
    :type provision_nodes: "true" or "false"
    :param exclusive_node_usage: Allocate exclusive nodes if allocator supports this mode
    :type exclusive_node_usage: "true" or "false"
    :param benchmark_name: Set benchmark name
    :type benchmark_name: str or unicode
    :param cloud: Specify cloud provider to use
    :type cloud: str or unicode
    :param emails: Emails to notify on bench results
    :type emails: List of strings
    :param env: Dictionary of environment variables to substitute
    :type env: Dictionary
    :returns: Operation status
    :rtype: Dictionary
    """
    import erl_utils
    import math

    script_terms = erl_utils.convert(script_content, env)
    includes = erl_utils.get_includes(script_terms)

    if workers_per_node is not None:
        desired_num_nodes = int(math.ceil(float(erl_utils.get_num_of_workers(script_terms))/float(workers_per_node)))
    else:
        desired_num_nodes = None

    if nodes is not None:
        if isinstance(nodes, int):
            params = [('nodes', desired_num_nodes if desired_num_nodes is not None else nodes)]
        else:
            params = [('nodes', ','.join(nodes[:desired_num_nodes] if desired_num_nodes is not None else nodes))]
    else:
        params = [] if desired_num_nodes is None else [('nodes', desired_num_nodes)]

    if deallocate_after_bench is not None:
        params += [('deallocate_after_bench', deallocate_after_bench)]
    if provision_nodes is not None:
        params += [('provision_nodes', provision_nodes)]
    if exclusive_node_usage is not None:
        params += [('exclusive_node_usage', exclusive_node_usage)]
    if benchmark_name is not None:
        params += [('benchmark_name', benchmark_name)]
    if cloud is not None:
        params += [('cloud', cloud)]
    if node_commit is not None:
        params += [('node_commit', node_commit)]
    
    params += [('email', email) for email in emails]
    params += [(k, v) for k, v in env.iteritems()]

    files = [('bench',
        {'filename': os.path.basename(script_file),
         'content': script_content})]

    for (incname, incurl) in includes:
        script_dir = os.path.dirname(script_file)

        if not re.search(r'^https?://', incurl, re.IGNORECASE):
            filename = os.path.join(script_dir, incurl)
            try:
                with open(filename) as fi:
                    files.append(('include',
                        {'filename': incurl, 'content': fi.read()}))
            except IOError as e:
                print >>sys.stderr, "Failed to get content for resource ({0}, {1}): {2}".format(
                        incname, incurl, e)
                raise

    body, headers = multipart.encode_multipart({}, files)

    return assert_successful_post(
        host,
        '/start',
        params,
        data=body, headers=headers)


def restart(host, bench_id):
    """Creates a copy of a bench

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id to copy
    :type bench_id: int
    :returns: operation status
    :rtype: dict
    """
    return assert_successful_get(host, '/restart', {'id': bench_id})


def log(host, bench_id):
    """Outputs log for a bench

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :returns: log
    :rtype: generator of str
    """
    for x in stream_lines(host, '/log', {'id': bench_id}):
        yield x


def userlog(host, bench_id):
    """Outputs user log for a bench

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :returns: log
    :rtype: generator of str
    """
    for x in stream_lines(host, '/userlog', {'id': bench_id}):
        yield x


def change_env(host, bench_id, env):
    """Changes environment variables for existing benchmark on the fly

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :param env: Dictionary of environment variables to substitute
    :type env: Dictionary
    """
    env['id'] = bench_id
    return assert_successful_get(host, '/change_env', env)


def data(host, bench_id):
    """Outputs CSV data for a bench

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :returns: CSV data
    :rtype: generator of str
    """
    for x in stream_lines(host, '/data', {'id': bench_id}):
        yield x


def status(host, bench_id, wait=False):
    """Get bench status

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :returns: benchmark status
    :rtype: dict
    """
    return assert_successful_get(
        host,
        '/status',
        {'id': bench_id,
         'wait': 'true' if wait else 'false'})


def stop(host, bench_id):
    """Stop a bench

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :returns: operation status
    :rtype: dict
    """
    return assert_successful_get(
        host,
        '/stop',
        {'id': bench_id})


def stream_lines(host, endpoint, args):
    try:
        response = requests.get(
            'http://' + host + endpoint + '?' + urlencode(args),
            stream=True)

        for line in fast_iter_lines(response, chunk_size=1024):
            try:
                yield line
            except ValueError:
                print line

        if response.status_code == 200:
            pass
        else:
            raise MZBenchAPIException('Server call to {0} failed with code {1}'.format(endpoint, response.status_code))

    except requests.exceptions.ConnectionError as e:
        raise MZBenchAPIException('Connect to "{0}" failed with message: {1}'.format(host, e))


def fast_iter_lines(response, chunk_size=512):
        pending = None

        for chunk in response.iter_content(chunk_size=chunk_size):

            lines = chunk.splitlines()

            if pending is not None:
                if lines:
                    lines[0] = pending + lines[0]
                else:
                    lines.append(pending)

            if lines and lines[-1] and chunk and lines[-1][-1] == chunk[-1]:
                pending = lines.pop()
            else:
                pending = None

            for line in lines:
                yield line

        if pending is not None:
            yield pending


def assert_successful_request(perform_request):
    def wrapped(*args, **kwargs):
        try:
            response = perform_request(*args, **kwargs)
            if response.status_code == 200:
                return response.json()
            else:
                try:
                    data = json.loads(response.text)
                except:
                    raise MZBenchAPIException('Server call with arguments {0} failed with code {1} respose body:\n{2}'.format(args, response.status_code, response.text))

                if ('reason_code' in data and 'reason' in data):
                    raise MZBenchAPIException('Server call with arguments {0} failed with code {1} and reason: {2}\n{3}'.format(args, response.status_code, data['reason_code'], data['reason']))
                else:
                    from StringIO import StringIO
                    io = StringIO()
                    json.dump(data, io, indent=4)
                    raise MZBenchAPIException('Server call with arguments {0} failed with code {1} respose body:\n{2}'.format(args, response.status_code, io.getvalue()))

        except requests.exceptions.ConnectionError as e:
            raise MZBenchAPIException('Connect to "{0}" failed with message: {1}'.format(args[0], e))
    return wrapped


@assert_successful_request
def assert_successful_get(host, endpoint, args):
    return requests.get(
        'http://' + host + endpoint + '?' + urlencode(args))


@assert_successful_request
def assert_successful_post(host, endpoint, args, data=None, headers=None):
    return requests.post(
        'http://' + host + endpoint + '?' + urlencode(args),
        data=data,
        headers=headers)

