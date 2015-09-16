
from urllib import urlencode
import json
import os
import sys
import requests
import multipart

class MZBenchAPIException(Exception):
    pass

def start(host, script_file, script_content,
          node_commit = None, nodes = None, deallocate_after_bench = None,
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
    :param includes: List of files to include
    :type includes: List of strings
    :param env: Dictionary of environment variables to substitute
    :type env: Dictionary
    :returns: Operation status
    :rtype: Dictionary
    """

    if nodes is not None:
        if isinstance(nodes, int):
            params = [('nodes', nodes)]
        else:
            params = [('nodes', ','.join(nodes))]
    else:
        params = []

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

    for inc in includes:
        script_dir = os.path.dirname(script_file)
        filename = os.path.join(script_dir, inc)
        try:
            with open(filename) as fi:
                files.append(
                    ('include', {'filename': inc, 'content': fi.read()}))
        except IOError:
            print "Warning: resource file '%s' is not found on the local machine" % filename

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
    :type host: int
    :returns: operation status
    :rtype: dict
    """
    return assert_successful_get(host, '/restart', {'id': bench_id})


def logs(host, bench_id):
    """Outputs logs for a bench

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type host: int
    :returns: logs
    :rtype: generator of str
    """
    for x in stream_lines(host, '/logs', {'id': bench_id}):
        yield x


def data(host, bench_id):
    """Outputs CSV data for a bench

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type host: int
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
    :type host: int
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
    :type host: int
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

        for line in response.iter_lines(chunk_size=1):
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


def assert_successful_request(perform_request):
    def wrapped(*args, **kwargs):
        try:
            response = perform_request(*args, **kwargs)
            if response.status_code == 200:
                return response.json()
            else:
                print 'Server call with arguments {0} failed with code {1}'.format(args, response.status_code)
                print 'Response body:'
                try:
                    data = json.loads(response.text)
                    json.dump(data, sys.stdout, indent=4)
                except:
                    raise MZBenchAPIException(response.text)
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

