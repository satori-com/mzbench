
from __future__ import print_function

try:
    from urllib.parse import urlencode
except ImportError:
    from urllib import urlencode

import json
import os
import sys
import re
import requests
import multipart
import string

class MZBenchAPIException(Exception):
    pass

def start(host, script_file, script_content,
          node_commit = None, nodes = None, workers_per_node = None, deallocate_after_bench = None,
          provision_nodes = None, benchmark_name = None,
          cloud = None, tags = None, emails=[], includes=[], env={}, no_cert_check = False
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
    :param benchmark_name: Set benchmark name
    :type benchmark_name: str or unicode
    :param cloud: Specify cloud provider to use
    :type cloud: str or unicode
    :param tags: Benchmark tags
    :type tags: str
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    :param emails: Emails to notify on bench results
    :type emails: List of strings
    :param env: Dictionary of environment variables to substitute
    :type env: Dictionary
    :returns: Operation status
    :rtype: Dictionary
    """
    import erl_utils
    import bdl_utils
    import math

    script_utils = bdl_utils if bdl_utils.is_bdl_scenario(script_content) else erl_utils

    script_terms = script_utils.convert(script_content, env)
    includes = script_utils.get_includes(script_terms)

    if workers_per_node is not None:
        desired_num_nodes = int(math.ceil(float(script_utils.get_num_of_workers(script_terms))/float(workers_per_node)))
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
    if benchmark_name is not None:
        params += [('benchmark_name', benchmark_name)]
    if cloud is not None:
        params += [('cloud', cloud)]
    if tags is not None:
        params += [('tags', tags)]
    if node_commit is not None:
        params += [('node_commit', node_commit)]

    params += [('email', email) for email in emails]
    params += [(k, v) for k, v in env.items()]

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
                print("Failed to get content for resource ({0}, {1}): {2}".format(
                        incname, incurl, e), file=sys.stderr)
                raise

    body, headers = multipart.encode_multipart({}, files)

    return assert_successful_post(
        host,
        '/start',
        params,
        data=body, headers=headers, no_cert_check = no_cert_check)


def restart(host, bench_id, no_cert_check = False):
    """Creates a copy of a bench

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id to copy
    :type bench_id: int
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    :returns: operation status
    :rtype: dict
    """
    return assert_successful_get(host, '/restart', {'id': bench_id})


def log(host, bench_id, no_cert_check = False):
    """Outputs log for a bench

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    :returns: log
    :rtype: generator of str
    """
    for x in stream_lines(host, '/log', {'id': bench_id}, no_cert_check = no_cert_check):
        yield x


def userlog(host, bench_id, no_cert_check = False):
    """Outputs user log for a bench

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    :returns: log
    :rtype: generator of str
    """
    for x in stream_lines(host, '/userlog', {'id': bench_id}, no_cert_check = no_cert_check):
        yield x


def change_env(host, bench_id, env, no_cert_check = False):
    """Changes environment variables for existing benchmark on the fly

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    :param env: Dictionary of environment variables to substitute
    :type env: Dictionary
    """
    env['id'] = bench_id
    return assert_successful_get(host, '/change_env', env, no_cert_check = no_cert_check)


def data(host, bench_id, no_cert_check = False):
    """Outputs CSV data for a bench

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    :returns: CSV data
    :rtype: generator of str
    """
    for x in stream_lines(host, '/data', {'id': bench_id}, no_cert_check = no_cert_check):
        yield x


def status(host, bench_id, wait=False, no_cert_check = False):
    """Get bench status

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    :returns: benchmark status
    :rtype: dict
    """
    return assert_successful_get(
        host,
        '/status',
        {'id': bench_id,
         'wait': 'true' if wait else 'false'}, no_cert_check = no_cert_check)


def results(host, bench_id, wait=False, no_cert_check = False):
    """Get bench results

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    :returns: benchmark resulting metric values
    :rtype: dict
    """
    return assert_successful_get(
        host,
        '/results',
        {'id': bench_id,
         'wait': 'true' if wait else 'false'}, no_cert_check = no_cert_check)


def stop(host, bench_id, no_cert_check = False):
    """Stop a bench

    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    :returns: operation status
    :rtype: dict
    """
    return assert_successful_get(
        host,
        '/stop',
        {'id': bench_id}, no_cert_check = no_cert_check)

def clusters_info(host, no_cert_check = False):
    """Get info about currenlty allocated clusters

    :param host: MZBench API server host with port
    :type host: str
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    """
    return assert_successful_get(host, '/clusters_info', {}, no_cert_check = no_cert_check)

def deallocate_cluster(host, cluster_id, no_cert_check = False):
    """Deallocate cluster

    :param host: MZBench API server host with port
    :type host: str
    :param cluster_id: id of target cluster
    :type cluster_id: int
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    """

    return assert_successful_get(host, '/deallocate_cluster', {'id': cluster_id}, no_cert_check = no_cert_check)

def remove_cluster_info(host, cluster_id, no_cert_check = False):
    """Remove cluster record from the table of current allocated cluster info

    :param host: MZBench API server host with port
    :type host: str
    :param cluster_id: id of target cluster
    :type cluster_id: int
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    """

    return assert_successful_get(host, '/remove_cluster_info', {'id': cluster_id}, no_cert_check = no_cert_check)

def add_tags(host, bench_id, tags, no_cert_check = False):
    """Add tags to an existing benchmark
    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :param tags: Tags to add
    :type tags: str
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    """

    return assert_successful_get(host, '/add_tags', {'id': bench_id, 'tags': tags}, no_cert_check = no_cert_check)

def remove_tags(host, bench_id, tags, no_cert_check = False):
    """Remove tags from an existing benchmark
    :param host: MZBench API server host with port
    :type host: str
    :param bench_id: benchmark run id
    :type bench_id: int
    :param tags: Tags to remove
    :type tags: str
    :param no_cert_check: Don't check server HTTPS certificate
    :type no_cert_check: boolean
    """

    return assert_successful_get(host, '/remove_tags', {'id': bench_id, 'tags': tags}, no_cert_check = no_cert_check)


def addproto(host):
    if host.startswith("http://") or host.startswith("https://"):
        return host
    return "http://" + host

def stream_lines(host, endpoint, args, no_cert_check = False):
    try:
        response = requests.get(
            addproto(host) + endpoint + '?' + urlencode(args),
            stream=True, verify = not no_cert_check, headers=get_auth_headers())

        for line in fast_iter_lines(response, chunk_size=1024):
            try:
                yield line
            except ValueError:
                print(line)

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
def assert_successful_get(host, endpoint, args, no_cert_check = False):
    return requests.get(
        addproto(host) + endpoint + '?' + urlencode(args),
        verify=not no_cert_check, headers=get_auth_headers())


@assert_successful_request
def assert_successful_post(host, endpoint, args, data=None, headers=None, no_cert_check = False):
    return requests.post(
        addproto(host) + endpoint + '?' + urlencode(args),
        data=data,
        headers=add_auth_headers(headers),
        verify=not no_cert_check)

def add_auth_headers(headers):
    auth_headers = get_auth_headers();
    if (headers is None):
        return auth_headers;

    if (auth_headers is None):
        return headers;

    headers.update(auth_headers)
    return headers

def get_auth_headers():
    token_file = os.path.expanduser("~/.config/mzbench/token")
    if (os.path.isfile(token_file)):
        with open(token_file) as f:
            token = f.read()
            return {"Authorization": "Bearer {}".format(string.rstrip(token, " \n\r"))}
    else:
        return None
