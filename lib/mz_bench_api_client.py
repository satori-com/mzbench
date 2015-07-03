
from urllib import urlencode
import json
import os
import sys

import requests

import multipart

def start(host, script_file, script_content,
        emails=[], node_commit='master',
        nodes=1, deallocate_after_bench='true', dont_provision_nodes='false', exclusive_node_usage='true', 
        includes=[], env={}):

    if isinstance(nodes, int):
        params = [('nodes', nodes)]
    else:
        params = [('nodes', ','.join(nodes))]

    params += [('deallocate_after_bench', deallocate_after_bench)]
    params += [('dont_provision_nodes', dont_provision_nodes)]
    params += [('exclusive_node_usage', exclusive_node_usage)]
    params += [('email', email) for email in emails]
    params += [('node_commit', node_commit)]
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
    return assert_successful_get(host, '/restart', {'id': bench_id})


def logs(host, bench_id):
    for x in stream_lines(host, '/logs', {'id': bench_id}):
        yield x


def data(host, bench_id):
    for x in stream_lines(host, '/data', {'id': bench_id}):
        yield x


def status(host, bench_id, wait=False):
    return assert_successful_get(
        host,
        '/status',
        {'id': bench_id,
         'wait': 'true' if wait else 'false'})


def stop(host, bench_id):
    return assert_successful_get(
        host,
        '/stop',
        {'id': bench_id})


def stream_lines(host, endpoint, args):
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
        print 'Server call to {0} failed with code {1}'.format(endpoint, response.status_code)
        sys.exit(3)


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
                    print response.text
                sys.exit(2)
        except requests.exceptions.ConnectionError as e:
            print 'Connection failure: {0}'.format(e)
            sys.exit(4)
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

