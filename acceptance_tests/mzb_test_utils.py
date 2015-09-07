
import os
import re
import sys
import shlex
import subprocess
import time
import json
from operator import xor
from contextlib import contextmanager

dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

from util import cmd

mzbench_dir = dirname + '/../'
scripts_dir = mzbench_dir + 'acceptance_tests/scripts/'
mzbench_script = mzbench_dir + 'bin/mzbench'

@contextmanager
def start_mzbench_server():
    if 'MZBENCH_RSYNC' in os.environ:
        node_location_param = '{{mzbench_rsync, "{0}"}}'.format(os.environ['MZBENCH_RSYNC'])
    elif 'MZBENCH_REPO' in os.environ:
        node_location_param = '{{mzbench_git, "{0}"}}'.format(os.environ['MZBENCH_REPO'])
    else:
        node_location_param = ''

    with open(dirname + "/test_server.config", "w") as config:
        config.write('[{{mzbench_api, [{0}]}}].'.format(node_location_param))

    with open('{0}/test_server.config'.format(dirname), 'r') as f:
        print(f.read())

    cmd('{0} start_server --config {1}/test_server.config'.format(mzbench_script, dirname))
    try:
        time.sleep(3) # give server some time to start
        yield
    except:
        print ''
        print '-------------------- >> begin server logs << ---------------------'
        logdir = os.path.join(mzbench_dir + 'server/_build/default/rel/mzbench_api/log')
        logfiles = [logfile for logfile in os.listdir(logdir)]
        logfile = sorted([os.path.join(logdir, l) for l in logfiles if l.startswith('erlang')], key=os.path.getmtime, reverse=True)[0]
        with open(logfile) as f:
            for line in f:
                print line.rstrip().replace('\\n', '\n')
        print '-------------------- >> end server logs   << ---------------------'
        print ''
        raise
    finally:
        cmd('{0} stop_server'.format(mzbench_script))

def run_successful_bench(name, nodes=None, workers_per_node=None, env={}, 
        email=None, exclusive_node_usage=False, expected_log_message_regex=None):
    return run_bench(name, should_fail=False,
        nodes=nodes, workers_per_node=workers_per_node, env=env, email=email,
        exclusive_node_usage=exclusive_node_usage,
        expected_log_message_regex=expected_log_message_regex)


def run_failing_bench(name, nodes=None, workers_per_node=None, env={}, 
        email=None, exclusive_node_usage=False, expected_log_message_regex=None):
    return run_bench(name, should_fail=True,
        nodes=nodes, workers_per_node=workers_per_node, env=env,
        exclusive_node_usage=exclusive_node_usage,
        expected_log_message_regex=expected_log_message_regex)


def run_bench(name=None, worker_package_with_default_scenario=None, nodes=None, 
        workers_per_node=None, env={}, email=None, should_fail=False, max_retries=2,
        exclusive_node_usage=False, expected_log_message_regex=None):

    email_option = ('--email=' + email) if email else ''

    if workers_per_node:
        nodes_option = '--workers_per_node ' + str(workers_per_node)
    else:
        if nodes:
            nodes_option = '--nodes ' + ','.join(nodes)
        else:
            nodes_option = '--nodes 1'

    env_option = ' '.join(('--env={0}={1}'.format(k, v)
        for k, v in env.iteritems()))

    def run():
        if 'NODE_COMMIT' in os.environ:
            node_commit_arg = '--node_commit={0}'.format(os.environ['NODE_COMMIT'])
        else:
            node_commit_arg = ''

        flags = ' '.join([
            '--host=localhost:4800',
            '--exclusive_node_usage=' + ('true' if exclusive_node_usage else 'false'),
            node_commit_arg,
            nodes_option,
            env_option,
            email_option])

        if name is not None:
            invocation = mzbench_dir + 'bin/mzbench ' + flags + ' start ' + name
        elif worker_package_with_default_scenario is not None:
            invocation = mzbench_dir + 'bin/mzbench ' + flags + ' start_default_scenario_of_worker ' + worker_package_with_default_scenario
        else:
            raise RuntimeError('Neither script filename nor default scenario package provided.')

        start = subprocess.Popen(shlex.split(invocation.encode('ascii')),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)
        start_out, start_err = start.communicate()

        try:
            bench_id = json.loads(start_out)['id']
        except Exception:
            print 'mzbench returned invalid json: \nCommand: {0}\nOutput: {1}\nStderr: {2}'.format(invocation, start_out, start_err)
            raise

        wait = subprocess.Popen(shlex.split(
            mzbench_dir + 'bin/mzbench --host=localhost:4800 status --wait {0}'.format(bench_id)),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)
        wait.communicate()

        return (bench_id, wait.returncode == 0)

    attempt = 0

    while attempt < max_retries:

        print 'Attempt #{0}'.format(attempt)

        try:
            (bench_id, success) = run()
        except Exception as e:
            print "Unexpected error: {0}".format(e)
            bench_id, success = (None, False)

        if xor(success, should_fail):
            if expected_log_message_regex:
                log_cmd = mzbench_dir + 'bin/mzbench --host=localhost:4800 log {0}'.format(bench_id)
                log = cmd(log_cmd)
                if isinstance(expected_log_message_regex, str):
                    regex = re.compile(expected_log_message_regex, re.S)
                else:
                    regex = expected_log_message_regex
                if not regex.search(log):
                    print
                    print "Log doesn't contain expected log message '{0}':".format(regex.pattern)
                    print
                    print log
                    raise RuntimeError
            return bench_id

        print 'Attempt #{0} for bench-id {1} unexpectedly {2}, retrying.'.format(attempt, bench_id, 'succeeded' if should_fail else 'failed')
        attempt += 1

    if (max_retries <= attempt):
        print('All {0} attempts failed'.format(max_retries))
        print('Log of the last attempt (bench {0}):'.format(bench_id))

        if bench_id is not None:
            log_cmd = mzbench_dir + 'bin/mzbench --host=localhost:4800 log {0}'.format(bench_id)
            print cmd(log_cmd).replace('\\n', '\n')

        raise RuntimeError('BenchId {0} for test {1} unexpectedly {2}'.format(
                bench_id, name, 'succeeded' if should_fail else 'failed'))


def restart_bench(bench_id):
    restart = subprocess.Popen(
        [mzbench_dir + 'bin/mzbench',
            '--host=localhost:4800',
            'restart',
            str(bench_id)],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    restart_out, restart_err = restart.communicate()

    try:
        return json.loads(restart_out)['id']
    except Exception:
        print 'mzbench restart returned invalid json:\nOutput: {0}\nStderr: {1}'.format(restart_out, restart_err)
        raise
