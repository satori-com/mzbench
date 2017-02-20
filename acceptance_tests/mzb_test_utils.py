
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
def start_mzbench_server(custom_data_location=None):
    if 'MZBENCH_RSYNC' in os.environ:
        node_location_param = '{{node_rsync, "{0}"}},'.format(os.environ['MZBENCH_RSYNC'])
    elif 'MZBENCH_REPO' in os.environ:
        node_location_param = '{{node_git, "{0}"}},'.format(os.environ['MZBENCH_REPO'])
    else:
        node_location_param = ''

    if custom_data_location:
        custom_data_location_param = '{{bench_data_dir, "{0}"}},'.format(custom_data_location)
    else:
        custom_data_location_param = ''

    with open(dirname + "/test_server.config", "w") as config:
        config.write('[{{mzbench_api, [{0} {1} {{node_log_port, 0}}, {{node_log_user_port, 0}}, {{node_management_port, 0}}, {{node_interconnect_port, 0}}]}}].'
                     .format(node_location_param, custom_data_location_param))

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
        email=None, expected_log_message_regex=None,
        check_log_function=None, check_user_log_function=None, post_start=None):
    return run_bench(name, should_fail=False,
        nodes=nodes, workers_per_node=workers_per_node, env=env, email=email,
        expected_log_message_regex=expected_log_message_regex,
        check_log_function=check_log_function,
        check_user_log_function=check_user_log_function, post_start=post_start)


def run_failing_bench(name, nodes=None, workers_per_node=None, env={},
        email=None, expected_log_message_regex=None,
        check_log_function=None, check_user_log_function=None, post_start=None):
    return run_bench(name, should_fail=True,
        nodes=nodes, workers_per_node=workers_per_node, env=env,
        expected_log_message_regex=expected_log_message_regex,
        check_log_function=check_log_function,
        check_user_log_function=check_user_log_function, post_start=post_start)


def run_bench(name=None, worker_package_with_default_scenario=None, nodes=None,
        workers_per_node=None, env={}, email=None, should_fail=False, max_retries=2,
        expected_log_message_regex=None,
        check_log_function=None, check_user_log_function=None, post_start=None):

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
        if 'worker_branch' in env:
            node_commit_arg = '--node_commit={0}'.format(env['worker_branch'])
        else:
            node_commit_arg = ''

        flags = ' '.join([
            '--host=localhost:4800',
            node_commit_arg,
            nodes_option,
            env_option,
            email_option])

        if name is not None:
            invocation = mzbench_dir + 'bin/mzbench ' + flags + ' start ' + name
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

        if (post_start is not None) and wait_status(bench_id, 'running', 240):
            print "Calling post start for {0}".format(bench_id)
            post_start(bench_id)

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
            if not expected_log_message_regex and not check_log_function and not check_user_log_function:
                # no need to check the log
                return bench_id

            log_cmd = mzbench_dir + 'bin/mzbench --host=localhost:4800 log {0}'.format(bench_id)
            log = cmd(log_cmd)

            if expected_log_message_regex:
                if isinstance(expected_log_message_regex, str) or isinstance(expected_log_message_regex, unicode):
                    regex = re.compile(expected_log_message_regex, re.DOTALL + re.UNICODE)
                else:
                    regex = expected_log_message_regex

                if not regex.search(log):
                    print
                    print u"Log doesn't contain expected log message '{0}':\n".format(regex.pattern)
                    print log
                    raise RuntimeError

            if check_log_function:
                maybe_error = check_log_function(log)

                if maybe_error:
                    print
                    print "Log doesn't pass custom check:\n{0}\n\n".format(maybe_error)
                    print log
                    raise RuntimeError

            if check_user_log_function:
                log_cmd = mzbench_dir + 'bin/mzbench --host=localhost:4800 userlog {0}'.format(bench_id)
                log = cmd(log_cmd)

                maybe_error = check_user_log_function(log)

                if maybe_error:
                    print
                    print "Log doesn't pass custom check:\n{0}\n\n".format(maybe_error)
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

def start_simple_bench(name, additional):
    restart = subprocess.Popen(
        [mzbench_dir + 'bin/mzbench',
            '--host=localhost:4800',
            'start',
            name, additional],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    restart_out, restart_err = restart.communicate()

    try:
        return json.loads(restart_out)['id']
    except Exception:
        print 'mzbench restart returned invalid json:\nOutput: {0}\nStderr: {1}'.format(restart_out, restart_err)
        raise

def wait_status(bench_id, status, n):
    if n <= 0:
        print 'ERROR: Wait for status "running" has timed out!'
        return False

    wait = subprocess.Popen(shlex.split(
        mzbench_dir + 'bin/mzbench --host=localhost:4800 status {0}'.format(bench_id)),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    out, err = wait.communicate()

    try:
        current_status = json.loads(out)['status']
    except Exception:
        print 'mzbench status returned invalid json: \nOutput: {0}\nStderr: {1}'.format(out, err)
        raise

    print "current_status: {0}".format(current_status)
    if current_status == status:
        return True
    elif current_status == 'failed':
        return False
    elif current_status == 'complete':
        return False
    else:
        time.sleep(5)
        return wait_status(bench_id, status, n - 1)
