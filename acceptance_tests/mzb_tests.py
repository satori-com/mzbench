
import os
import sys
import shlex
import subprocess
import time
import json
import re
from contextlib import contextmanager
from binascii import hexlify
from operator import xor

import nose
import nose.plugins.multiprocess

dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

from util import cmd

mz_bench_dir = os.path.dirname(os.path.realpath(__file__)) + '/../'
scripts_dir = mz_bench_dir + 'acceptance_tests/scripts/'
mzbench_script = mz_bench_dir + 'bin/mzbench'


def worker_from_git_test():
    run_successful_bench(scripts_dir + 'worker_from_git.erl')


def correct_test():
    run_successful_bench(scripts_dir + 'correct_script.erl')


def emulate_crash_test():
    run_failing_bench(scripts_dir + 'correct_script.erl', env={'emulate_bench_crash': 'true'})

def semantic_error_test():
    run_failing_bench(scripts_dir + 'semantic_error.erl')


def runtime_error_test():
    run_failing_bench(scripts_dir + 'runtime_error.erl')


def signal_test():
    run_successful_bench(mz_bench_dir + 'examples/signal.erl', env={})


def signal_count_test():
    run_successful_bench(mz_bench_dir + 'examples/signal_count.erl', env={})


def assertions_succ_test():
    run_successful_bench(mz_bench_dir + 'examples/assertions.erl', env={})


def assertions_fail_test():
    run_failing_bench(mz_bench_dir + 'examples/assertions_fail.erl', env={})


def env_test():
    run_successful_bench(scripts_dir + 'env.erl', env={
        'jozin': 'bazin',
        'wait_ms': '10',
        'pool_size': '2',
        'loop_time': '5',
        'loop_rate': '2'})


def env_param_missing_test():
    run_failing_bench(scripts_dir + 'env.erl', env={})

def unicode_resources_test():
    run_successful_bench(scripts_dir + 'unicode_resource.erl',
        env={'strings_filename':'unicode_strings.txt'})


def data_endpoint_test():
    bench_id = run_successful_bench(scripts_dir + 'data_script.erl')

    csv_data_process = subprocess.Popen(
        [mzbench_script,
            '--host=localhost:4800',
            '--format=csv',
            'data',
            str(bench_id)],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)

    json_data_process = subprocess.Popen(
        [mzbench_script,
            '--host=localhost:4800',
            '--format=json',
            'data',
            str(bench_id)],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)

    log_process = subprocess.Popen(
        [mzbench_script,
            '--host=localhost:4800',
            'log',
            str(bench_id)],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)

    lout, lerr = log_process.communicate()
    print 'Log collector stdout'
    print lout
    print 'Log collector stderr'
    print lerr

    csv_out, csv_err = csv_data_process.communicate()
    print 'CSV data collector stdout'
    print csv_out
    print 'CSV data collector stderr'
    print csv_err

    json_out, json_err = json_data_process.communicate()
    print 'JSON data collector stdout'
    print json_out
    print 'JSON data collector stderr'
    print json_err

    time.sleep(3)

    csv_data_ret_code = csv_data_process.poll()
    json_data_ret_code = json_data_process.poll()

    assert csv_data_ret_code == 0
    assert json_data_ret_code == 0
    assert 'mzb.print.value,' in csv_out
    assert 'mzb.print.value' in\
        [metric['target'] for metric in json.loads(json_out)]

def restart_test():
    bench_id = run_successful_bench(scripts_dir + 'correct_script.erl')
    restarted_id = restart_bench(bench_id)
    cmd(mz_bench_dir + 'bin/mzbench status --wait {0}'.format(restarted_id))

def signal_deadlock_test():
    run_failing_bench(scripts_dir + 'signals_deadlock.erl')

def nobody_waits_for_signal_test():
    run_failing_bench(scripts_dir + 'signals_deadlock2.erl')

def nobody_sets_signal_test():
    run_failing_bench(scripts_dir + 'signals_deadlock3.erl')

def loop_without_rate_test():
    run_successful_bench(scripts_dir + 'superloop.erl')

def devtool_run_local_tests():
    cmd(mz_bench_dir + 'bin/mzbench validate loop_rate.erl')

    cmd(mz_bench_dir + 'bin/mzbench run_local loop_rate.erl')

    cmd(mz_bench_dir + 'bin/mzbench run_local data_script.erl')

    try:
        cmd(mz_bench_dir + 'bin/mzbench run_local syntax_error.erl')
        assert False
    except subprocess.CalledProcessError:
        pass

    try:
        cmd(mz_bench_dir + 'bin/mzbench run_local semantic_error.erl')
        assert False
    except subprocess.CalledProcessError:
        pass


def devtool_list_templates_test():
    templates = os.listdir(mz_bench_dir + 'worker-templates')
    got_templates = filter(
        lambda x: x,
        cmd(mz_bench_dir + 'bin/mzbench list_templates').split('\n'))
    if sorted(templates) != sorted(got_templates):
        print sorted(templates)
        print sorted(got_templates)
        assert sorted(templates) == sorted(got_templates)


def run_successful_bench(name, nodes=None, env={}, email=None,
        exclusive_node_usage=False):
    return run_bench(name, should_fail=False,
        nodes=nodes, env=env, email=email,
        exclusive_node_usage=exclusive_node_usage)


def run_failing_bench(name, nodes=None, env={}, email=None,
        exclusive_node_usage=False):
    return run_bench(name, should_fail=True,
        nodes=nodes, env=env,
        exclusive_node_usage=exclusive_node_usage)


def run_bench(name=None, worker_package_with_default_scenario=None, nodes=None,
        env={}, email=None, should_fail=False, max_retries=2,
        exclusive_node_usage=False):

    email_option = ('--email=' + email) if email else ''

    if nodes:
        nodes_option = '--nodes ' + ','.join(nodes)
    else:
        nodes_option = '--nodes 1'

    env_option = ' '.join(('--env={0}={1}'.format(k, v)
        for k, v in env.iteritems()))

    def run():
        node_commit = os.environ.get('NODE_COMMIT', 'master')

        flags = ' '.join([
            '--host=localhost:4800',
            '--exclusive_node_usage=' + ('true' if exclusive_node_usage else 'false'),
            '--node_commit={0}'.format(node_commit),
            nodes_option,
            env_option,
            email_option])

        if name is not None:
            invocation = mz_bench_dir + 'bin/mzbench ' + flags + ' start ' + name
        elif worker_package_with_default_scenario is not None:
            invocation = mz_bench_dir + 'bin/mzbench ' + flags + ' start_default_scenario_of_worker ' + worker_package_with_default_scenario
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
            mz_bench_dir + 'bin/mzbench --host=localhost:4800 status --wait {0}'.format(bench_id)),
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
            return bench_id

        print 'Attempt #{0} for bench-id {1} unexpectedly {2}, retrying.'.format(attempt, bench_id, 'succeeded' if should_fail else 'failed')
        attempt += 1

    if (max_retries <= attempt):
        print('All {0} attempts failed'.format(max_retries))
        print('Log of the last attempt (bench {0}):'.format(bench_id))

        if bench_id is not None:
            log_cmd = mz_bench_dir + 'bin/mzbench --host=localhost:4800 log {0}'.format(bench_id)
            print cmd(log_cmd).replace('\\n', '\n')

        raise RuntimeError('BenchId {0} for test {1} unexpectedly {2}'.format(
                bench_id, name, 'succeeded' if should_fail else 'failed'))


def restart_bench(bench_id):
    restart = subprocess.Popen(
        [mz_bench_dir + 'bin/mzbench',
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
