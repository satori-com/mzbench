#!/usr/bin/env python

import os
import sys
import subprocess
import time
import json
import nose
import re

dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

from util import cmd

from mzb_test_utils import run_successful_bench, restart_bench, start_mzbench_server

mzbench_dir = dirname + '/../'
scripts_dir = mzbench_dir + 'acceptance_tests/scripts/'
scripts_bdl_dir = mzbench_dir + 'acceptance_tests/scripts.bdl/'
mzbench_script = mzbench_dir + 'bin/mzbench'

def correct_test():
    run_successful_bench(scripts_dir + 'correct_script.erl')
    run_successful_bench(scripts_bdl_dir + 'correct_script.bdl')


def worker_from_rsync_test():
    env = {'exec_worker_dir': os.path.abspath('../workers/exec') + '/'}
    run_successful_bench(scripts_dir + 'worker_from_rsync.erl', env=env)
    run_successful_bench(scripts_bdl_dir + 'worker_from_rsync.bdl', env=env)


def lua_worker_from_git_test():
    worker_commit = os.environ.get('NODE_COMMIT', 'master')
    mzbench_repo = os.environ.get('MZBENCH_REPO', 'https://github.com/machinezone/mzbench')
    env = {'worker_branch': worker_commit,
             'mzbench_repo':  mzbench_repo}
    run_successful_bench(scripts_dir + 'lua_worker_from_git.erl', env=env)
    run_successful_bench(scripts_bdl_dir + 'lua_worker_from_git.bdl', env=env)


def python_worker_from_git_test():
    worker_commit = os.environ.get('NODE_COMMIT', 'master')
    mzbench_repo = os.environ.get('MZBENCH_REPO', 'https://github.com/machinezone/mzbench')
    env = {'worker_branch': worker_commit,
             'mzbench_repo':  mzbench_repo}
    run_successful_bench(scripts_dir + 'python_worker_from_git.erl', env=env)
    run_successful_bench(scripts_bdl_dir + 'python_worker_from_git.bdl', env=env)


def worker_from_git_test():
    # worker is located in the same repo as node
    worker_commit = os.environ.get('NODE_COMMIT', 'master')
    mzbench_repo = os.environ.get('MZBENCH_REPO', 'https://github.com/machinezone/mzbench')
    env = {'worker_branch': worker_commit,
             'mzbench_repo':  mzbench_repo}
    run_successful_bench(scripts_dir + 'worker_from_git.erl', env=env)
    run_successful_bench(scripts_bdl_dir + 'worker_from_git.bdl', env=env)


def poisson_loop_test():
    run_successful_bench(scripts_dir + 'loop_poisson.erl')
    run_successful_bench(scripts_bdl_dir + 'loop_poisson.bdl')


def env_test():
    env = {
        'jozin': 'bazin',
        'wait_ms': '10',
        'pool_size': '2',
        'loop_time': '5',
        'loop_rate': '2'}
    run_successful_bench(scripts_dir + 'env.erl', env=env)
    run_successful_bench(scripts_bdl_dir + 'env.bdl', env=env)


def vars_defaults_test():
    def check_log(log):
        regexp1 = re.compile('the_var1_value_is_var1_default_value', re.DOTALL + re.UNICODE)
        regexp2 = re.compile('the_var2_value_is_var2_new_value', re.DOTALL + re.UNICODE)
        
        if regexp1.search(log) and regexp2.search(log):
            return False
        else:
            return 'Unable to find correct values in log'

    run_successful_bench(scripts_dir + 'vars_defaults.erl',
        env={'var2': 'var2_new_value'},
        check_user_log_function=check_log)

    run_successful_bench(scripts_bdl_dir + 'vars_defaults.bdl',
        env={'var2': 'var2_new_value'},
        check_user_log_function=check_log)


def poisson_worker_start_test():
    bench_id = run_successful_bench(mzbench_dir + 'examples.bdl/worker_start_poisson.bdl')

    output = subprocess.check_output(
        [mzbench_script,
            '--host=localhost:4800',
            '--format=csv',
            'data',
            str(bench_id)])

    assert re.compile('workers\.pool1\.started\.rps,.*,1\.', re.UNICODE).search(output)


def unicode_resources_test():
    env = {'strings_filename':'unicode_strings.txt'}
    run_successful_bench(scripts_dir + 'unicode_resource.erl', env=env)
    run_successful_bench(scripts_bdl_dir + 'unicode_resource.bdl', env=env)


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
    assert 'print,' in csv_out
    assert 'print' in\
        [metric['target'] for metric in json.loads(json_out)]

def bench_results_test():
    bench_id = run_successful_bench(scripts_dir + 'data_script.erl')

    json_results_process = subprocess.Popen(
        [mzbench_script,
            '--host=localhost:4800',
            'results',
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

    json_out, json_err = json_results_process.communicate()
    print 'JSON results collector stdout'
    print json_out
    print 'JSON results collector stderr'
    print json_err

    time.sleep(3)

    json_data_ret_code = json_results_process.poll()

    assert json_data_ret_code == 0
    results = json.loads(json_out)

    assert results["dummy"]["type"] == "histogram"
    assert results["dummy"]["percentiles"]["max"] > 0
    assert results["print"]["type"] == "counter"
    assert results["print"]["value"] == 1

def restart_test():
    bench_id = run_successful_bench(scripts_dir + 'correct_script.erl')
    restarted_id = restart_bench(bench_id)
    cmd(mzbench_dir + 'bin/mzbench status --wait {0}'.format(restarted_id))


def loop_without_rate_test():
    run_successful_bench(scripts_dir + 'superloop.erl')
    run_successful_bench(scripts_bdl_dir + 'superloop.bdl')


def wait_zero_signal_test():
    run_successful_bench(scripts_dir + 'wait_zero_signal.erl')
    run_successful_bench(scripts_bdl_dir + 'wait_zero_signal.bdl')


def assertions_succ_test():
    run_successful_bench(mzbench_dir + 'examples/assertions.erl', env={})
    run_successful_bench(mzbench_dir + 'examples.bdl/assertions.bdl', env={})


def loop_assert_test():
    run_successful_bench(scripts_bdl_dir + 'loop_while.bdl', env={})

def ignore_failure_test():
    run_successful_bench(scripts_dir + 'ignore_failure_test.erl')
    run_successful_bench(scripts_bdl_dir + 'ignore_failure_test.bdl')


def comb_test():
    run_successful_bench(mzbench_dir + 'examples/comb.erl')
    run_successful_bench(mzbench_dir + 'examples.bdl/comb.bdl')


def workers_per_node_test():
    workers_per_node = 3
    bench_id = run_successful_bench(scripts_dir + 'workers_per_node.erl', workers_per_node=workers_per_node)

    log_process = subprocess.Popen(
        [mzbench_script,
            '--host=localhost:4800',
            'log',
            str(bench_id)],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    out, err = log_process.communicate()
    log = "{0} {1}".format(out, err)
    if not re.findall('nodes_arg => 4', log):
        print 'Out: ', out
        print 'Err: ', err
        raise RuntimeError("The bench should have allocated 4 worker nodes")


def log_compression_test():
    bench_id = run_successful_bench(scripts_dir + 'correct_script.erl')
    log_cmd = 'curl --head -X GET http://localhost:4800/log?id={0}'.format(bench_id)
    assert("content-encoding: deflate" in cmd(log_cmd))


def run_command_test():

    def run_command(bid):
        print "Running command for {0}".format(bid)
        change_env_process = subprocess.Popen(
            [mzbench_script,
                '--host=localhost:4800',
                'run_command',
                str(bid),
                'print("zzzzzz999")'],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)
        change_env_res, change_env_err = change_env_process.communicate()
        print 'Running command for {0}\n{1}'.format(change_env_res, change_env_err)

    run_successful_bench(
                scripts_dir + 'loop_with_vars.erl',
                post_start=run_command,
                expected_log_message_regex=r'zzzzzz999',
                env={'time': '60', 'rate': '1', 'message':'abc'})


def env_change_test():

    def change_var(bid):
        print "Changing env for {0}".format(bid)
        change_env_process = subprocess.Popen(
            [mzbench_script,
                '--host=localhost:4800',
                'change_env',
                str(bid),
                '--env=message=zzz'],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)
        change_env_res, change_env_err = change_env_process.communicate()
        print 'Change env output: {0}\n{1}'.format(change_env_res, change_env_err)
        assert('set' == json.loads(change_env_res)['status'])
        time.sleep(20)
        change_env_process = subprocess.Popen(
            [mzbench_script,
                '--host=localhost:4800',
                'change_env',
                str(bid),
                '--env=rate=5'],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)
        change_env_res, change_env_err = change_env_process.communicate()
        print 'Change env output: {0}\n{1}'.format(change_env_res, change_env_err)
        assert('set' == json.loads(change_env_res)['status'])

    bench_id = run_successful_bench(
                scripts_dir + 'loop_with_vars.erl',
                post_start=change_var,
                expected_log_message_regex=r'zzz',
                env={'time': '60', 'rate': '1', 'message':'abc'})

    json_data_process = subprocess.Popen(
        [mzbench_script,
            '--host=localhost:4800',
            '--format=json',
            'data',
            str(bench_id)],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)

    json_out, json_err = json_data_process.communicate()

    time.sleep(3)

    json_data_ret_code = json_data_process.poll()
    assert json_data_ret_code == 0
    datapoints = [metric['datapoints'] for metric in json.loads(json_out) if metric['target'] == 'print.rps'][0]
    values = [d[0] for d in datapoints]
    print "Datapoints: {0}".format(values)
    assert(0.8 < values[1] < 1.2)
    assert(4.8 < values[4] < 5.2)

def websocket_available_test():
    from websocket import create_connection
    ws = create_connection("ws://localhost:4800/ws")
    ws.send('{"cmd":"ping"}')
    assert("pong" in ws.recv())
    ws.send('{"cmd":"get_server_info"}')
    assert("SERVER_INFO" in ws.recv())
    ws.close()

def hooks_test():
    def check_log(log):
        regex = re.compile(r"Run exec hook \"echo pre_hook_1\"(.*)Run exec hook \"echo pre_hook_2\"(.*)Run exec hook \"echo post_hook_1\"", re.DOTALL)
        assert regex.search(log)

    def check_user_log(log):
        regex = re.compile(r"Dummy print: \"bar\"", re.DOTALL)
        assert regex.search(log)

    run_successful_bench(scripts_dir + 'hooks.erl',
        check_log_function=check_log,
        check_user_log_function=check_user_log)

    run_successful_bench(scripts_bdl_dir + 'hooks.bdl',
        check_log_function=check_log,
        check_user_log_function=check_user_log)


def main():
    from nose.plugins.multiprocess import MultiProcess
    with start_mzbench_server():
        if not nose.run(defaultTest=[__name__, 'mzb_signal_tests', 'mzb_negative_tests'], addplugins=[MultiProcess()]):
            raise RuntimeError("some tests failed")


if __name__ == '__main__':
    main()

