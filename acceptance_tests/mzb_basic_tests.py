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
mzbench_script = mzbench_dir + 'bin/mzbench'


def correct_test():
    run_successful_bench(scripts_dir + 'correct_script.erl')


def worker_from_rsync_test():
    run_successful_bench(
        scripts_dir + 'worker_from_rsync.erl',
        env={'exec_worker_dir': os.path.abspath('../workers/exec') + '/'})


def lua_worker_from_git_test():
    worker_commit = os.environ.get('NODE_COMMIT', 'master')
    mzbench_repo = os.environ.get('MZBENCH_REPO', 'https://github.com/machinezone/mzbench')
    run_successful_bench(
        scripts_dir + 'lua_worker_from_git.erl',
        env={'worker_branch': worker_commit,
             'mzbench_repo':  mzbench_repo})


def python_worker_from_git_test():
    worker_commit = os.environ.get('NODE_COMMIT', 'master')
    mzbench_repo = os.environ.get('MZBENCH_REPO', 'https://github.com/machinezone/mzbench')
    run_successful_bench(
        scripts_dir + 'python_worker_from_git.erl',
        env={'worker_branch': worker_commit,
             'mzbench_repo':  mzbench_repo})


def worker_from_git_test():
    # worker is located in the same repo as node
    worker_commit = os.environ.get('NODE_COMMIT', 'master')
    mzbench_repo = os.environ.get('MZBENCH_REPO', 'https://github.com/machinezone/mzbench')
    run_successful_bench(
        scripts_dir + 'worker_from_git.erl',
        env={'worker_branch': worker_commit,
             'mzbench_repo':  mzbench_repo})


def env_test():
    run_successful_bench(scripts_dir + 'env.erl', env={
        'jozin': 'bazin',
        'wait_ms': '10',
        'pool_size': '2',
        'loop_time': '5',
        'loop_rate': '2'})


def poisson_worker_start_test():
    run_successful_bench(mzbench_dir + 'examples/worker_start_poisson.erl',
        expected_log_message_regex='workers\.pool1\.started\.rps = 1\.')


def unicode_resources_test():
    run_successful_bench(scripts_dir + 'unicode_resource.erl',
        env={'strings_filename':'unicode_strings.txt'},
        expected_log_message_regex=r'https://travis-ci\.org/machinezone/mzbench')


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
    assert 'print.value,' in csv_out
    assert 'print.value' in\
        [metric['target'] for metric in json.loads(json_out)]


def restart_test():
    bench_id = run_successful_bench(scripts_dir + 'correct_script.erl')
    restarted_id = restart_bench(bench_id)
    cmd(mzbench_dir + 'bin/mzbench status --wait {0}'.format(restarted_id))


def loop_without_rate_test():
    run_successful_bench(scripts_dir + 'superloop.erl')


def assertions_succ_test():
    run_successful_bench(mzbench_dir + 'examples/assertions.erl', env={})


def ignore_failure_test():
    run_successful_bench(scripts_dir + 'ignore_failure_test.erl')


def comb_test():
    run_successful_bench(mzbench_dir + 'examples/comb.erl')


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
    log_cmd = 'curl --head -X GET http://localhost:4800/logs?id={0}'.format(bench_id)
    assert("content-encoding: deflate" in cmd(log_cmd))

def main():
    with start_mzbench_server():
        if not nose.run(defaultTest=__name__):
            raise RuntimeError("some tests failed")

if __name__ == '__main__':
    main()

