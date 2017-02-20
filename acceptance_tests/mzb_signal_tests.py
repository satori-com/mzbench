#!/usr/bin/env python

import os
import sys
import nose

dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

from mzb_test_utils import run_successful_bench, run_failing_bench, start_mzbench_server, start_simple_bench, wait_status

mzbench_dir = dirname + '/../'
scripts_dir = mzbench_dir + 'acceptance_tests/scripts/'
scripts_bdl_dir = mzbench_dir + 'acceptance_tests/scripts.bdl/'
mzbench_script = mzbench_dir + 'bin/mzbench'

def signal_test():
    run_successful_bench(mzbench_dir + 'examples/signal.erl', env={})
    run_successful_bench(mzbench_dir + 'examples.bdl/signal.bdl', env={})

def signal_count_test():
    run_successful_bench(mzbench_dir + 'examples/signal_count.erl', env={})
    run_successful_bench(mzbench_dir + 'examples.bdl/signal_count.bdl', env={})

def signal_assert_test():
    run_successful_bench(scripts_bdl_dir + 'signal_assert.bdl', env={})

def signal_parallel_test():
    run_successful_bench(scripts_dir + 'signal_parallel.erl', env={})
    run_successful_bench(scripts_bdl_dir + 'signal_parallel.bdl', env={})

def signal_deadlock_test():
    run_failing_bench(scripts_dir + 'signal_error1.erl',
        expected_log_message_regex=r'Deadlock is posible')
    run_failing_bench(scripts_bdl_dir + 'signal_error1.bdl',
        expected_log_message_regex=r'Deadlock is posible')


def nobody_waits_for_signal_test():
    run_failing_bench(scripts_dir + 'signal_error2.erl',
        expected_log_message_regex=r'Nobody sets signal')
    run_failing_bench(scripts_bdl_dir + 'signal_error2.bdl',
        expected_log_message_regex=r'Nobody sets signal')


def nobody_sets_signal_test():
    run_failing_bench(scripts_dir + 'signal_error3.erl',
        expected_log_message_regex=r'Nobody waits for signal')
    run_failing_bench(scripts_bdl_dir + 'signal_error3.bdl',
        expected_log_message_regex=r'Nobody waits for signal')


def nobody_sets_signal_in_loop_test():
    run_failing_bench(scripts_dir + 'signal_error4.erl',
        expected_log_message_regex=r'Nobody waits for signal')
    run_failing_bench(scripts_bdl_dir + 'signal_error4.bdl',
        expected_log_message_regex=r'Nobody waits for signal')

def dynamic_deadlock_test():
    run_failing_bench(scripts_bdl_dir + 'signal_dyn_deadlock.bdl',
        expected_log_message_regex=r'Dynamic deadlock detected')

def exclusive_test():
    id1 = start_simple_bench(mzbench_dir + 'examples.bdl/comb.bdl', '--exclusive=tagA')
    id2 = start_simple_bench(mzbench_dir + 'examples.bdl/comb.bdl', '--exclusive=tagA')
    if wait_status(id1, 'running', 240):
        if wait_status(id2, 'wait_exclusive', 240):
            return wait_status(id2, 'complete', 240)
        else:
            return False
    else:
        return False

def main():
    from nose.plugins.multiprocess import MultiProcess
    with start_mzbench_server():
        if not nose.run(defaultTest=__name__, addplugins=[MultiProcess()]):
            raise RuntimeError("some tests failed")

if __name__ == '__main__':
    main()
