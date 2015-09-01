#!/usr/bin/env python

import os
import sys
import nose

dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

from mzb_test_utils import run_successful_bench, run_failing_bench, start_mzbench_server

mzbench_dir = dirname + '/../'
scripts_dir = mzbench_dir + 'acceptance_tests/scripts/'
mzbench_script = mzbench_dir + 'bin/mzbench'

def signal_test():
    run_successful_bench(mzbench_dir + 'examples/signal.erl', env={})

def signal_count_test():
    run_successful_bench(mzbench_dir + 'examples/signal_count.erl', env={})

def signal_parallel_test():
    run_successful_bench(scripts_dir + 'signal_parallel.erl', env={})

def signal_deadlock_test():
    run_failing_bench(scripts_dir + 'signal_error1.erl',
        expected_log_message_regex=r'Deadlock is posible')


def nobody_waits_for_signal_test():
    run_failing_bench(scripts_dir + 'signal_error2.erl',
        expected_log_message_regex=r'Nobody sets signal')


def nobody_sets_signal_test():
    run_failing_bench(scripts_dir + 'signal_error3.erl',
        expected_log_message_regex=r'Nobody waits for signal')


def nobody_sets_signal_in_loop_test():
    run_failing_bench(scripts_dir + 'signal_error4.erl',
        expected_log_message_regex=r'Nobody waits for signal')


def main():
    with start_mzbench_server():
        if not nose.run(defaultTest=__name__):
            raise RuntimeError("some tests failed")

if __name__ == '__main__':
    main()
