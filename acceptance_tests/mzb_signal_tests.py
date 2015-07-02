#!/usr/bin/env python

import os
import sys
import nose

dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

from mzb_test_utils import run_successful_bench, run_failing_bench, start_mzbench_server

mz_bench_dir = dirname + '/../'
scripts_dir = mz_bench_dir + 'acceptance_tests/scripts/'
mzbench_script = mz_bench_dir + 'bin/mzbench'

def signal_test():
    run_successful_bench(mz_bench_dir + 'examples/signal.erl', env={})


def signal_count_test():
    run_successful_bench(mz_bench_dir + 'examples/signal_count.erl', env={})


def signal_deadlock_test():
    run_failing_bench(scripts_dir + 'signals_deadlock.erl')


def nobody_waits_for_signal_test():
    run_failing_bench(scripts_dir + 'signals_deadlock2.erl')


def nobody_sets_signal_test():
    run_failing_bench(scripts_dir + 'signals_deadlock3.erl')

def main():
    with start_mzbench_server():
        if not nose.run(defaultTest=__name__):
            raise RuntimeError("some tests failed")

if __name__ == '__main__':
    main()
