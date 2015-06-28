#!/usr/bin/env python

import os
import sys
import nose

dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

from mzb_test_utils import run_failing_bench, start_mzbench_server

mz_bench_dir = os.path.dirname(os.path.realpath(__file__)) + '/../'
scripts_dir = mz_bench_dir + 'acceptance_tests/scripts/'
mzbench_script = mz_bench_dir + 'bin/mzbench'

def emulate_crash_test():
    run_failing_bench(scripts_dir + 'correct_script.erl', env={'emulate_bench_crash': 'true'})


def semantic_error_test():
    run_failing_bench(scripts_dir + 'semantic_error.erl')


def runtime_error_test():
    run_failing_bench(scripts_dir + 'runtime_error.erl')


def env_param_missing_test():
    run_failing_bench(scripts_dir + 'env.erl', env={})

def main():
    with start_mzbench_server():
        if not nose.run(defaultTest=__name__):
            raise RuntimeError("some tests failed")

if __name__ == '__main__':
    main()
