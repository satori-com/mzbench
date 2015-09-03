#!/usr/bin/env python

import os
import sys
import nose

dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

from mzb_test_utils import run_successful_bench, start_mzbench_server

mzbench_dir = dirname + '/../'
scripts_dir = mzbench_dir + 'acceptance_tests/scripts/'
mzbench_script = mzbench_dir + 'bin/mzbench'


def correct_test():
    run_successful_bench(scripts_dir + 'correct_script.erl')


def main():
    with start_mzbench_server():
        if not nose.run(defaultTest=__name__):
            raise RuntimeError("some tests failed")

if __name__ == '__main__':
    main()

