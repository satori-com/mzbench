#!/usr/bin/env python

import os
import sys
import nose
import shutil
import tempfile

dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

from util import cmd
from mzb_test_utils import run_successful_bench, start_mzbench_server

mzbench_dir = dirname + '/../'
scripts_dir = mzbench_dir + 'acceptance_tests/scripts/'

def check_migrations_test():
    mzbench_data_dir = tempfile.mkdtemp(prefix='mzbench_data_')

    with start_mzbench_server(custom_data_location=mzbench_data_dir):
        for i in range(5):
            run_successful_bench(scripts_dir + 'correct_script.erl')

    try:
        cmd(mzbench_dir + '/bin/migrate.py ' + mzbench_data_dir)
    finally:
        shutil.rmtree(mzbench_data_dir)


def main():
    if not nose.run(defaultTest=__name__):
        raise RuntimeError("some tests failed")


if __name__ == '__main__':
    main()
