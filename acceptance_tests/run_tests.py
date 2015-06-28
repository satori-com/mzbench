#!/usr/bin/env python

import os
import sys

import nose
import nose.plugins.multiprocess


dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")


_multiprocess_shared_ = True

from mzb_test_utils import start_mzbench_server

def main():
    if not nose.run(defaultTest='mzb_local_tests'):
        raise RuntimeError("some tests failed")

    with start_mzbench_server():
        if not nose.run(defaultTest=['mzb_basic_tests', 'mzb_signal_tests', 'mzb_negative_tests']):
            raise RuntimeError("some tests failed")


if __name__ == '__main__':
    main()

