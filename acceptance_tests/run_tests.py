#!/usr/bin/env python

import os
import sys
from contextlib import contextmanager

import nose
import nose.plugins.multiprocess


dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

from util import cmd

_multiprocess_shared_ = True


@contextmanager
def start_mzbench_server():
    if 'MZBENCH_REPO' in  os.environ:
        mzbench_git_param = '{{mzbench_git, "{0}"}}'.format(os.environ['MZBENCH_REPO'])
    else:
        mzbench_git_param = ''

    with open("/mz/mz_bench_api/mz_bench_server.config", "w") as config:
        config.write('[{{mz_bench_api, [{0}]}}].'.format(mzbench_git_param))

    cmd('/mz/mz_bench_api/bin/mz_bench_api start')
    try:
        yield
    finally:
        cmd('/mz/mz_bench_api/bin/mz_bench_api stop')


def main():
    if not nose.run(defaultTest='mzb_local_tests'):
        raise RuntimeError("some tests failed")

    with start_mzbench_server():
        if not nose.run(defaultTest='mzb_tests'):
            raise RuntimeError("some tests failed")


if __name__ == '__main__':
    main()

