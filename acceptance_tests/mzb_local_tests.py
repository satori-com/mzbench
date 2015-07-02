#!/usr/bin/env python

import os
import sys
import subprocess
import nose

dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

from util import cmd

mz_bench_dir = dirname + '/../'
scripts_dir = mz_bench_dir + 'acceptance_tests/scripts/'
mzbench_script = mz_bench_dir + 'bin/mzbench'

def devtool_run_local_tests():
    cmd(mz_bench_dir + 'bin/mzbench validate ' + scripts_dir +'loop_rate.erl')

    cmd(mz_bench_dir + 'bin/mzbench run_local ' + scripts_dir + 'loop_rate.erl')

    cmd(mz_bench_dir + 'bin/mzbench run_local ' + scripts_dir + 'data_script.erl')

    try:
        cmd(mz_bench_dir + 'bin/mzbench run_local ' + scripts_dir + 'syntax_error.erl')
        assert False
    except subprocess.CalledProcessError:
        pass

    try:
        cmd(mz_bench_dir + 'bin/mzbench run_local ' + scripts_dir + 'semantic_error.erl')
        assert False
    except subprocess.CalledProcessError:
        pass


def devtool_list_templates_test():
    templates = os.listdir(mz_bench_dir + 'worker-templates')
    got_templates = filter(
        lambda x: x,
        cmd(mz_bench_dir + 'bin/mzbench list_templates').split('\n'))
    if sorted(templates) != sorted(got_templates):
        print sorted(templates)
        print sorted(got_templates)
        assert sorted(templates) == sorted(got_templates)

def main():
    if not nose.run(defaultTest=__name__):
        raise RuntimeError("some tests failed")

if __name__ == '__main__':
    main()

