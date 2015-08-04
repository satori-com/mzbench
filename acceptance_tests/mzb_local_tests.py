#!/usr/bin/env python

import os
import sys
import subprocess
import nose

dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

from util import cmd

mzbench_dir = dirname + '/../'
scripts_dir = mzbench_dir + 'acceptance_tests/scripts/'
mzbench_script = mzbench_dir + 'bin/mzbench'

def devtool_run_local_tests():
    cmd(mzbench_dir + 'bin/mzbench validate ' + scripts_dir +'loop_rate.erl')

    cmd(mzbench_dir + 'bin/mzbench validate ' + scripts_dir +'env.erl --env pool_size=20 --env jozin=jozin --env wait_ms=100')

    cmd(mzbench_dir + 'bin/mzbench run_local ' + scripts_dir + 'loop_rate.erl')

    cmd(mzbench_dir + 'bin/mzbench run_local ' + scripts_dir + 'data_script.erl')

    try:
        cmd(mzbench_dir + 'bin/mzbench run_local ' + scripts_dir + 'syntax_error.erl')
        assert False
    except subprocess.CalledProcessError:
        pass

    try:
        cmd(mzbench_dir + 'bin/mzbench run_local ' + scripts_dir + 'semantic_error.erl')
        assert False
    except subprocess.CalledProcessError:
        pass


def devtool_list_templates_test():
    templates = os.listdir(mzbench_dir + 'worker_templates')
    got_templates = filter(
        lambda x: x,
        cmd(mzbench_dir + 'bin/mzbench list_templates').split('\n'))
    if sorted(templates) != sorted(got_templates):
        print sorted(templates)
        print sorted(got_templates)
        assert sorted(templates) == sorted(got_templates)

def main():
    if not nose.run(defaultTest=__name__):
        raise RuntimeError("some tests failed")

if __name__ == '__main__':
    main()

