#!/usr/bin/env python

import os
import shlex
import subprocess
import sys
import nose

dirname = os.path.dirname(os.path.realpath(__file__))
os.chdir(dirname)
sys.path.append("../lib")

import util
from mzb_test_utils import run_failing_bench, start_mzbench_server

mz_bench_dir = dirname + '/../'
scripts_dir = mz_bench_dir + 'acceptance_tests/scripts/'
mzbench_script = mz_bench_dir + 'bin/mzbench'

def emulate_crash_test():
    run_failing_bench(scripts_dir + 'correct_script.erl',
        env={'emulate_bench_crash': 'true'},
        expected_log_message_regex=r"\[error\].*Benchmark received 'EXIT' from .* with reason {emulated_crash,\n\s*nothing_to_see_here,\n\s*please_move_along},")


def syntax_error_test():
    p = subprocess.Popen(
        shlex.split('../bin/mzbench start scripts/syntax_error.erl'),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    (out, err) = p.communicate()
    assert p.returncode
    expected_out = '''Syntax error: Rule 'entry' matched in its entirety, but it didn't consume all the text. The non-matching portion of the text begins with '{pool, [],
        [}.
        ' (line 1, column 1).'''
    assert util.multiline_strip(out) == util.multiline_strip(expected_out)


def runtime_error_test():
    run_failing_bench(scripts_dir + 'runtime_error.erl',
        expected_log_message_regex=r'\[error\].*Worker.*has crashed: "no_because_no"')


def env_param_missing_test():
    run_failing_bench(scripts_dir + 'env.erl', env={},
        expected_log_message_regex=r'''\[error\] \[ API \] Stage 'pipeline - provisioning': failed\n\s*Benchmark has failed on provisioning with reason:\n\s*{substitution_error,variable_name_is_unbound,"pool_size",at_location,\n\s*"line 1: "}''')


def main():
    with start_mzbench_server():
        if not nose.run(defaultTest=__name__):
            raise RuntimeError("some tests failed")

if __name__ == '__main__':
    main()
