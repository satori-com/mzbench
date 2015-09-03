#!/usr/bin/env python

import os
import sys
import subprocess
from distutils.spawn import find_executable

sys.dont_write_bytecode = True

def files_with_extension(ext, root='.'):
    for d, _, files in os.walk(root):
        for f in files:
            if '/deps/' in d:
                continue
            if f.endswith(ext):
                yield os.path.join(d, f)


def lint_python(root):
    os.environ['PATH'] = ':'.join((
        os.path.join(os.environ['HOME'], '.local/bin'),
        os.environ['PATH']))
    pyflakes = find_executable('pyflakes')

    python_files = [os.path.join(root, 'bin/mzbench')] + list(files_with_extension('.py', root))
    return subprocess.call([pyflakes] + python_files)


def lint_escript(root):
    escript_files = files_with_extension('.escript', root)
    def return_codes():
        for f in escript_files:
            yield subprocess.call(['escript', '-s', f])
    if any(list(return_codes())):
        return 1
    return 0


def lint_eunit_file_naming(root):
    orphans = []
    for appdir in ['common_apps/mzbench_utils', 'common_apps/mzbench_language',
            'node/apps/mzbench', 'node/apps/dummy_worker', 'node/apps/mz_histogram', 'server']:
        try:
            for t in os.listdir(os.path.join(root, appdir, 'test')):
                if t.endswith('_test.erl'):
                    print "{0}/test/{1} has '_test' instead of '_tests' in the name and won't be tested by eunit".format(appdir, t)
                if t.endswith('.erl') and not t.replace('_tests', '') in os.listdir(os.path.join(root, appdir, 'src')):
                    orphans.append(os.path.join(appdir, 'test', t))
        except OSError:
            pass

    if orphans:
        print 'Orphan eunit files found: {0}'.format(orphans)
        sys.exit(1)


if __name__ == '__main__':

    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

    if any([lint_python(root), lint_escript(root), lint_eunit_file_naming(root)]):
        sys.exit(1)