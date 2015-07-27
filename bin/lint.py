#!/usr/bin/env python
"""Run python linter on mz-bench and check escripts for errors

Usage:
    lint.py <MZBENCH_PATH>

Options:
    -h, --help       Show this help
"""

import os
import sys
import docopt
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


if __name__ == '__main__':

    args = docopt.docopt(__doc__)
    root = args['<MZBENCH_PATH>'] or os.curdir

    if any([lint_python(root), lint_escript(root)]):
        sys.exit(1)