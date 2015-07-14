#!/usr/bin/env python
"""Run python linter on mz-bench

Usage:
    lint.py <MZBENCH_PATH>

Options:
    -h, --help       Show this help
"""

import os
import sys
import docopt
import glob
import subprocess
from distutils.spawn import find_executable

sys.dont_write_bytecode = True

if __name__ == '__main__':
    args = docopt.docopt(__doc__)

    os.environ['PATH'] = ':'.join((
        os.path.join(os.environ['HOME'], '.local/bin'),
        os.environ['PATH']))
    pyflakes = find_executable('pyflakes')

    root = args['<MZBENCH_PATH>'] or os.curdir

    python_files = [os.path.join(root, 'bin/mzbench')] + glob.glob(root + '/**/*.py')
    sys.exit(subprocess.call([pyflakes] + python_files))
