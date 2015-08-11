
from __future__ import print_function

import sys

def print_red(*args, **kwargs):
    if sys.stdout.isatty():
        print('\033[31m', end='')
        print(*args, **kwargs)
        print('\033[0m', end='')
    else:
        print(*args, **kwargs)
