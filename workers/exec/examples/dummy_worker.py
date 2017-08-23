#!/usr/bin/env python

from __future__ import print_function
import socket
import time
import sys

DEFAULT_IP = '127.0.0.1'
DEFAULT_PORT = 8125

if __name__ == '__main__':
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    for i in xrange(1, 3000):
        s.sendto("dummy.counter1:{0}|c\n".format(i), (DEFAULT_IP, DEFAULT_PORT))
        s.sendto("dummy.gauge1:{0}|g\n".format(i), (DEFAULT_IP, DEFAULT_PORT))
        print("Reporting {0}".format(i), file=sys.stdout)
        print("Some error at {0}".format(i), file=sys.stderr)
        time.sleep(0.1)
