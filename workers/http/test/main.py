#!/usr/bin/env python

import os
import server
import socket
import subprocess
import sys

dirname = os.path.dirname(os.path.realpath(__file__))
sys.path.append(os.path.join(dirname, "../../../lib"))
sys.dont_write_bytecode = True

hostname = socket.gethostname()

import util

def main():
    user = os.environ['USER']
    subprocess.check_call(
        ['sudo', 'yum', 'install', '-y', 'mz_bench', 'mz_bench_dev'])
    with util.chdir(os.path.join(dirname, '..')):

        if '--local' in sys.argv:
            run_command = ['mz-bench-dev', 'run-local']
        else:
            run_command =\
                ['mz-bench'
                , 'run'
                , '--user_repo=' + user 
                , '--exclusive_node_usage=false'
                ]

        with server.background_server():
            subprocess.check_call(
                run_command +
                [ 'examples/http_post.erl'
                , '--env=host=' + hostname
                , '--env=port=' + str(server.port)
                , '--env=max_rps=2'
                , '--env=endpoint=/update-db'
                ])
            subprocess.check_call(
                run_command +
                [ 'examples/http_get.erl'
                , '--env=host=' + hostname
                , '--env=port=' + str(server.port)
                , '--env=max_rps=2'
                , '--env=endpoint=/index.html'
                ])

if __name__ == '__main__':
    main()