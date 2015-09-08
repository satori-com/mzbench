#!/usr/bin/env python

import os
import server
import socket
import subprocess
import requests
import sys

dirname = os.path.dirname(os.path.realpath(__file__))
sys.path.append(os.path.join(dirname, "../../../lib"))
sys.dont_write_bytecode = True

hostname = socket.gethostname()

import util

def checkServer(): # 0 -- no server running, 1 -- server is running, -1 -- alien server is running
    try:
        r = requests.get("http://localhost:4800/report.json")
        if r.status_code != 200:
            return -1
        return 1
    except requests.ConnectionError:
        return 0


def main():

    with util.chdir(os.path.join(dirname, '../')):
        serverStatus = checkServer()

        if '--local' in sys.argv:
            if serverStatus == 1:
                print("Please stop MZBench server before running local mode")
                sys.exit()
            run_command = ['../../bin/mzbench', 'run_local']
        else:
            if serverStatus == -1:
                print("Non-MZBench server is listening on 4800")
                sys.exit()
            if serverStatus == 0:
                if 'MZBENCH_REPO' in  os.environ:
                    mzbench_git_param = '{{mzbench_git, "{0}"}}'.format(os.environ['MZBENCH_REPO'])
                else:
                    mzbench_git_param = ''

                with open(dirname + '/mzbench_server.config', 'w') as config:
                    config.write('[{{mzbench_api, [{0}]}}].'.format(mzbench_git_param))

                subprocess.check_call(['../../bin/mzbench', 'start_server', '--config', dirname + '/mzbench_server.config'])

            run_command = ['../../bin/mzbench', 'run']
            if 'NODE_COMMIT' in os.environ:
                run_command += ['--node_commit=' + os.environ['NODE_COMMIT']]

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
        if ('--local' not in sys.argv) and (serverStatus == 0):
            subprocess.check_call(['../../bin/mzbench', 'stop_server'])

if __name__ == '__main__':
    main()
