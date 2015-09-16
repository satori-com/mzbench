#!/usr/bin/env python
"""mzbench CLI interface

Usage:
  mzbench clean
  mzbench start_server [--config=<config>]
  mzbench stop_server
  mzbench restart_server [--config=<config>]
  mzbench [--host=<host>] (start|run)
    [--nodes=<nodes>|--workers_per_node=<workers_per_node>]
    [--nodes_file=<filename>]
    [(--email=<email> ...)]
    [(--env=<name=value>...)]
    [--node_commit=<commit>]
    [--deallocate_after_bench=<deallocate_after_bench>]
    [--provision_nodes=<provision_nodes>]
    [--exclusive_node_usage=<exclusive_node_usage>]
    [--name=<benchmark_name>]
    [--cloud=<cloud_provider_name>]
    <script_file>
  mzbench [--host=<host>] restart <bench_id>
  mzbench [--host=<host>] status [--wait] <bench_id>
  mzbench [--host=<host>] stop <bench_id>
  mzbench [--host=<host>] log <bench_id>
  mzbench [--host=<host>] data [(--target=<name>...)] [--format=(csv|json)] <bench_id>
  mzbench run_local <script> [--env=<name=value>...]
  mzbench validate <script> [--env=<name=value>...]
  mzbench list_templates
  mzbench new_worker [--template=<template>] <name>
  mzbench selfcheck
  mzbench -h | --help

Options:
    -h, --help       Show this help
    --skip_install   Skip stage with building and deploying RPMs
    --tests <tests>  Execute only the specified tests
    --host <host>    Specify host providing mzbench api. Alternatively,
                     you can set environment variable MZBENCH_API_HOST.
    --nodes <nodes>  Specify either an amount of nodes to be allocated or a
                     comma-separated list of hostnames of preallocated nodes.
                     Beware that one additional node will be allocated for
                     the director. If you provide preallocated nodes, you 
                     must provide at least two (one for director and one for
                     the workers).
                     Note: If preallocated nodes are used they won't be
                     deallocated after benchmark by any means, although the
                     erlang nodes will be stopped automatically. If you want to
                     prevent the erlang nodes from stopping (in case of
                     debugging, for example) --deallocate_after_bench param
                     could be used.
    --workers_per_node <workers_per_node>
                     Let the benchmarking system to allocate the number of
                     nodes needed so each node will run the requested number 
                     of workers. Beware that one additional node will be 
                     allocated for the director.
    --nodes_file <filename>
                     Specify a file with hostnames of preallocated nodes
                     separated by newline.
    --deallocate_after_bench <deallocate_after_bench>
                     Either "true" or "false". Default is "true". Setting it to
                     false will force mzbench to skip deallocation phase of
                     the benchmark and leave erlang worker nodes alive.
                     *Warning* This parameter is for debugging purpose only.
                     Could lead to serious resource leakage.
    --provision_nodes <provision_nodes>
                     Either "true" or "false". Default is "true". Setting this 
                     to "true" indicates to perform nodes provision. Disabling 
                     it is useful when reusing already preallocated and 
                     provisioned nodes.
    --exclusive_node_usage <exclusive_node_usage>
                     Either "true" or "false". Default is "true".
                     If set to true mzbench will request exclusive physical
                     hosts for each worker node.
    --name <benchmark_name>
                     Set benchmark name.
    --cloud <cloud_provider_name>
                     Specify cloud provider to use. If not set server will use
                     first available provider.
    --env <name=value>
                     Pass a variable to script
    --template <template> 
                     Choose a template for new worker, default is 'empty'
    --config <config>
                     Configuation file to use
"""

from __future__ import print_function

from shutil import copytree
import csv
import glob
import json
import os
import re
import subprocess
import sys
import time
import math

def check_module_version(module, required_version):
    import distutils.version
    return distutils.version.StrictVersion(module.__version__) >= distutils.version.StrictVersion(required_version)


try:
    import docopt
    import requests
    
    if not check_module_version(requests, '2.7.0'):
        print('mzbench requires the module requests to be version 2.7.0 or higher,')
        print('but version {0} is installed.'.format(requests.__version__))
        print()
        print('Please update your installation and try again.')
        sys.exit(1)
    
except ImportError:
    print('mzbench requires docopt and requests')
    print()
    print('These packages can be installed using your preferred package manager')
    print('or using pip, typically:')
    print()
    print(' sudo pip install -r requirements.txt')
    sys.exit(1)

dirname = os.path.dirname(os.path.realpath(__file__))
sys.path.append(dirname + "/../lib")
sys.dont_write_bytecode = True
server_rel_dir = dirname + "/../server/_build/default/rel/mzbench_api/"

import color_terminal
import util
from subprocess import check_call

import mzbench_api_client as api


def require(executable):
    descriptions = {'escript': 'Erlang distribution (17.* or 18.*)', 'c++': 'C++ compiler'}
    from distutils.spawn import find_executable
    if not find_executable(executable):
        print('This command requires {0}.'.format(
            descriptions.get(executable, executable)))
        print('It can be installed using your preferred package manager.')
        sys.exit(1)


#=================================================================================================
# Installation and cleanup
#=================================================================================================
NODE_PROJECTS = [dirname + "/../node"]
SERVER_PROJECTS = [dirname + "/../server"]
CHECK_PROJECTS = NODE_PROJECTS + SERVER_PROJECTS + \
    [dirname + '/../common_apps/mzbench_language',
     dirname + '/../common_apps/mzbench_utils',
     dirname + '/../examples',
     dirname + '/../acceptance_tests/invalid_scripts']

def execute_make(projects, target):
    for subproject in projects:
        cmd('make -C ' + subproject + ' ' + target)

def compile_projects(projects):
    execute_make(projects, 'compile')

def clean_projects(projects):
    execute_make(projects, 'clean')

def install_projects(projects):
    execute_make(projects, 'install')

def clean():
    clean_projects(NODE_PROJECTS + SERVER_PROJECTS)

#=================================================================================================
# API server startup and stop
#=================================================================================================
def start_server(config):
    require('escript')
    execute_make(SERVER_PROJECTS, 'generate')
    if config is not None:
        config_arg = '-config_file {0}'.format(os.path.abspath(config))
    else:
        config_arg = ''
    cmd(server_rel_dir + 'bin/mzbench_api start {0}'.format(config_arg))

def stop_server():
    require('escript')
    execute_make(SERVER_PROJECTS, 'generate')
    cmd(server_rel_dir + 'bin/mzbench_api stop')

def restart_server(config):
    stop_server()
    start_server(config)

#=================================================================================================
# Testing
#=================================================================================================

def selfcheck():
    require('nosetests')
    require('pyflakes')
    require('escript')
    cmd('{0}/lint.py {0}/../'.format(dirname))
    execute_make(CHECK_PROJECTS, 'check')
    try:
        tests_environ = os.environ
        tests_environ['MZBENCH_REPO'] = dirname + '/../'
        subprocess.check_call(['make', '-C', dirname + '/../acceptance_tests'], env=tests_environ)
    except subprocess.CalledProcessError as e:
        sys.exit(e.returncode)


#=================================================================================================
# Communication with the API server
#=================================================================================================
def pretty_print(data):
    json.dump(data, sys.stdout, indent=4)

def run(host, script_file, script_content, email, nodes, workers_per_node,
        node_commit, deallocate_after_bench, provision_nodes, exclusive_node_usage,
        name, cloud, env):
    bench_id = start(host, script_file, script_content, email, node_commit,
        nodes, workers_per_node, deallocate_after_bench, provision_nodes,
        exclusive_node_usage, name, cloud, env)
    response = status(host, bench_id, wait=True)
    if response[u'status'] == u'failed':
        print('Bench failed')
        sys.exit(3)

def start(host, script_file, script_content, email, node_commit, nodes,
          workers_per_node, deallocate_after_bench, provision_nodes,
          exclusive_node_usage, name, cloud, env):
    import erl_utils
    import erl_terms
        
    try:
        script_terms = erl_utils.convert(script_content, env)
        include = erl_utils.get_includes(script_terms)
        
        if workers_per_node is not None:
            nodes = int(math.ceil(float(erl_utils.get_num_of_workers(script_terms))/float(workers_per_node)))
        
        response = api.start(host,
            script_file,
            script_content,
            emails=email,
            node_commit=node_commit,
            includes=include,
            deallocate_after_bench=deallocate_after_bench,
            provision_nodes=provision_nodes,
            exclusive_node_usage=exclusive_node_usage,
            benchmark_name=name,
            cloud=cloud,
            env=env,
            nodes=nodes)
        pretty_print(response)
        return response['id']
    except erl_terms.ParseError as e:
        print('Syntax error: {0}'.format(e))
        sys.exit(7)

def restart(host, bench_id):
    response = api.restart(host, bench_id)
    pretty_print(response)

def status(host, bench_id, wait):
    if not wait:
        response = api.status(host, bench_id, wait)
        pretty_print(response)
        return response
    else:
        print('')
        start_time = int(time.time())
        try:
            # server doesn't support 'wait' flag yet
            while True:
                response = api.status(host, bench_id, wait)
                if 'DEBUG' in os.environ:
                    pretty_print(response)

                if sys.stdout.isatty():
                    current_time = int(time.time())
                    mins = (current_time - start_time) / 60
                    secs = (current_time - start_time) % 60
                    statusline = 'status: {0}{1}{2}'.format(
                        response[u'status'],
                        ' ' * (30 - len(response[u'status'])),
                        '{0:02}:{1:02}'.format(mins, secs))
                    print('\r' + statusline, end='')

                if response[u'status'] in [u'failed', u'complete', u'stopped']:
                    print('')
                    pretty_print(response)
                    if response[u'status'] == u'failed':
                        sys.exit(3)
                    return response
                sys.stdout.flush()
                time.sleep(3)
        except KeyboardInterrupt:
            should_stop = '?'
            while should_stop not in 'yn':
                should_stop = raw_input('\nStop the bench before quitting? (y/n) ')
            if should_stop == 'y':
                stop(host, bench_id)
            sys.exit(130)

def stop(host, bench_id):
    response = api.stop(host, bench_id)
    pretty_print(response)

def log(host, bench_id):
    print('Start of log for bench {0}'.format(bench_id))
    for line in api.logs(host, bench_id):
        if '[error]' in line:
            color_terminal.print_red(line)
        else:
            print(line)
    print('End of log for bench {0}'.format(bench_id))

def data(host, bench_id, target, format):
    def data_generator():
        for line in api.data(host, bench_id):
            worker_timestamp, name, value = line.split("\t")

            try:
                value = int(value)
            except ValueError:
                try:
                    value = float(value)
                except:
                    pass

            if (name in target) or not target:
                timestamp = int(worker_timestamp)
                yield (timestamp, name, value)

    if format == "csv":
        data_csv(data_generator)
    else:
        data_default(data_generator)

def data_csv(gen):
    writer = csv.writer(sys.stdout, delimiter=',',
                        quotechar='|', quoting=csv.QUOTE_MINIMAL)
    for timestamp, name, value in gen():
        writer.writerow([name, timestamp, value])

def data_default(gen):
    result = {}
    for timestamp, name, value in gen():
        if name not in result:
            result[name]=[]
        result[name].append([value, timestamp])

    pretty_print(map(lambda (k,v): {'target':k, 'datapoints':v}, result.iteritems()))

#=================================================================================================
# Local bench script execution
#=================================================================================================
def run_or_validate(script, env, validate):
    if validate:
        with util.silent_stdout():
            compile_projects(NODE_PROJECTS)
    else:
        compile_projects(NODE_PROJECTS)
    
    escript_name = 'run_local.escript'
    escript_path = os.path.join(dirname, '../node/scripts')
    
    maybe_plugin_name = [app_src_filename[4:-8] for app_src_filename in glob.glob('src/*.app.src')]
    if len(maybe_plugin_name) == 1:
        [plugin_name] = maybe_plugin_name
        worker_beam_rebar2 = 'ebin/' + plugin_name + '.beam'
        worker_beam_rebar3 = '_build/default/deps/' + plugin_name + '/ebin/' + plugin_name + '.beam'
        if not os.path.exists(worker_beam_rebar2) and not os.path.exists(worker_beam_rebar3):
            print("WARNING: no worker beam files detected in {0} or {1}, launching make".format(
                worker_beam_rebar2, worker_beam_rebar3))
            check_call(['make'])
            if not os.path.exists(worker_beam_rebar2) and not os.path.exists(worker_beam_rebar3):
                print ("WARNING: make didn't produce worker beam files")

    pa = []
    ebin_dirs = (glob.glob(os.getcwd() + "/deps/*/ebin") +
        glob.glob(os.getcwd() + "/apps/*/ebin") +
        glob.glob(os.getcwd() + "/_build/default/deps/*/ebin"))
    for p in ebin_dirs:
        pa += ["--pa", p]
    cmd = [os.path.join(escript_path, escript_name), script, '--pa', os.getcwd() + "/ebin"] + pa

    for k, v in env.iteritems():
        cmd.append('--env')
        cmd.append(k + '=' + v)

    if validate:
        cmd.append('--validate')

        p = subprocess.Popen(cmd, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
        (out, err) = p.communicate()
        if p.returncode != 0:
            if out:
                print(out)
            print(err)
        else:
            print('ok')
        return p.returncode
    else:
        return subprocess.call(cmd)

def run_local(script, env):
    require('escript')
    sys.exit(run_or_validate(script, env, validate=False))
    
def validate(script, env):
    require('escript')
    sys.exit(run_or_validate(script, env, validate=True))

#=================================================================================================
# Worker creation
#=================================================================================================
def list_templates():
    templates_path = os.path.join(dirname, '../worker_templates/')
    
    for tname in os.listdir(templates_path):
        print(tname)

def new_worker(template, name):
    templates_path = os.path.join(dirname, '../worker_templates/')
    
    template_name = template or 'empty'
    worker_name = name
    copytree(os.path.join(templates_path, template_name), worker_name)

    for dirpath, dirs, files in os.walk(worker_name):
        for filename in files:
            # replace contents
            with open(os.path.join(dirpath, filename)) as f:
                indata = f.read()
            if template_name in indata:
                new = re.sub('\\b' + template_name, worker_name, indata)
                with open(os.path.join(dirpath, filename), "w") as output:
                    output.write(new)

            if template_name in filename:
                os.rename(os.path.join(dirpath, filename), 
                    os.path.join(dirpath, filename.replace(template_name, worker_name)))

    print('new worker ' + worker_name + ' has been created\n')

#=================================================================================================
# Internal routines
#=================================================================================================
def augment_args(args):
    if not args['--host']:
        args['--host'] = os.environ.get('MZBENCH_API_HOST') or 'localhost:4800'

    if args['--nodes'] is not None:
        try:
            args['--nodes'] = int(args['--nodes'])
        except ValueError:
            args['--nodes'] = args['--nodes'].split(',')

    if args['--nodes_file'] is not None:
        if not os.path.exists(args['--nodes_file']):
            print("File '{0}' doesn't exist.".format(args['--nodes_file']), file=sys.stderr)
            sys.exit(2)
        with open(args['--nodes_file']) as fi:
            args['--nodes'] = fi.read().split('\n')
    
    if args['--workers_per_node'] is not None:
        try:
            args['--workers_per_node'] = int(args['--workers_per_node'])
            if args['--workers_per_node'] <= 0:
                raise ValueError
        except ValueError:
            print("invalid value for --workers_per_node: {0}".format(args['--workers_per_node']))
            sys.exit(4)

    args['--env'] = dict(map(lambda x: x.split("=", 2), args['--env']))

    if args['--deallocate_after_bench'] not in [None, 'true', 'false']:
        print("invalid value for --deallocate_after_bench: {0}".format(
                args['--deallocate_after_bench']))
        sys.exit(4)
    
    if args['--provision_nodes'] not in [None, 'true', 'false']:
        print("invalid value for --provision_nodes: {0}".format(
                args['--provision_nodes']))
        sys.exit(4)

    if args['--exclusive_node_usage'] not in [None, 'true', 'false']:
        print("invalid value for --exclusive_node_usage: {0}".format(
                args['--exclusive_node_usage']))
        sys.exit(4)

    script_file = args['<script_file>']
    if script_file:
        if not os.path.exists(script_file):
            print("File '{0}' doesn't exist.".format(script_file), file=sys.stderr)
            sys.exit(2)
        with open(script_file) as fi:
            args['<script_content>'] = fi.read()

def apply_args(fun, kwargs):
    import inspect
    argnames = inspect.getargspec(fun)[0]

    def get_arg(argname, kwargs):
        result = kwargs.get('--' + argname)
        if result is not None:
            return result
        return kwargs.get('<' + argname + '>')

    return fun(**dict((k, get_arg(k, kwargs)) for k in argnames))

def main():
    args = docopt.docopt(__doc__, version='0.1.0')
    augment_args(args)

    known_commands = [k for k in args
        if not k.startswith('-') and not k.startswith('<')]

    for cmd in known_commands:
        if args[cmd]:
            try:
                return apply_args(globals()[cmd], args)
            except api.MZBenchAPIException as e:
                print(e.args[0])
                sys.exit(1)

    print('Unsupported command', file=sys.stderr)
    sys.exit(1)

def cmd(command):
    import subprocess
    try:
        util.cmd(command)
    except subprocess.CalledProcessError as e:
        sys.exit(e.returncode)

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        sys.exit(130)
