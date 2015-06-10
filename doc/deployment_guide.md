# Introduction

This document explains how to deploy and configure a MZBench API server on your own infrastructure.

# MZBench architecture

In its more general form, a complete MZBench installation consists of several separate hosts:

   * An MZBench API server that accepts the client requests and provides the dashboard;
   * A metric gathering host, usually Graphite, providing the graph plotting services;
   * A set of hosts to use as a worker nodes.

The API server and Graphite hosts are permanent, while the worker nodes can be dynamically allocated when needed, for example on Amazon.

This document details the installation and configuration of the API server. Installation and configuration of the Graphite host is out of it scope. Moreover, the Graphite host is optional, although you will certainly need it for any serious use.

# Server installation

## Requirements

To run an MZBench API server, you need to install the following packages:

   * A C++ compiler;
   * Erlang R17 distribution (http://www.erlang.org);
   * Python v2.6 (https://www.python.org);
   * PIP Python package manager (https://pip.pypa.io/en/stable/).

## Server installation

To install the MZBench API server, perform the following commands:

    # Clone the current MZBench source code
    git clone https://github.com/machinezone/mzbench.git
    
    # Install additional Python packages
    pip install -r mz-bench/requirements.txt
    
    # Install the server
    cd mz-bench
    ./bin/mz-bench install_server

    # Start the server
    ./bin/mz-bench start_server

    # Stop the server
    ./bin/mz-bench start_server

# Server configuration

To apply configuration changes you need to stop (if started) and start the server.

## Configuration file format

The MZBench server parameters are set in `/mz/mz_bench_api/mz_bench_server.config` configuration file. 

This file is essentially an Erlang term. At the top level, it is a list of tuples, terminated by the dot. Each tuple represent a configuration category. It first element is an atom identifying the category, its second element is a list of actual parameters. Each of parameters is a tuple by itself, containing the parameter name and the value.

For the sake of clarity, let's see the following example configuration:

    [
        {mz_bench_api, [
            {graphite, "172.21.8.192"},
            {listen_port, 80}
        ]}
    ].

It contains only one category: `mz_bench_api`. This category is used for the parameters of the server itself.

Here, we specify two parameters: `graphite` and `listen_port`. `graphite` specify the IP address of the host containing the Graphite server to use (here `172.21.8.192`). `listen_port` specify on which port should be used to access the server dashboard (here `80`).

## Server parameters (`mz_bench_api`)

These parameters are going to the `mz_bench_api` category.

### `{cloud_plugin, {module, <name>} | {application, <name>}}`

A plugin responsible for node allocation, {module, mzb_api_dummycloud_plugin} by default.
mzb_api_dummycloud_plugin does not allocate any hosts, localhost will be used for node application.
mzb_api_ec2_plugin module allocates hosts in EC2 and requires additional configuration.
Application should be a valid erlang application with its own configuration and dependencies.

### `{aws_config, [{key, value}]}` `{ec2_instance_spec, [{key, value}]}`

AWS specific configuration, details could be found in erlcloud documentation.
AWS image tips: an image should contain Erlang R17, gcc, gcc-c++, git, sudo.
sudo shoud be available for non-tty execution (put "Defaults !requiretty" to /etc/sudoers).
It is also required to have ssh and tcp 4801/4802 ports connectivity between server and nodes for
logs and metrics. Please refer EC2 documentation on image publish process, it can be done from
web AWS console within few clicks.

### `{bench_data_dir, "<path>"}`

This parameters specifies where to store the various benchmark generated data.

By default `mz_bench_api_data` in the home directory of the user who started the server.

### `{graphite, "<hostname or ip>"}`

This parameters specifies the host or IP address of the Graphite server to send the metrics to.

### `{graphite_api_key, "<string>"}`

Hostedgraphite.com API key.

### `{graphite_url, "<URL>"}`

Hostedgraphite.com URL, usually something like this: https://www.hostedgraphite.com/xxxxxxxx/graphite/

By default, no metrics will be send. 

### `{listen_port, <port>}`

This parameter is used to specify the port used to access the server dashboard.

By default, the dashboard listens on `4800`.

### `{network_interface, "<ip address>"}`

This parameter is used to specify the interface for dashboard to listen by default it is "127.0.0.1",
it means that the dashboard wont be available for any external connections, specify "0.0.0.0"
to open dashboard for everyone. _Warning:_ mz-bench doesn't provide any authentication and opening it
to everyone would bring additional vulnerability to your server, mz-bench dashboard should be protected with an external auth proxy like nginx.

### `{mzbench_git, "<url>"}`

Specify the MZBench Git repository used to deploy the worker nodes.

By default, the MZBench source code will be taken from `https://github.com/machinezone/mzbench.git`.

### `{ntp_max_timediff, <float>}`

Maximum distance between node timers (default is 0.1), this check is optional and would only print a warning if failed.
