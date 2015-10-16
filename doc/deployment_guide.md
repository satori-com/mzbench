# Introduction

This document explains how to deploy and configure an MZBench API server on your own infrastructure.
The scope includes installation and configuration of the API server. Installation
and configuration of the Graphite host is not covered. The Graphite host is
 optional, although you will certainly need it for any serious use.


# MZBench architecture

In its more general form, a complete MZBench installation consists of several separate hosts:

   * An MZBench API server that accepts the client requests and provides the dashboard
   * A metric gathering host, usually Graphite, providing the graph plotting services
   * A set of hosts to use as a worker nodes

The API server and Graphite hosts are permanent, while the worker nodes can be dynamically
allocated when needed, for example on Amazon.


# Server installation

## Requirements

To run an MZBench API server, you need to install the following packages:

   * A C++ compiler
   * Erlang R17 distribution (http://www.erlang.org)
   * Python v2.6 (https://www.python.org)
   * PIP Python package manager (https://pip.pypa.io/en/stable/)

## Server installation

To install the MZBench API server, perform the following commands:

    # Clone the current MZBench source code
    git clone https://github.com/machinezone/mzbench.git

    # Install additional Python packages
    sudo pip install -r mzbench/requirements.txt

    # Start the server
    ./mzbench/bin/mzbench start_server

    # Stop the server
    ./mzbench/bin/mzbench stop_server

# Server configuration

To apply configuration changes you need to restart the server if it's running.

## Configuration file format

The MZBench server parameters are set in configuration file.
It can be passed as an argument to bin/mzbench start_server command.

For example: bin/mzbench start_server --config ./server.config

By default, the server tries to load configuration from the following places:

~/.config/mzbench/server.config
/etc/mzbench/server.config

The configuraion file is essentially an Erlang term. At the top level, it is a list of tuples, terminated by the dot.
 Each tuple represent a configuration category. Its first element is an atom identifying the
 category, its second element is a list of actual parameters. Each of parameters is a tuple
  by itself, containing the parameter name and the value.

For the sake of clarity, let's see the following example configuration:

    [
        {mzbench_api, [
            {graphite, "172.21.8.192"},
            {listen_port, 80}
        ]}
    ].

It contains only one category: `mzbench_api`. This category is used for the parameters of the server itself.

Here, we specify two parameters: `graphite` and `listen_port`. `graphite` specifies the IP
 address of the host containing the Graphite server to use (here `172.21.8.192`). `listen_port`
 specifies which port should be used to access the server dashboard (here `80`).

## Server parameters (`mzbench_api`)

These parameters are going to the `mzbench_api` category.

### `{cloud_plugins, [{Name :: atom(), Opts :: #{}}]}`

Plugins responsible for node allocation, [{dummy, #{module => mzb_dummycloud_plugin, hosts => ["localhost"]}}] by default.
Opts must contain either module or application key, other keys in Opts depend on particular plugin type.
It is possible to specify several plugins. The actually used plugin can be specified with --cloud option of the bin/mzbench utility.
There are two cloud plugins available in mzbench so far. See below for configuration details and a few examples.
See also: [Cloud plugin creation guide](doc/cloud_plugin.md)

#### Configuration of the AWS EC2 cloud plugin

mzb_api_ec2_plugin module allocates hosts in EC2 and requires 'instance_spec' and 'config' keys to be specified in Opts (see above).
For AWS specific configuration details, see [erlcloud documentation](https://github.com/gleber/erlcloud).
AWS image tips: an image should contain Erlang R17, gcc, gcc-c++, git, sudo.
sudo shoud be available for non-tty execution (put "Defaults !requiretty" to /etc/sudoers).
It is also required to have ssh and tcp 4801/4802 ports connectivity between server and nodes for
logs and metrics. Please refer EC2 documentation on image publish process, it can be done from
web AWS console within few clicks. We prepared such image with Amazon Linux (ami-3b90a80b), but you
may require some additional software. For this image you need to register and specify your keypair name
with your AWS credentials.

Configuration example:

    {cloud_plugins, [{ec2, #{module => mzb_api_ec2_plugin,
                             instance_spec => [
                              {image_id, "ami-2c03b22c"},
                              {group_set, ""},
                              {key_name, "-"},
                              {subnet_id, "-"},
                              {instance_type, "t2.micro"},
                              {availability_zone, "us-west-2a"}
                            ],
                            config => [
                              {ec2_host, "ec2.us-west-2.amazonaws.com"},
                              {access_key_id, "-"},
                              {secret_access_key, "-"}
                             ]
                            instance_user => "ec2-user",
                        }}]},

#### Configuration of dummy cloud plugin

mzb_api_dummycloud_plugin does not allocate any hosts, the specified list of hosts is used instead.

Configuration example:

    {cloud_plugins, [{dummy, #{module => mzb_dummycloud_plugin,
                               hosts => ["host1", "host2"]
                               }}]}

### `{bench_data_dir, "<path>"}`

This parameter specifies where to store the various benchmark generated data.

By default `mzbench_api_data` in the home directory of the user who started the server.

### `{graphite, "<hostname or ip>"}`

This parameter specifies where to send the metrics, the host or IP address of the Graphite server.

### `{graphite_api_key, "<string>"}`

Hostedgraphite.com API key.

### `{graphite_url, "<URL>"}`

Hostedgraphite.com URL, usually something like this: https://www.hostedgraphite.com/xxxxxxxx/graphite/

By default, no metrics will be sent.

### `{listen_port, <port>}`

This parameter is used to specify the port used to access the server dashboard.

By default, the dashboard listens on `4800`.

### `{network_interface, "<ip address>"}`

This parameter is used to specify the interface for dashboard to listen (by default it is "127.0.0.1"),
so the dashboard won't be available for any external connections. To open dashboard
 for everyone, specify "0.0.0.0"
 _Warning:_ mzbench doesn't provide any authentication and opening it
to everyone would bring additional vulnerability to your server, mzbench dashboard should be protected with an external auth proxy like nginx.

### `{node_git, "<url>"}`

Specifies the MZBench Git repository used to deploy the worker nodes.

By default, the MZBench source code will be taken from `https://github.com/machinezone/mzbench.git`.

### `{node_commit, "<string>"}`

Specifies Git commit SHA or Git branch name used to deploy the worker nodes.

### `{ntp_max_timediff, <float>}`

Maximum distance between node timers (default is 0.1), this check is optional and would only print a warning if failed.
