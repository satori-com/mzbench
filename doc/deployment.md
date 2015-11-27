A complete MZBench installation consists of two parts:

 - MZBench API server that accepts client requests and provides the dashboard.
 - A set of hosts operating as a worker nodes.

Once launched, the API server is permanent, whereas the worker nodes are dynamically
allocated on demand on AWS or another cloud provider.

Here's how you install and configure MZBench for real-life use.


# Installation

Requirements:

 - [Erlang R17](http://www.erlang.org)
 - C++ compiler
 - [Python](https://www.python.org) 2.6 or 2.7
 - [Pip]((https://pip.pypa.io/en/stable/)) Python package manager

Install and start the MZBench API server:

```bash
# Clone the current MZBench source code:
git clone https://github.com/machinezone/mzbench.git

# Install Python requirements:
sudo pip install -r mzbench/requirements.txt

# Start the server
./mzbench/bin/mzbench start_server
```


# Configuration

!!!note
    Every time you update the configuration, [restart the server](cli.md#restart_server).


## Format

The MZBench server parameters are defined in the configuration file.

By default, the server tries to load configuration from *~/.config/mzbench/server.config* and */etc/mzbench/server.config*.

To specify a configuration file from a different location, use the `--config` param:

```bash
$ ./bin/mzbench start_server --config /path/to/server.config
```

The configuration file is an Erlang list with the `mzbench_api` tuple. This tuple holds the list of the server [parameters](#parameters): 

```erlang
[
    {mzbench_api, [
        {network_interface, "127.0.0.1"},
        {listen_port, 80}
    ]}
].
```

Here, two parameters are specified: [network_interface](#network_interface) and [listen_port](#listen_port).


## Parameters

### cloud_plugins

```erlang
{cloud_plugins: [
    {<PluginName>, #{module => <ModuleName>,
                    <Option1> => <Value1>,
                    <Option2> => <Value2>,
                    ...
                    }
    },
    ...
    ]
}
```

List of [cloud plugins](cloud_plugin.md) that can be used to allocate nodes. One plugin can be listed multiple times with different settings and under different names.

**`<PluginName>`** is an atom identifying a plugin instance.

**`<ModuleName>`** is the name of the plugin module. There're four modules available by default:

mzb_api_ec2_plugin
:   Amazon EC2.

mzb_dummycloud_plugin
:   Dummy provider, doesn't really do anything, but can be used as a reference during [cloud plugin development](cloud_plugin.md#how-to-write-a-cloud-plugin).

mzb_multicloud_plugin
:   ???

mzb_staticcloud_plugin
:   Plugin that allocates hosts from a static pool.


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

#### Configuration of static cloud plugin

mzb_staticcloud_plugin does not allocate any hosts, the specified list of hosts is used instead.

Configuration example:

    {cloud_plugins, [{static, #{module => mzb_staticcloud_plugin,
                               hosts => ["host1", "host2"]
                               }}]}

#### Configuration of dummy cloud plugin

mzb_dummycloud_plugin does not allocate any hosts, localhost is used instead.

Configuration example:

    {cloud_plugins, [{dummy, #{module => mzb_dummycloud_plugin}}]}

### bench_data_dir

"<path>"}`

This parameter specifies where to store the various benchmark generated data.

By default `mzbench_api_data` in the home directory of the user who started the server.

### listen_port

<port>}`

This parameter is used to specify the port used to access the server dashboard.

By default, the dashboard listens on `4800`.

### network_interface

, "<ip address>"}`

This parameter is used to specify the interface for dashboard to listen (by default it is "127.0.0.1"),
so the dashboard won't be available for any external connections. To open dashboard
 for everyone, specify "0.0.0.0"
 _Warning:_ mzbench doesn't provide any authentication and opening it
to everyone would bring additional vulnerability to your server, mzbench dashboard should be protected with an external auth proxy like nginx.

### node_git

, "<url>"}`

Specifies the MZBench Git repository used to deploy the worker nodes.

By default, the MZBench source code will be taken from `https://github.com/machinezone/mzbench.git`.

### node_commit

, "<string>"}`

Specifies Git commit SHA or Git branch name used to deploy the worker nodes.

### ntp_max_timediff

, <float>}`

Maximum distance between node timers (default is 0.1), this check is optional and would only print a warning if failed.

### bench_log_compression

### bench_metrics_compression