A complete MZBench installation consists of two parts:

 - MZBench API server that accepts client requests and provides the dashboard.
 - A set of hosts operating as a worker nodes.

Once launched, the API server is permanent, whereas the worker nodes are dynamically
allocated on demand on AWS or another cloud provider.

Here's how you install and configure MZBench for real-life use.


# Installation

Requirements:

 - [Erlang R17+](http://www.erlang.org)
 - C++ compiler (preinstalled on most UNIX-based systems)
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

All parameters are optional.


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

List of [cloud plugins](cloud_plugins.md) that can be used to allocate nodes. One plugin can be listed multiple times with different settings and under different names.

**`<PluginName>`** is an atom identifying a plugin instance.

**`<ModuleName>`** is the name of the plugin module. Each module has its specific **`<Options>`**.

There are four built-in plugins:

[mzb_api_ec2_plugin](cloud_plugins.md#amazon-ec2)
:   Allocate hosts from the Amazon EC2 cloud.

[mzb_staticcloud_plugin](cloud_plugins.md#static-cloud)
:   Allocates hosts from a static pool.

[mzb_dummycloud_plugin](cloud_plugins.md#dummy)
:   Dummy provider, treats localhost as unlimited number of hosts, useful for debug. It can be also used as a reference during [cloud plugin development](cloud_plugins.md#how-to-write-a-cloud-plugin).

[mzb_multicloud_plugin](cloud_plugins.md#multicloud)
:   Allocate hosts from multiple sources with the given ratio.


### network_interface

```erlang
{network_interface, "<ip address>"}
```

Specify the IP address for the dashboard to run on. By default it's `"127.0.0.1"`, so the dashboard is unavailable for external connections.

To open the dashboard to the public, set this param to `"0.0.0.0"`.

!!!warning
    By default MZBench provides no authentication. Opening the dashboard to the public makes your server vulnerable.

    To protect your server, please, see [authentication](deployment.md#authentication) and [protocol](deployment.md#protocol).


### listen_port

```erlang
{listen_port, <port>}
```

Specify the port to access the dashboard.

Default value: `4800`.

### protocol

By default protocol is set to `http`, but `https` is also available. MZBench generates self-signed certificates on first start. If you need to replace them, please use the following configuration parameters:

```erlang
{protocol, https},
{cacertfile, none},
{certfile, "~/.local/share/mzbench_api/server.crt"},
{keyfile, "~/.local/share/mzbench_api/server.key"},
```

CA certificate is not required unless you use custom CA.

### authentication

API server supports Google and GitHub auth.

 - To create Google credentials open [Google API manager page](https://console.developers.google.com). Click Credentials -> Create credentials -> OAuth Client ID -> Web Application, then specify your server URL. Copy `client_id` and `client_secret` to a following structure.

```erlang
{user_authentication,
         [
          {"google", [{caption, "Google"},
                      {client_id, "..."},
                      {client_secret, "..."},
                      {redirect_url, "http://localhost:4800"}]}
         ]
     }
```

`http://localhost:4800` should be replaced with your server's address.

 - To create GitHub credentials open [GitHub developer application](https://github.com/settings/developers) page. Click Register New Application. Put your server url to "Homepage URL" and "Authorization callback URL" and click "Register application".

```erlang
{user_authentication,
         [
          {"github", [{caption, "GitHub"},
                      {client_id, "..."},
                      {client_secret, "..."}]}
         ]
     }
```

If GitHub Enterprise is used it may be usefull to add the following two parameters:

```erlang
    {url, "https://<GitHub URL>"},
    {api_url, "https://<GitHub API URL>"},
```

After successful setup you will be able to authorize yourself at dashboard using Google account and create tokens for Command Line Utilities. To create one hover your name at top-right corner of the dashboard and click "Generate token" link.

### bench_log_file

```erlang
{bench_log_file, "<filename>"}
```

The name of the benchmark log files. The files are stored in `<bench_data_dir>/<bench_id>` where `<bench_data_dir>` is defined in the [bench_data_dir](#bench_data_dir) param and `<id>` is the ID of a particular benchmark.

Default value: `"log.txt"`


### bench_log_compression

```erlang
{bench_log_compression, (deflate|none)}
```

Enable or disable log compression with the [DEFLATE](https://en.wikipedia.org/wiki/DEFLATE) algorithm.

Default value: `deflate`


### bench_metrics_file

```erlang
{bench_metrics_file, "<filename>"}
```

The name of the benchmark metrics data files. The files are stored in `<bench_data_dir>/<bench_id>` where `<bench_data_dir>` is defined in the [bench_data_dir](#bench_data_dir) param and `<id>` is the ID of a particular benchmark.

Default value: `"metrics.txt"`


### bench_metrics_compression

```erlang
{bench_metrics_compression, (none|deflate)}
```

Enable or disable metrics data compression with the [DEFLATE](https://en.wikipedia.org/wiki/DEFLATE) algorithm.

Default value: `none`


### node_git

```erlang
{node_git, "<url>"}
```

The MZBench git repository used to deploy worker nodes.

By default, the MZBench source code is taken from <https://github.com/machinezone/mzbench.git>.


### node_commit

```erlang
{node_commit, "<string>"}
```

The git commit SHA or branch name used to deploy worker nodes.

By default, the latest revision is used.

### node_rsync

```erlang
{node_rsync, "<folder>"}
```

Use local folder when deploying worker nodes. This option has a precedency over node_git.


### node_deployment_path

```erlang
{node_deployment_path, "<path>"}
```

The path to the MZBench installation on node machines.

Default value: `"/.local/share"`


### worker_deployment_path

```erlang
{worker_deployment_path, "<path>"}
```

The to the [workers](workers.md) installation on node machines.

Default value: `"~/.local/share/mzbench_workers"`


### plugins_dir

```erlang
{plugins_dir, "<path>"}
```

Directory with additional [cloud plugins](cloud_plugins.md).

Default value: `"../../../../plugins"`


### bench_data_dir

```erlang
{bench_data_dir, "<path>"}
```

The location to store the data generated during the benchmark.

Default value: `"~/.local/share/mzbench_api/data"`.


### tgz_packages_dir

```erlang
{tgz_packages_dir, "<path>"}
```

The location to store prebuilt worker archives.

Default value: `"~/.local/cache/mzbench_api/packages"`.


### max_bench_num

```erlang
{max_bench_num, <integer>}
```

Maximal number of benchmarks running at the same time.

Default value: `1000`.


### vm_args

```erlang
{vm_args, <args>}
```

Additional arguments for the [Erlang VM](Additional arguments for the [Erlang VM]().

Default value: `[]`.


### ntp_max_timediff

```erlang
{ntp_max_timediff, <float>}
```

Maximum timeout between node creation in seconds.

This check is optional and only prints a warning if fails.

Default value: `0.1`.


## Dev Parameters

Set these params only if you are an MZBench developer.

### bench_read_at_once

```erlang
{bench_read_at_once, <integer>}
```

The number of bytes to read from the logs and metrics feed per request.

Default value: `1024`


### bench_poll_timeout

```erlang
{bench_poll_timeout, <integer>}
```

The timeout between requests to logs and metrics feeds in milliseconds.

Default value: `1000`


### node_log_port

```erlang
{node_log_port, <integer>}
```

The TCP port for the logs feed.

Default value: `4801`.


### node_management_port

```erlang
{node_management_port, <integer>}
```

The TCP port used to control the server internally.

Default value: `4802`.
