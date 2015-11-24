# Built-in Cloud Plugins

## Amazon EC2

`mzb_api_ec2_plugin`

The module allocates hosts in an Amazon EC2 cloud.

The AWS image must have Erlang R17, gcc, gcc-c++, git, and sudo installed. Sudo must be available for non-tty execution, put `Defaults !requiretty` in `/etc/sudoers`.

It is also required to have ssh and tcp 4801/4802 ports connectivity between server and nodes for logs and metrics. Please refer to the EC2 documentation on image publishing process, it can be done from the web AWS console within few clicks. We prepared such image with Amazon Linux (ami-3b90a80b), but you may require some additional software. For this image you need to register and specify your keypair name
with your AWS credentials.

Minimally, the config requires `instance_spec` and `config` keys specified. Learn more about AWS-specific config in the [erlcloud documentation](https://github.com/gleber/erlcloud).

Configuration example:

```erlang
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
```

## Static Cloud

`mzb_staticcloud_plugin`

The module allocated hosts from a prespecified list of hosts.

Configuration example:

```erlang
{cloud_plugins, [{static, #{module => mzb_staticcloud_plugin,
                           hosts => ["host1", "host2"]
                           }}]}
```


## Dummy

`mzb_dummycloud_plugin`

The module does not allocate any hosts, localhost is used instead.

Configuration example:

```erlang
{cloud_plugins, [{dummy, #{module => mzb_dummycloud_plugin}}]}
```


## Multicloud

`mzb_multicloud_plugin`

Combine multiple plugins to allocate hosts from multiple sources.


# How to Write a Cloud Plugin

Cloud plugin is responsible for cluster allocation and deallocation, it is required to implement the following methods:

    -spec start(Name, Opts) -> PluginRef when
        Name :: atom(),
        Opts :: #{},
        PluginRef :: term().

    -spec create_cluster(PluginRef, NumNodes, Config) -> {ok, ClusterID, UserName, [Host]} when
        PluginRef :: term(),
        NumNodes :: pos_integer(),
        Config :: #{},
        ClusterID :: term()
        UserName :: string(),
        Host :: string().

    -spec destroy_cluster(ClusterID) -> ok when
        ClusterID :: term().

'start' function starts the particular instance of the plugin and returns instance reference

* Name - The name of the particular instance of the plugin (specfied in the configuration file).
* Opts - Options passed from the server configuration file for a particular instance of the plugin.
* NumNodes - Number of nodes to be allocated.
* Config - Consists of user, name, description and exclusive_node_usage fields.

'create_cluster' function should allocate required number of hosts and return a tuple of {ok, ClusterID, UserName, HostList}.

* ClusterID - This term will be passed to destroy_cluster/1 when the benchmarking system wants to deallocate the compute nodes. Its content is up to the plugin developer.
* UserName - The benchmarking system will use this user name to ssh to the allocated compute nodes.
* HostList - is the list of hostnames or IPs of allocated nodes.

## Using the Cloud Plugin

Once you have a plugin, it should be specified at MZBench config file inside mzbench_api section:

    [
      {mzbench_api, [
        {cloud_plugins, [{my_cloud1, #{application => privatecloud_plugin,
                                       Opt1 => Value1,
                                       Opt2 => Value2, ...}},
                         ...
                        ]},
        {plugins_dir, "../../plugins"}
        ]},
    ].

This file is normally located at "/etc/mzbench/server.config" or "~/.config/mzbench/server.config".

Plugin-specific config could be specified at the same file inside corresponding section like at the example above. This config is optional.

Note: Plugin application will be started using application:ensure_all_started/1 just before the benchmarks start

Plugin binaries should be placed inside 'plugins_dir', for "../../plugins/" it would be "mzbench/server/plugins/privatecloud-0.1.1/ebin/privatecloud.ebin"
