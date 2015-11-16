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

## Using a cloud plugin

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
