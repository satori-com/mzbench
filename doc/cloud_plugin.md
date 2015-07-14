# Implementing a cloud plugin for MZBench

Cloud plugin is responsible for cluster allocation and deallocation, it is required to implement the following methods:

    -spec create_cluster(Name :: string(), NumNodes :: pos_integer(), Config :: #{}) -> {ok, term(), string(), [string()]}.

    -spec destroy_cluster(term()) -> ok.

* Name - Unique cluster name provided by MZBench.
* NumNodes - Number of nodes to be allocated.
* Config - Consists of user, description and exclusive_node_usage fields.

'create_cluster' function should return a tuple of {ok, ClusterID, UserName, HostList}.

* ClusterID - This term will be passed to destroy_cluster/1 when the benchmarking system wants to deallocate the compute nodes. Its content is up to the plugin developer.
* UserName - The benchmarking system will use this user name to ssh to the allocated compute nodes.
* HostList - is the list of hostnames or IPs of allocated nodes.

## Using a cloud plugin

Once you have a plugin, it should be specified at MZBench config file inside mzbench_api section:

    [
      {mzbench_api, [
        {cloud_plugin, {application, privatecloud_plugin}},
        {plugins_dir, "../../plugins"}
        ]},
      {privatecloud_plugin, [
        {name1, value2},
        {name2, value2}
      ]}].

This file is normally located at "/etc/mzbench/server.config" or "~/.config/mzbench/server.config".

Plugin-specific config could be specified at the same file inside corresponding section like at the example above. This config is optional.

Plugin name atom is preceded by 'module' or 'application' atom, in case of 'application' application:start(privatecloud_plugin) is invoked at MZBench server start. Application could have its own supervisor tree. In case of module no extra calls or config loads are executed.

Plugin binaries should be placed inside 'plugins_dir', for "../../plugins/" it would be "mzbench/server/plugins/privatecloud-0.1.1/ebin/privatecloud.ebin"
