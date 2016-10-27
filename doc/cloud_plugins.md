# Built-in Cloud Plugins

## Amazon EC2

`mzb_api_ec2_plugin`

The module allocates nodes in an Amazon EC2 cloud.

The AWS node images must have Erlang R17+, gcc, gcc-c++, git, and sudo installed. Sudo must be available for non-tty execution; put `Defaults !requiretty` in `/etc/sudoers`. The SSH and TCP ports 4801-4804 must be open; MZBench uses them internally to send logs and metrics data from nodes to the server.

There's a set of ready-to-use Amazon Linux images with all necessary dependencies for all availability zones:

```
Erlang 19:
us-west-2       ami-da24ffba
us-west-1       ami-77571c17
us-east-2       ami-86055fe3
us-east-1       ami-d0efb2c7
eu-west-1       ami-ab8ec7d8
eu-central-1    ami-bdaa52d2
ap-northeast-1  ami-7a25841b
ap-northeast-2  ami-99eb3ff7
ap-southeast-1  ami-65c36206
ap-southeast-2  ami-11fac772
ap-south-1      ami-615e2a0e
sa-east-1       ami-a5c955c9

Erlang 18:
us-west-2       ami-ee8d718e
us-east-1       ami-61f11f0c
us-west-1       ami-fc28509c
eu-west-1       ami-4554c136
eu-central-1    ami-8d48a5e2
ap-northeast-1  ami-78a24419
ap-northeast-2  ami-ef579f81
ap-southeast-1  ami-66fd2b05
ap-southeast-2  ami-21634c42
sa-east-1       ami-7798101b
```

To use one of these images, specify it in the cloud plugin config as `image_id`:

```erlang
{image_id, "ami-ee8d718e"
```

You can, of course, build your own image based on the requirements listed above. [Learn more](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIs.html#creating-an-ami) in the official Amazon docs.

Configuration example:

```erlang
{cloud_plugins, [{ec2, #{module => mzb_api_ec2_plugin,
                         instance_spec => [
                          {image_id, "ami-ee8d718e"},
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

Minimally, the config requires `instance_spec` and `config` keys specified. Learn more about AWS-specific config in the [erlcloud documentation](https://github.com/gleber/erlcloud).


## Static Cloud

`mzb_staticcloud_plugin`

The module allocated nodes from a list of hosts.

Configuration example:

```erlang
{cloud_plugins, [{static, #{module => mzb_staticcloud_plugin,
                           hosts => ["123.45.67.89", "hostname"]
                           }}]}
```


## Dummy

`mzb_dummycloud_plugin`

The module does not allocate any hosts; localhost is used instead.

Configuration example:

```erlang
{cloud_plugins, [{dummy, #{module => mzb_dummycloud_plugin}}]}
```


## Multicloud

`mzb_multicloud_plugin`

Combine multiple plugins to allocate hosts from multiple sources.

Configration example:

```
{cloud_plugins, [
        {some_cloud_provider1, ...},
        {some_cloud_provider2, ...},
        {cloud_multi, #{module => mzb_multicloud_plugin,
                          clouds => [
                              {some_cloud_provider1, 3},
                              {some_cloud_provider2, 10}
                      ]}}
]}
```

Here, if the cluster is allocated using `cloud_multi`, it will contain 3 nodes from `some_cloud_provider1` for every 10 nodes from `some_cloud_provider2`.


# How to Write a Cloud Plugin

Cloud plugin is an Erlang module with at least three methods:

```erlang
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
```

`start`
:   Start a particular instance of the plugin and get an instance reference.

    `Name`
    :   The name of the particular instance of the plugin specified in the [configuration file](deployment.md#cloud_plugins).
    
    `Opts`
    :   Options passed from the server [configuration file](deployment.md#cloud_plugins) for the particular plugin instance.
    
`create_cluster`
:   Allocate the required number of nodes and return a tuple: `{ok, ClusterID, UserName, HostList}`.   
    
    `NumNodes`
    :   Number of nodes to allocate.
    
    `Config`
    :   Map with keys `user`, `name`, `description`, and `exclusive_node_usage`.

    `ClusterID`
    :   This term will be passed to `destroy_cluster/1` when it's time it deallocate the nodes. Its content is up to the plugin developer.

    `UserName`
    :   SSH username to connect to the allocated nodes.

    `HostList`
    :   List of hostnames or IPs of the allocated nodes.

`destroy_cluster`
:   Deallocate the required number of nodes and return `ok`.

    `ClusterID`
    :   Term returned by `create_cluster/1`.


## Using the Cloud Plugin

Specify the [plugin module](deployment.md#cloud_plugins) and the [path to it](deployment.md#plugins_dir) in the MZBench config file in the `mzbench_api` section:

```erlang
    [
      {mzbench_api, [
        {cloud_plugins, [{my_cloud1, #{module => mycloud_plugin,
                                       Opt1 => Value1,
                                       Opt2 => Value2, ...}},
                         ...
                        ]},
        {plugins_dir, "/path/to/my/plugin"}
        ]},
    ].
```

The plugin binaries must be placed in a subdirectory inside `plugins_dir`, e.g. `/mycloud-0.1.1/ebin/mycloud.ebin`.

!!!note
    Plugin will be started using `application:ensure_all_started/1` just before the benchmark start.