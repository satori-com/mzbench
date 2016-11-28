The **MZBench CLI** lets you control the server and benchmarks from the command line. It utilizes the [MZBench API](api.md), but goes beyond it: it can do things even without a running MZBench server.

The commands are invoked by the `mzbench` script in the `bin` directory:

```bash
# Inside the MZBench directory:
$ ./bin/mzbench start --env foo=bar --nodes=5
# or:
$ cd bin
$ ./mzbench start --env foo=bar --nodes=5
```

# Commands

## Server Control

### start_server

```bash
$ ./bin/mzbench start_server
Executing make -C /path/to/mzbench/bin/../server generate
...
```

Start the MZBench server.

Optional params:

`--config <config_file>`
:   Path to the [server config](deployment.md#configuration) file.


### stop_server

```bash
$ ./bin/mzbench stop_server
Executing make -C /path/to/mzbench/bin/../server generate
...
```

Stop the MZBench server.


### restart_server

```bash
$ ./bin/mzbench restart_server
Executing make -C /path/to/mzbench/bin/../server generate
...
```

Restart the MZBench server, i.e. [stop](#stop_server) + [start](#start_server).

Optional params:

`--config <config_file>`
:   Path to the [server config](deployment.md#configuration) file.


## Benchmark Control

### start

```bash
$ mzbench start --env foo=bar --nodes=5 foo.bdl
{
    "status": "pending", 
    "id": 86
}
```

Start the benchmark from the given scenario file.

Positional param:

`<scenario_file>`
:   The path to the [scenario](scenarios/spec.md) file for the benchmark.
 
Optional params:

`--name <benchmark_name>`
:   Benchmark name.

`--nodes <nodes>`
:   Number of nodes or a comma-separated list of node hostnames to run the benchmark on.

`--nodes_file <filename>`
:   Path to a file with node hostnames separated by newlines.

`--env <name=value> ...`
:   [Environment variable](scenarios/spec.md#environment-variables) definitions.

`--cloud <cloud_provider_name>`
:   Name of the cloud provider from the [server config](deployment.md#configuration). If not specified, the first one on the list is used.

`--email <email> ...`
:   Emails for notifications. When the benchmark finishes, the results will be sent to these emails.

`--deallocate_after_bench false`
:   Skip node deallocation after the benchmark.

`--provision_nodes false`
:   Skip MZBench installation on the nodes.

`--exclusive_node_usage false`
:   Allow multiple nodes to be hosted on the same physical machine, i.e. do not allocate a physical machine exclusively for each node.

`--node_commit=<commit>`
:   Commit hash or branch name in the MZBench repository pointing to the MZBench version to install on the nodes.


### run

```bash
$ mzbench run --env foo=bar --nodes=5 foo.bdl
{
    "status": "pending", 
    "id": 86
}
```

Same as [start](#start-scenario_file), but blocks until the benchmark is complete.

Positional param:

`<scenario_file>`
:   The path to the [scenario](scenarios/spec.md) file for the benchmark.


### run_local

```bash
$ mzbench run_local --env foo=bar foo.bdl
Executing make -C /path/to/mzbench/bin/../node compile
...
```

Run the benchmark without a server. The logs are printed to stdout.

Positional param:

`<scenario_file>`
:   The path to the [scenario](scenarios/spec.md) file for the benchmark.

Optional param:

`--env <name=value> ...`
:   [Environment variable](scenarios/spec.md#environment-variables) definitions.


### validate

```bash
$ mzbench validate foo.bdl
ok
```

Validate the scenario file without executing it.

Positional param:

`<scenario_file>`
:   The path to the [scenario](scenarios/spec.md) file for the benchmark.


### status

```bash
$ mzbench status 86
{
    "status": "provisioning", 
    "start_time": "2015-11-18T13:52:04Z"
}
```

Get the status, start time, and, when completed, finish time of the benchmark.

Positional param:

`<benchmark_id>`
:   The ID of the benchmark as returned by [start](#start) or [run](#run).


### stop

```bash
$ ./bin/mzbench stop 89
{
    "status": "stopped"
}
```

Stop the benchmark.

Positional param:

`<benchmark_id>`
:   The ID of the benchmark as returned by [start](#start) or [run](#run).


### log

```bash
$ ./bin/mzbench log 89
Start of log for bench 89
13:52:05.001 [info] [ API ] Node repo: {git_install_spec,
...
```

View the benchmark system logs.

Positional param:

`<benchmark_id>`
:   The ID of the benchmark as returned by [start](#start) or [run](#run).

### userlog

```bash
$ ./bin/mzbench userlog 89
Start of userlog for bench 89
14:02:47.080 [info] <0.237.0> Dummy print: "FOO"
...
```

View the benchmark worker logs.

Positional param:

`<benchmark_id>`
:   The ID of the benchmark as returned by [start](#start) or [run](#run).


### data

```bash
$ ./bin/mzbench data 89
[
    {
        "target": "workers.pool1.km.ended.rps.value", 
        "datapoints": [
...
```

View the metrics data collected during the benchmark.

Positional param:

`<benchmark_id>`
:   The ID of the benchmark as returned by [start](#start) or [run](#run).


### change_env

```bash
$ mzbench change_env 86 --env foo=baz
{
    "status": "set"
}
```

Redefine an environment variable without interrupting the benchmark:

Positional param:

`<benchmark_id>`
:   The ID of the benchmark as returned by [start](#start) or [run](#run).

Optional param:

`--env <name=value> ...`
:   [Environment variable](scenarios/spec.md#environment-variables) definitions.

### clusters_info

```bash
$ mzbench clusters_info
[
    {
        "timestamp": 1479140779,
        "bench_id": 29,
        "n": 2,
        "state": "allocated",
        "hosts": [
            "127.0.0.1"
        ],
        "provider": "mzb_dummycloud_plugin",
        "id": 5
    },
...
```

Check for currently allocated clusters.

### remove_clusters_info

```bash
$ mzbench remove_cluster_info 5
{}
```

Remove cluster from a list of allocated clusters.

Positional param:

`<cluster_id>`
:   The ID of the cluster as returned by clusters_info.

### deallocate_cluster

```bash
$ mzbench deallocate_cluster 5
{}
```

Deallocate cluster and remove it from a list of allocated clusters.

Positional param:

`<cluster_id>`
:   The ID of the cluster as returned by clusters_info.

### add_tags

```bash
$ mzbench add_tags 50 a,b
{}
```

Add tags to a specified benchmark.

Positional param:

`<benchmark_id>`
:   The ID of the benchmark as returned by [start](#start) or [run](#run).
`<tags>`
:   Comma-separated tag list.

### remove_tags

```bash
$ mzbench remove_tags 50 a,b
{}
```

Remove tags from a specified benchmark.

Positional param:

`<benchmark_id>`
:   The ID of the benchmark as returned by [start](#start) or [run](#run).
`<tags>`
:   Comma-separated tag list.

## Misc

### selfcheck

```bash
$ mzbench selfcheck
Executing /path/to/mzbench/bin/lint.py /path/to/mzbench/bin/../
```

Run the tests on MZBench.


### list_templates

```bash
$ mzbench list_templates
amqp
empty
python_empty
tcp
```

List the available worker templates to base [new workers](workers.md#how-to-write-a-worker) on.

### new_worker

```bash
$ mzbench new_worker --template python_empty bar
new worker bar has been created
```

Create a new worker directory for [development purposes](workers.md#how-to-write-a-worker).

Positional param:

`<worker_name>`
:   The name of the new worker.

Optional params:

`--template <template_name>`
:   The template for the new worker. See the full list of available templates with [list_templates](#list_templates).