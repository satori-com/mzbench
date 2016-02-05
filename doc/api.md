The **MZBench API** lets you run benchmarks on and collect data from a remote MZBench server with HTTP requests.

The API accepts POST and GET requests and returns data in JSON format or plaintext. In case of an error, the response is `{"reason": "Error description", "reason_code": "short_textual_id"}`:

```bash
$ curl http://mzbench.myserver.com/status?id=1
{
    "finish_time": "2015-11-03T13:41:27Z",
    "start_time": "2015-11-03T13:39:21Z",
    "status": "complete"
}

$ curl http://mzbench.myserver.com/logs?id=100500
{
    "reason": "Benchmark 100500 is not found",
    "reason_code": "not_found"
}
```

!!!important
    If you have [logs](deployment.md#bench_log_compression) or [metrics compression](deployment.md#bench_metrics_compression) enabled in the server config, make sure you can handle compressed responses from the MZBench API.


# Endpoints

## POST /start

Ask the server to start a benchmark from the given [scenario file](scenarios/spec.md). The file is submitted as form data. A successful response is a JSON object with "id" and "status" fields:

```bash
# Start a benchmark from scenario.erl:
$ curl -XPOST --form bench=@scenario.erl http://mzbench.myserver.com/start
{
    "id": 46,
    "status": "pending"
}
```

Optional query parameters:

benchmark_name
:   The name of the benchmark.

nodes
:   The number of nodes to be allocated or a comma-separated list of node hostnames or IP addresses.

    Default is "1".

deallocate_after_bench=false
:   Pass to cancel node deallocation after the benchmark has finished.

provision_nodes=false
:   Pass to skip MZBench installation on the nodes.

node_commit
:   Commit hash or branch name in the [MZBench repository](https://github.com/machinezone/mzbench/) that should be installed on the nodes.

Examples:

```bash
# Start scenario.erl on five cloud nodes:
curl -XPOST --form bench=@scenario.erl http://mzbench.myserver.com/start?nodes=5

# Start scenario.erl on three preallocated nodes 123.45.67.89 and node.myserver.com:
curl -XPOST --form bench=@scenario.erl http://mzbench.myserver.com/start?nodes=123.45.67.89,node.myserver.com

# Start scenario.erl and disable cloud nodes deallocation:
curl -XPOST --form bench=@scenario.erl http://mzbench.myserver.com/start?deallocate_after_bench=false
```


## GET /status?id=number

Request the benchmark status. A successful response is a JSON object with the fields "status", "start_time," and "finish_time":

```bash
$ curl 'http://mzbench.myserver.com/status?id=122'
{
    "status":"complete",
    "start_time":"2015-08-12T07:25:42Z",
    "finish_time":"2015-08-12T07:26:29Z"
}
```

## GET /stop?id=number

Stop the benchmark. In case of success, the response JSON object will contain one field, "status":

```bash
$ curl 'http://mzbench.myserver.com/stop?id=122'
{
    "status": "stopped"
}
```


## GET /restart?id=number

Clone and start a previously executed benchmark. The response is the same as for the [start](#post-start) endpoint:

```bash
$ curl http://mzbench.myserver.com/restart?id=122
{
    "id": 123,
    "status": "pending"
}
```


## GET /logs?id=number

Request benchmark logs. The response is plaintext.

If the benchmark is still running, the logs will be streamed continuously until it finishes.

```bash
$ curl http://mzbench.myserver.com/logs?id=122
12:17:16.000 [info] [ API ] Node repo: {git_install_spec,
                                        "https://github.com/machinezone/mzbench.git",
                                        "2729662cb1a393f66b84b25b27f58190afd43e85",
                                        "node"}
...
```


## GET /data?id=number

Request benchmark metrics data. The response will be tab-delimited CSV with timestamp followed by metric name and value:

```bash
$ curl http://mzbench.myserver.com/data?id=1236
1439245024  mzb.workers.failed.value    0
1439245024  mzb.workers.failed.rps.value    0.0
1439245024  mzb.workers.started.value   2000
```

Metric data for running benches is streamed similarly to [logs](#get-logsidnumber).


# Conventions

 - Benchmark ID is a non-negative integer.
 - Status is a string: "pending", "running", "complete", "failed", or "stopped".
 - Date is a string in ISO 8601 format: "2015-08-12T07:25:42Z".
