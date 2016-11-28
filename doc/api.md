The **MZBench API** lets you run benchmarks on and collect data from a remote MZBench server with HTTP requests.

The API accepts POST and GET requests and returns data in JSON format or plaintext. In case of an error, the response is `{"reason": "Error description", "reason_code": "short_textual_id"}`:

```bash
$ curl 'http://mzbench.myserver.com/status?id=1'
{
    "finish_time": "2015-11-03T13:41:27Z",
    "start_time": "2015-11-03T13:39:21Z",
    "status": "complete"
}

$ curl 'http://mzbench.myserver.com/logs?id=100500'
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
# Start a benchmark from scenario.bdl:
$ curl -XPOST --form bench=@scenario.bdl http://mzbench.myserver.com/start
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
# Start scenario.bdl on five cloud nodes:
curl -XPOST --form bench=@scenario.bdl http://mzbench.myserver.com/start?nodes=5

# Start scenario.bdl on three preallocated nodes 123.45.67.89 and node.myserver.com:
curl -XPOST --form bench=@scenario.bdl http://mzbench.myserver.com/start?nodes=123.45.67.89,node.myserver.com

# Start scenario.bdl and disable cloud nodes deallocation:
curl -XPOST --form bench=@scenario.bdl http://mzbench.myserver.com/start?deallocate_after_bench=false
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
$ curl 'http://mzbench.myserver.com/restart?id=122'
{
    "id": 123,
    "status": "pending"
}
```

## GET /change_env?id=number&name=value

Change enviroment variables during a bench run.

```bash
$ curl 'http://mzbench.myserver.com/change_env?id=122&loop_rate=2'
{"status":"set"}
```

## GET /log?id=number

Request benchmark system logs. The response is plaintext.

If the benchmark is still running, the logs will be streamed continuously until it finishes.

```bash
$ curl 'http://mzbench.myserver.com/log?id=122'
12:17:16.000 [info] [ API ] Node repo: {git_install_spec,
                                        "https://github.com/machinezone/mzbench.git",
                                        "2729662cb1a393f66b84b25b27f58190afd43e85",
                                        "node"}
...
```

## GET /userlog?id=number

Request benchmark userlogs these logs are worker-specific. The response is plaintext.

If the benchmark is still running, the logs will be streamed continuously until it finishes.

```bash
$ curl 'http://mzbench.myserver.com/userlog?id=122'
14:02:47.080 [info] <0.237.0> Dummy print: "FOO"
14:02:47.245 [info] <0.238.0> Dummy print: "FOO"
14:02:47.331 [info] <0.239.0> Dummy print: "FOO"
...
```

## GET /data?id=number

Request benchmark metrics data. The response will be a tab-delimited CSV with timestamp followed by metric name and value:

```bash
$ curl 'http://mzbench.myserver.com/data?id=1236'
1439245024  mzb.workers.failed.value    0
1439245024  mzb.workers.failed.rps.value    0.0
1439245024  mzb.workers.started.value   2000
```

Metric data for running benches is streamed similarly to [logs](#get-logsidnumber).

## GET /results?id=number

Get bench final results.

```bash
$ curl 'http://mzbench.myserver.com/results?id=122'
{
    "systemload.netrx.utun1.mzb_director49_0": {
        "type": "gauge",
        "percentiles": {
            "min": 12.975898566003504,
            "max": 4831.400347074634,
            "95": 3401.1115750135386,
            "90": 882.3189827822952,
            "50": 18.57593673155794
        }
    },
    "print": {
        "value": 900,
        "type": "counter",
        "rps": {
            "min": 2.996889528358517,
            "max": 3.0588292392263448,
            "95": 2.9984995508247674,
            "90": 2.9980785314691816,
            "50": 2.9975917348002614
        }
    }
...
```

## GET /email_report?id=number&addr=myname@mydomain.com[,...]

Send email report to addresses specified. Please note that smtp should be properly configured for API server to use this function.

## GET /server_logs&severity=level

Stream API server logs starting from the point when this command was executed. These logs contain information on node allocations, bench starts and all other operations performing by the server itself. Please note that server logs could be buffered.

```bash
$ curl 'http://mzbench.myserver.com/server_logs?severity=info'
17:31:36.770 [info] <0.714.0> [ GET ] /stop
17:31:36.770 [info] <0.142.0> [ SERVER ] Stop bench #51 request received
17:31:36.770 [info] <0.570.0> [ BENCH #51 ] Bench final: stopped
17:31:36.771 [info] <0.570.0> [ BENCH #51 ] Stage 'finalize - saving_bench_results': started
17:31:36.771 [info] <0.714.0> [ RESPONSE ] : 200 #{status => <<"stopped">>}
```

## GET /clusters_info

Get information on currently allocated clusters.

```bash
$ curl 'http://mzbench.myserver.com/clusters_info'
[{
    "timestamp": 1479139259,
    "state": "allocated",
    "provider": "mzb_dummycloud_plugin",
    "n": 2,
    "id": 1,
    "hosts": ["127.0.0.1"],
    "bench_id": 24
},
...
```

## GET /deallocate_cluster?id=number

Deallocate cluster and remove it from clusters list.

```bash
$ curl 'http://mzbench.myserver.com/deallocate_cluster?id=1'
{"reason_code":"not_found","reason":"Cluster not found"}
```

## GET /remove_cluster_info?id=number

Remove cluster from clusters list.

```bash
$ curl 'http://mzbench.myserver.com/remove_cluster_info?id=1'
{"reason_code":"not_found","reason":"Cluster not found"}
```

## GET /add_tags?id=number&tags=a,b,c

Add tags to a given benchmark.

```bash
$ curl 'http://mzbench.myserver.com/add_tags?id=50&tags=a,b,c'
{}
```

## GET /remove_tags?id=number&tags=a,b,c

Remove tags from a given benchmark.

```bash
$ curl 'http://mzbench.myserver.com/remove_tags?id=50&tags=a,b'
{}
```

# Conventions

 - Benchmark ID is a non-negative integer.
 - Status is a string: "pending", "running", "complete", "failed", or "stopped".
 - Date is a string in ISO 8601 format: "2015-08-12T07:25:42Z".
