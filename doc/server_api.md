# MZBench server API

API allows driving bench execution and gathering metrics. It can't execute scripts locally or validate because these functions are not remote. As other HTTP APIs it has some endpoints and POST/GET parameters. Please notice that if log/metric compression is enabled at server.config -- you need to be able to handle compressed answers from MZBench server.

Most of the requests return JSON objects, in case of an error it has "reason" -- error text and "reason_code" -- short error textual id.

API requests can be debugged with curl utility, for example:

    $ curl http://mzbench.myserver.com/logs?id=777
    {"reason_code":"not_found","reason":"Benchmark 777 is not found"}

    $ curl http://mzbench.myserver.com/status?id=1236
    {"status":"complete","start_time":"2015-02-10T21:10:34Z","finish_time":"2015-02-10T22:17:06Z"}

## Endpoints

### POST /start?nodes=...

Asyncronous bench start, the only required POST parameter is file to be executed, it should be named "bench", follow [DSL Reference](doc/scenario_dsl.md) to learn how to write such files.

Optional query parameters:

* nodes -- the number of nodes to be allocated or a comma-separated list of nodes to be used (for pre-allocated cluster), default is 1.
* node_commit -- commit or branch to be used for node. It is useful when you need to run different versions of the node with one server.
* deallocate_after_bench -- whether nodes require to be deallocated after the execution has finished. This option can be "true" or "false", default is "true".
* provision_nodes -- whether software installation is required for nodes. In some cases, you already have MZBench installed and you do not need to reinstall it, default is "true".

In a case of success, response is JSON object contains "id" and "status" fields.

Examples:

    # start scenario.erl on five cloud nodes
    curl -XPOST --form bench=@scenario.erl http://mzbench.myserver.com/start?nodes=5
    {"status":"pending","id":122}

    # start scenario.erl on five preallocated nodes (n1 .. n5)
    curl -XPOST --form bench=@scenario.erl http://mzbench.myserver.com/start?nodes=n1,n2,n3,n4,n5

    # start scenario.erl using node from a personal branch
    curl -XPOST --form bench=@scenario.erl http://mzbench.myserver.com/start?node_commit=my_branch

    # start scenario.erl and disable cloud nodes deallocation for debugging purposes
    curl -XPOST --form bench=@scenario.erl http://mzbench.myserver.com/start?deallocate_after_bench

    # start scenario.erl using node from a personal branch
    curl -XPOST --form bench=@scenario.erl http://mzbench.myserver.com/start?node_commit=my_branch

### GET /status?id=number

Request bench status, response fields are "status", "start_time" and "finish_time".

Example:

    curl 'http://mzbench.myserver.com/status?id=122'
    {"status":"complete","start_time":"2015-08-12T07:25:42Z","finish_time":"2015-08-12T07:26:29Z"}

### GET /stop?id=number

Stop requested benchmark, in a case of success, response JSON object will contain the only field "status".

### GET /restart?id=number

Clone and start one of the previously executes scenarios. The only parameter is an id of a bench to be cloned. Response is similar to "/start" function.

### GET /log?id=number

Request benchmark logs, in a case of success, response will be plain bench log text. If the bench is still running, log will be streamed continuously until it finishes.

### GET /data?id=number

Request benchmark data, in a case of success, response will be tab-delimited CSV with timestamp followed by metric name and value, for example:

    curl http://mzbench.myserver.com/data?id=1236
    1439245024  mzb.workers.failed.value    0
    1439245024  mzb.workers.failed.rps.value    0.0
    1439245024  mzb.workers.started.value   2000

Metric data for running benches is streamed in the same manner as logs.

## Conventions

* Bench id is a non-negative integer
* Status is a string, allowed values are: "pending", "running", "complete", "failed", "stopped"
* Date is a string in ISO 8601 format, like this: "2015-08-12T07:25:42Z"