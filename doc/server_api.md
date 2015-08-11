# MZBench server API

API allows to drive bench execution and gather metrics. It could not execute scripts locally or validate because these functions are not remote. As other HTTP APIs it has some endpoints and POST/GET parameters. Please notice that if log/metric compression is enabled at server.config -- you need to be able to handle compressed answers from MZBench server.

Most of the requests return JSON objects, in case of an error it has "reason" -- error text and "reason_code" -- short error textual id.

API requests could be debugged with curl utility, for example:

    $ curl http://mzbench.myserver.com/logs?id=777
    {"reason_code":"not_found","reason":"Benchmark 777 is not found"}

    $ curl http://mzbench.myserver.com/status?id=1236
    {"status":"complete","start_time":"2015-02-10T21:10:34Z","finish_time":"2015-02-10T22:17:06Z"}

## POST /start?nodes=...

Asyncronous bench start, the only required parameter is file to be executed, it should be named "bench", follow [DSL Reference](doc/scenario_dsl.md) to learn how to write such files.

Url parameters:

* nodes -- the number of nodes to be allocated or a list of nodes to be used (for pre-allocated cluster), default is 1.
* node_commit -- commit or branch to be used for node. It is useful when you need to run different versions of the node with one server.
* deallocate_after_bench -- whether nodes require to be deallocated after the execution has finished. This option could be "true" or "false", default is "true".
* provision_nodes -- whether software installation is required for nodes. In some cases, you already have MZBench installed and you do not need to reinstall it, default is "true".

In a case of success, response is JSON object contains "id" and "status" fields.

## POST /run?nodes=...

This call has the same parameters as "/start" but it is syncronous, you will be locked until the end of the execution.

## GET /status?id=number

Request bench status, response fields are "status", "start_time" and "finish_time".

## GET /stop?id=number

Stop requested benchmark, in a case of success, response JSON object would contain the only filed "status".

## GET /restart?id=number

Clone and start one of the previously executes scenarios. The only parameter is an id of a script to be cloned. Response is similar to "/start" function.

## GET /log?id=number

Request benchmark logs, in a case of success, response would be plain bench log text.

## GET /data?id=number

Request benchmark data, in a case of success, response would be tab-delimited CSV with timestamp followed by metric name and value, for example:

    curl http://mzbench.myserver.com/data?id=1236
    1439245024  mzb.workers.failed.value    0
    1439245024  mzb.workers.failed.rps.value    0.0
    1439245024  mzb.workers.started.value   2000
