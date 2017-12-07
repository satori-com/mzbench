To benchmark a particular protocol or service, MZBench uses a matching _worker_.

**Worker** is a library of [statements](scenarios/spec.md) to access a particular service and collect statistics about its usage. MZBench ships with workers for HTTP and XMPP protocols and a worker that executes console commands, and in many cases they are enough. But if you need to test a specific service, you'll probably need to write a worker for it.

A **worker** can be implemented in either [Erlang](#how-to-write-an-erlang-worker) or [Python](#how-to-write-a-python-worker) programming languages. Please note that the support for Erlang programming language is a little bit more complete.


# How to Write a Worker

## Command Line Utilities

MZBench distribution provides [command line utilities](cli.md) to assist you during your development effort.

!!!note
    All command examples below are executed in the MZBench directory. To run them from a different location, specify the full path to MZBench:
    
        $ /path/to/mzbench/bin/mzbench


### Generate

First, generate an _Erlang_ empty worker application with [`new_worker`](cli.md#new_worker):

```bash
$ ./bin/mzbench new_worker <worker_name>
```

It creates a new directory `<worker_name>` with a minimalistic but fully functional MZBench worker named `<worker_name>`. Particularly interesting files are `src/<worker_name>.erl`, which holds the worker source code, and `examples/<worker_name>.erl`, which contains a simple MZBench [scenario](scenarios/spec.md) using it.

If the worker you develop works over a common protocol like TCP or you wish to develop it using a different programming language, the `new_worker` command can generate your a more elaborate worker already containing the usual boilerplate code for this type of services. List available protocol templates with [`list_templates`](cli.md#list_templates):

```bash
$ ./bin/mzbench list_templates
```

Then generate your worker with the additional `--template` parameter:

```bash
$ ./bin/mzbench new_worker --template <protocol> <worker_name>
```

For instance, you can generate a _Python_ based worker using the following command:

```bash
$ ./bin/mzbench new_worker --template python_empty <worker_name>
```

The worker entry point is contained in the file `src/<worker_name>.py` in this case.

### Compile and Debug

MZBench lets you quickly build a worker and launch a local instance of a benchmarking scenario without running a server.

In the worker directory, run [`run_local <script>`](cli.md#run_local), where `<script>` is the path to the scenario to run:

```bash
$ ./bin/mzbench run_local <script>
```

You can define environment variables with the `--env` option.

!!!note
    All [`make_install`](scenarios/spec.md#make_install) top-level statements are ignored in this execution mode.


### Execute

After debugging, test the worker with a real MZBench server and real cloud nodes. To make it installable on MZBench nodes, specify the worker's git address in your benchmark scenario with [`make_install(git = <URL>, branch = <Branch>, dir = <Dir>)`](scenarios/spec.md#make_install).

[Simple HTTP worker example →](https://github.com/satori-com/mzbench/blob/master/workers/simple_http/src/simple_http_worker.erl)


## How to write an Erlang worker

!!!note
    You need basic Erlang knowledge to understand this tutorial. Refer to [Getting Started with Erlang User's Guide](http://www.erlang.org/doc/getting_started/users_guide.html) or to the [Learn You Some Erlang for great good!](http://learnyousomeerlang.com) book for an introduction to Erlang.

### The Erlang Worker Structure

A worker provides DSL statements and metrics. The statements need not to be independent as the worker can have internal state.

To understand the general structure of a worker, let's see the source code of the `simple_http_worker` provided with the MZBench distribution:

```erlang
-module(simple_http_worker).

-export([initial_state/0, metrics/0,
         get/3]).

initial_state() -> [].

metrics() ->
    [
        {group, "Summary", [
            {graph, #{title => "HTTP Response",
                      units => "N",
                      metrics => [{"http_ok", counter}, {"http_fail", counter}, {"other_fail", counter}]}},
            {graph, #{title => "Latency",
                      units => "microseconds",
                      metrics => [{"latency", histogram}]}}
        ]}
    ].

get(State, _Meta, URL) ->
    StartTime = os:timestamp(),
    Response = hackney:request(get, list_to_binary(URL), [], <<"">>, []),

    case Response of
        {ok, _, _, BodyRef} -> hackney:skip_body(BodyRef);
        _ -> ok
    end,

    Latency = timer:now_diff(os:timestamp(), StartTime),
    mzb_metrics:notify({"latency", histogram}, Latency),

    case Response of
        {ok, 200, _, _} ->
            mzb_metrics:notify({"http_ok", counter}, 1);
        {ok, _, _, _} = Reply ->
            lager:error("GET failed: ~p", [Reply]),
            mzb_metrics:notify({"http_fail", counter}, 1);
        E ->
            lager:error("hackney:request failed: ~p", [E]),
            mzb_metrics:notify({"other_fail", counter}, 1)
    end,
    {nil, State}.
```

It exports three functions: `initial_state/0`, `metrics/0`, and `get/3`. The first two are mandatory for any worker. 

`initial_state/0`
:   Set the worker's initial state. Each parallel job has its own state, so this function will be called once per job start.

`metrics/0`
:   Return a group of metrics generated by this worker. [Declaring Metrics](#declaring-metrics).

The rest of the exported functions define the BDL statements provided by this worker. You can, of course, provide none, although such a worker wouldn't be very useful. The `simple_http_worker`, for instance, provides the `get` statement to fetch particular HTTP document.


### How to Define Statements

To define a BDL statement provided by your worker, export an Erlang function that will be called when this statement is encountered:

```erlang
<statement_name>(State, Meta, [<Param1>, [<Param2>, ...]]) ->
    {ReturnValue, NewState}.
```

The function has the same name as the statement it defines. It accepts at least two parameters: the worker internal state at the moment the statement is executed and *meta* information proplist. The function can also accept any number of other parameters. They correspond to the parameters of the statement.

For example, this function:

```erlang
foo(State, Meta, X, Y) ->
    {nil, State}.
```

is called as `foo(X, Y)` from a benchmarking scenario.

If you want to use key arguments at your function call as `foo(x = 1, y = 2)`, you need to declare three argument function foo and expect third parameter to be [proplist](http://erlang.org/doc/man/proplists.html). For example:

```erlang
foo(State, Meta, Proplist) ->
    1 = proplists:get_value(x, Proplist),
    2 = proplists:get_value(y, Proplist),
    {nil, State}.
```

The statement function must return a tuple of two values:

- the return value of statement; return `nil` if your statement has no return value
- the next worker state

Statements are processed sequentially; each statement receives the state from the previous one and passes it further.
 
Two exceptions are the statements within the [`parallel`](scenarios/spec.md#parallel_1) section and iterations within a `loop` with [parallel > 1](scenarios/spec.md#parallel). In these cases the statements within the same thread share the same sequence of statements, which parallel threads don't. The final state of the whole `parallel` or `loop` statement is the one from the first "thread"; other threads' states don't affect the final state.


### Metrics

Metrics are numerical values collected during the scenario execution. They are the main result of your worker and represent the values you want to evaluate with your benchmark.


#### Metric Types

MZBench currently support four types of metrics:

`counter`
:   A single additive value. New values are simply added to the current one.

`gauge`
:   A single non-additive value. New value replaces the previous one.

`histogram`
:   A set of numerical values that quantify a distribution of values. New values are added to the distribution.

`derived`
:   Evaluated periodically using user-defined function based on another metric values. [Learn more](#derived-metrics).

For example, if you are consuming TCP packets of various sizes and you want to track overall amount of data being transferred, use `counter`. If you are interested in its distribution–mean size, 50 percentile, and so on–you need a `histogram`.


#### Declaring Metrics

Declare the groups of metrics collected by your worker in the list returned by `metrics/0`. Each group corresponds to a structure with following spec:

```
graph_group() :: {group, Name :: string(), [graph()]}
               | graph().
graph()       :: {graph, Opts :: #{metrics => [metric()],
                                   units => string(),
                                   title => string()}}
               | [metric()]
               | metric().
metric()      :: {Name :: string(), Type :: metric_type() }
               | {Name :: string(), Type :: metric_type(), Opts :: map()}.
metric_type() :: counter | gauge | histogram.
```

This structure has a three-level hierarchy:

- Group of graphs is placed on the top of this hierarchy. It consists of one or more graphs and defines a group of graphs under the same name.
- Graph consists of one or more metrics that will be plotted on the same chart. Furthermore, you could specify additional options for the chart: units, title, etc.
- Metric is the lowest unit of this hierarchy. It specifies the name and type of the user-defined metric.

Let's see the following metrics declaration:

```
metrics() -> [{group, "HTTP Requests", [
                {graph, #{metrics => [{"success_requests", counter}, {"failed_requests", counter}]}},
                {graph, #{title => "Request's latency",
                          units => "ms",
                          metrics => [{"latency", histogram}]}}]}].
```

In this example, a group of graphs with the name "HTTP Requests" is created. It consists of several graphs representing the number of successful and failed requests and the request latencies.

A graph can produce several charts. In the example above, the graph for successful and failed request produces two charts: absolute counters and their rps.


#### Dynamic metrics declaration

Metrics could be also declared during the bench run:

```
mzb_metrics:declare_metrics([{group, "HTTP Requests", [
                {graph, #{metrics => [{"success_requests", counter}, {"failed_requests", counter}]}},
                {graph, #{title => "Request's latency",
                          units => "ms",
                          metrics => [{"latency", histogram}]}}]}]).
```

This feature is useful if your metric set depends on some external conditions or script configuration.

#### Derived Metrics

Derived metrics are basically gauges which are evaluated on the director node every ~10sec. To define a derived metric, specify the `resolver` function in the metric opts dictionary. This function is used to evaluate the metric value.

Typical example of a derived metric is the current number of pending requests. We specify a function (`pending_requests`) to calculate the metric value in the metric options and then define the function as simple difference between the number of sent requests and received responses:

```erlang
metrics() -> [{group, "Requests", [
                {graph, #{metrics => [
                    {"requests_sent", counter},
                    {"responses_received", counter},
                    {"pending_requests", derived, #{resolver => pending_requests}}]}},
                ]}].

pending_requests() ->
    mzb_metrics:get_value("requests_sent") - mzb_metrics:get_value("responses_received").
```


#### Hooks

[Pre and post hooks](scenarios/spec.md#pre_hook-and-post_hook) let you run custom code before and after a benchmark. Hooks can be applied on every node or only on the director node. You can change any environment variable in your hook handler and use it in your scenario.

Scenario:

```python
pre_hook():
    exec(all, "yum install mylib")
    worker_call(fetch_url, simple_http_worker)

pool(size = 3, worker_type = simple_http_worker):
    loop(time = 1 sec,
         rate = ramp(linear, 10 rps, 50 rps)):
        get(var("url", "http://mydomain.com"))
```

Worker:

```erlang
fetch_url(Env) ->
    {ok, [{"url", "http://mycdn.com/myresource"} | Env]}.
```


### Updating Metrics

You can update a metric from anywhere inside your worker. Simply call the following function:

```erlang
mzb_metrics:notify({"<metric_name>", <metric_type>}, <value>)
```

The tuple `{"<metric_name>", <metric_type>}` is the same that was used during the metric declaration and identifies the metric to update. `<value>` is the value to add to the metric.

## How to write a Python worker

!!!note
    You need a basic knowledge about the Python programming language to understand this tutorial. Refer to [The Python Tutorial](https://docs.python.org/2.7/tutorial/index.html) for an introduction to Python.

### The Python Worker Structure

A worker provides DSL statements and metrics. The statements need not to be independent as the worker can have internal state.

To understand the general structure of a worker, let's see the source code of the `python_empty` worker template provided with the MZBench distribution:

```python
import random
import mzbench

def initial_state():
    pass


def metrics():
    return [
        [
            ('print', 'counter'),
            ('print_2', 'counter')
        ],
        ('dummy', 'histogram')
    ]


def my_print(msg):
    mzbench.notify(('print', 'counter'), 1)
    mzbench.notify(('print_2', 'counter'), 2)

    print "{0}".format(msg)

    mzbench.notify(('dummy', 'histogram'), random.uniform(0, 1000000000)/7)
```

It contains three functions: `initial_state()`, `metrics()`, and `my_print()`. The first two are mandatory for any worker. 

`initial_state()`
:   Useful to initialize the worker's initial state. Each parallel job has its own separate Python interpreter process, so this function will be called once per job start.

`metrics()`
:   Return a group of metrics generated by this worker. [Declaring Metrics in Python](#declaring-metrics-in-python).

The rest of the exported functions define the DSL statements provided by this worker. You can, of course, provide none, although such a worker wouldn't be very useful. The `python_empty` worker, for instance, provides the `my_print` statement to output a string to the standard output.

### How to Define Statements

To define a DSL statement provided by your worker, write a Python function that will be called when this statement is encountered:

```python
def <statement_name>([<Param1>, [<Param2>, ...]]):
    [return <ReturnValue>]
```

The function has the same name as the statement it defines. It can accept any number of parameters, they correspond to the parameters of the statement.

For example, this function:

```python
def foo(X, Y):
    pass
```

is called as `foo(X, Y)` from a benchmarking scenario.

The statement function may return a value corresponding to the return value of the statement or nothing if the statement has no return value.

Statements are processed sequentially in the same interpreter, so the values of the global variables are conserved between calls.

!!!note
    Please beware that the [`parallel`](scenarios/spec.md#parallel_1) section and [parallel > 1](scenarios/spec.md#parallel) option of the `loop` 
    can't and shouldn't be used with the Python based workers.

### Metrics

Metrics are numerical values collected during the scenario execution. They are the main result of your worker and represent the values you want to evaluate with your benchmark.

#### Metric Types

MZBench currently support three types of metrics with the Python based workers:

`counter`
:   A single additive value. New values are simply added to the current one.

`gauge`
:   A single non-additive value. New value replaces the previous one.

`histogram`
:   A set of numerical values that quantify a distribution of values. New values are added to the distribution.

For example, if you are consuming TCP packets of various sizes and you want to track overall amount of data being transferred, use `counter`. If you are interested in its distribution–mean size, 50 percentile, and so on–you need a `histogram`.

#### Declaring Metrics in Python

Declare the metrics and the groups of metrics collected by your worker in the list returned by the `metrics()` function. Each metric is a tuple `(<Name>, <Type>)` where `<Name>` is the name of the metric and `<Type>` is the metric type. Each group is a list of metrics tuples. A group corresponds to metrics that will be plotted on the same chart.

Let's see the following metrics declaration:

```python
def metrics():
    return [
        [
            ('print', 'counter'),
            ('print_2', 'counter')
        ],
        ('dummy', 'histogram')
    ]
```

In this example, three metrics are declared. `print` and `print_2` are _counters_ and `dummy` is a _histogram_. Moreover, the metrics `print` and `print_2` will be plotted on the same graph.

### Updating Metrics

You can update a metric from anywhere inside your worker. Simply call the following function:

```python
mzbench.notify(('<metric_name>', '<metric_type>'), <value>)
```

The tuple `('<metric_name>', '<metric_type>')` is the same that was used during the metric declaration and identifies the metric to update. `<value>` is the value to add to the metric.

## Importing external metrics

It could be useful to have an external data at MZBench dashboard. For this purpose a worker could fetch metrics from outside or implement statsd server and push all the data to MZBench API server. Please refer to [tcpkali](https://github.com/satori-com/mzbench/blob/master/workers/tcpkali/src/tcpkali_worker.erl) worker code as an example.
