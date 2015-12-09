Today internet is a huge set of ever changing technologies. It would be impossible in practice for MZBench distribution to provide an extensive library of functions to access all the possible services and protocols. Instead, it uses a plugin system called _workers_.

**Worker** is an Erlang application providing [statements](scenarios.md) to access a particular service (such as HTTP or SSH server) and collecting statistics about its usage. Some workers are bundled with the distribution, but you will likely need to write your own to access the service you want to benchmark. This document is here to guide you through this process.

Because a worker is an Erlang application, you need basic knowledge of this programming language to understand this document. Refer to [Getting Started with Erlang User's Guide](http://www.erlang.org/doc/getting_started/users_guide.html) or to the [Learn You Some Erlang for great good!](http://learnyousomeerlang.com) book for an introduction to the matter.


# How to Write a Worker

## Command Line Utilities

MZBench distribution provides [command line utilities](cli.md) to assist you during your development effort.

!!!note
    All command examples below are executed in the MZBench directory. To run them from a different location, specify the full path to MZBench:
    
        $ /path/to/mzbench/bin/mzbench


### Generate

First of all, generate an empty worker application with [`new_worker`](cli.md#new_worker):

```bash
$ ./bin/mzbench new_worker <worker_name>
```

It creates a new directory `<worker_name>` with a minimalistic but fully functional MZBench worker named `<worker_name>`. Particularly interesting files are `src/<worker_name>.erl`, which holds the worker source code, and `examples/<worker_name>.erl`, which contains a simple MZBench [scenario](scenarios.md) using it.

If the service you develop a worker for is based on a well-known protocol like TCP, the `new_worker` command can generate your a more elaborate worker already containing the usual boilerplate code for this type of service. List available protocol templates with [`list_templates`](cli.md#list_templates):

```bash
$ ./bin/mzbench list_templates
```

Then generate your worker with the additional `--template` parameter:

```bash
./bin/mzbench new_worker --template <protocol> <worker_name>
```


### Compile and Debug

During development, you'll need to do a lot of debugging. If you had to launch a complete benchmark every time you want to launch your test scenario, it would be cumbersome. MZBench provides a quicker way to build your worker and launch a local instance of your benchmarking scenario using it.

In the worker directory, run [`run_local <script>`](cli.md#run_local), where `<script>` is the path to the scenario to run:

```bash
$ ./bin/mzbench run_local <script>
```

You can define environment variables with the `--env` option.

!!!note
    All [`make_install`](scenarios.md#make_install) top-level statements are ignored in this execution mode.


### Execute

After you have done with the debugging, execute your worker in a cloud: Specify the worker git address in your benchmark scenario with [`{make_install, [{git, <URL>}, {branch, <Branch>}, {dir, <Dir>}]}`](scenarios.md#make_install).

[Simple HTTP worker example →](../workers/simple_http/examples/simple_http.erl)


## General Worker Structure

A worker provides DSL statements and metrics. The statements need not to be independent as the worker can have internal state.

To understand the general structure of a worker, let's see the source code of the `dummy_worker` provided with the MZBench distribution:

```erlang
-module(dummy_worker).
-export([initial_state/0, metrics/0,
         print/3]).

-include("mzb_types.hrl").

-type state() :: string().
-type meta() :: [{Key :: atom(), Value :: any()}].

-type graph_group() :: {group, Name :: string(), [graph()]}
                     | graph().
-type graph()       :: {graph, Opts :: #{metrics => [metric()],
                                         units => string(),
                                         title => string()}}
                     | [metric()]
                     | metric().
-type metric()      :: {Name :: string(), Type :: metric_type() }
                     | {Name :: string(), Type :: metric_type(), Opts :: map() }.
-type metric_type() :: counter | gauge | histogram.

-spec initial_state() -> state().
initial_state() -> [].

-spec metrics() -> [graph_group()].
metrics() -> [{group, "Application Metrics", [
                {graph, #{ title => "Dummy counter",
                           units => "budger",
                           metrics => [{"dummy_counter", counter}]}}
             ]}].

-spec print(state(), meta(), string()) -> {nil, state()}.
print(State, Meta, Text) ->
    mzb_metrics:notify({"dummy_counter", counter}, 1),
    lager:info("Printing ~p, Meta: ~p~n", [Text, Meta]),
    {nil, State}.
```

It exports three functions: `initial_state/0`, `metrics/0`, and `print/3`. The first two are mandatory for any worker. 

`initial_state/0`
:   Set the worker's initial state. Each parallel job has its own state, so this function will be called once per job start.

`metrics/0`
:   Return a group of metrics generated by this worker. [How to define metrics](#how-to-define-metrics).

The rest of the exported functions define the DSL statements provided by this worker. You can, of course, provide none, although such a worker wouldn't be very useful. The `dummy_worker`, for instance, provides the `print` statement to output a string to the standard output. 


## How to Define Statements

To define a DSL statement provided by your worker, export an Erlang function that will be called when this statement is encountered:

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

is called as `{foo, X, Y}` from a benchmarking scenario.

The statement function must return a tuple of two values:

- the return value of statement; return `nil` if your statement has no return value
- the next worker state

Statements are processed sequentially; each statement receives the state from the previous one and passes it further.
 
Two exceptions are the statements within the [`{parallel}`](scenarios.md#parallel_1) section and iterations within a `{loop}` with [parallel > 1](scenarios.md#parallel). In these cases the statements within the same thread share the same sequence of statements, which parallel threads don't. The final state of the whole `{parallel}` or `{loop}` statement is the one from the first "thread"; other threads' states don't affect the final state.


## Metrics

Metrics are numerical values collected during the scenario execution. They are the main result of your worker and represent the values you want to evaluate with your benchmark.


### Metric Types

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


### Declaring Metrics

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


### Derived Metrics

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


### Hooks

[Pre and post hooks](scenarios.md#pre_hook-and-post_hook) let you run custom code before and after a benchmark. Hooks can be applied on every node or only on the director node. You can change any environment variable in your hook handler and use it in your scenario.

Scenario:

```erlang
{pre_hook, [
    {exec, all, "yum install zlib"},
    {worker_call, fetch_commit, my_worker}
]}

{pool, [{size, 3}, {worker_type, dummy_worker}], [
    {loop, [{time, {1, sec}},
            {rate, {ramp, linear, {10, rps}, {50, rps}}}],
        [{print, {var, "commit", "default"}}]}]},
```

Worker:

```erlang
fetch_commit(Env) ->
    {ok, [{"commit", "0123456"} | Env]}.
```


### Updating Metrics

You can update a metric from anywhere inside your worker. Simply call the following function:

```erlang
mzb_metrics:notify({"<metric_name>", <metric_type>}, <value>)
```

The tuple `{"<metric_name>", <metric_type>}` is the same that was used during the metric declaration and identifies the metric to update. `<value>` is the value to add to the metric.
