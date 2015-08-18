# Introduction

Today internet is a huge set of ever changing technologies. It would be impossible in practice for MZBench distribution to provide an extensive library of functions to access all the possible services and protocols. Instead, it uses a plugin system called _workers_.

A _worker_ is an Erlang application providing a set of MZBench DSL statements to access a particular service (such as HTTP or SSH server) and collecting a set of statistics about its usage. A small set of standard _workers_ is provided with the distribution, but you will likely need to write your own to access the service you want to benchmark. This document is here to guide you through this process.

Because a MZBench _worker_ is an Erlang application, you need some basic knowledge this programming language to understand this document. You can refer yourself to [Getting Started with Erlang User's Guide](http://www.erlang.org/doc/getting_started/users_guide.html) or to the [Learn You Some Erlang for great good!](http://learnyousomeerlang.com) book for an introduction to the matter.

# Command line utilities

MZBench distribution provide some command line utilities to assist you during your _worker_ development effort.

## New worker generation

First of all, you can generate an empty _worker_ application using the following command (here and later `<MZBENCH_SRC>` means the path to the MZBench source code):

    <MZBENCH_SRC>/bin/mzbench new_worker <worker_name>

This will create a new directory `<worker_name>` containing a minimalistic, but fully functional MZBench _worker_ named `<worker_name>`. Particularly interesting generated files are `src/<worker_name>.erl` containing the _worker_ source code and `examples/<worker_name>.erl` containing a simple MZBench scenario using it.

If the service you want to access is based on some well known protocol, such as TCP, the `new_worker` command can generate your a more elaborate _worker_ already containing the usual boilerplate code for this type of service. You can obtain a list of available protocols by executing:

    <MZBENCH_SRC>/bin/mzbench list_templates

Then generate your _worker_ by adding an additional parameter to the `new_worker` command:

    <MZBENCH_SRC>/bin/mzbench new_worker --template <protocol> <worker_name>

## Worker compilation and debugging

During any serious development, you will certainly need a to do a lot of debugging. If every time you want to launch your test scenario you would need to commit your _worker_ to a remote Git repository and to launch a complete benchmark, it would be cumbersome. So MZBench provide you with a way to quickly build your worker and launch a local instance of your benchmarking scenario using it.

Inside your _worker_ source code directory (the root one, not the `src`), execute the following command replacing `<script>` by the path to the benchmarking scenario you want to run:

    <MZBENCH_SRC>/bin/mzbench run_local <script>

You can, of course, pass the environment variables using the `--env` option. But, please note that all `make_install` top-level statements will be ignored in this execution mode.

## Worker execution

After you have done with debugging, you need to execute your worker in a cloud. To do that, you need to specify worker git address at your benchmark script with `{make_install, [{git, <URL>}, {branch, <Branch>}, {dir, <Dir>}]}`, [http worker example](../workers/simple_http/examples/simple_http.erl).

# General worker structure

An MZBench _worker_ provides a set of DSL statements (i.e. sub-routines) and a set of metrics. The different sub-routines need not to be independent as the worker can have internal state.

To understand the general structure of a _worker_, let see the source code of the `dummy_worker` provided with the MZBench distribution:

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
    initial_state() -> "".

    -spec metrics() -> [graph_group()].
    metrics() -> [{group, "Application Metrics", [
                    {graph, #{ title => "Dummy counter",
                               units => "budger",
                               metrics => [{"dummy_counter", counter}]}}
                 ]}].

    -spec print(state(), meta(), string()) -> {nil, state()}.
    print(State, Meta, Text) ->
        mzb_metrics:notify({"dummy_counter", counter}, 1),
        lager:info("Appending ~p, Meta: ~p~n", [Text, Meta]),
        {nil, State ++ Text}.

As can be seen, it export's 3 functions: `initial_state/0`, `metrics/0` and `print/3`. First two of them are mandatory for any _worker_. 

`initial_state/0` function can return anything and is used to initialize the _worker_ initial state. Each parallel execution job have its own state, so this function will be called once per job start. If your worker is stateless, the good practice is to use an empty string of characters as state.

`metrics/0` function is also mandatory. It return a group of metrics generated by this _worker_. Please refer yourself to [How to define metrics](#how to define metrics) for further reference concerning this function.

All the remaining exported functions defines the DSL statements provided by this _worker_. You can, of course, provide none, although such a _worker_ wouldn't be very useful. The `dummy_worker`, for instance, provide the `print` statement useful to output some string of characters to the standard output. Refer yourself to [How to define statements](#how to define statements) for a more detailed discussion on this topic.

# How to define statements

To define a DSL statement provided by your _worker_ you export an Erlang function that will be called when such a statement is encountered. The exported function is of the following general form:

    <statement_name>(State, [<Param1>, [<Param2>, ...]]) ->
        {ReturnValue, NewState}.

The function must have the same name as the statement it defines. It must take at least two parameters: the _worker_ internal state at the moment the statement is executed and _meta_ information proplist. The function can also accept any number of other parameters. They correspond to the parameters of the statement.

The statement function must return a tuple of two values. The first one is the return value of statement. You must return `nil` if your statement have no return value. The second member of the tuple is the new _worker_ initial state after the statement execution.

For example, the following function:

    foo(State, X, Y) ->
        {nil, State}.

Can be called as `{foo, X, Y}` from a benchmarking scenario.

# How to define metrics

Metrics are numerical values collected during the scenario execution. They are the main result of your _worker_ and represent the values you want to evaluate with your benchmark.

## Metric types

The MZBench currently support three types of metrics:

   * `counter` - A single additive value. New values are simply added to the current one.
   * `gauge` - A single non additive value. New value replaces the previous one.
   * `histogram` - A set of numerical values that quantify a distribution of values. New values are added to the distribution.

For example, if you are consuming TCP packets of various sizes and you want to track overall amount of data being transferred — you need `counter` and if you are interested in its distribution: mean size, 50 percentile and so on — you need a `histogram`.

## Declaring collected metrics

You declare the groups of metrics collected by your _worker_ in the list returned by the `metrics/0` function. Each group corresponds to a structure with following spec:

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


This structure supports a three-level hierarchy:

* Group of graphs is placed on the top of this hierarchy. It consists of the one or more graphs and define a group of graphs under the same name.
* Graph consists of the one or more metrics that will be plotted on the same chart. Furthermore, you could specify additional option for the chart (e.g. units, title etc)
* Metric is the lowest unit of this hierarchy. It specifies the name and type of the user-defined metric.

Let see the following metrics declaration:

    metrics() -> [{group, "HTTP Requests", [
                    {graph, #{metrics => [{"success_requests", counter}, {"failed_requests", counter}]}},
                    {graph, #{title => "Request's latency",
                              units => "ms",
                              metrics => [{"latency", histogram}]}}]}].

In this example, a group of graphs with name "HTTP Requests" will be created. It will consist of the several graphs presented the number of success/failed requests and the request's latencies. Please note then a graph could produce several charts. In the mentioned example, the graph for success and failed request will produce two charts: absolute counters and their rps.

## Updating metrics

You can update a declared metric from anywhere inside your _worker_. Simply call the following function:

    mzb_metrics:notify({"<metric_name>", <metric_type>}, <value>)

The tuple `{"<metric_name>", <metric_type>}` is the same that was used during the metric declaration and identifies the metric to update. The `<value>` is the value to add to the metric.
