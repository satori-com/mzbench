In MZBench, scenarios are .erl files written in a special DSL. Think of it as a simplified, declarative version of Erlang.

*[DSL]: Domain-Specific Language

MZBench test scenarios consist of *lists*, *tuples*, and *atoms*:

  - A **list** is a comma-separated sequence enclosed in square brackets: `[A, B, C]`
  - A **tuple** is a comma-separated sequence enclosed in curly braces: `{A, B, C}`
  - **Atoms** are reserved words—function names, rate units, conditions. Basically, anything that's not a list, a tuple, a number, or a string is an atom: `print, make_install, lte, rpm`  

The whole scenario is a list of tuples with a dot at the end:

```erlang
[
    {function1, param1, param2},
    {function2, [{param_name, param_value}]},
    ...
].
```

Each of these tuples is called a *statement*. A statement represents a function call: the first item is the function name, the others are the params to pass to it. Each param in its turn can also be a list, a tuple, or an atom.

Some statements only appear at the top level of a scenario. They're called *top-level statements*. There're two kinds of top-level statements: [directives](#directives) and [pools](#pools).

[See live examples of MZBench scenarios on GitHub →](https://github.com/satori-com/mzbench/tree/master/examples)


# Directives

**Directives** prepare the system for the benchmark and clean it up after it. This includes installing an external [worker](../workers.md) on test nodes, registering resource files, checking conditions, and executing shell commands before and after the test.

## Top-Level Directives

All top-level directives are optional.

### make_install

```erlang
{make_install, [{git, "<URL>"}, {branch, "<Branch>"}, {dir, "<Dir>"}]}
```

Install an external worker from a remote git repository on the test nodes before running the benchmark.

MZBench downloads the worker and builds a .tgz archive, which is then distributed among the nodes and used in future provisions.

The following actions are executed during `make_install`:

```bash
$ git clone <URL> temp_dir
$ cd temp_dir
$ git checkout <Branch>
$ cd <Dir>
$ make generate_tgz
```

If `branch` is not specified, the default git branch is used.

If `dir` is not specified, `.` is used.

### defaults

```erlang
{defaults, [{"<VarName1>", <Value1>}, {"<VarName2>", <Value2>}, ...]}
```

Allows to define the default values for environment variables, i.e. the values used if no value was provided for this variable on the command line.

See [Environment Variables](#environment-variables) for additional information.

### include_resource

```erlang
{include_resource, <ResourceName>, "<FileName>", <Type>}
{include_resource, <ResourceName>, "<FileURL>", <Type>}`
```

Register a [resource file](#resource-files) as `<ResourceName>`.

If the file is on your local machine, put it in the same directory where you invoke `mzbench run`.

**`<Type>`** is one of the following atoms:

`text`
:   Plain text file, interpreted as a single string.

`json`
:   JSON file. Lists are interpreted as [Erlang lists](http://www.erlang.org/doc/man/lists.html), objects are interpreted as [Erlang maps](http://www.erlang.org/doc/man/maps.html).

`tsv`
:   File with tabulation separated values, interpreted as a list of lists.

`erlang`
:    Erlang source file, interpreted directly as an [Erlang term](http://erlang.org/doc/reference_manual/expressions.html#id77790).

`binary`
:   Custom binary (image, executable, archive, etc.), not interpreted.

### pre_hook and post_hook

```erlang
{pre_hook, <Actions>}
{post_hook, <Actions>}
```

Run actions before and after the benchmark. Two kinds of actions are supported: *exec commands* and *worker calls*:

    Actions = [Action]
    Action = {exec, Target, BashCommand}
        | {worker_call, WorkerMethod, WorkerModule}
        | {worker_call, WorkerMethod, WorkerModule, WorkerType}
    Target = all | director

**Exec commands** let you to run any shell command on all nodes or only on the director node.

**Worker calls** are functions defined by the worker. They can be executed only on the director node. Worker calls are used to update the [environment variables](#environment-variables) used in the benchmark.

### assert

```erlang
{assert, always, <Condition>}
{assert, <Time>, <Condition>}
```

Check if the condition `<Condition>` is satisfied throughout the entire benchmark or at least for the amount of time [`<Time>`](#time_1).

`<Condition>` is a comparison of two value and is defined as a tuple `{<Operation>, <Operand1>, <Operand2>}`.

`<Operation>` is one of four atoms:

`lt`
:   Less than.

`gt`
:   Greater than.

`lte`
:   Less than or equal to.

`gte`
:   Greater than or equal to.

`<Operand1>` and `<Operand2>` are the values to compare. They can be integers, floats, or *metrics* values.

[Metrics](../workers.md#metrics) are numerical values collected by the worker during the benchmark. To get the metric value, put its name between double quotation marks:

```erlang
{gt, "http_ok", 20}
```

The `http_ok` metric is provided by the [simple_http](https://github.com/satori-com/mzbench/blob/master/workers/simple_http/src/simple_http_worker.erl) worker. This condition passes if the number of successful HTTP responses is greater than 20.    


# Pools

**Pool** represents a sequence of **jobs**—statements to run. The statements are defined by the [worker](../workers.md) and [MZBench's standard library](#standard-library). The jobs are evenly distributed between nodes, so they can be executed in parallel.

Here's a pool that sends HTTP GET requests to two sites on 10 nodes in parallel:

```erlang
    [ {pool,
        [ {size, 10}, {worker_type, simple_http_worker} ],
        [
            {get, "http://example.com"},
            {get, "http://foobar.com"} 
        ]
    } ].
```

The `get` statement is provided by the built-in [simple_http](https://github.com/satori-com/mzbench/blob/master/workers/simple_http/src/simple_http_worker.erl) worker.

The first param in the `pool` statement is a list of *pool options*.

## Pool Options

### size

*required*

```erlang
{size, <NumberOfJobs>}`
```

How many times you want the pool executed.

If there's enough nodes and `worker_start` is not set, MZBench will start the jobs simultaneously and run them in parallel.

**`<NumberOfJobs>`** is any positive number.


### worker_type

*required*

```erlang
{worker_type, <WorkerName>}
```

The worker that provides statements for the jobs.
    
!!!hint
    A pool uses exactly one worker. If you need multiple workers in the benchmark, just write a pool for each one.


### worker_start

```erlang
{worker_start, {linear, <Rate>}}
{worker_start, {poisson, <Rate>}}
{worker_start, {exp, <Scale>, <Time>}}
{worker_start, {pow, <Exponent>, <Scale>, <Time>}}
```

Start the jobs with a given rate:
    
`linear`
:   Constant rate [`<Rate>`](#rate_1), e.g. 10 per minute.

`poisson`
:   Rate defined by a [Poisson process](http://en.wikipedia.org/wiki/Poisson_process) with λ = [`<Rate>`](#rate_1).

`exp`
:   Start jobs with [exponentially growing](https://en.wikipedia.org/wiki/Exponential_growth) rate with the scale factor `<Scale>`:

    *Scale × e<sup>Time</sup>*

`pow`
:   Start jobs with rate growing as a [power function](https://en.wikipedia.org/wiki/Power_function) with the exponent `<Exponent>` and the scale factor `<Scale>`:
    
    *Scale × Time<sup>Exponent</sup>*

You can customize and combine rates:

### ramp

```erlang
{ramp, linear, <StartRate>, <EndRate>}
```

Linearly change the rate from [`<StartRate>`](#rate_1) at the beginning of the pool to [`<EndRate>`](#rate_1) at its end.

### comb

```erlang
{comb, <Rate1>, <Time1>, <Rate2>, <Time2>, ...}
```

Start jobs with rate [`<Rate1>`](#rate_1) for [`<Time1>`](#time_1), then switch to [`<Rate2>`](#rate_1) for [`<Time2>`](#time_1), etc.


# Loops

**Loop** is a sequence of statements executed over and over for a given time.

A loop looks similar to a [pool](#pools)—it consists of a list of [options](#loop options) and a list statements to run:

```erlang
{loop, [
        {time, <Time>},
        {rate, <Rate>},
        {parallel, <N>},
        {iterator, <Name>},
        {spawn, <Spawn>}
    ],
    [
        <Statement1>,
        <Statement2>,
        ...
    ]
}
```

Here's a loop that sends HTTP GET requests for 30 seconds with a growing rate of 1 → 5 rps:

```erlang
{loop, [
        {time, {30, sec}},
        {rate, {ramp, linear, {1, rps}, {5, rps}}}
    ],
    [
        {get, "http://example.com"}
    ]
}
```

You can put loops inside loops. Here's a nested loop that sends HTTP GET requests for 30 seconds, increasing the rate by 1 rps every three seconds:

```erlang
{loop, [
        {time, {30, sec}},
        {rate, {10, rpm}},
        {iterator, "i"}
    ],
    [
        {loop, [
                {time, {3, sec}}, 
                {rate, {{var, "i"}, rps}}
            ],
            [
                {get, "http://google.com"}
            ]
        }
    ]
}
```

The difference between these two examples is that in the first case the rate is growing smoothly and in the second one it's growing in steps.


## Loop options

### time

*required*
    
```erlang
{time, <Time>}
``` 

Run the loop for [`<Time>`](#time_1).

### rate

```erlang
{rate, <Rate>}
```

Repeat the loop with the [`<Rate>`](#rate_1) rate.

### think_time

```erlang
{think_time, <Time>, <Rate>}
```

Start jobs with rate [`<Rate>`](#rate_1) for a second, then sleep for [`<Time>`](#time_1) and repeat.

### parallel

```erlang
{parallel, <N>}
```

Run `<N>` iterations of the loop in parallel.

### iterator

```erlang
{iterator, "<IterName>"}
```

Define a variable named `<IterName>` inside the loop that contains the current iteration number. It can be accessed with `{var, <IterName>}`.

### spawn

```erlang
{spawn, (true|false)}
```

If `true`, every iteration runs in a separate, spawned process. Default is `false`.


# Resource Files

**Resource file** is an external data source for the benchmark.

To declare a resource file for the benchmark, use [`include_resource`](#include-resource).

Once the resource file is registered, its content can be included at any place in the scenario using the `resource` statement: `{resource, <ResourceName>}`.

For example, suppose we have a file `names.json`:

    [
        "Bob",
        "Alice",
        "Guido"
    ]

Here's how you can use this file in a scenario:

```erlang
[
    {include_resource, names, "names.json", json},
    {pool, [
            {size, 3},
            {worker_type, dummy_worker}
        ],
        [
            {loop, [
                    {time, {5, sec}},
                    {rate, {1, rps}}
                ],
                [
                    {print, {choose, {resource, names}}} % print a random name from the file
                ]
            }
        ]
    }
].
```


# Standard Library

## Environment Variables

**Environment variables** are global values that can be accessed at any point of the benchmark. They are useful to store the benchmark global state like its total duration, or global params like the execution speed.

To set an environment variable, call `mzbench` with the `--env` param:

```bash
$ ./bin/mzbench run --env foo=bar --env n=42
```

### var

```erlang
{var, "<VarName>"}
```

To get the value of a variable, refer to it by the name: `{var, "<VarName>"}`.

```erlang
{var, "foo"} % returns "bar"
{var, "n"} % returns "42", a string
```

If you refer to an undefined variable, the benchmark crashes. You can avoid this by setting a default value for the variable, see [defaults top-level directive](#defaults).

### numvar

```erlang
{numvar, "<VarName>"}
```

By default, variable values are considered strings. To get a numerical value (integer or float), use `{numvar, "VarName"}`:

```erlang
{numvar, "n"} % returns 42, an integer.
```


## Parallelization and Syncing

### parallel

```erlang
{parallel, <Statement1>, <Statement2>, ...}
```

Execute multiple statements in parallel. Unlike executing statements in a pool, this way all statements are executed on the same node.

### set_signal

```erlang
{set_signal, <SignalName>}
{set_signal, <SignalName>, <Count>]}
```

Emit a global signal `<SignalName>`.

If `<Count>` is specified, the signal is emitted `<Count>` times.

`<SignalName>` is a string, atom, number, or, in fact, any [Erlang term](http://erlang.org/doc/reference_manual/expressions.html#id77790).

### wait_signal

```erlang
{wait_signal, <SignalName>}
{wait_signal, <SignalName>, <Count>}
```

Wait for the global signal `<SignalName>` to be emitted. If `<Count>` is specified, wait for the signal to be emitted `<Count>` times.

## Errors Handling

### ignore_failure

```erlang
{ignore_failure, <Statement>}
```

Execute the statement `<Statement>` and continue with the benchmark even if it fails.

If the statement succeeds, its result is returned; otherwise, the failure reason is returned.


## Randomization

### random_number

```erlang
{random_number, <Min>, <Max>}
{random_number, <Max>}
```

Return a random number between `<Min>` and `<Max>`, including `<Min>` and not including `<Max>`.

`{random_number, <Max>}` is equivalent to `{random_number, 0, <Max>}`

### random_list

```erlang
{random_list, <Size>}
```

Return a list of random integer of length `<Size>`.

### random_binary

```erlang
{random_binary, <Size>}
```

Return a binary sequence of `<Size>` random bytes.

### choose

```erlang
{choose, <N>, <List>}
{choose, <List>}
```

Return a list of `<N>` random elements of the list `<List>`.

`{choose, <List>}` is equivalent to `{choose, 1, <List>}`.

### round_robin

```erlang
{round_robin, <List>}
```

Pick the next element of the list. When the last one is picked, start over from the first one.

**BEWARE:** The `round_robin` function complexity is `O(n)` when `n` is the length of the `<List>`, so it is extremely slow for big lists. You should consider to cache the value somehow if it is the case.


## Logging

### dump

```erlang
{dump, "<Text>"}
```

Write `<Text>` to the benchmark log.

### sprintf

```erlang
{sprintf, "<Format>", [<Value1>, <Value2>, ...]}
```

Return [formatted text](http://www.erlang.org/doc/man/io.html#fwrite-1) with a given format and placeholder values.


## Data Conversion

### t

```erlang
{t, <List>}
```

Convert `<List>` to a tuple.

### term_to_binary

```erlang
{term_to_binary, <term>}
```

Convert an Erlang term to a binary object. [Learn more](http://www.erlang.org/doc/man/erlang.html#term_to_binary-1) in the Erlang docs.


## Pause

### wait

```erlang
{wait, <Time>}
```

Pause the current job for [`<Time>`](#time_1).

# Conventions

## Time

**`<Time>`** is a tuple `{<Duration>, (ms|sec|min|h)}`:

```erlang
{1, sec}` % one second
{10, min} % 10 minutes
{0.5, h} % half hour
```


## Rate

**`<Rate>`** is a tuple `{<N>, (rps|rpm|rph)}`:

```erlang
{10, rps} % 10 jobs per second
{12, rpm} % 12 jobs per minute
{100, h} % 100 jobs per hour
```
