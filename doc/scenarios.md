**Scenarios** describe the behavior you want MZBench to emulate during the benchmark. If you're testing an online store, your scenario will probably include opening a product page and adding the product to cart. For a search service, the scenario may be searching for a random word. You get the idea.

In MZBench, scenarios are .erl files written in a simple DSL. It looks a lot like Erlang but is much simpler.

Read on to learn how to write (and read) MZBench's test scenarios.

*[DSL]: Domain-Specific Language


# Syntax

MZBench test scenarios consist of *lists*, *tuples*, and *atoms*:

  - A **list** is a comma-separated sequence enclosed in square brackets: `[A, B, C]`
  - A **tuple** is a comma-separated sequence enclosed in curly braces: `{A, B, C}`
  - **Atoms** are reserved words—function names, rate units, conditions. Basically, anything that's not a list, a tuple, a number, or a string is an atom: `print, make_install, lte, rpm`  

The whole scenario is a list of tuples with a dot in the end:

```erlang
[
    {function1, param1, param2},
    {function2, [{param_name, param_value}]},
].
```

Each of these tuples is called a *statement*. A statement represents a function call: the first item is the function name, the others are the params to pass to it. Each param in its turn can also be a list, a tuple, or an atom.

Some statements only appear at the top level of a scenario. They're called *top-level statements*. There're two kinds of top-level statements: [top-level directives](#top-level-directives) and [pools](#pools).

[See live examples of MZBench scenarios on GitHub →](https://github.com/machinezone/mzbench/tree/master/examples)


# Top-Level Directives

**Top-level directives** prepare the system for the benchmark and clean it up after it. This includes installing an external [worker](workers.md) on test nodes, including resource files, checking conditions, and executing shell commands before and after the test.

`{make_install, [{git, "<URL>"}, {branch, "<Branch>"}, {dir, "<Dir>"}]}`
:   Install an external worker from a remote git repository on the test nodes before running the benchmark.

    MZBench downloads the worker and builds a .tgz archive, which is then distributed among the nodes and used in future provisions.

    The following actions are executed during `make_install`:

        git clone <URL> temp_dir
        cd temp_dir
        git checkout <Branch>
        cd <Dir>
        make generate_tgz

    If `branch` is not specified, the default git branch is used.

    If `dir` is not specified, `.` is used.

<a name="include-resource"></a>

`{include_resource, <ResourceName>, "<FileName>", <Type>}`
`{include_resource, <ResourceName>, "<FileURL>", <Type>}`
:   Register a [resource file](#resource-files) as `<ResourceName>`.

    If the file is on your local machine, put it in the same directory where you invoke `mzbench run`.

    **`<Type>`** is one of the following atoms:

    `text`
    :   Plain text file interpreted as a list of lines.

    `json`
    :   JSON file.
   
    `tsv`
    :   File with tabulation separated values.   
   
    `erlang`
    :    Erlang file interpreted directly as a valid Erlang term. [Read more](http://erlang.org/doc/reference_manual/expressions.html#id77790) about Erlang terms.
   
    `binary`
    :   Custom binary (image, executable, archive, etc.), not interpreted.


`{pre_hook, <Actions>}` and `{post_hook, <Actions>}`
:   Run actions before and after the benchmark. Two kinds of actions are supported: *exec commands* and *worker calls*:

        Actions = [Action]
        Action = {exec, Target, BashCommand}
            | {worker_call, WorkerMethod, WorkerModule}
            | {worker_call, WorkerMethod, WorkerModule, WorkerType}
        Target = all | director

    **Exec commands** let you to run any shell command on all nodes or only on the director node.
    
    **Worker calls** are functions defined by the worker. They can be executed only on the director node. Worker calls are used to update the environment variables used in the benchmark. These environment variables can be obtained with the `var` directive. [Read more →](workers.md#hooks)

`{assert, always, <Condition>}`
`{assert, <Time>, <Condition>}` 
:   Check if the condition `<Condition>` is satisfied throughout the entire benchmark or at least for the amount of time `<Time>`.

    `<Time>` is a tuple `{<Duration>, (ms|sec|min|h)}`, e.g. `{1, sec}`, `{10, ms}`.
    
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

    `<Operand1>` and `<Operand2>` are the values to compare. They can be integers, floats, or *metric* values.
    
    [Metrics](workers.md#metrics) are numerical values collected by the worker during the benchmark. To get the metric value, put its name between double quotation marks:
    
        {gt, "http_ok", 20}
    
    The `http_ok` metric is provided by the [simple_http](https://github.com/machinezone/mzbench/blob/master/workers/simple_http/src/simple_http_worker.erl) worker. This condition passes if the number of successful HTTP responses is greater than 20.    


# Pools

**Pool** represents a sequence of **jobs**—statements to run. The statements are defined by the [worker](workers.md) and [MZBench's standard library](#standard-library). The jobs are evenly distributed between nodes, so they can be executed in parallel.

Apart from the actual list of jobs, each `pool` statement takes a list of [options](#pool-options) that define how the jobs should be executed: with which worker, at what intensity and pace, etc.

Here's a pool that sends HTTP GET requests to two sites on 10 nodes in parallel:

```erlang
    [ {pool,
        [ {size, 10}, {worker_type, simple_http} ],
        [
            {get, "http://example.com"},
            {get, "http://foobar.com"} 
        ]
    } ].
```

The `get` statement is provided by the built-in [simple_http](https://github.com/machinezone/mzbench/blob/master/workers/simple_http/src/simple_http_worker.erl) worker.


## Pool options

`{size, <NumberOfJobs>}` *required*
:   How many times you want the pool executed.

    If there's enough nodes and `worker_start` is not set, MZBench will start the jobs simultaneously and run them in parallel.
    
    **`<NumberOfJobs>`** is any positive number.

`{worker_type, <WorkerName>}` *required*
:   The worker that provides statements for the jobs.
    
    !!!hint
        A pool uses exactly one worker. If you need multiple workers in the benchmark, just write a pool for each one.

`{worker_start, {linear, <Rate>}}`
`{worker_start, {poisson, <Rate>}}`
:   Start the jobs with a given rate:
    
    `linear`
    :   Constant rate `<Rate>`, e.g. 10 per minute
    
    `poisson`
    :   Rate defined by a [Poisson process](http://en.wikipedia.org/wiki/Poisson_process) with λ = `<Rate>`

    <a name="rate"></a>
    
    **`<Rate>`** is a tuple `{<N>, (rps|rpm|rph)}`. It means `<N>` jobs per second, minute, or hour.

    You can customize and combine rates:
 
    `{think_time, <Time>, <Rate>}`
    :   Start jobs with rate `<Rate>` for a second, then sleep for `<Time>` and repeat.
    
        `<Time>` is a tuple `{<Duration>, (ms|sec|min|h)}`, e.g. `{1, sec}`, `{10, ms}`. 

    `{ramp, linear, <StartRate>, <EndRate>}`
    :   Linearly change the rate from `<StartRate>` at the beginning of the pool to `<EndRate>` at its end.
    
    `{comb, <Rate1>, <Time1>, <Rate2>, <Time2>, ...}`
    :   Start jobs with rate `<Rate1>` for `<Time1>`, then switch to `<Rate2>` for `<Time2>`, etc.

    
`{worker_start, {exp, <Scale>, <Time>}}`
:   Start jobs with [exponentially growing](https://en.wikipedia.org/wiki/Exponential_growth) rate with the scale factor `<Scale>`: (*Scale × e<sup>Time</sup>*)  

`{worker_start, {pow, <Exponent>, <Scale>, <Time>}}`
:   Start jobs with rate growing as a [power function](https://en.wikipedia.org/wiki/Power_function) with the exponent `<Exponent>` and the scale factor `<Scale>`: *Scale × Time<sup>Exponent</sup>*


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

`{time, <Time>}` *required*
:   How long the loop is repeated.
    `<Time>` is a tuple `{<Duration>, (ms|sec|min|h)}`, e.g. `{1, sec}`, `{10, ms}`.

`{rate, <Rate>}`
:   How frequently the loop is repeated.
    
    `<Rate>` is defined the same way as in [pool options](#rate).

`{parallel, <N>}`
:   Run `<N>` iterations of the loop in parallel.

`{iterator, "<IterName>"}`
:   Define a variable named `<IterName>` inside the loop that contains the current iteration number. It can be accessed with `{var, <IterName>}`.

`{spawn, (true|false)}`
:   If `true`, every iteration runs in a separate, spawned process.
 
    Default is `false`.


# Resource Files

**Resource file** is an external data source for the benchmark.

To declare a resource file for the benchmark, use the [`include_resource` top-level directive](#include-resource).

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

To get the value of a variable, refer to it by the name: `{var, "<VarName>"}`.

```erlang
{var, "foo"} % returns "bar"
{var, "n"} % returns "42", a string
```

By default, variable values are considered strings. To get a numerical value (integer or float), use `{numvar, "VarName"}`:

```erlang
{numvar, "n"} % returns 42, an integer.
```

If you refer to an undefined variable, the benchmark crashes. You can avoid this by setting a default value for the variable: `{var, "<VarName>", <DefaultValue}`:

```erlang
{var, "anothervar", "Foo"} % returns "Foo" if anothervar is not set
```

If you do want the benchmark to crash, but you also want to show a sensible error message, set one with `{var, "<VarName>", {error, "<ErrorMessage>"}}`:

```erlang
{var, "myvar", {error, "Please define myvar with --env myvar=value"}} % shows the error message if myvar is not set
```


## Parallel Jobs and Synchronization

`{parallel, [<statement>]}`
:   Execute statements in parallel. If you have any initialization statements, they shouldn't be executed in parallel with work statements because they don't share worker state changes, also worker state modifications for thread > 1 wont be available after a parallel section. For example, [{parallel, [2, 3]}] is equal to [2], the same for [{parallel, [2, 3, 4, 5]}] == [2].

`{set_signal, <term> [, <count>]}`
:   Register global `<term>` for synchronization between different pools or workers. If the optional `<count>` parameter is specified, the `<term>` will be registered `<count>` times.

`{wait_signal, <term> [, <count>]}`
:   Wait for some particular kind of `<term>` to be registered. If the optional `<count>` parameter is specified, wait for the `<term>` to be registered `<count>` times.


## Errors Handling

`{ignore_failure, <Statement>}`
:   Execute the statement `<Statement>` and continue with the benchmark even if it fails.

    If the statement succeeds, its result is returned; otherwise, the failure reason is returned.


## Randomization

`{random_number, <Min>, <Max>}`
:   Return a random number between `<Min>` and `<Max>`, including `<Min>` and not including `<Max>`.

`{random_number, <Max>}`
:   Equivalent to `{random_number, 0, <Max>}`

`{random_list, <Size>}`
:   Return a list of random integer of length `<Size>`.

`{random_binary, <Size>}`
:   Return a binary sequence of `<Size>` random bytes.

`{choose, <List>}`
:   Return a random element of the list `<List>`.

`{choose, <N>, <List>}`
:   Return a list of `<N>` random elements of the list `<List>`.

`{round_robin, <List>}`
:   Similar to `{choose, <List>}`, but the selected element depends on the current node.


## Logging output

### `{dump, "<text>"}`

Write `<text>` to the benchmark log.

### `{sprintf, "<format>", [<var1>, <var2>, ...]}`

This statement is substituted with a formatted text. See [Erlang fwrite/1](http://www.erlang.org/doc/man/io.html#fwrite-1) for a detailed description of the available formatting options.


## Miscellaneous routines

### `{t, <list>}`

This statement is substituted with a tuple containing the elements of the `<list>`, i.e. converts list to tuple.

### `{term_to_binary, <term>}`

See [Erlang term_to_binary/1](http://www.erlang.org/doc/man/erlang.html#term_to_binary-1).

### `{wait, <time_constant>}`

The current parallel job will be stopped for `<time_constant>` amount of time. The `<time_constant>` can be specified as follow:

   * `{N, h}` - wait for `N` hours;
   * `{N, min}` - wait for `N` minutes;
   * `{N, sec}` - wait for `N` seconds;
   * `{N, ms}` - wait for `N` milliseconds.
