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

`{include_resource, <ResourceName>, "<FileName>"}` 
:   Download and register resource files for the scenario. [Read more →](#resource-files)

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

    `<Time>` is a tuple `{<Duration>, (ms|sec|min|h)}`: `{1, sec}`, `{10, ms}`.
    
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

Apart from the actual list of jobs, each `pool` statement takes a list of [pool options](#pool-options) that define how the jobs should be executed: with which worker, at what intensity and pace, etc.

Here's a pool that sends HTTP GET requests to Google and DuckDuckGo on 10 nodes in parallel:

```erlang
    [ {pool,
        [ {size, 10}, {worker_type, simple_http} ],
        [
            {get, "http://google.com"},
            {get, "http://duckduckgo.com"} 
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

    In the simplest case, **`<Rate>`** is a tuple:
    
    `{<N>, (rps|rpm|rph)}`
    :   Start `<N>` jobs per second, minute, or hour 
 
    You can customize and combine rates:
 
    `{think_time, <Time>, <Rate>}`
    :   Start jobs with rate `<Rate>` for a second, then sleep for `<Time>` and repeat.
    
        `<Time>` is a tuple `{<Duration>, (ms|sec|min|h)}`: `{1, sec}`, `{10, ms}`. 

    `{ramp, linear, <StartRate>, <EndRate>}`
    :   Linearly change the rate from `<StartRate>` at the beginning of the pool to `<EndRate>` at its end.
    
    `{comb, <Rate1>, <Time1>, <Rate2>, <Time2>, ...}`
    :   Start jobs with rate `<Rate1>` for `<Time1>`, then switch to `<Rate2>` for `<Time2>`, etc.

    
`{worker_start, {exp, <Scale>, <Time>}}`
:   Start jobs with [exponentially growing](https://en.wikipedia.org/wiki/Exponential_growth) rate with the scale factor `<Scale>`: (*Scale × e<sup>Time</sup>*)  

`{worker_start, {pow, <Exponent>, <Scale>, <Time>}}`
:   Start jobs with rate growing as a [power function](https://en.wikipedia.org/wiki/Power_function) with the exponent `<Exponent>` and the scale factor `<Scale>`: *Scale × Time<sup>Exponent</sup>*


# Loops

The `loop` statement allows to instruct the benchmarking system to repeat some block of statements several times. It is one of the most important statements that can be used inside a pool because it allows to generate different load profiles.

In it most general form, the `loop` statement is defined as follow:

    {loop, [
            {time, <time>},
            {rate, <rate>},
            {parallel, <N>},
            {iterator, <name>},
            {spawn, <spawn>}
        ],
        [
            <Statement1>,
            <Statement2>,
            ...
        ]
    }

As you can see, similarly to the `pool` statement, it takes two parameters: a list of options and a list of statements. A list of statements defines the actual job to be repeated and can contain any worker or standard library defined statements. The options define how to repeat this job. 

## Loop options

### `{time, <time>}`

This option specify for how long the block of instructions must be repeated. The `<time>` can be specified as follow:
   
   * `{N, h}` - repeat for `N` hours;
   * `{N, min}` - repeat for `N` minutes;
   * `{N, sec}` - repeat for `N` seconds;
   * `{N, ms}` - repeat for `N` milliseconds.

This option is mandatory.

### `{rate, <rate>}`

This option specify how often the block of instructions must be repeated. The `<rate>` can be specified as follow:

   * `{N, rph}` - repeat N times per hour;
   * `{N, rpm}` - repeat N times per minute;
   * `{N, rps}` - repeat N times per second;
   * `{think_time, {M, (ms|sec|min|h)}, {N, rp(s|m|h)}}` - keep this rate for 1 second and sleep for specified period of time in a loop, this option is similar to {comb, {N, rp(s|m|h)}, {1, sec}, {0, rps}, {M, (ms|sec|min|h)}} which is described below.
   * `{ramp, linear, <start-rate>, <end-rate>}` - linearly change the repeating rate from `<start-rate>` to `<end-rate>`.
   * `{comb, <rate1>, <time1>, <rate2>, <time2>...}` - use these rates for these periods of time in a loop, for example for {comb, {1, rps}, {1, sec}, {5, rps}, {5, sec}} these rates will be repeated until whole loop time is over.

This option is optional. If no rate is specified, the block of instructions will be repeated as often as possible.

### `{parallel, <N>}`

This option indicates that `<N>` iterations of the loop must be executed in parallel.

This option is optional. By default the iterations are performed one by one.

### `{iterator, <name>}`

Defines a variable named `<name>` inside the repeated block of instructions that contain the current iteration number. It can be accessed with the following instruction: `{var, <name>}`. See [Environment variables](#environment_variables) for more information.

### `{spawn, <spawn>}`

If true every iteration of the loop will be executed in spawned process.
Default: false

## Examples

Two examples below show different kinds of rate-variant loops you can use:

    {loop, [
            {time, {5, sec}},
            {rate, {ramp, linear, {60, rpm}, {5, rps}}}
        ],
        [
            {print, "E!"}
        ]
    }

In this first example the loop body execution rate will grow continuously from 1 rps to 5 rps during five seconds.

    {loop, [
            {time, {5, sec}},
            {rate, {1, rps}},
            {iterator, "i"}
        ],
        [
            {loop, [
                    {time, {1, sec}}, 
                    {rate, {{var, "i"}, rps}}
                ],
                [
                    {print, "E!"}
                ]
            }
        ]
    }

The second example uses a nested loop with a repeat rate defined using a variable. 

The difference between these two cases is the way how the rate grows. If you take a period of few minutes and obtain a chart you probably see a straight line in first case and a step function in the second.

# Environment variables

The benchmarking scenarios often need to contain some number of values such as speed of execution or the overall duration. Of course, such values can be hardcoded inside your script, but this would be impractical. Instead you can use what is called _environment variables_.

Replace the hardcoded values by a `var` statement of the form: `{var, <name> [, <default_value>]}` where `<name>` is a string of characters identifying your value. `<default_value>` parameter is optional.

Then you can pass the actual values when you launch the benchmark using the `--env` command line parameter. For instance:

    mzbench run --env <name1>=<value1> --env <name2>=<value2> ...

The `var` statement is substituted with the provided value at the script launch time. If no value was provided, it is substituted with `<default_value>`. If no default value was provided either, the benchmark will crash.

If you have some required parameters without any default values and you need to generate useful message to benchmark user you can use `error` statement the following way:

    {var, <name>, {error, <message>}}

In this case the benchmarking scenario will not be executed if the environment variable `<name>` was not provided at the command line and the error message `<message>` will be displayed to the user.

# Resource files

If you need to include a huge amount of data inside your benchmarking scenario you can use the `resource` statement.

First, you need to declare your resource file using the `include_resource` top-level directive. It is defined as follow:

    {include_resource, <resource_name>, <file_name>, <type>}
    {include_resource, <resource_name>, <http_url>, <type>}

Where `<resource_name>` is an atom that will identify this resource file inside the scenario, for example `my_resource`, `<file_name>` is a string of characters providing the file name of the resource file and, finally, the `<type>` parameter is an atom indicating how the content of the file should be interpreted.

Every resource name in a script must be unique.

Currently supported file types are the following:

   * `erlang` - the resource file can be interpreted directly as a valid Erlang term. See [Erlang terms](http://erlang.org/doc/reference_manual/expressions.html#id77790);
   * `text` - the resource file is a plain text file;
   * `json` - the resource file is a json file;
   * `binary` - the resource file is a custom binary and should not be interpreted;
   * `tsv` - the resource file is a table with tabulation separated values.

Once the resource file has been registered, its content can be included at any place inside your scenario using the `resource` statement: `{resource, <resource_name>}`.

For example, suppose we have a file `text.erl` with the following content:

    [
        "str1",
        "str2",
        "str3"
    ].

It can be included in a benchmarking scenario using `{include_resource, my_resource, "text.erl", erlang}` top-level statement. Then, we can insert it content by writing `{resource, my_resource}`. So the following scenario will randomly print the strings defined in the resource file:

    [
        {include_resource, my_resource, "text.erl", erlang},
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
                        {print, {choose, {resource, my_resource}}}
                    ]
                }
            ]
        }
    ].

# Standard library

All the following statements are built-in and can be used with any worker.

## Environment variables substitution

### `{var, <string>}`

This statement is substituted with the textual value of the environment variable `<string>` or crash the benchmark if this variable wasn't defined.

### `{var, <string>, <default>}`

This statement is substituted with the textual value of the environment variable `<string>` or `<default>` if this variable wasn't defined.

### `{numvar, <string>}`

This statement is substituted with the numerical value of the environment variable `<string>` or crash the benchmark if this variable wasn't defined.

### `{numvar, <string>, <default>}`

This statement is substituted with the numerical value of the environment variable `<string>` or `<default>` if this variable wasn't defined.

## Parallel jobs and synchronization

### `{parallel, [<statement>]}`

Execute statements in parallel. If you have any initialization statements, they shouldn't be executed in parallel with work statements because they don't share worker state changes, also worker state modifications for thread > 1 wont be available after a parallel section. For example, [{parallel, [2, 3]}] is equal to [2], the same for [{parallel, [2, 3, 4, 5]}] == [2].

### `{set_signal, <term> [, <count>]}`

Register global `<term>` for synchronization between different pools or workers. If the optional `<count>` parameter is specified, the `<term>` will be registered `<count>` times.

### `{wait_signal, <term> [, <count>]}`

Wait for some particular kind of `<term>` to be registered. If the optional `<count>` parameter is specified, wait for the `<term>` to be registered `<count>` times.

## Errors handling

### `{ignore_failure, <statement>}`

Executes the statement `<statement>`. Even if the `<statement>` fails, the benchmarking will continue it execution as if it succeeded.

This statements returns the return value of the `<statement>` if it was successful and the failure reason otherwise.

## Randomization routines

### `{random_number, <max>}`

Equivalent to `{random_number, 0, <max>}`

### `{random_number, <min>, <max>}`

This statement is substituted by a random number N, <min> =< N < <max>. <max> must be larger than <min>.

### `{random_list, <size>}`

This statement is substituted with a list of `<size>` random integers.

### `{random_binary, <size>}`

This statement is substituted with a binary value made of `<size>` random bytes.

### `{choose, <list>}`

This statement is substituted with a random element of the list `<list>`.

### `{choose, <N>, <list>}`

This statement is substituted with a list of `<N>` random values of the list `<list>`.

### `{round_robin, <list>}`

This statement works like [`{choose, <list>}`](#`{choose, <list>}`), but the selected element depends on the current node.

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
