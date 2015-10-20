# Introduction

This document describes the DSL language used to write testing scenarios for the MZBench testing suite.

# General language description

The language is using a slightly Lisp-like notation. It consists of lists and tuples:

  * A list is a comma separated list of items enclosed in brackets. For example: `[A, B, C]`;
  * A tuple is a comma separated list of items enclosed in braces. For example: `{A, B, C}`.

A MZBench scenario is a list of top-level statements and a dot. For example:

    [
        Statement1,
        Statement2
    ].

Each statement is a tuple. The first element of this tuple indicates the name of a function to call, for example `pool` or `assert`. The other elements are the parameters to pass to this function. They can be atoms, tuples or lists. For example: `{print, "Hello, world!"}` or `{add, 2, 3}`.

# Top-level statements

Top-level statements are the statements that can appear in the top-level list describing the scenario. They are of two kinds:

  * Top-level directives allow to tell the system some general facts about the scenario or define some global parameters;
  * The pools allow to describe the actual work to perform.

## Top-level directives

The top-level directives are as follows:

### `{make_install, [{git, <URL>}, {branch, <Branch>}, {dir, <Dir>}]}`

Instructs the benchmarking system to install external worker from a remote git repository on the nodes before executing the benchmark. MZBench builds worker application and creates tar archive at the first time. MZBench server stores this archive and uses it to install workers on the nodes in a further provisionings.

The following actions are executed during make_install:

    git clone <URL> temp_dir
    cd temp_dir
    git checkout <Branch>
    cd <Dir>
    make generate_tgz

If no branch is specified, default git branch is used.

If no `dir` is specified, `.` is used by default.

After that, a worker package is installed from tgz file. Please use [simple_http](../workers/simple_http) worker as an example.

### `{include_resource, handle, "filename.txt"}` 

Instructs the benchmarking system to include additional resource files in your scenario. See [Resource files](#resource-files) for more details on this functionality.

### `{pre_hook, Commands}` and `{post_hook, Commands}`

    Commands = [Command]
    Command = {exec, Target, BashCommand} | {worker_call, WorkerMethod, WorkerModule} | {worker_call, WorkerMethod, WorkerModule, WorkerType}
    Target = all | director

Instructs the benchmarking system to run some actions before or after the benchmark. There two supported kinds of hooks: exec commands and worker calls.

Exec commands allow you to run any bash command on every nodes or on director only. Worker calls could be executed on the director node only.

You could change any environment values in your hooks if they are implemented as worker calls. Benchmark system passes current environment to your hooks and gets new environment from it. You could get access to this value in your scenario by using `var` directive.

You could check [hooks example](../examples/hooks.erl) and review implementation of hook method in our [dummy worker](../node/apps/dummy_worker/src/dummy_worker.erl) for addition information. 

### `{assert, always, <Condition>}`

Instructs the benchmarking system to check that specified condition is satisfied all the time while bench is running.

See [Conditions](#conditions) for details on how to specify the condition to check.

### `{assert, <TimeConstant>, <Condition>}` 

Instructs the benchmarking system to check that condition is satisfied at least for the amount of time specified.

See [Conditions](#conditions) for details on how to specify the condition to check.

## The pools

The pool directive represents a pool of jobs to be distributed among the computation nodes and to be done in parallel where job is a set of instructions defined by a _worker_. A _worker_ is a MZBench plugin that defines a set of instructions to access a particular service (i.e. HTTP server, FTP server, Twitter, ...). Some workers are provided with the MZBench distribution, but you can also write you own. Detailed instructions on how to do this are provided in the [Worker HOWTO](worker_howto.md) document.

A pool is defined using the `pool` top-level statement:

    {pool, 
        [
            <PoolOption1>, 
            <PoolOption2>,
            ...
        ], 
        [
            <Statement1>,
            <Statement2>, 
            ...
        ]
    }

It takes two arguments, a list of options and a list of statements.

The list of pool options define how much jobs must be launched in parallel, what worker to use to define the list of allowed statements and how the jobs must be launched. See the next sub-section [Pool options](#pool-options) for the complete list of possible options, but please note that `size` and `worker_type` are mandatory.

The list of statements actually defines the job. You can use the statements defined by the selected worker and the statements of the standard library. Please refer to the worker documentation for the list of worker defined statements. For the list of standard library statements, please refer yourself to [Standard library](#standard-library).

### An example

To clarify what was previously said let's see the following benchmarking scenario:

    [{pool, [{size, 10},
             {worker_type, dummy_worker}],
       [ {print, "AAA"} ]
     },
     {pool, [{size, 5},
             {worker_type, dummy_worker}],
       [ {print, "BBB"} ]
    }].

It consists of two pools, so it defines a two sets of jobs.

The job defined by the first pool will be launched 10 times in parallel as defined by the `size` option. The job will be described using the set of statements defined by the `dummy_worker`. It is a very simple worker shipped with the MZBench suite that defines only one statement: `print`. It allows to print a string of characters to the standard output. Because nothing else is specified, all the jobs will be launched at the same time.

The actual job associated with this pool consist in one single statement: `{print, "AAA"}`. So, when started, it will print the string of characters `AAA` to the standard output, then terminate.

The second pool is defined in a similar manner. Its job is defined using the `dummy_worker` and consists in printing `BBB` and terminating. It will be launched 5 times in parallel and at the same time.

So the result of this script will be `AAA` printed 10 times and `BBB` printed 5 times, all in parallel. If we run such a script on 3 nodes, these 15 strings will be evenly distributed between all nodes.

### Pool options

#### `{size, <int>}`

Instructs the benchmarking system on how many jobs must be launched. An integer from 1 to infinity.

This option is mandatory.

#### `{worker_type, <Atom>}`

Indicates the worker that defines the set of instructions to write this particular job. 

Please note that you can use only one worker per pool. If you need to use several workers in your benchmarking scenario, define several pools.

This option is mandatory.

#### `{worker_start, {linear, <rate>}}`

Indicates to the system that parallel jobs must be started with a constant delay between them. The `<rate>` indicates how much jobs must be started per second.

By default, all the jobs are started at the same time.

#### `{worker_start, {poisson, <rate>}}`

Indicates to the system that the jobs must be started at a rate defined by a Poisson process (see [Poisson process](http://en.wikipedia.org/wiki/Poisson_process)).

By default, all the jobs are started at the same time.

#### `{worker_start, {exp, <W>, <time>}}`

In this case <W> workers are expected to start in a given period of <time> with k*E^Time function.

#### `{worker_start, {pow, <N>, <W>, <time>}}`

In this case <W> workers are expected to start in a given period of <time> with k*Time^N function.

# Conditions

Some statements take a boolean condition as an argument. Such a condition is defined by a triplet. The first element is an atom that defines the used comparison operation. The possible operations are:

  * `lt` - lesser then;
  * `gt` - greater then;
  * `lte` - lesser then or equal;
  * `gte` - greater then or equal.

The second and third elements are the two values to compare. They can be either a number (an integer or floating point value) or a metric name. Metrics are numerical values collected during the benchmarking. They are defined by the worker should be checked in a worker-specific documentation.

For example, if you use `dummy_worker` provided with the MZBench distribution, you can write the following condition:

    {gt, "print.value", 20}

It will succeed if the `print` operation was performed more then 20 times (i.e. it means `print.value > 20`).

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
