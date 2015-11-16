# Welcome to MZBench

***Expressive, infinitely scalable load testing tool***

---

MZBench helps software testers and developers test their products under huge load. By benchmarking your product before going to production, you reduce the risk of outages under real life highload. 

MZBench runs your test scenarios on hundreds of thousands of machines simultaneously, pushing even the largest services to their limit.

As your benchmark is running, you see real-time stats and graphs in the built-in web dashboard.


## Installation

To use MZBench, you'll need:

 - Erlang R17
 - C++ compiler
 - Python 2.6 or 2.7 with pip

Most UNIX systems have C++ compiler and Python preinstalled.

Erlang is available in the [official repositories on most GNU/Linux distros](http://pkgs.org/search/erlang). If your distro doesn't have Erlang R17, [build it from source](http://www.erlang.org/doc/installation_guide/INSTALL.html).  

Download MZBench from GitHub and install Python requirements:

```bash
$ git clone https://github.com/machinezone/mzbench
$ sudo pip install -r mzbench/requirements.txt 
```

## Quickstart

Start the MZBench server on localhost:

```bash
$ cd mzbench
$ ./bin/mzbench start_server
Executing make -C /path/to//mzbench/bin/../server generate
Executing /path/to//mzbench/bin/../server/_build/default/rel/mzbench_api/bin/mzbench_api start
```

!!!note
    The first server start takes a few minutes. The shell will not respond, which is OK; please be patient. Further starts will be much faster.

When the server is running, launch an example benchmark:

```bash
$ ./bin/mzbench run examples/ramp.erl
{
    "status": "pending", 
    "id": 6
}
status: running                       00:09
```

Go to [localhost:4800](http://localhost:4800) and see the benchmark live status:

![Test Benchmark](images/test_benchmark.png)

## How It Works

Here be description.