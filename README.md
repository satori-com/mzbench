# MZBench

MZBench is a robust load testing tool. Some key features:
* Ability to generate workload in distributed mode.
* Flexible DSL for running various workload scenarios.
* Putting the results into common metrics-gathering systems like Graphite.

## Quickstart

To install MZBench, you need: Erlang R17, CC, C++, Python and PIP.

    # clone MZBench repo
    git clone https://github.com/machinezone/mzbench.git

    # install python packages
    pip install -r mz-bench/requirements.txt

    cd mz-bench

    # install MZBench server
    ./bin/mz-bench install_server

    # start MZBench server
    ./bin/mz-bench start_server

    # run a benchmark with graphite
    ./bin/mz-bench run ../examples/ramp.erl --env graphite=<graphite_address>

    # or run a benchmark with graphite
    ./bin/mz-bench run ../examples/ramp.erl

    # check dashboard at http://localhost:4800/ for the results

[Deployment guide](doc/deployment_guide.md)

## Further reading

MZBench scenarios are DSL programs

[Examples](doc/examples.md)

[DSL description](doc/scenario_dsl.md)

[How to write language extensions](doc/worker_howto.md)
