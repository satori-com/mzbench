# MZBench [![Build Status](https://travis-ci.org/machinezone/mzbench.svg?branch=master)](https://travis-ci.org/machinezone/mzbench)

MZBench is a robust load testing tool. Some key features:
* Ability to generate workload in distributed mode.
* Flexible Domain-specific Language (DSL) for running various workload scenarios.
* Putting the results into common metrics-gathering systems like Graphite.

## Quickstart

To install MZBench, you need: Erlang R17, CC, C++, Python and PIP.

    # clone MZBench repo
    git clone https://github.com/machinezone/mzbench.git

    # install python packages
    sudo pip install -r mzbench/requirements.txt

    cd mzbench

    # install MZBench server
    ./bin/mzbench install_server

    # start MZBench server
    ./bin/mzbench start_server

    # run a benchmark with graphite
    ./bin/mzbench run examples/ramp.erl --env graphite=<graphite_address>

    # or run a benchmark without graphite
    ./bin/mzbench run examples/ramp.erl

    # check dashboard at http://localhost:4800/ for the results

## Deployment Guide

To deploy and configure an MZBench API server on your own infrastructure, see the

[Deployment guide](doc/deployment_guide.md)

## MZBench DSL Reference

To write test scenarios using the MZBench language, see [DSL Reference](doc/scenario_dsl.md).

### DSL Code Examples

MZBench scenarios are DSL programs. To see some pre-coded examples using MZBench DSL
language, see [DSL Code Examples](doc/examples.md).

### Writing Language Extensions

To learn how to write language extensions called "workers," to access a particular service
or protocol, see [How to write language extensions](doc/worker_howto.md).

## Support

Please report an issue on github.
