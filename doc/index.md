# Welcome to MZBench

***Expressive, scalable load testing tool***

---

![Graphs](images/graphs.gif)

**MZBench** helps developers test software under huge load. With this, it helps reduce the risk of outages in production.

MZBench runs test scenarios on many machines simultaneously, maintaining millions of connections. This makes it suitable even for large scale products (we're talking Facebook large scale here).

MZBench is:

 - **Cloud-aware:** allocates nodes directly from Amazon EC2 or run on a local machine.
 - **Scalable:** tested with 100 nodes and millions of connections.
 - **Extendable:** lets you write your own [cloud plugins](cloud_plugins#how-to-write-a-cloud-plugin) and [workers](workers.md#how-to-write-a-worker).
 - **Open-source:** released under the [BSD license](https://github.com/machinezone/mzbench/blob/master/LICENSE).


## Installation

## Installation

### From RPM and Pip

Available for CentOS 7 and Amazon Linux.

Download MZBench RPM from [Github releases page](https://github.com/machinezone/mzbench/releases)

```bash
# Install RPM
sudo yum install -y <rpm_file_downloaded_from_github_releases>

# Install Python package
sudo pip install mzbench_api_client

# Start the server
mzbench start_server
```

### From Docker container

Docker is a container platform, more information is available at its [website](https://www.docker.com/). If you have Docker up and running, use the following command to start MZBench server:

```bash
docker run -d -p 4800:80 --name mzbench_server docker.io/ridrisov/mzbench
```

After that, open http://localhost:4800/ to see the dashboard. Sources for this docker image are available on [github](https://github.com/machinezone/mzbench/tree/master/docker).

### From sources

To use MZBench, you'll need:

 - Erlang R17+
 - C++ compiler
 - Python 2.6 or 2.7 with pip

Download MZBench from GitHub and install Python requirements:

```bash
$ git clone https://github.com/machinezone/mzbench
$ sudo pip install -r mzbench/requirements.txt
```

If you want to use virtualenv (optional) to isolate Python dependencies:

```bash
$ git clone https://github.com/machinezone/mzbench
$ cd mzbench
$ virtualenv venv
$ source venv/bin/activate
$ pip install -r requirements.txt
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
$ ./bin/mzbench run workers/http/examples/localhost.bdl
{
    "status": "pending",
    "id": 6
}
status: running                       00:09
```

Go to [localhost:4800](http://localhost:4800) and see the benchmark live status:

![Test Benchmark](images/test_benchmark.png)


## How It Works

MZBench runs your test scenarios on many *nodes*, simultaneously. This allows it to put extraordinarily high load on the target system—we're talking about *millions* of simultaneous connections here.

**Node** is a machine, virtual or physical, that runs your scenarios. In real-life testing, MZBench is used with a cloud service like Amazon EC2 that provides nodes on demand. Alternatively, you can manually list the available node hosts. Anyway, you have to provide MZBench the machines to run on. If there's not enough nodes to run all the jobs at the same time, MZBench evenly distributes the jobs between the available nodes.

There's one node that doesn't run scenarios—the **director node**. It collects the metrics from the other nodes and runs [post and pre hooks](scenarios/spec.md#pre_hook-and-post_hook). So, if you want to run jobs on 10 nodes, reserve 11.

![MZBench Architecture Provisioning](images/scheme_1.png)

When the MZBench server runs your scenarios, it allocates the nodes, prepares them, and distributes the jobs. During the test run, the nodes send the collected data to the director node which then submits them to the server. The server uses the data to render graphs and show stats:

![MZBench Architecture Running](images/scheme_2.png)

To know what kind of jobs MZBench can run, it's important to understand the concept of a *worker*.

**Worker** is an Erlang module that provides functions for test scenarios. A worker may implement a common protocol like HTTP or XMPP, or a specific routine that is relevant only for a particular test case. It also implements the related metrics.

MZBench ships with workers for [HTTP](../workers/http), [MySQL](../workers/mysql), [PostgreSQL](../workers/pgsql), [MongoDB](../workers/mongo), [Cassandra](../workers/cassandra), [XMPP](../workers/xmpp), [AMQP](../workers/amqp), [TCP](../workers/tcp), [Shell commands execution](../workers/exec), [Simplified HTTP](../workers/simple_http), and [TCPKali](../workers/tcpkali). This should be enough for most common test cases, but you can use your own workers in necessary.


## Read Next

 - [How to write scenarios →](scenarios/tutorial.md)
 - [How to control MZBench from command line →](cli.md)
 - [How to deploy MZBench →](deployment.md)
 - [How to write your own worker →](workers.md#how-to-write-a-worker)
