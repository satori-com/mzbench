# Cassandra worker for MZBench

Supports multiple groups of metrics.

## connect

	connect(host = "127.0.0.1", keyspace = "system")

Host is "127.0.0.1" by default, port is 9042.
For more options please refer to `cqerl:get_client` [modes](https://github.com/matehat/cqerl#all-modes).

## query

	query("SELECT * from local")

Execute query and report latencies.

[Full example](examples/cassandra.bdl)

## set_prefix

Set prefix for metric reporting, for example you could differ between two queries or two threads stats.

	set_prefix("mygroup")
