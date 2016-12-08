# PostgreSQL worker for MZBench

Supports multiple groups of metrics.

## connect

	connect(user = "tester", password = "test_password", database = "test_database", host = "localhost")

For more options please refer to `epgsql:connect` function description at pgsql [driver](https://github.com/epgsql/epgsql#connect).

## query

	query("SELECT * from test_table")

Execute query and report latencies to `default` group.

	query("mygroup", "SELECT * from test_table")

To report to `mygroup`

[Full example](examples/pgsql.bdl)