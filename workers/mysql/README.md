# MySQL worker for MZBench

Supports multiple groups of metrics.

## connect

	connect(user = "root", password = "supersecret")

For more options please refer to `emysql:add_pool` function description at mysql [driver](https://github.com/Eonblast/Emysql).

## query

	query("SELECT * from user")

Execute query and report latencies to `default` group.

	query("mygroup", "SELECT * from user")

To report to `mygroup`