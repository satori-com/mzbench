# TCP worker for MZBench

Supports syncronous and asynchronous modes.

## connect_sync

	connect_sync("service_host", 4444)

Connect to a specific host with port.

## request_sync

	request_sync(<<"myrequest">>)

Send something and wait for reply with default timeout 5 seconds.

[Full example](examples/sync.bdl)

## connect

	connect("service_host", 4444)

Connect to a specific host with port in asynchronous mode. The difference between `connect_sync` is that it spawns additional process to receive messages.

## request

	request(<<"myrequest">>)

Put request to a queue and return control immediately.

## wait_finish

	wait_finish()

Wait for all async requests to be finished.

[Full example](examples/async.bdl)
