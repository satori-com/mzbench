# AMQP worker for MZBench

Supports multiple groups of metrics.
For more information on AMQP protocol please visit [RabbitMQ tutorial](https://www.rabbitmq.com/tutorials/tutorial-three-python.html).

## connect

	connect(host = "127.0.0.1")

Supported parameters: host, port, username, and password. By default username and password are "guest".

## declare_queue

	declare_queue("q1")

Queue needs to be declared before publish or get.

## declare_exchange

	declare_exchange("x1")

## bind

	bind("x1", "key1", "q1")

Binds specific key for an exchange to a queue.

## publish

	publish("q1", "message")

or

	publish("x1", "key1", "message")

Publish specific message to a queue or a key and exchange.

## get

	get("q1")

Read specific queue and return control.

## subscribe

	subscribe("q1")

Spawn additional thread and subscribe to a queue.

## set_prefix

Set prefix for metric reporting, for example you could differ between two queries or two threads stats.

	set_prefix("mygroup")


# Examples

[Pubsub](examples/pubsub.bdl), [exchange](examples/exchange.bdl), [subscribe](examples/subscribe.bdl).