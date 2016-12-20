# MongoDB worker for MZBench

Supports multiple groups of metrics.

## connect

	connect(login = "tester", password = "supersecret", host = "localhost", database = "test_database")

For more options please refer to `connect` function at mongodb [driver](https://github.com/comtihon/mongodb-erlang#connecting).

## insert

	insert("mycollection", [t("field1", "value1", "field2", "value2")])

Insert a set of documents to "mycollection". In this case `{field1:"value1", field2:"value2"}`

## delete

	delete("mycollection", t("field1", "value1"))

Delete all documents from "mycollection" matching given selector.

## find_one

	find_one("mycollection", t("field1", "value1"))

Lookup document from "mycollection" matching given selector.

## count

	count("mycollection", t("field1", "value1"))

Count all documents from "mycollection" matching given selector.

[Full example](examples/mongo.bdl)