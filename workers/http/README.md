# HTTP worker for MZBench

Supports the following methods:

connect/4, set_options/3, disconnect/2,
    get/3, post/4, set_prefix

## connect

	connect("mywebserver.com", 80)

## disconnect

	disconnect()

## get

	get("/")

## post

	post("/", "something useful")

## set_options

Set options for requests.

	set_options(follow_redirect = true, max_redirect = 5)

Complete list of available options could be found at [hackney](https://github.com/benoitc/hackney) documentation.

## set_prefix

Set prefix for metric reporting, for example you could differ between two server stats.

	set_prefix("mygroup")

Please refer to [keepalive example](examples/keepalive.bdl) if you need more details.

# Full examples

[HTTP get](examples/http_get.bdl), [HTTP post](examples/http_post.bdl), [keepalive](examples/keepalive.bdl).