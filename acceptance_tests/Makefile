
.PHONY: check
check: server
	./run_tests.py -v

.PHONY: server
server:
	../bin/mzbench install_server
	# we need it until we get rid of server installation procedure
	sudo chmod go+wx /mz/mz_bench_api

