
.PHONY: check
check: server
	./run_tests.py -v

.PHONY: local-tests
local-tests: server
	./mzb_local_tests.py -v

.PHONY: basic-tests
basic-tests: server
	./mzb_basic_tests.py -v

.PHONY: negative-tests
negative-tests: server
	./mzb_negative_tests.py -v

.PHONY: signal-tests
signal-tests: server
	./mzb_signal_tests.py -v

.PHONY: server
server:
	../bin/mzbench install_server
	# we need it until we get rid of server installation procedure
	sudo chmod go+wx /mz/mz_bench_api

