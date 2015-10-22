
.PHONY: check
check: local-tests basic-tests negative-tests signal-tests

.PHONY: local-tests
local-tests:
	./mzb_local_tests.py -v

.PHONY: basic-tests
basic-tests:
	./mzb_basic_tests.py -v --processes=10 --process-timeout=1200

.PHONY: quick-test
quick-test:
	./mzb_quick_test.py -v

.PHONY: clean
clean:
	-@rm -rf data.*
	-@rm -rf log.*
