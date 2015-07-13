
.PHONY: check
check: local-tests basic-tests negative-tests signal-tests

.PHONY: local-tests
local-tests:
	./mzb_local_tests.py -v

.PHONY: basic-tests
basic-tests:
	./mzb_basic_tests.py -v

.PHONY: negative-tests
negative-tests:
	./mzb_negative_tests.py -v

.PHONY: signal-tests
signal-tests:
	./mzb_signal_tests.py -v


.PHONY: clean
clean:
	-@rm -rf data.*
	-@rm -rf log.*
