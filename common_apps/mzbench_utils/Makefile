
REBAR := $(abspath $(shell which ../../bin/rebar || which rebar))

# all files not in ebin or .eunit
SOURCES := $(shell find src test -name .eunit -prune -o -name ebin -prune -o -type f -print)

.PHONY: compile
compile: .make/compilation-up-to-date

.PHONY: test-unit
test-unit: compile
	$(REBAR) eunit skip_deps=true

.PHONY: check
check: test-unit dialyzer.log

.PHONY: clean
clean:
	rm -rf .make
	rm -rf deps
	rm -rf tmp
	$(REBAR) clean

.make/deps-cloned: rebar.config
	rm -rf deps && $(REBAR) get-deps
	mkdir -p .make && touch $@

.make/deps-compiled: .make/deps-cloned
	$(REBAR) compile
	mkdir -p .make && touch $@

.make/compilation-up-to-date: .make/deps-compiled $(SOURCES)
	$(REBAR) compile skip_deps=true
	mkdir -p .make && touch $@

-include ../../lib/dialyzer-without-deps.mk