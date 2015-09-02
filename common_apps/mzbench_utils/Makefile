
REBAR := $(abspath $(shell which ../../bin/rebar3 || which rebar3))

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

.make/compilation-up-to-date: $(SOURCES) rebar.config
	$(REBAR) compile
	mkdir -p .make && touch $@

DEPS_PLT = 
-include ../../lib/dialyzer.mk