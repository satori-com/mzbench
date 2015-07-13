
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

.PHONY: distclean
distclean: clean
	rm .mz_bench_utils.plt

.mz_bench_utils.plt: .make/deps-compiled
	- dialyzer --output_plt $@ --build_plt --apps erts kernel stdlib eunit crypto

dialyzer.log: .mz_bench_utils.plt compile
	- dialyzer --plt $< \
		ebin \
		-I src \
		-o dialyzer.log \
		-Wno_undefined_callbacks \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions
	-@perl -ne 'print if not /lager_not_running/' -i dialyzer.log
	-@perl -ne "print if not /The pattern {'error', UtilFailedReason} can never match the type float()/" -i dialyzer.log
	-@cat dialyzer.log
	@python -c 'with open("dialyzer.log") as f: import sys; sys.exit(1 if f.read().strip() else 0)'

.make/deps-cloned: rebar.config
	rm -rf deps && $(REBAR) get-deps
	mkdir -p .make && touch $@

.make/deps-compiled: .make/deps-cloned
	$(REBAR) compile
	mkdir -p .make && touch $@

.make/compilation-up-to-date: .make/deps-compiled $(SOURCES)
	$(REBAR) compile skip_deps=true
	mkdir -p .make && touch $@
