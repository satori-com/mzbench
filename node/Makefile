RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
SECOND_ARG := $(wordlist 2,2,$(MAKECMDGOALS))

.SILENT: new-worker

REBAR := $(abspath $(shell which ../bin/rebar || which rebar))
SERVICE_PREFIX=/mz
SERVICE_NAME=mz_bench
SERVICE_LOG_DIR=/var/log/${SERVICE_NAME}
RELVSN := $(shell ../bin/relvsn.erl $(SERVICE_NAME))

# all files not in ebin or .eunit
SOURCES := $(shell find apps -name .eunit -prune -o -name ebin -prune -o -type f -print)
MZ_BENCH_LANGUAGE_SOURCES := $(shell find ../common_apps/mz_bench_language -name .eunit -prune -o -type f -print)

.PHONY: compile
compile: .make/compilation-up-to-date

.PHONY: run
run: compile
	./scripts/run_local.escript $(realpath $(SECOND_ARG))

.PHONY: generate
generate: rel/$(SERVICE_NAME)/bin/$(SERVICE_NAME)

.PHONY: console
console: generate
	rel/$(SERVICE_NAME)/bin/$(SERVICE_NAME) console $(realpath $(RUN_ARGS))

.PHONY: $(SECOND_ARG)
$(SECOND_ARG):
	@true

.PHONY: test-unit
test-unit: compile
	$(REBAR) eunit skip_deps=true

.PHONY: test-network-usage-script
test-network-usage-script: scripts/report_network_usage.py
	# Just a sanity check that script exists and outputs a valid erlang term
	erl -noshell -eval\
		'X = os:cmd("./scripts/report_network_usage.py"), {ok, Tokens, _} = erl_scan:string(X), {ok, Term} = erl_parse:parse_term(Tokens), io:format("ok").'\
		-run init stop > expected_ok
	grep -qx ok expected_ok
	rm expected_ok

.PHONY: check
check: test-unit test-network-usage-script dialyzer.log

.PHONY: install
install: generate
	mkdir -p ~/mz
	cp -rf rel/mz_bench ~/mz

.PHONY: generate_tgz
generate_tgz: generate
	mkdir -p pkgroot
	cp -rf rel/mz_bench pkgroot/
	cd pkgroot && tar czf ../node.tgz ./mz_bench && cd ..
	rm -rf pkgroot

.PHONY: clean
clean:
	$(REBAR) clean skip_deps=true
	rm -rf .make
	rm -rf *.rpm
	rm -rf *.tgz
	rm -rf rel/$(SERVICE_NAME)
	rm -rf deps
	rm -rf pkgroot

.PHONY: distclean
distclean: clean
	rm .mz_bench.plt

# RPM creation
.PHONY: change-version
change-version:
	@if test "$(new-version)" = "" ; then \
		echo "new-version is undefined, use 'make change-version new-version=X.X.X' command"; \
		exit 1; \
	fi
	sed -i "s/${RELVSN}/$(new-version)/g" rel/reltool.config
	git commit rel/reltool.config -m "Bump mzbench version to $(new-version)"

.mz_bench.plt: .make/deps-compiled
	- dialyzer --output_plt .mz_bench.plt --build_plt --apps erts kernel stdlib eunit crypto os_mon -r deps

dialyzer.log: .mz_bench.plt compile
	- dialyzer --plt .mz_bench.plt \
		apps/mz_bench/ebin apps/mz_histogram/ebin \
		-I apps/mz_bench/src \
		-I deps -o dialyzer.log \
		-Wno_undefined_callbacks \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions
	-@perl -ne 'print if not /lager_not_running/' -i dialyzer.log
	-@perl -ne "print if not /The pattern {'error', UtilFailedReason} can never match the type float()/" -i dialyzer.log
	-@ cat dialyzer.log
	@python -c 'with open("dialyzer.log") as f: import sys; sys.exit(1 if f.read().strip() else 0)'

rel/$(SERVICE_NAME)/bin/$(SERVICE_NAME): compile rel/reltool.config
	rm -rf rel/$(SERVICE_NAME)
	cd rel && $(REBAR) generate -f target_dir=$(SERVICE_NAME)
	cp rel/$(SERVICE_NAME)/releases/$(RELVSN)/$(SERVICE_NAME).boot rel/$(SERVICE_NAME)/releases/$(RELVSN)/start.boot #workaround for rebar bug
	echo $(RELVSN) > rel/$(SERVICE_NAME)/relvsn

.make/deps-cloned: rebar.config $(MZ_BENCH_LANGUAGE_SOURCES)
	rm -rf deps && $(REBAR) get-deps
	mkdir -p .make && touch $@

.make/deps-compiled: .make/deps-cloned
	$(REBAR) compile
	mkdir -p .make && touch $@

.make/compilation-up-to-date: .make/deps-compiled $(SOURCES)
	$(REBAR) compile skip_deps=true
	mkdir -p .make && touch $@
