RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
SECOND_ARG := $(wordlist 2,2,$(MAKECMDGOALS))

.SILENT: new-worker

REBAR := $(abspath $(shell which ../bin/rebar3 || which rebar3))
SERVICE_PREFIX=/mz
SERVICE_NAME=mzbench
SERVICE_LOG_DIR=/var/log/${SERVICE_NAME}
RELVSN := $(shell ../bin/relvsn.erl $(SERVICE_NAME))
GIT_HEAD := $(shell git rev-parse HEAD)

# all files not in ebin or .eunit
SOURCES := $(shell find apps -name .eunit -prune -o -name ebin -prune -o -type f -print)
MZBENCH_COMMON_SOURCES := $(wildcard ../common_apps/*/include/*) $(wildcard ../common_apps/*/src/*)

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
	cp -rf rel/mzbench ~/mz

.PHONY: generate_tgz
generate_tgz: generate
	mkdir -p pkgroot
	cp -rf rel/mzbench pkgroot/
	cd pkgroot && tar czf ../node.tgz ./mzbench && cd ..
	rm -rf pkgroot

.PHONY: clean
clean:
	$(REBAR) clean
	rm -rf .make
	rm -rf *.rpm
	rm -rf *.tgz
	rm -rf rel/$(SERVICE_NAME)
	rm -rf deps
	rm -rf pkgroot

# RPM creation
.PHONY: change-version
change-version:
	@if test "$(new-version)" = "" ; then \
		echo "new-version is undefined, use 'make change-version new-version=X.X.X' command"; \
		exit 1; \
	fi
	sed -i "s/${RELVSN}/$(new-version)/g" rel/reltool.config
	git commit rel/reltool.config -m "Bump mzbench version to $(new-version)"

DIALYZABLE_EBINS := apps/*/ebin
-include ../lib/dialyzer.mk

rel/$(SERVICE_NAME)/bin/$(SERVICE_NAME): .make/compilation-up-to-date rel/reltool.config
	rm -rf rel/$(SERVICE_NAME)
	cd rel && $(REBAR) generate -f target_dir=$(SERVICE_NAME)
	cp rel/$(SERVICE_NAME)/releases/$(RELVSN)/$(SERVICE_NAME).boot rel/$(SERVICE_NAME)/releases/$(RELVSN)/start.boot #workaround for rebar bug
	echo $(RELVSN) > rel/$(SERVICE_NAME)/relvsn

.make/compilation-up-to-date: rebar.config $(SOURCES) .make/git-rev-$(GIT_HEAD)
	$(REBAR) compile
	make -C _build/default/deps/jiffy
	mkdir -p .make && touch $@

.make/git-rev-$(GIT_HEAD):
	rm -rf .make/git-rev-*
	mkdir -p .make && touch $@

