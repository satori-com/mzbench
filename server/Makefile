
REBAR := $(abspath $(shell which ../bin/rebar || which rebar))
SERVICE_NAME=mzbench_api
RELVSN := $(shell ../bin/relvsn.erl $(SERVICE_NAME))
GIT_HEAD := $(shell git rev-parse HEAD)

SOURCES := $(shell find src priv test -name .eunit -prune -o -type f -print)
MZBENCH_COMMON_SOURCES := $(shell find ../common_apps -name .eunit -prune -o -name deps -prune -o -type f -print)

.PHONY: compile
compile: .make/compilation-up-to-date

.PHONY: console
console: generate
	rel/$(SERVICE_NAME)/bin/$(SERVICE_NAME) console

.PHONY: test-unit
test-unit: compile
	$(REBAR) eunit skip_deps=true

.PHONY: check
check: test-unit dialyzer.log

.PHONY: clean
clean:
	$(REBAR) clean
	rm -rf .make
	rm -rf rel/$(SERVICE_NAME)
	rm -rf deps

.PHONY: generate
generate: rel/$(SERVICE_NAME)/bin/$(SERVICE_NAME)

.PHONY: change-version
change-version:
	@if test "$(new-version)" = "" ; then \
		echo "new-version is undefined, use 'make change-version new-version=X.X.X' command"; \
		exit 1; \
	fi
	sed -i "s/${RELVSN}/$(new-version)/g" rel/reltool.config
	git commit rel/reltool.config -m "Bump mzbench-api version to $(new-version)"

.make/deps-cloned: rebar.config $(MZBENCH_COMMON_SOURCES)
	rm -rf deps && $(REBAR) get-deps
	PATH="$$PWD/../bin:$$PATH" make -C deps/mzbench_language clean
	PATH="$$PWD/../bin:$$PATH" make -C deps/mzbench_utils clean
	perl -pe 's/ERLC_FLAGS=$$/ERLC_FLAGS=+debug_info/' -i deps/merl/Makefile
	mkdir -p .make && touch $@

.make/deps-compiled: .make/deps-cloned
	$(REBAR) compile
	mkdir -p .make && touch $@

.make/compilation-up-to-date: .make/deps-compiled $(SOURCES) .make/git-rev-$(GIT_HEAD)
	$(REBAR) compile skip_deps=true
	mkdir -p .make && touch $@

.make/git-rev-$(GIT_HEAD):
	rm -rf .make/git-rev-*
	mkdir -p .make && touch $@

rel/$(SERVICE_NAME)/bin/$(SERVICE_NAME): .make/compilation-up-to-date rel/reltool.config
	rm -rf rel/$(SERVICE_NAME)
	cd rel && $(REBAR) generate -f target_dir=$(SERVICE_NAME)
	cp rel/$(SERVICE_NAME)/releases/$(RELVSN)/$(SERVICE_NAME).boot rel/$(SERVICE_NAME)/releases/$(RELVSN)/start.boot #workaround for rebar bug
	echo $(RELVSN) > rel/$(SERVICE_NAME)/relvsn


.mzbench_server.plt: .make/deps-compiled
	- dialyzer --output_plt $@ --build_plt --apps erts kernel stdlib eunit crypto inets deps/cowboy -pa deps/*/ebin

dialyzer.log: .mzbench_server.plt .make/compilation-up-to-date
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
