
REBAR := $(abspath $(shell which ../bin/rebar3 || which rebar3))
SERVICE_NAME=mzbench_api
RELVSN := $(shell ../bin/relvsn.erl $(SERVICE_NAME))
GIT_HEAD := $(shell git rev-parse HEAD)

SOURCES := $(shell find src priv test -name .eunit -prune -o -type f -print)
MZBENCH_COMMON_SOURCES := $(wildcard ../common_apps/*/include/*) $(wildcard ../common_apps/*/src/*)
RELEASE_BINARY := _build/default/rel/$(SERVICE_NAME)/bin/$(SERVICE_NAME)

.PHONY: compile
compile: .make/compilation-up-to-date

.PHONY: console
console: $(RELEASE_BINARY)
	$(RELEASE_BINARY) console

.PHONY: js-dev
js-dev: compile
	erl -pa mzbench_api/ebin -pa mzbench_api/deps/*/ebin -eval 'application:ensure_all_started(mzbench_api)'

.PHONY: jsbuild-auto
jsbuild-auto:
	cd priv && npm start

.PHONY: jsbuild-prod
jsbuild-prod:
	cd priv && npm run build

.PHONY: test-unit
test-unit: compile
	$(REBAR) eunit skip_deps=true

.PHONY: check
check: test-unit dialyzer.log

.PHONY: clean
clean:
	$(REBAR) clean
	rm -rf _build
	rm -rf .make

.PHONY: generate
generate: $(RELEASE_BINARY)

.make/compilation-up-to-date: rebar.config $(SOURCES) .make/git-rev-$(GIT_HEAD)
	$(REBAR) compile

	make -C _build/default/deps/jiffy # Until https://github.com/davisp/jiffy/issues/96 is resolved

	mkdir -p .make && touch $@

.make/git-rev-$(GIT_HEAD):
	rm -rf .make/git-rev-*
	mkdir -p .make && touch $@

$(RELEASE_BINARY): .make/compilation-up-to-date rel/files/sys.config rel/files/vm.args
	$(REBAR) release

BUILD_PLT_FLAGS := --apps deps/cowboy
-include ../lib/dialyzer.mk
