
REBAR := $(abspath $(shell which ../bin/rebar || which rebar))
SERVICE_PREFIX=/mz
SERVICE_NAME=mz_bench_api
RELVSN := $(shell ../bin/relvsn.erl $(SERVICE_NAME))

SOURCES := $(shell find src priv test -name .eunit -prune -o -type f -print)
MZ_BENCH_LANGUAGE_SOURCES := $(shell find ../common_apps/mz_bench_language -name .eunit -prune -o -type f -print)

.PHONY: compile
compile: .make/compilation-up-to-date

.PHONY: console
console: generate
	rel/$(SERVICE_NAME)/bin/$(SERVICE_NAME) console

.PHONY: install
install: generate
	sudo mkdir -p $(SERVICE_PREFIX)
	sudo cp -r rel/$(SERVICE_NAME) $(SERVICE_PREFIX)
	sudo mkdir -p $(SERVICE_PREFIX)/$(SERVICE_NAME)/packages
	sudo chmod -R go+rwx $(SERVICE_PREFIX)/$(SERVICE_NAME)/packages
	sudo chmod -R go+rwx $(SERVICE_PREFIX)/$(SERVICE_NAME)/log

.PHONY: test-unit
test-unit: compile
	$(REBAR) eunit skip_deps=true

.PHONY: check
check: test-unit

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

.make/deps-cloned: rebar.config $(MZ_BENCH_LANGUAGE_SOURCES)
	rm -rf deps && $(REBAR) get-deps
	mkdir -p .make && touch $@

.make/deps-compiled: .make/deps-cloned
	$(REBAR) compile
	mkdir -p .make && touch $@

.make/compilation-up-to-date: .make/deps-compiled $(SOURCES)
	$(REBAR) compile skip_deps=true
	mkdir -p .make && touch $@

rel/$(SERVICE_NAME)/bin/$(SERVICE_NAME): compile rel/reltool.config
	rm -rf rel/$(SERVICE_NAME)
	cd rel && $(REBAR) generate -f target_dir=$(SERVICE_NAME)
	cp rel/$(SERVICE_NAME)/releases/$(RELVSN)/$(SERVICE_NAME).boot rel/$(SERVICE_NAME)/releases/$(RELVSN)/start.boot #workaround for rebar bug
	echo $(RELVSN) > rel/$(SERVICE_NAME)/relvsn
