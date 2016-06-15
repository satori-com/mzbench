
REBAR := $(abspath $(shell which ../../bin/rebar3 || which rebar3))

# all files not in ebin or .eunit
SOURCES := $(shell find src include test -name .eunit -prune -o -name ebin -prune -o -type f -print)

.PHONY: compile
compile: .make/compilation-up-to-date

.PHONY: test-unit
test-unit: compile
	$(REBAR) eunit skip_deps=true

.PHONY: check
check: test-unit dialyzer.log

.PHONY: rebuild-parser
rebuild-parser:
	mkdir -p _build
	-git clone https://github.com/seancribbs/neotoma _build/neotoma
	make -C _build/neotoma compile
	make -C _build/neotoma escript
	./_build/neotoma/neotoma src/mzbl_syntax.peg
	-rm src/mzbl_syntax.erl.temp
	mv src/mzbl_syntax.erl src/mzbl_syntax.erl.temp
	echo "%%% This file is auto-generated from mzbl_syntax.peg, run make rebuild-parser if you wish to change it, do not edit directly\n" > src/mzbl_syntax.erl
	cat src/mzbl_syntax.erl.temp >> src/mzbl_syntax.erl
	-rm src/mzbl_syntax.erl.temp

.PHONY: clean
clean:
	rm -rf .make
	rm -rf _build
	rm -rf tmp
	$(REBAR) clean

.make/compilation-up-to-date: $(SOURCES) rebar.config
	$(REBAR) as pedantic compile
	make -C _build/default/deps/jiffy
	mkdir -p .make && touch $@

-include ../../lib/dialyzer.mk