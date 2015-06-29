.PHONY: all compile get-deps generate_tgz clean

REBAR := $(abspath $(shell which ./rebar || which rebar))

PKG_NAME = exec_worker

all: get-deps compile

compile: get-deps
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps

generate_tgz: get-deps compile
	mkdir -p tmp/mz/mz_bench_workers/${PKG_NAME}/ebin
	cp ebin/* tmp/mz/mz_bench_workers/${PKG_NAME}/ebin/
	if [ -d "deps" ]; then cp -Rf deps tmp/mz/mz_bench_workers/${PKG_NAME}; fi
	if [ -d "examples" ]; then cp -Rf examples tmp/mz/mz_bench_workers/${PKG_NAME}; fi
	if [ -d "resources" ]; then cp -Rf resources tmp/mz/mz_bench_workers/${PKG_NAME}; fi
	if [ -f "sys.config" ]; then cp sys.config tmp/mz/mz_bench_workers/${PKG_NAME}/; fi
	
	cd tmp && tar czf ../${PKG_NAME}.tgz ./mz && cd ..
	rm -rf tmp

clean:
	rm -rf deps
	rm -rf log*
	rm -rf data.*
	rm -f *.rpm
	rm -f *.tgz
	$(REBAR) clean
	rm -rf ebin
	rm -rf tmp

