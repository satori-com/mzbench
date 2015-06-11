.PHONY: all compile get-deps install clean

REBAR := $(abspath $(shell which ./rebar || which rebar))

PKG_NAME = empty_worker

all: get-deps compile

compile:
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps

install: get-deps compile
	mkdir -p ~/mz/mz_bench_workers/${PKG_NAME}/ebin
	cp ebin/* ~/mz/mz_bench_workers/${PKG_NAME}/ebin/
	if [ -d "deps" ]; then cp -Rf deps ~/mz/mz_bench_workers/${PKG_NAME}; fi
	if [ -d "examples" ]; then cp -Rf examples ~/mz/mz_bench_workers/${PKG_NAME}; fi
	if [ -d "resources" ]; then cp -Rf resources ~/mz/mz_bench_workers/${PKG_NAME}; fi
	if [ -f "sys.config" ]; then cp sys.config ~/mz/mz_bench_workers/${PKG_NAME}/; fi

clean:
	rm -rf pkgroot
	rm -rf deps
	rm -rf log*
	rm -rf data.*
	rm -f *.rpm
	$(REBAR) clean
	rm -rf ebin
