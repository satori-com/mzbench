.PHONY: all compile get-deps install clean pkgroot

REBAR := $(abspath $(shell which ./rebar || which rebar))

PKG_NAME = amqp_worker

all: get-deps compile

compile:
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps

install: get-deps compile
	sudo mkdir -p /mz/mz_bench_workers/${PKG_NAME}/ebin
	sudo cp ebin/* /mz/mz_bench_workers/${PKG_NAME}/ebin/
	if [ -d "deps" ]; then sudo cp -R deps /mz/mz_bench_workers/${PKG_NAME}; fi
	if [ -f "sys.config" ]; then sudo cp sys.config /mz/mz_bench_workers/${PKG_NAME}/; fi

clean:
	rm -rf pkgroot
	rm -rf deps
	rm -rf log*
	rm -rf data.*
	rm -f *.rpm
	$(REBAR) clean
	rm -rf ebin

