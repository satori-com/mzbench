.PHONY: all compile get-deps generate_tgz clean pkgroot

REBAR := $(abspath $(shell which ./rebar || which rebar))

PKG_NAME = amqp_worker

all: get-deps compile

compile:
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps

generate_tgz: get-deps compile
	mkdir -p pkgroot/${PKG_NAME}/ebin
	cp ebin/* pkgroot/${PKG_NAME}/ebin/
	if [ -d "deps" ]; then cp -Rf deps pkgroot/${PKG_NAME}; fi
	if [ -f "sys.config" ]; then cp sys.config pkgroot/${PKG_NAME}/; fi
	
	cd pkgroot && tar czf ../${PKG_NAME}.tgz ./${PKG_NAME} && cd ..
	rm -rf pkgroot

clean:
	rm -rf pkgroot
	rm -rf deps
	rm -rf log*
	rm -rf data.*
	rm -f *.rpm
	rm -f *.tgz
	$(REBAR) clean
	rm -rf ebin
	rm -rf pkgroot

