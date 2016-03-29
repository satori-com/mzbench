.PHONY: all compile generate_tgz clean

REBAR := $(abspath $(shell which ./rebar3 || which rebar3))

PKG_NAME = tcp_worker

all: compile

compile:
	$(REBAR) compile

generate_tgz: compile
	mkdir -p pkgroot/${PKG_NAME}/ebin
	if [ -d "_build/default/deps" ]; then cp -Rf _build/default/deps pkgroot/${PKG_NAME}; fi
	mv pkgroot/${PKG_NAME}/deps/${PKG_NAME}/ebin/* pkgroot/${PKG_NAME}/ebin/
	rm -rf pkgroot/${PKG_NAME}/deps/${PKG_NAME}
	if [ -d "examples" ]; then cp -Rf examples pkgroot/${PKG_NAME}; fi
	if [ -d "resources" ]; then cp -Rf resources pkgroot/${PKG_NAME}; fi
	if [ -f "sys.config" ]; then cp sys.config pkgroot/${PKG_NAME}/; fi
	
	cd pkgroot && tar czf ../${PKG_NAME}.tgz ./${PKG_NAME} && cd ..
	rm -rf pkgroot

clean:
	rm -rf log*
	rm -rf data.*
	rm -f *.tgz
	$(REBAR) clean
	rm -rf pkgroot
	-rm -rf _build/default/deps/*/ebin
