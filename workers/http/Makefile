REBAR := $(abspath $(shell which ../../bin/rebar3 || which rebar3))

SOURCES := src/http_worker.erl src/http_worker.app.src

.PHONY: compile
compile: .make/compilation-up-to-date

.PHONY: generate_tgz
generate_tgz: pkgroot
	mkdir -p pkgroot
	rsync -aW pkgroot/ pkgroot
	cd pkgroot && tar czf ../http_worker.tgz ./${PKG_NAME} && cd ..
	rm -rf pkgroot

.make/compilation-up-to-date: $(SOURCES) rebar.config
	$(REBAR) compile
	mkdir -p .make && touch $@

.PHONY: clean
clean:
	rm -f *.tgz
	rm -rf pkgroot
	rm -rf pkgroot
	rm -rf .make
	$(REBAR) clean

.PHONY: check
check:
	cd test && ./main.py

.PHONY: check-local
check-local:
	cd test && ./main.py --local

PKG_NAME = http_worker

.PHONY: pkgroot
pkgroot: .make/compilation-up-to-date
	rm -rf pkgroot
	mkdir -p pkgroot/${PKG_NAME}/ebin
	mkdir pkgroot/${PKG_NAME}/deps
	cp _build/default/deps/*/ebin/* pkgroot/${PKG_NAME}/ebin/
	if [ -d "examples" ]; then cp -Rf examples pkgroot/${PKG_NAME}; fi
	if [ -d "resources" ]; then cp -Rf resources pkgroot/${PKG_NAME}; fi
