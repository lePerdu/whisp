all:
	$(MAKE) -f debug.mk $@

run:
	$(MAKE) -f debug.mk $@

debug:
	$(MAKE) -f debug.mk $@

test:
	$(MAKE) -f debug.mk $@

bench:
	$(MAKE) -f release.mk $@

configure:
	$(MAKE) -f release.mk $@

build-install:
	$(MAKE) -f release.mk all

install:
	$(MAKE) -f release.mk $@

uninstall:
	$(MAKE) -f release.mk $@

clean:
	$(MAKE) -f debug.mk $@
	$(MAKE) -f release.mk $@

.PHONY: all run debug test bench configure build-install install uninstall clean
