all:
	$(MAKE) -f Makefile.debug $@

run:
	$(MAKE) -f Makefile.debug $@

debug:
	$(MAKE) -f Makefile.debug $@

test:
	$(MAKE) -f Makefile.debug $@

configure:
	$(MAKE) -f Makefile.release $@

build-install:
	$(MAKE) -f Makefile.release all

install:
	$(MAKE) -f Makefile.release $@

uninstall:
	$(MAKE) -f Makefile.release $@

clean:
	$(MAKE) -f Makefile.debug $@
	$(MAKE) -f Makefile.release $@

.PHONY: all run debug test install uninstall clean
