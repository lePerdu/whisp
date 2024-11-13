CFLAGS = -O3 -flto
BUILD = build/release

CONFIGURED_MAKEFILE = .configured.mk
-include $(CONFIGURED_MAKEFILE)
PREFIX ?= /usr/local
WHISP_LIB_DIR = $(PREFIX)/lib/whisp

include common.mk

# Save the configured settings for future "install"s
# Also delete existing generated files so they will be re-generated on next build
configure:
	printf 'PREFIX = %s\n' $(PREFIX) >$(CONFIGURED_MAKEFILE)
	$(RM) $(GENERATED_CONFIG)

EXEC_INSTALL = $(PREFIX)/bin/$(EXEC_NAME)
LIB_INSTALL = $(WHISP_LIB_DIR)

# uninstall before installing to avoid old files sticking around
install: uninstall $(EXEC) $(WHISP_LIB_SRCS)
	install --mode=755 -D $(EXEC) $(EXEC_INSTALL)
	install --mode=644 -D  -t $(LIB_INSTALL) $(WHISP_LIB_SRCS)

uninstall:
	$(RM) $(EXEC_INSTALL)
	$(RM) -r $(LIB_INSTALL)

# Use local lib folder so that benchmarks can be run with optimizations without
# installing globally
bench: $(EXEC)
	WHISP_LIB_DIR=./lib eval $< bench/all.wh

.PHONY: install configure uninstall bench
