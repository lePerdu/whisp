CFLAGS = -O3
BUILD = build/release

CONFIGURED_MAKEFILE = $(BUILD)/configured.mk
-include $(CONFIGURED_MAKEFILE)
PREFIX ?= /usr/local
WHISP_LIB_DIR = $(PREFIX)/lib/whisp

# configure has to be run before building/installing
GENERATE_CONFIG_TARGET = configure-generate-config

include common.mk

configure: configure-generate-config
	# Save the prefix used at configure time for future "install"s
	printf 'PREFIX = %s\n' $(PREFIX) >$(CONFIGURED_MAKEFILE)

EXEC_INSTALL = $(PREFIX)/bin/$(EXEC_NAME)
LIB_INSTALL = $(WHISP_LIB_DIR)

# uninstall before installing to avoid old files sticking around
install: uninstall $(EXEC) $(WHISP_LIB_SRCS)
	install --mode=755 -D $(EXEC) $(EXEC_INSTALL)
	install --mode=644 -D  -t $(LIB_INSTALL) $(WHISP_LIB_SRCS)

uninstall:
	$(RM) $(EXEC_INSTALL)
	$(RM) -r $(LIB_INSTALL)

.PHONY: install configure uninstall
