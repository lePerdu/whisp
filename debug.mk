CFLAGS = -g -Og -fno-omit-frame-pointer -fsanitize=address
BUILD = build/debug
WHISP_LIB_DIR = ./lib

include common.mk

run: $(EXEC)
	eval $<

debug: $(EXEC)
	gdb $<

test: $(EXEC)
	eval $< test/all.wh

.PHONY: run debug test
