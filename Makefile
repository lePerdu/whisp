CFLAGS = -Wall -Wextra -Werror -std=c17 -g -Og
LDFLAGS = $(CFLAGS)
LDLIBS = -lreadline -lm

BUILD = build
BIN = bin

EXEC = $(BIN)/whisp
SRCS = main.c \
	bytecode.c \
	compiler.c \
	core.c \
	core_helper.c \
	eval.c \
	file.c \
	memory.c \
	printer.c \
	reader.c \
	types.c \
	val_array.c \
	vm.c
OBJS = $(SRCS:%.c=$(BUILD)/%.o)

all: $(EXEC)

run: $(EXEC)
	./$<

debug: $(EXEC)
	gdb $<

test: $(EXEC)
	./$< test/all.wh
.PHONY: test

clean:
	$(RM) -r $(BUILD) $(BIN)

.PHONY: run debug clean

$(EXEC): $(OBJS) | $(BIN)
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

$(BUILD)/%.o: %.c | $(BUILD)
	$(CC) $(CFLAGS) -MMD -c -o $@ $<

$(BIN):
	mkdir -p $@

$(BUILD):
	mkdir -p $@

-include $(BUILD)/*.d
