CFLAGS += -Wall -Wextra -Werror -std=c17
LDFLAGS = $(CFLAGS)
LDLIBS = -lm

BIN = $(BUILD)/bin

EXEC_NAME = whisp
EXEC = $(BIN)/$(EXEC_NAME)
SRCS = \
	main.c \
	bytecode.c \
	compiler.c \
	core.c \
	env.c \
	eval.c \
	file.c \
	hash_table.c \
	memory.c \
	ports.c \
	printer.c \
	reader.c \
	symbol.c \
	types.c \
	val_array.c \
	vm.c
OBJS = $(SRCS:%.c=$(BUILD)/%.o)

WHISP_LIB_SRCS_DIR = lib
WHISP_LIB_SRC_FILES = \
	boot.wh \
	array.wh \
	array-builder.wh \
	basic.wh \
	bool.wh \
	exception.wh \
	fluid.wh \
	func.wh \
	generic.wh \
	hash-table.wh \
	list.wh \
	macros.wh \
	math.wh \
	ports.wh \
	ratio.wh \
	record.wh \
	repl.wh \
	startup.wh \
	string.wh \
	vector.wh
WHISP_LIB_SRCS = $(WHISP_LIB_SRC_FILES:%=$(WHISP_LIB_SRCS_DIR)/%)

GENERATED = $(BUILD)/generated
GENERATED_CONFIG = $(GENERATED)/config.h

CFLAGS += -I$(GENERATED)

all: $(EXEC)

clean:
	$(RM) -r $(BUILD)

.PHONY: all configure clean

$(EXEC): $(OBJS) | $(BIN)
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

$(BUILD)/%.o: %.c | $(BUILD) $(GENERATED_CONFIG)
	$(CC) $(CFLAGS) -MMD -c -o $@ $<

# Extra `echo` to expand `~` in the path.
# TODO Figure out a cleaner way to do this
$(GENERATED_CONFIG): config.h.in | $(GENERATED)
	sed "s|%WHISP_LIB_DIR%|$$(echo $(WHISP_LIB_DIR))|" $< >$@

$(BIN):
	mkdir -p $@

$(BUILD):
	mkdir -p $@

$(GENERATED):
	mkdir -p $@

-include $(BUILD)/*.d
