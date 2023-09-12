CFLAGS = -Wall -Wextra -std=c17 -g -Og
LDFLAGS = $(CFLAGS) -lreadline -lm

BUILD = build
BIN = bin

EXEC = $(BIN)/whisp
SRCS = main.c types.c reader.c printer.c memory.c core.c core_helper.c eval.c file.c
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
	$(CC) $(LDFLAGS) -o $@ $^

$(BUILD)/%.o: %.c | $(BUILD)
	$(CC) $(CFLAGS) -MMD -c -o $@ $<

$(BIN):
	mkdir -p $@

$(BUILD):
	mkdir -p $@

-include $(BUILD)/*.d
