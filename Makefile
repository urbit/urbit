include config.mk

jets     = $(wildcard jets/*/*.c)
jets_all = jets/tree.c $(jets)

noun     = $(wildcard noun/*.c)

vere     = $(wildcard vere/*.c)

sources  = $(jets_all) $(noun) $(vere)

headers  = $(shell find include -type f)

objs     = $(shell echo $(sources) | sed 's/\.c/.o/g')

################################################################################

all: urbit

clean:
	rm -f $(objs) ./urbit

################################################################################

urbit: $(objs)
	@echo CC -o urbit
	@$(CC) $^ $(LDFLAGS) -o urbit

%.o: %.c $(headers)
	@echo CC $<
	@$(CC) -I./include $(CFLAGS) -c $< -o $@
