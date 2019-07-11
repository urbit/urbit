include config.mk

jets = jets/tree.c $(wildcard jets/*/*.c)
noun = $(wildcard noun/*.c)
vere = $(wildcard vere/*.c)
daemon = $(wildcard daemon/*.c)
worker = $(wildcard worker/*.c)

common  = $(jets) $(noun) $(vere)
headers = $(shell find include -type f)

common_objs = $(shell echo $(common) | sed 's/\.c/.o/g')
daemon_objs = $(shell echo $(daemon) | sed 's/\.c/.o/g')
worker_objs = $(shell echo $(worker) | sed 's/\.c/.o/g')

all_objs = $(common_objs) $(daemon_objs) $(worker_objs)
all_srcs = $(common) $(daemon) $(worker)
all_exes = ./ames_tests ./mug_tests jam_tests ./hashtable_tests ./urbit ./urbit-worker


# -Werror promotes all warnings that are enabled into errors (this is on)
# -Wall issues all types of errors.  This is off (for now)
CFLAGS := $(CFLAGS)


################################################################################

.PHONY: all test clean mkproper

################################################################################

all: urbit urbit-worker ames_tests hashtable_tests jam_tests mug_tests

test: ames_tests hashtable_tests jam_tests mug_tests
	./ames_tests
	./hashtable_tests
	./jam_tests
	./mug_tests

clean:
	rm -f ./tags $(all_objs) $(all_exes)

mrproper: clean
	rm -f config.mk include/config.h

################################################################################

ames_tests: $(common_objs) tests/ames_tests.o
	@echo CC -o $@
	@$(CC) $^ $(LDFLAGS) -o $@

hashtable_tests: $(common_objs) tests/hashtable_tests.o
	@echo CC -o $@
	@$(CC) $^ $(LDFLAGS) -o $@

jam_tests: $(common_objs) tests/jam_tests.o
	@echo CC -o $@
	@$(CC) $^ $(LDFLAGS) -o $@

mug_tests: $(common_objs) tests/mug_tests.o
	@echo CC -o $@
	@$(CC) $^ $(LDFLAGS) -o $@

urbit: $(common_objs) $(daemon_objs)
	@echo CC -o $@
	@$(CC) $^ $(LDFLAGS) -o $@

urbit-worker: $(common_objs) $(worker_objs)
	@echo CC -o $@
	@$(CC) $^ $(LDFLAGS) -o $@

%.o: %.c $(headers)
	@echo CC $<
	@$(CC) -I./include $(CFLAGS) -c $< -o $@

tags: $(all_srcs) $(headers)
	ctags $^
