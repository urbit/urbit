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
all_exes = ./build/ames_tests ./build/mug_tests ./build/jam_tests \
	./build/noun_tests ./build/hashtable_tests \
	./build/urbit ./build/urbit-worker

# -Werror promotes all warnings that are enabled into errors (this is on)
# -Wall issues all types of errors.  This is off (for now)
CFLAGS := $(CFLAGS)

################################################################################

.PHONY: all test clean mkproper

################################################################################

all: $(all_exes)

test: build/ames_tests build/hashtable_tests build/jam_tests build/mug_tests build/newt_tests build/noun_tests
	./build/ames_tests
	./build/hashtable_tests
	./build/jam_tests
	./build/mug_tests
	./build/newt_tests
	./build/noun_tests

clean:
	rm -f ./tags $(all_objs) $(all_exes)

mrproper: clean
	rm -f config.mk include/config.h

################################################################################

build/ames_tests: $(common_objs) tests/ames_tests.o
	@echo CC -o $@
	@mkdir -p ./build
	@$(CC) $^ $(LDFLAGS) -o $@

build/hashtable_tests: $(common_objs) tests/hashtable_tests.o
	@echo CC -o $@
	@mkdir -p ./build
	@$(CC) $^ $(LDFLAGS) -o $@

build/jam_tests: $(common_objs) tests/jam_tests.o
	@echo CC -o $@
	@mkdir -p ./build
	@$(CC) $^ $(LDFLAGS) -o $@

build/mug_tests: $(common_objs) tests/mug_tests.o
	@echo CC -o $@
	@mkdir -p ./build
	@$(CC) $^ $(LDFLAGS) -o $@

build/newt_tests: $(common_objs) tests/newt_tests.o
	@echo CC -o $@
	@mkdir -p ./build
	@$(CC) $^ $(LDFLAGS) -o $@

build/noun_tests: $(common_objs) tests/noun_tests.o
	@echo CC -o $@
	@mkdir -p ./build
	@$(CC) $^ $(LDFLAGS) -o $@

build/urbit: $(common_objs) $(daemon_objs)
	@echo CC -o $@
	@mkdir -p ./build
	@$(CC) $^ $(LDFLAGS) -o $@

build/urbit-worker: $(common_objs) $(worker_objs)
	@echo CC -o $@
	@mkdir -p ./build
	@$(CC) $^ $(LDFLAGS) -o $@

%.o: %.c $(headers)
	@echo CC $<
	@$(CC) -I./include $(CFLAGS) -c $< -o $@

tags: $(all_srcs) $(headers)
	ctags $^
