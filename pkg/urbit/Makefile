include config.mk

jets = jets/tree.c $(wildcard jets/*/*.c)
noun = $(wildcard noun/*.c)
vere = $(wildcard vere/*.c)
daemon = $(wildcard daemon/*.c)
worker = $(wildcard worker/*.c)
tests  = $(wildcard tests/*.c)

common  = $(jets) $(noun) $(vere)
headers = $(shell find include -type f)

common_objs = $(shell echo $(common) | sed 's/\.c/.o/g')
daemon_objs = $(shell echo $(daemon) | sed 's/\.c/.o/g')
worker_objs = $(shell echo $(worker) | sed 's/\.c/.o/g')

all_objs = $(common_objs) $(daemon_objs) $(worker_objs)
all_srcs = $(common) $(daemon) $(worker)

test_exes = $(shell echo $(tests) | sed 's/tests\//.\/build\//g' | sed 's/\.c//g')
all_exes  = $(test_exes) ./build/urbit ./build/urbit-worker

# -Werror promotes all warnings that are enabled into errors (this is on)
# -Wall issues all types of errors.  This is off (for now)
CFLAGS := $(CFLAGS)

################################################################################

.PHONY: all test clean mkproper

################################################################################

all: $(all_exes)

test: $(test_exes)
	@FAIL=0;                                         \
	for x in $^;                                     \
	do echo "\n$$x" && ./$$x;                        \
	if [ $$? != 0 ]; then FAIL=1; fi;                \
	done;                                            \
	if [ $$FAIL != 0 ]; then echo "\n" && exit 1; fi;

clean:
	rm -f ./tags $(all_objs) $(all_exes)

mrproper: clean
	rm -f config.mk include/config.h

################################################################################

build/%_tests: $(common_objs) tests/%_tests.o
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
