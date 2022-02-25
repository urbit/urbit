include config.mk
include $(foreach dir,$(compat),$(wildcard compat/$(dir)/*.mk))

c    = $(wildcard c/*.c)
jets = jets/tree.c $(wildcard jets/*/*.c)
noun = $(wildcard noun/*.c)
ur   = $(wildcard ur/*.c)
vere = $(wildcard vere/*.c) $(wildcard vere/*/*.c)
daemon = $(wildcard daemon/*.c)
worker = $(wildcard worker/*.c)
tests  = $(wildcard tests/*.c)
bench  = $(wildcard bench/*.c)

compat := $(foreach dir,$(compat),$(wildcard compat/$(dir)/*.c))

common  = $(c) $(jets) $(noun) $(ur) $(vere) $(compat)
headers = $(shell find include -type f)

common_objs = $(shell echo $(common) | sed 's/\.c/.o/g')
daemon_objs = $(shell echo $(daemon) | sed 's/\.c/.o/g')
worker_objs = $(shell echo $(worker) | sed 's/\.c/.o/g')

all_objs = $(common_objs) $(daemon_objs) $(worker_objs)
all_srcs = $(common) $(daemon) $(worker)

test_exes  = $(shell echo $(tests) | sed 's/tests\//.\/build\//g' | sed 's/\.c//g')
bench_exes = $(shell echo $(bench) | sed 's/bench\//.\/build\//g' | sed 's/\.c//g')
all_exes   = $(test_exes) $(bench_exes) ./build/urbit ./build/urbit-worker

# -Werror promotes all warnings that are enabled into errors (this is on)
# -Wall issues all types of errors.  This is off (for now)
CFLAGS := $(CFLAGS)

################################################################################

.PHONY: all test clean mrproper

################################################################################

all: $(all_exes)

test: $(test_exes)
	@FAIL=0;                                         \
	for x in $^;                                     \
	do echo $$'\n'"$$x" && ./$$x;                    \
	if [ $$? != 0 ]; then FAIL=1; fi;                \
	done;                                            \
	if [ $$FAIL != 0 ]; then echo "\n" && exit 1; fi;

bench: $(bench_exes)
	build/ur_bench

clean:
	rm -f ./tags $(all_objs) $(all_exes)

mrproper: clean
	rm -f config.mk include/config.h

################################################################################

build/ur_bench: $(common_objs) bench/ur_bench.o
	@echo CC -o $@
	@mkdir -p ./build
	@$(CC) $^ $(LDFLAGS) -o $@

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

# CCDEPS and CCEXTRA are empty except in MingW build,
# which uses them to inject a C source transform step
%.o: %.c $(headers) $(CCDEPS)
	@echo CC $<
	@$(CC) -I./include $(CFLAGS) $< $(CCEXTRA) -c -o $@

tags: $(all_srcs) $(headers)
	ctags $^
