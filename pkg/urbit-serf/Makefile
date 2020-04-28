include config.mk

jets = jets/tree.c $(wildcard jets/*/*.c)
noun = $(wildcard noun/*.c)
vere = $(wildcard vere/*.c)
serf = $(wildcard serf/*.c)
tests  = $(wildcard tests/*.c)

libr  = $(jets) $(noun) $(vere)
hdrs = $(shell find include -type f)

libr_objs = $(shell echo $(libr) | sed 's/\.c/.o/g')
serf_objs = $(shell echo $(serf) | sed 's/\.c/.o/g')

all_objs = $(libr_objs) $(serf_objs)
all_srcs = $(libr) $(daemon) $(serf)

test_exes = $(shell echo $(tests) | sed 's/tests\//.\/build\//g' | sed 's/\.c//g')
all_exes  = $(test_exes) ./build/urbit-serf

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

build/%_tests: $(libr_objs) tests/%_tests.o
	@echo CC -o $@
	@mkdir -p ./build
	@$(CC) $^ $(LDFLAGS) -o $@

build/urbit-serf: $(libr_objs) $(serf_objs)
	@echo CC -o $@
	@mkdir -p ./build
	@$(CC) $^ $(LDFLAGS) -o $@

%.o: %.c $(hdrs)
	@echo CC $<
	@$(CC) -I./include $(CFLAGS) -c $< -o $@

tags: $(all_srcs) $(hdrs)
	ctags $^
