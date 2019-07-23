include config.mk

jets = jets/tree.c $(wildcard jets/*/*.c)
noun = $(wildcard noun/*.c)
vere = $(wildcard vere/*.c)
daemon = $(wildcard daemon/*.c)
worker = $(wildcard worker/*.c)

common  = $(jets) $(noun) $(vere)
headers = $(shell find include -type f) include/ca-bundle.h include/ivory.h

common_objs = $(shell echo $(common) | sed 's/\.c/.o/g')
daemon_objs = $(shell echo $(daemon) | sed 's/\.c/.o/g')
worker_objs = $(shell echo $(worker) | sed 's/\.c/.o/g')

all_objs = $(common_objs) $(daemon_objs) $(worker_objs)
all_srcs = $(common) $(daemon) $(worker)
all_exes = ./build/mug_tests ./build/jam_tests ./build/hashtable_tests \
    ./build/urbit ./build/urbit-worker


# -Werror promotes all warnings that are enabled into errors (this is on)
# -Wall issues all types of errors.  This is off (for now)
CFLAGS := $(CFLAGS)

ifeq ($(SSL_CERT_FILE),)
  $(error SSL_CERT_FILE is undefined)
endif

ifeq ($(IVORY),)
  $(error IVORY is undefined)
endif

################################################################################

.PHONY: all test clean mkproper

################################################################################

all: $(all_exes)

test: build/hashtable_tests build/jam_tests build/mug_tests
	./build/hashtable_tests
	./build/jam_tests
	./build/mug_tests

clean:
	rm -f ./tags $(all_objs) $(all_exes)

mrproper: clean
	rm -f config.mk include/config.h include/ca-bundle.h include/ivory.h

################################################################################

include/ca-bundle.h:
	@echo XXD -i $(SSL_CERT_FILE)
	@cat $(SSL_CERT_FILE) > include/ca-bundle.crt
	@xxd -i include/ca-bundle.crt > include/ca-bundle.h
	@rm include/ca-bundle.crt

include/ivory.h:
	@echo XXD -i $(IVORY)
	@cat $(IVORY) > u3_Ivory.pill
	@xxd -i u3_Ivory.pill > include/ivory.h
	@rm u3_Ivory.pill

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
