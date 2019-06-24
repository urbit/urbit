CC     ?= cc
AR     ?= ar
PREFIX ?= ./out

ifndef IMPL
$(error IMPL must be set)
endif

################################################################################

.PHONY: all test install clean

all: ent.c ent.h
	$(CC) -D$(IMPL) -O3 -Wall -Werror -pedantic -std=gnu99 -c ent.c
	$(AR) rcs libent.a ent.o

test: all
	$(CC) -D$(IMPL) -O3 -Wall -Werror -pedantic -std=gnu99 ent.c test.c -o test
	./test

install: all
	@mkdir -p $(PREFIX)/lib/
	@mkdir -p $(PREFIX)/include/
	cp libent.a $(PREFIX)/lib/
	cp ent.h $(PREFIX)/include/

clean:
	rm -f *.o *.a test
	rm -rf ./out
