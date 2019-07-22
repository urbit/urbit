CC     ?= cc
AR     ?= ar
PREFIX ?= ./out

################################################################################

.PHONY: all test install clean

all: ge-additions.c ge-additions.h
	$(CC) $(CFLAGS) -O3 -Wall -Werror -pedantic -std=gnu99 -c ge-additions.c
	$(AR) rcs libge-additions.a ge-additions.o

install: all
	@mkdir -p $(PREFIX)/lib/
	@mkdir -p $(PREFIX)/include/
	cp libge-additions.a $(PREFIX)/lib/
	cp ge-additions.h $(PREFIX)/include/

clean:
	rm -rf ./out
