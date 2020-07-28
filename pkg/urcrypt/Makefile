CC     ?= cc
AR     ?= ar
PREFIX ?= ./out

################################################################################

.PHONY: all test install clean

all: urcrypt.c urcrypt.h
	$(CC) $(CFLAGS) -O3 -Wall -Werror -pedantic -std=gnu99 -c urcrypt.c
	$(AR) rcs liburcrypt.a urcrypt.o

install: all
	@mkdir -p $(PREFIX)/lib/
	@mkdir -p $(PREFIX)/include/
	cp liburcrypt.a $(PREFIX)/lib/
	cp urcrypt.h $(PREFIX)/include/

clean:
	rm -rf ./out
