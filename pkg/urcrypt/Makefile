CC     ?= cc
AR     ?= ar
PREFIX ?= ./out

################################################################################

.PHONY: all install clean

CFLAGS := $(CFLAGS) -O3 -Wall -Werror -pedantic -std=gnu99
SOURCES = urcrypt.c urcrypt.h

liburcrypt.a: $(SOURCES)
	$(CC) $(CFLAGS) -c urcrypt.c -o urcrypt-static.o
	$(AR) rcs liburcrypt.a urcrypt-static.o

liburcrypt.so: $(SOURCES)
	$(CC) $(CFLAGS) -fPIC -c urcrypt.c -o urcrypt-shared.o
	$(CC) -shared urcrypt-shared.o -o liburcrypt.so \
		-led25519 -lge-additions -lssl -largon2 -laes_siv \
		-Wl,--no-undefined

all: liburcrypt.a liburcrypt.so

install: all
	@mkdir -p $(PREFIX)/lib/
	@mkdir -p $(PREFIX)/include/
	cp liburcrypt.a $(PREFIX)/lib/
	cp liburcrypt.so $(PREFIX)/lib/
	cp urcrypt.h $(PREFIX)/include/

clean:
	rm urcrypt-static.o urcrypt-shared.o liburcrypt.a liburcrypt.so
