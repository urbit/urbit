CC     ?= cc
AR     ?= ar
PREFIX ?= ./out

################################################################################

.PHONY: all install clean

CFLAGS := $(CFLAGS) -g -O3 -Wall -Werror -pedantic -std=gnu99
SOURCES = urcrypt.c urcrypt.h

GEO = ge-additions/ge-additions.o

$(GEO): ge-additions/ge-additions.c ge-additions/ge-additions.h
	$(CC) $(CFLAGS) -c ge-additions/ge-additions.c -o $(GEO)

urcrypt-static.o: $(SOURCES) $(GEO)
	$(CC) $(CFLAGS) -D URCRYPT_STATIC -c urcrypt.c -o urcrypt-static.o

liburcrypt.a: urcrypt-static.o $(GEO)
	$(AR) rcs liburcrypt.a urcrypt-static.o $(GEO)

urcrypt-shared.o: $(SOURCES) $(GEO)
	$(CC) $(CFLAGS) -fPIC -c urcrypt.c -o urcrypt-shared.o

liburcrypt.so: urcrypt-shared.o
	$(CC) -shared urcrypt-shared.o $(GEO) -o liburcrypt.so \
		-led25519 -lssl -largon2 -laes_siv -lgmp -lsecp256k1 \
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
