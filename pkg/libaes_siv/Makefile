CC     ?= cc
AR     ?= ar
PREFIX ?= ./out

################################################################################

.PHONY: all test install clean

all: aes_siv.c aes_siv.h
	$(CC) $(CFLAGS) -Wall -Wextra -Wstrict-prototypes -Wconversion -O3 -fomit-frame-pointer -funroll-loops -ftree-vectorize -DNDEBUG -c aes_siv.c
	$(AR) rcs libaes_siv.a aes_siv.o

install: all
	@mkdir -p $(PREFIX)/lib/
	@mkdir -p $(PREFIX)/include/
	cp libaes_siv.a $(PREFIX)/lib/
	cp aes_siv.h $(PREFIX)/include/

clean:
	rm -rf ./out
