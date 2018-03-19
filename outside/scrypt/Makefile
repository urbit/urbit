default: all

CC?=gcc
CFLAGS?=-O2 -ffast-math \
	-Wall -g -D_FORTIFY_SOURCE=2 -fPIC
CFLAGS_EXTRA?=-Wl,-rpath=.

all: scrypt.a

OBJS= crypto_scrypt-nosse.o sha256.o

scrypt.a: $(OBJS)
	ar rcs scrypt.a  $(OBJS)

clean:
	rm -f *.o scrypt.a
