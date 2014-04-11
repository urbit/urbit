# A simple makefile.
#
CFLAGS= -O2 -g -Wall

.c.o:
	 $(CC) -c $(CFLAGS) -o $@ $<

OFILES=\
	src/add_scalar.o \
	src/fe.o \
	src/ge.o \
	src/key_exchange.o \
	src/keypair.o \
	src/sc.o \
	src/seed.o \
	src/sha512.o \
	src/sign.o \
	src/verify.o

all: ed25519.a($(OFILES))

clean:
	-rm $(OFILES) ed25519.a
