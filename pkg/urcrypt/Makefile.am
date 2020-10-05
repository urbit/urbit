ACLOCAL_AMFLAGS = -I build-aux/m4

AM_CFLAGS = -Wall

lib_LTLIBRARIES = liburcrypt.la
noinst_LTLIBRARIES = libed25519.la \
										 libge_additions.la \
										 libargon2.la

include_HEADERS = urcrypt.h
noinst_HEADERS = ed25519/src/ed25519.h \
								 ed25519/src/ge.h \
								 ge-additions/ge-additions.h \
								 argon2/include/argon2.h
								 argon2/src/blake.h

liburcrypt_la_CPPFLAGS = -Ied25519/src \
												 -Ige-additions \
												 -Iargon2/include \
												 -Iargon2/src/blake2
liburcrypt_la_LIBADD = $(SECP256K1_LIBS) \
											 libed25519.la \
											 libge_additions.la \
											 libargon2.la
liburcrypt_la_CFLAGS = $(SECP256K1_CFLAGS)
liburcrypt_la_SOURCES = urcrypt.c

libed25519_la_SOURCES = ed25519/src/fixedint.h \
												ed25519/src/sha512.h \
												ed25519/src/fe.h \
												ed25519/src/precomp_data.h \
												ed25519/src/sc.h \
												ed25519/src/add_scalar.c \
												ed25519/src/keypair.c \
												ed25519/src/sc.c \
												ed25519/src/seed.c \
												ed25519/src/verify.c \
												ed25519/src/ge.c \
												ed25519/src/key_exchange.c \
												ed25519/src/sha512.c \
												ed25519/src/sign.c

libge_additions_la_CPPFLAGS = -Ied25519/src
libge_additions_la_CFLAGS = -Werror -pedantic -std=gnu99
libge_additions_la_SOURCES = ge-additions/ge-additions.c

libargon2_la_CPPFLAGS = -Iargon2/include -DARGON2_NO_THREADS 
libargon2_la_CFLAGS = -Wno-unused-value -Wno-unused-function
libargon2_la_SOURCES = argon2/src/core.h \
											 argon2/src/blake2/blake2.h \
											 argon2/src/thread.h \
											 argon2/src/encoding.h \
											 argon2/src/blake2/blake2-impl.h \
											 argon2/src/blake2/blamka-round-opt.h \
											 argon2/src/blake2/blamka-round-ref.h \
											 argon2/src/argon2.c \
											 argon2/src/core.c \
											 argon2/src/blake2/blake2b.c \
											 argon2/src/thread.c \
											 argon2/src/encoding.c
