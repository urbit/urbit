ACLOCAL_AMFLAGS = -I build-aux/m4

AM_CFLAGS = -Wall -g -O3

lib_LTLIBRARIES = liburcrypt.la
noinst_LTLIBRARIES = libed25519.la \
		     libge_additions.la \
		     libargon2.la \
		     libscrypt.la

include_HEADERS = urcrypt/urcrypt.h
noinst_HEADERS = urcrypt/util.h \
		 ed25519/src/ed25519.h \
		 ed25519/src/ge.h \
		 ge-additions/ge-additions.h \
		 argon2/include/argon2.h \
		 argon2/src/blake2/blake2.h \
		 scrypt/sha256.h \
		 scrypt/libscrypt.h

# main library
pkgconfig_DATA = liburcrypt.pc
DISTCLEANFILES = $(pkgconfig_DATA)

liburcrypt_la_CPPFLAGS = -I$(srcdir)/ed25519/src \
			 -I$(srcdir)/ge-additions \
			 -I$(srcdir)/argon2/include \
			 -I$(srcdir)/argon2/src/blake2 \
			 -I$(srcdir)/scrypt
liburcrypt_la_LIBADD = $(LIBCRYPTO_LIBS) \
		       $(LIBSECP256K1_LIBS) \
		       $(LIBAES_SIV_LIBS) \
		       libed25519.la \
		       libge_additions.la \
		       libargon2.la \
		       libscrypt.la
liburcrypt_la_CFLAGS = $(LIBCRYPTO_CFLAGS) \
		       $(LIBSECP256K1_CFLAGS) \
		       $(LIBAES_SIV_CFLAGS)
# urcrypt_ is used for public symbols, urcrypt__ for internal.
liburcrypt_la_LDFLAGS = -export-symbols-regex '^urcrypt_[^_]' \
												-version-info $(URCRYPT_LT_VERSION)
liburcrypt_la_SOURCES = urcrypt/aes_cbc.c \
			urcrypt/aes_ecb.c \
			urcrypt/aes_siv.c \
			urcrypt/argon.c \
			urcrypt/ed25519.c \
			urcrypt/ge_additions.c \
			urcrypt/ripemd.c \
			urcrypt/scrypt.c \
			urcrypt/secp256k1.c \
			urcrypt/sha.c \
			urcrypt/util.c \
			urcrypt/util.h

# ed25519
libed25519_la_CFLAGS = -Wno-unused-result
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
			ed25519/src/fe.c \
			ed25519/src/key_exchange.c \
			ed25519/src/sha512.c \
			ed25519/src/sign.c

# ge-additions
libge_additions_la_CPPFLAGS = -I$(srcdir)/ed25519/src
libge_additions_la_CFLAGS = -Werror -pedantic -std=gnu99
libge_additions_la_SOURCES = ge-additions/ge-additions.c

# argon2
libargon2_la_CPPFLAGS = -I$(srcdir)/argon2/include -DARGON2_NO_THREADS
libargon2_la_CFLAGS = -Wno-unused-value -Wno-unused-function
libargon2_la_SOURCES = argon2/src/core.h \
		       argon2/src/thread.h \
		       argon2/src/encoding.h \
		       argon2/src/blake2/blake2-impl.h \
		       argon2/src/blake2/blamka-round-opt.h \
		       argon2/src/blake2/blamka-round-ref.h \
		       argon2/src/argon2.c \
		       argon2/src/core.c \
		       argon2/src/blake2/blake2b.c \
		       argon2/src/thread.c \
		       argon2/src/encoding.c \
		       argon2/src/opt.c

# scrypt
libscrypt_la_CPPFLAGS = -D_FORTIFY_SOURCE=2
libscrypt_la_SOURCES = scrypt/b64.c \
		       scrypt/crypto-mcf.c \
		       scrypt/crypto-scrypt-saltgen.c \
		       scrypt/crypto_scrypt-check.c \
		       scrypt/crypto_scrypt-hash.c \
		       scrypt/crypto_scrypt-hexconvert.c \
		       scrypt/crypto_scrypt-nosse.c \
		       scrypt/main.c \
		       scrypt/sha256.c \
		       scrypt/slowequals.c \
		       scrypt/b64.h \
		       scrypt/crypto_scrypt-hexconvert.h \
		       scrypt/slowequals.h \
		       scrypt/sysendian.h
