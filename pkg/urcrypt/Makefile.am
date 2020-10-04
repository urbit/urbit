ACLOCAL_AMFLAGS = -I build-aux/m4

lib_LTLIBRARIES = liburcrypt.la
include_HEADERS = urcrypt.h
noinst_HEADERS = $(ed25519_includes) \
								 $(ge_additions_includes) \
								 $(argon2_includes)
liburcrypt_la_SOURCES = urcrypt.c \
												$(ed25519_sources) \
												$(ge_additions_sources) \
												$(argon2_sources)
liburcrypt_la_CFLAGS = $(SECP256K1_CFLAGS)
liburcrypt_la_CPPFLAGS = -Ied25519/src -Ige-additions -Iargon2/include
liburcrypt_la_LIBADD = -lgmp -laes_siv $(SECP256K1_LIBS)

### ed25519
ed25519_sources = ed25519/src/add_scalar.c ed25519/src/keypair.c \
                  ed25519/src/sc.c ed25519/src/seed.c \
                  ed25519/src/verify.c ed25519/src/ge.c \
                  ed25519/src/key_exchange.c ed25519/src/sha512.c \
                  ed25519/src/sign.c
ed25519_includes = ed25519/src/fixedint.h ed25519/src/ge.h \
                   ed25519/src/sha512.h ed25519/src/ed25519.h \
                   ed25519/src/fe.h ed25519/src/precomp_data.h \
                   ed25519/src/sc.h

### ge-additions
ge_additions_sources = ge-additions/ge-additions.c
ge_additions_includes = ge-additions/ge-additions.h

### argon2
argon2_sources = argon2/src/argon2.c argon2/src/core.c \
								 argon2/src/blake2/blake2b.c argon2/src/thread.c \
								 argon2/src/encoding.c
argon2_includes = argon2/include/argon2.h argon2/src/core.h \
									argon2/src/blake2/blake2.h argon2/src/thread.h \
									argon2/src/encoding.h \
                  argon2/src/blake2/blake2-impl.h \
									argon2/src/blake2/blamka-round-opt.h \
									argon2/src/blake2/blamka-round-ref.h
									
