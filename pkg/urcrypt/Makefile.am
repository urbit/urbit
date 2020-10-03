ACLOCAL_AMFLAGS = -I build-aux/m4

lib_LTLIBRARIES = liburcrypt.la
include_HEADERS = urcrypt.h
noinst_HEADERS = $(ed25519_includes) $(ge_additions_includes) 
liburcrypt_la_SOURCES = urcrypt.c $(ed25519_sources) $(ge_additions_sources) 
liburcrypt_la_CPPFLAGS = -Ied25519/src -Ige-additions

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
