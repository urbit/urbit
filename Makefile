# A simple makefile.
#

default: all
-include .make.conf

CORE=.MAKEFILE-VERSION

# Pick one of:
#   linux
#   osx

UNAME=$(shell uname)
ifeq ($(UNAME),Darwin)
  OS=osx
else ifeq ($(UNAME),Linux)
  OS=linux
else ifeq ($(UNAME),FreeBSD)
  OS=bsd
else ifeq ($(UNAME),OpenBSD)
  OS=bsd
else
  $(error unknown unix)
endif

# Pick one of:
#   little
#   big
#
ENDIAN=little

# Binary directory - not in quotes.
#
BIN=bin

# Only include/link with this if it exists.
# (Mac OS X El Capitan clean install does not have /opt)
ifneq (,$(wildcard /opt/local/.))
  OPTLOCALINC?=/opt/local/include
  OPTLOCALLIB?=/opt/local/lib
endif

# Only include/link with this if it exists.
# (`brew install openssl` on Mac OS X El Capitan puts openssl here)
ifneq (,$(wildcard /usr/local/opt/openssl/.))
  OPENSSLINC?=/usr/local/opt/openssl/include
  OPENSSLLIB?=/usr/local/opt/openssl/lib
endif

# can't have empty -I or -L options due to whitespace sensitivity
ifdef OPTLOCALINC
  OPTLOCALIFLAGS=-I$(OPTLOCALINC)
endif
ifdef OPTLOCALLIB
  OPTLOCALLFLAGS=-L$(OPTLOCALLIB)
endif
ifdef OPENSSLINC
  OPENSSLIFLAGS=-I$(OPENSSLINC)
endif
ifdef OPENSSLLIB
  OPENSSLLFLAGS=-L$(OPENSSLLIB)
endif

CURLINC=$(shell curl-config --cflags)
CURLLIB=$(shell curl-config --libs)

RM=rm -f
CC=cc
LN=ln -f
CXX=c++
CXXFLAGS=$(CFLAGS)
CLD=c++ $(CFLAGS) -L/usr/local/lib $(OPTLOCALLFLAGS) $(OPENSSLLFLAGS)

ifeq ($(OS),osx)
  CLDOSFLAGS=-bind_at_load
  OSLIBS=-framework CoreServices -framework CoreFoundation
endif
ifeq ($(OS),linux)
  OSLIBS=-lpthread -lrt -lcurses -lz
  DEFINES=-D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE
endif
ifeq ($(OS),bsd)
  OSLIBS=-lpthread -lncurses -lkvm
endif

ifeq ($(STATIC),yes)
LIBS=-lssl -lcrypto -luv -lncurses /usr/local/lib/libsigsegv.a /usr/local/lib/libgmp.a $(CURLLIB) $(OSLIBS)
else
LIBS=-lssl -lcrypto -luv -lgmp -lncurses -lsigsegv $(CURLLIB) $(OSLIBS)
endif

INCLUDE=include
MDEFINES=-DU3_OS_$(OS) -DU3_OS_ENDIAN_$(ENDIAN)

DEBUG=no

ifeq ($(DEBUG),yes)
CFLAGS=-g
else
CFLAGS?=-O3
endif

# NOTFORCHECKIN - restore -O3
# 	-DGHETTO \
#   -DHUSH
CFLAGS+= $(COSFLAGS) -ffast-math \
	-funsigned-char \
	-I/usr/local/include \
	$(OPTLOCALIFLAGS) \
	$(OPENSSLIFLAGS) \
	$(CURLINC) \
	-I$(INCLUDE) \
	-Ioutside/anachronism/include \
	-Ioutside/ed25519/src \
	-Ioutside/commonmark/src \
	-Ioutside/commonmark/build/src \
	-Ioutside/scrypt \
	-Ioutside/softfloat-3/source/include \
	-Ioutside/murmur3 \
	$(DEFINES) \
	$(MDEFINES)

# TODO remove -Wno-*
CWFLAGS=-Wall \
        -Wextra \
        -Wno-sign-compare \
        -Wno-unused-parameter \
        -Wno-missing-field-initializers \
        -Wno-strict-aliasing \
        -Wno-error
ifneq ($(OS),bsd)
  CWFLAGS+=-Wno-error=unused-result
endif

# glibc 2.24 deprecates readdir_r; iff glibc >=2.24,
# don't upgrade 'deprecated declarations' warnings to errors
# dependency: `getconf`, which comes w/glibc
GLIBC := $(lastword $(shell getconf GNU_LIBC_VERSION 2>/dev/null))
# dependency: none, uses make's native functions
GLIBC_MAJ := $(word 1, $(subst ., ,$(GLIBC))) 
GLIBC_MIN := $(word 2, $(subst ., ,$(GLIBC)))
# dependency: `expr` shell built-in
GLIBC_GE_2_24 := $(shell expr $(GLIBC_MAJ) ">" 2 "|" \
        $(GLIBC_MAJ) "=" 2 "&" $(GLIBC_MIN) ">=" 24 2>/dev/null)
ifeq (1,$(GLIBC_GE_2_24))
  CWFLAGS+=-Wno-error=deprecated-declarations
endif

ifdef NO_SILENT_RULES
%.o: %.c $(CORE)
	$(CC) -c $(CWFLAGS) $(CFLAGS) -o $@ $<
	@$(CC) -MM -MP $(CWFLAGS) $(CFLAGS) -MT $@ $< -MF .d/$*.d
else
%.o: %.c $(CORE)
	@echo "    CC    $@"
	@$(CC) -c $(CWFLAGS) $(CFLAGS) -o $@ $<
	@$(CC) -MM -MP $(CWFLAGS) $(CFLAGS) -MT $@ $< -MF .d/$*.d
endif

N_OFILES=\
       noun/allocate.o \
       noun/events.o \
       noun/hashtable.o \
       noun/imprison.o \
       noun/jets.o \
       noun/manage.o \
       noun/nock.o \
       noun/retrieve.o \
       noun/trace.o \
       noun/xtract.o \
       noun/vortex.o \
       noun/zave.o

J_A_OFILES=\
       jets/a/add.o \
       jets/a/dec.o \
       jets/a/div.o \
       jets/a/gte.o \
       jets/a/gth.o \
       jets/a/lte.o \
       jets/a/lth.o \
       jets/a/mod.o \
       jets/a/mul.o \
       jets/a/sub.o

J_B_OFILES=\
       jets/b/bind.o \
       jets/b/clap.o \
       jets/b/drop.o \
       jets/b/flop.o \
       jets/b/lent.o \
       jets/b/levy.o \
       jets/b/lien.o \
       jets/b/murn.o \
       jets/b/need.o \
       jets/b/reap.o \
       jets/b/reel.o \
       jets/b/roll.o \
       jets/b/skid.o \
       jets/b/skim.o \
       jets/b/skip.o \
       jets/b/scag.o \
       jets/b/slag.o \
       jets/b/snag.o \
       jets/b/sort.o \
       jets/b/turn.o \
       jets/b/weld.o

J_C_OFILES=\
       jets/c/bex.o \
       jets/c/xeb.o \
       jets/c/can.o \
       jets/c/cap.o \
       jets/c/cat.o \
       jets/c/con.o \
       jets/c/cut.o \
       jets/c/dor.o \
       jets/c/dvr.o \
       jets/c/dis.o \
       jets/c/end.o \
       jets/c/gor.o \
       jets/c/hor.o \
       jets/c/lsh.o \
       jets/c/mas.o \
       jets/c/met.o \
       jets/c/mix.o \
       jets/c/mug.o \
       jets/c/muk.o \
       jets/c/peg.o \
       jets/c/po.o  \
       jets/c/pow.o \
       jets/c/rap.o \
       jets/c/rep.o \
       jets/c/rip.o \
       jets/c/rsh.o \
       jets/c/sqt.o \
       jets/c/vor.o

J_D_OFILES=\
       jets/d/in_has.o \
       jets/d/in_int.o \
       jets/d/in_gas.o \
       jets/d/in_mer.o \
       jets/d/in_put.o \
       jets/d/in_tap.o \
       jets/d/in_uni.o \
       jets/d/in_wyt.o \
       jets/d/in_bif.o \
       jets/d/in_dif.o \
       jets/d/by_gas.o \
       jets/d/by_get.o \
       jets/d/by_has.o \
       jets/d/by_int.o \
       jets/d/by_put.o \
       jets/d/by_uni.o \
       jets/d/by_bif.o \
       jets/d/by_dif.o

J_E_OFILES=\
       jets/e/aes_ecb.o \
       jets/e/aes_cbc.o \
       jets/e/aesc.o \
       jets/e/cue.o \
       jets/e/fl.o \
       jets/e/jam.o \
       jets/e/mat.o \
       jets/e/mink.o \
       jets/e/mule.o \
       jets/e/parse.o \
       jets/e/rd.o \
       jets/e/rq.o \
       jets/e/rs.o \
       jets/e/rh.o \
       jets/e/rub.o \
       jets/e/scr.o \
       jets/e/shax.o \
       jets/e/lore.o \
       jets/e/loss.o \
       jets/e/lune.o \
       jets/e/trip.o

J_E_OFILES_ED=\
       jets/e/ed_puck.o \
       jets/e/ed_sign.o \
       jets/e/ed_veri.o \
       jets/e/ed_shar.o

J_F_OFILES=\
       jets/f/al.o \
       jets/f/ap.o \
       jets/f/cell.o \
       jets/f/comb.o \
       jets/f/cons.o \
       jets/f/core.o \
       jets/f/face.o \
       jets/f/fitz.o \
       jets/f/flan.o \
       jets/f/flip.o \
       jets/f/flor.o \
       jets/f/fork.o \
       jets/f/help.o \
       jets/f/hike.o \
       jets/f/look.o \
       jets/f/loot.o

J_F_OFILES_UT=\
       jets/f/ut.o \
       jets/f/ut_burn.o \
       jets/f/ut_buss.o \
       jets/f/ut_conk.o \
       jets/f/ut_crop.o \
       jets/f/ut_find.o \
       jets/f/ut_fire.o \
       jets/f/ut_fish.o \
       jets/f/ut_fuse.o \
       jets/f/ut_gain.o \
       jets/f/ut_lose.o \
       jets/f/ut_mint.o \
       jets/f/ut_mull.o \
       jets/f/ut_nest.o \
       jets/f/ut_peek.o \
       jets/f/ut_peel.o \
       jets/f/ut_play.o \
       jets/f/ut_repo.o \
       jets/f/ut_rest.o \
       jets/f/ut_tack.o \
       jets/f/ut_toss.o \
       jets/f/ut_wrap.o

J_Z_OFILES=\
       jets/z/al.o \
       jets/z/ap.o \
       jets/z/cell.o \
       jets/z/comb.o \
       jets/z/cons.o \
       jets/z/core.o \
       jets/z/face.o \
       jets/z/fitz.o \
       jets/z/flan.o \
       jets/z/flip.o \
       jets/z/flor.o \
       jets/z/fork.o \
       jets/z/help.o \
       jets/z/hike.o \
       jets/z/look.o \
       jets/z/loot.o

J_Z_OFILES_UT=\
       jets/z/ut.o \
       jets/z/ut_burn.o \
       jets/z/ut_buss.o \
       jets/z/ut_conk.o \
       jets/z/ut_crop.o \
       jets/z/ut_find.o \
       jets/z/ut_fire.o \
       jets/z/ut_fish.o \
       jets/z/ut_fuse.o \
       jets/z/ut_gain.o \
       jets/z/ut_lose.o \
       jets/z/ut_mint.o \
       jets/z/ut_mull.o \
       jets/z/ut_nest.o \
       jets/z/ut_peek.o \
       jets/z/ut_peel.o \
       jets/z/ut_play.o \
       jets/z/ut_repo.o \
       jets/z/ut_rest.o \
       jets/z/ut_tack.o \
       jets/z/ut_toss.o \
       jets/z/ut_wrap.o

J_G_OFILES=\
       jets/g/down.o

J_OFILES=\
       $(J_A_OFILES) \
       $(J_B_OFILES) \
       $(J_C_OFILES) \
       $(J_D_OFILES) \
       $(J_E_OFILES) \
       $(J_E_OFILES_ED) \
       $(J_F_OFILES) \
       $(J_F_OFILES_UT) \
       $(J_G_OFILES) \
       jets/tree.o

BASE_OFILES=$(N_OFILES) $(J_OFILES)

OUT_OFILES=\
       outside/jhttp/http_parser.o \
       outside/murmur3/MurmurHash3.o

V_OFILES=\
       vere/ames.o \
       vere/behn.o \
       vere/cttp.o \
       vere/http.o \
       vere/newt.o \
       vere/reck.o \
       vere/term.o \
       vere/time.o \
       vere/unix.o \
       vere/save.o \
       vere/serf.o \
       vere/king.o \
       vere/pier.o \
       vere/foil.o \
       vere/walk.o \
       vere/ivory.o

MAIN_FILE =\
       vere/main.o

VERE_OFILES=\
       $(OUT_OFILES) \
       $(BASE_OFILES) \
       $(MAIN_FILE) \
       $(V_OFILES)

VERE_DFILES=$(VERE_OFILES:%.o=.d/%.d)

-include $(VERE_DFILES)

LIBED25519=outside/ed25519/ed25519.a

LIBANACHRONISM=outside/anachronism/build/libanachronism.a

LIBCOMMONMARK=outside/commonmark/build/src/libcmark.a

LIBSCRYPT=outside/scrypt/scrypt.a

LIBSOFTFLOAT=outside/softfloat-3/build/Linux-386-GCC/softfloat.a

TAGS=\
       .tags \
       .etags \
       GPATH GTAGS GRTAGS \
       cscope.in.out cscope.po.out cscope.out

all: urbit links

.MAKEFILE-VERSION: Makefile .make.conf
	@echo "Makefile update."
	@touch .MAKEFILE-VERSION

.make.conf:
	@echo "# Set custom configuration here, please!" > ".make.conf"

links: urbit
	$(LN) $(BIN)/urbit $(BIN)/urbit-worker

urbit: $(BIN)/urbit
booter: $(BIN)/booter

$(LIBED25519):
	$(MAKE) -C outside/ed25519

$(LIBANACHRONISM):
	$(MAKE) -C outside/anachronism static

$(LIBCOMMONMARK):
	$(MAKE) -C outside/commonmark

$(LIBSCRYPT):
	$(MAKE) -C outside/scrypt MDEFINES="$(MDEFINES)"

$(LIBSOFTFLOAT):
	$(MAKE) -C outside/softfloat-3/build/Linux-386-GCC

$(V_OFILES): include/vere/vere.h

ifdef NO_SILENT_RULES
$(BIN)/urbit: $(LIBCOMMONMARK) $(VERE_OFILES) $(LIBED25519) $(LIBANACHRONISM) $(LIBSCRYPT) $(LIBSOFTFLOAT)
	mkdir -p $(BIN)
	$(CLD) $(CLDOSFLAGS) -o $(BIN)/urbit $(VERE_OFILES) $(LIBED25519) $(LIBANACHRONISM) $(LIBS) $(LIBCOMMONMARK) $(LIBSCRYPT) $(LIBSOFTFLOAT)
else
$(BIN)/urbit: $(LIBCOMMONMARK) $(VERE_OFILES) $(LIBED25519) $(LIBANACHRONISM) $(LIBSCRYPT) $(LIBSOFTFLOAT)
	@echo "    CCLD  $(BIN)/urbit"
	@mkdir -p $(BIN)
	@$(CLD) $(CLDOSFLAGS) -o $(BIN)/urbit $(VERE_OFILES) $(LIBED25519) $(LIBANACHRONISM) $(LIBS) $(LIBCOMMONMARK) $(LIBSCRYPT) $(LIBSOFTFLOAT)
endif

tags: ctags etags gtags cscope

ctags:
	@ctags -R -f .tags --exclude=root || true

etags:
	@etags -f .etags $$(find . -name '*.c' -or -name '*.h') || true

gtags:
	@gtags || true

cscope:
	@cscope -b -q -R || true

osxpackage:
	$(RM) -r inst
	$(MAKE) distclean
	$(MAKE) $(BIN)/urbit LIB=/usr/local/lib/urb STATIC=yes
	mkdir -p inst/usr/local/lib/urb inst/usr/local/bin
	cp $(BIN)/urbit inst/usr/local/bin
	cp urb/urbit.pill inst/usr/local/lib/urb
	pkgbuild --root inst --identifier org.urbit.urbit --version 0.2 urbit.pkg

debbuild:
	$(MAKE) $(BIN)/urbit LIB=/usr/share/urb

debinstall:
	mkdir -p $(DESTDIR)/usr/bin $(DESTDIR)/usr/share/urb
	install -m755 $(BIN)/urbit $(DESTDIR)/usr/bin
	cp urb/urbit.pill $(DESTDIR)/usr/share/urb

clean:
	$(RM) $(VERE_OFILES) $(BIN)/urbit urbit.pkg $(VERE_DFILES) $(TAGS)
	$(RM) -r debian/files debian/urbit*

# 'make distclean all -jn' ∀ n>1 still does not work because it is possible
# Make will attempt to build urbit while it is also cleaning urbit..
distclean: clean
	$(MAKE) -C outside/ed25519 clean
	$(MAKE) -C outside/anachronism clean
	$(MAKE) -C outside/scrypt clean
	$(MAKE) -C outside/softfloat-3/build/Linux-386-GCC clean

.PHONY: clean debbuild debinstalldistclean etags osxpackage tags
