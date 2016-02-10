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

LIB=$(shell pwd)/urb

# Only include/link with this if it exists.
# (Mac OS X El Capitan clean install does not have /opt)
ifneq (,$(wildcard /opt/local/.))
  OPTLOCALINC=-I/opt/local/include
  OPTLOCALLIB=-L/opt/local/lib
endif

# Only include/link with this if it exists.
# (`brew install openssl` on Mac OS X El Capitan puts openssl here)
ifneq (,$(wildcard /usr/local/opt/openssl/.))
  OPENSSLINC=-I/usr/local/opt/openssl/include
  OPENSSLLIB=-L/usr/local/opt/openssl/lib
endif

RM=rm -f
ifneq ($(UNAME),FreeBSD)
CC=gcc
CXX=g++
CXXFLAGS=$(CFLAGS)
CLD=g++ -O3 -L/usr/local/lib $(OPTLOCALLIB) $(OPENSSLLIB)
else
CC=cc
CXX=c++
CXXFLAGS=$(CFLAGS)
CLD=c++ -O3 -L/usr/local/lib $(OPTLOCALLIB) $(OPENSSLLIB)
endif

ifeq ($(OS),osx)
  COSFLAGS=-fno-diagnostics-fixit-info
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
LIBS=-lssl -lcrypto -lncurses /usr/local/lib/libsigsegv.a /usr/local/lib/libgmp.a $(OSLIBS)
else
LIBS=-lssl -lcrypto -lgmp -lncurses -lsigsegv $(OSLIBS)
endif

INCLUDE=include
MDEFINES=-DU3_OS_$(OS) -DU3_OS_ENDIAN_$(ENDIAN) -D U3_LIB=\"$(LIB)\"

DEBUG=no

ifeq ($(DEBUG),yes)
DEBUGFLAGS=-g
else
DEBUGFLAGS=-O3
endif

# libuv version
LIBUV_VER=libuv_0.11
#LIBUV_VER=libuv-v1.7.5

ifeq ($(LIBUV_VER),libuv_0.11)
LIBUV_CONFIGURE_OPTIONS=--disable-dtrace
else
LIBUV_CONFIGURE_OPTIONS=
endif

# NOTFORCHECKIN - restore -O3
# 	-DGHETTO \
#   -DHUSH
CFLAGS= $(COSFLAGS) $(DEBUGFLAGS) -ffast-math \
	-funsigned-char \
	-I/usr/local/include \
	$(OPTLOCALINC) \
	$(OPENSSLINC) \
	-I$(INCLUDE) \
	-Ioutside/$(LIBUV_VER)/include \
	-Ioutside/anachronism/include \
	-Ioutside/bpt \
	-Ioutside/re2 \
	-Ioutside/cre2/src/src \
	-Ioutside/ed25519/src \
	-Ioutside/commonmark/src \
	-Ioutside/commonmark/build/src \
	-Ioutside/scrypt \
	-Ioutside/softfloat-3/source/include \
	$(DEFINES) \
	$(MDEFINES)

# TODO remove -Wno-*
CWFLAGS=-Wall \
        -Wextra \
        -Wno-sign-compare \
        -Wno-unused-parameter \
        -Wno-missing-field-initializers \
        -Wno-strict-aliasing \
        -Werror
ifneq ($(OS),bsd)
  CWFLAGS+=-Wno-error=unused-result
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
       jets/c/peg.o \
       jets/c/po.o  \
       jets/c/pow.o \
       jets/c/rap.o \
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
       jets/e/repg.o \
       jets/e/rexp.o \
       jets/e/rub.o \
       jets/e/scr.o \
       jets/e/shax.o \
       jets/e/lore.o \
       jets/e/loss.o \
       jets/e/trip.o

J_E_OFILES_ED=\
       jets/e/ed_puck.o \
       jets/e/ed_sign.o \
       jets/e/ed_veri.o

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
       jets/f/hike.o \
       jets/f/look.o \

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
       jets/f/ut_park.o \
       jets/f/ut_peek.o \
       jets/f/ut_play.o \
       jets/f/ut_repo.o \
       jets/f/ut_rest.o \
       jets/f/ut_tack.o \
       jets/f/ut_toss.o \
       jets/f/ut_wrap.o

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

CRE2_OFILES=\
       outside/cre2/src/src/cre2.o

OUT_OFILES=\
       outside/jhttp/http_parser.o

V_OFILES=\
       vere/ames.o \
       vere/behn.o \
       vere/cttp.o \
       vere/http.o \
       vere/loop.o \
       vere/raft.o \
       vere/reck.o \
       vere/sist.o \
       vere/term.o \
       vere/time.o \
       vere/unix.o \
       vere/save.o \
       vere/walk.o

MAIN_FILE =\
       vere/main.o

VERE_OFILES=\
       $(CRE2_OFILES) \
       $(OUT_OFILES) \
       $(BASE_OFILES) \
       $(MAIN_FILE) \
       $(V_OFILES)

VERE_DFILES=$(VERE_OFILES:%.o=.d/%.d)

-include $(VERE_DFILES)

# This is a silly hack necessitated by the fact that libuv uses configure
#
#    * Making 'all' obviously requires outside/libuv,
#      which requires the libuv Makefile to be created.
#    * Making distclean on outside/libuv destroys the makefile.
#    * ...so configuring outside/libuv is parodoxically required
#      in order to distclean it!
#    * But what if developer types 'make distclean all' ?
#    * first target makes libuv Makefile, then destroys it...and
#      second target knows that it was made.
#    * ...so second target borks.
#    * Solution: make libuv not only depend on its own Makefile,
#      but on a side effect of creating its own makefile.
#
LIBUV_MAKEFILE=outside/$(LIBUV_VER)/Makefile
LIBUV_MAKEFILE2=outside/$(LIBUV_VER)/config.log

LIBUV=outside/$(LIBUV_VER)/.libs/libuv.a

LIBRE2=outside/re2/obj/libre2.a

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

all: urbit

.MAKEFILE-VERSION: Makefile .make.conf
	@echo "Makefile update."
	@touch .MAKEFILE-VERSION

.make.conf:
	@echo "# Set custom configuration here, please!" > ".make.conf"

urbit: $(BIN)/urbit

$(LIBUV_MAKEFILE) $(LIBUV_MAKEFILE2):
	cd outside/$(LIBUV_VER) ; sh autogen.sh ; ./configure $(LIBUV_CONFIGURE_OPTIONS)

# [h]act II: the plot thickens
#
#     * Specifying two targets that each configure libuv works
#       when the rules are executed sequentially,
#     * but when attempting a parallel build, it is likely Make
#       will try to configure libuv simultaneously.
#     * We can specify a dependency between the two targets so
#       that execution of their rule(s) is serialized.
#     * Further, libuv does not seem to be friendly towards
#       parallel builds either. A true fix is out of scope here
#     * ...so we must instruct Make to only use one job when it
#       attempts to build libuv.
#
$(LIBUV_MAKEFILE2): $(LIBUV_MAKEFILE)

$(LIBUV): $(LIBUV_MAKEFILE) $(LIBUV_MAKEFILE2)
	$(MAKE) -C outside/$(LIBUV_VER) all-am -j1

$(LIBRE2):
	$(MAKE) -C outside/re2 obj/libre2.a

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

$(CRE2_OFILES): outside/cre2/src/src/cre2.cpp outside/cre2/src/src/cre2.h $(LIBRE2)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(V_OFILES): include/vere/vere.h

ifdef NO_SILENT_RULES
$(BIN)/urbit: $(LIBCRE) $(LIBCOMMONMARK) $(VERE_OFILES) $(LIBUV) $(LIBRE2) $(LIBED25519) $(LIBANACHRONISM) $(LIBSCRYPT) $(LIBSOFTFLOAT)
	mkdir -p $(BIN)
	$(CLD) $(CLDOSFLAGS) -o $(BIN)/urbit $(VERE_OFILES) $(LIBUV) $(LIBCRE) $(LIBRE2) $(LIBED25519) $(LIBANACHRONISM) $(LIBS) $(LIBCOMMONMARK) $(LIBSCRYPT) $(LIBSOFTFLOAT)
else
$(BIN)/urbit: $(LIBCRE) $(LIBCOMMONMARK) $(VERE_OFILES) $(LIBUV) $(LIBRE2) $(LIBED25519) $(LIBANACHRONISM) $(LIBSCRYPT) $(LIBSOFTFLOAT)
	@echo "    CCLD  $(BIN)/urbit"
	@mkdir -p $(BIN)
	@$(CLD) $(CLDOSFLAGS) -o $(BIN)/urbit $(VERE_OFILES) $(LIBUV) $(LIBCRE) $(LIBRE2) $(LIBED25519) $(LIBANACHRONISM) $(LIBS) $(LIBCOMMONMARK) $(LIBSCRYPT) $(LIBSOFTFLOAT)
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
	cp -R urb/zod inst/usr/local/lib/urb
	pkgbuild --root inst --identifier org.urbit.urbit --version 0.2 urbit.pkg

debbuild:
	$(MAKE) $(BIN)/urbit LIB=/usr/share/urb

debinstall:
	mkdir -p $(DESTDIR)/usr/bin $(DESTDIR)/usr/share/urb
	install -m755 $(BIN)/urbit $(DESTDIR)/usr/bin
	cp urb/urbit.pill $(DESTDIR)/usr/share/urb
	cp -R urb/zod $(DESTDIR)/usr/share/urb

clean:
	$(RM) $(VERE_OFILES) $(BIN)/urbit urbit.pkg $(VERE_DFILES) $(TAGS)
	$(RM) -r debian/files debian/urbit*

# 'make distclean all -jn' ∀ n>1 still does not work because it is possible
# Make will attempt to build urbit while it is also cleaning urbit..
distclean: clean $(LIBUV_MAKEFILE)
	$(MAKE) -C outside/$(LIBUV_VER) distclean
	$(MAKE) -C outside/re2 clean
	$(MAKE) -C outside/ed25519 clean
	$(MAKE) -C outside/anachronism clean
	$(MAKE) -C outside/scrypt clean
	$(MAKE) -C outside/softfloat-3/build/Linux-386-GCC clean

.PHONY: clean debbuild debinstalldistclean etags osxpackage tags
