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

RM=rm -f
ifneq ($(UNAME),FreeBSD)
CC=gcc
CXX=g++
CXXFLAGS=$(CFLAGS)
CLD=g++ -O3 -L/usr/local/lib -L/opt/local/lib
else
CC=cc
CXX=c++
CXXFLAGS=$(CFLAGS)
CLD=c++ -O3 -L/usr/local/lib -L/opt/local/lib
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

INCLUDE=i
MDEFINES=-DU3_OS_$(OS) -DU3_OS_ENDIAN_$(ENDIAN) -D U3_LIB=\"$(LIB)\"

# NOTFORCHECKIN - restore -O3
# 	-DGHETTO
CFLAGS= $(COSFLAGS) -O3 -msse3 -ffast-math \
	-funsigned-char \
	-I/usr/local/include \
	-I/opt/local/include \
	-I$(INCLUDE) \
	-Ioutside/libuv_0.11/include \
	-Ioutside/anachronism/include \
	-Ioutside/bpt \
	-Ioutside/re2 \
	-Ioutside/cre2/src/src \
	-Ioutside/ed25519/src \
	-Ioutside/commonmark/src \
	-Ioutside/commonmark/build/src \
	-Ioutside/scrypt \
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
       n/a.o \
       n/e.o \
       n/h.o \
       n/i.o \
       n/j.o \
       n/m.o \
       n/n.o \
       n/r.o \
       n/t.o \
       n/x.o \
       n/v.o \
       n/z.o

J_A_OFILES=\
       j/a/add.o \
       j/a/dec.o \
       j/a/div.o \
       j/a/gte.o \
       j/a/gth.o \
       j/a/lte.o \
       j/a/lth.o \
       j/a/mod.o \
       j/a/mul.o \
       j/a/sub.o

J_B_OFILES=\
       j/b/bind.o \
       j/b/clap.o \
       j/b/drop.o \
       j/b/flop.o \
       j/b/lent.o \
       j/b/levy.o \
       j/b/lien.o \
       j/b/need.o \
       j/b/reel.o \
       j/b/roll.o \
       j/b/skim.o \
       j/b/skip.o \
       j/b/scag.o \
       j/b/slag.o \
       j/b/snag.o \
       j/b/sort.o \
       j/b/turn.o \
       j/b/weld.o

J_C_OFILES=\
       j/c/bex.o \
       j/c/can.o \
       j/c/cap.o \
       j/c/cat.o \
       j/c/con.o \
       j/c/cut.o \
       j/c/dor.o \
       j/c/dis.o \
       j/c/end.o \
       j/c/gor.o \
       j/c/hor.o \
       j/c/lsh.o \
       j/c/mas.o \
       j/c/met.o \
       j/c/mix.o \
       j/c/mug.o \
       j/c/peg.o \
       j/c/po.o  \
       j/c/rap.o \
       j/c/rip.o \
       j/c/rsh.o \
       j/c/vor.o

J_D_OFILES=\
       j/d/in_has.o \
       j/d/in_int.o \
       j/d/in_gas.o \
       j/d/in_mer.o \
       j/d/in_put.o \
       j/d/in_tap.o \
       j/d/in_uni.o \
       j/d/by_gas.o \
       j/d/by_get.o \
       j/d/by_has.o \
       j/d/by_int.o \
       j/d/by_put.o \
       j/d/by_uni.o

J_E_OFILES=\
       j/e/aesc.o \
       j/e/cue.o \
       j/e/jam.o \
       j/e/mat.o \
       j/e/mink.o \
       j/e/mule.o \
       j/e/parse.o \
       j/e/rd.o \
       j/e/repg.o \
       j/e/rexp.o \
       j/e/rub.o \
       j/e/scr.o \
       j/e/shax.o \
       j/e/lore.o \
       j/e/loss.o \
       j/e/trip.o

J_E_OFILES_ED=\
       j/e/ed_puck.o \
       j/e/ed_sign.o \
       j/e/ed_veri.o

J_F_OFILES=\
       j/f/al.o \
       j/f/ap.o \
       j/f/bull.o \
       j/f/cell.o \
       j/f/comb.o \
       j/f/cons.o \
       j/f/core.o \
       j/f/cube.o \
       j/f/face.o \
       j/f/fitz.o \
       j/f/flan.o \
       j/f/flay.o \
       j/f/flip.o \
       j/f/flor.o \
       j/f/fork.o \
       j/f/hike.o \
       j/f/look.o \

J_F_OFILES_UT=\
       j/f/ut.o \
       j/f/ut_burn.o \
       j/f/ut_busk.o \
       j/f/ut_bust.o \
       j/f/ut_conk.o \
       j/f/ut_crop.o \
       j/f/ut_cull.o \
       j/f/ut_find.o \
       j/f/ut_fink.o \
       j/f/ut_fire.o \
       j/f/ut_firm.o \
       j/f/ut_fish.o \
       j/f/ut_fuse.o \
       j/f/ut_gain.o \
       j/f/ut_heal.o \
       j/f/ut_lose.o \
       j/f/ut_mint.o \
       j/f/ut_mull.o \
       j/f/ut_nest.o \
       j/f/ut_park.o \
       j/f/ut_peek.o \
       j/f/ut_play.o \
       j/f/ut_repo.o \
       j/f/ut_rest.o \
       j/f/ut_seek.o \
       j/f/ut_swab.o \
       j/f/ut_tack.o \
       j/f/ut_tock.o \
       j/f/ut_wrap.o

J_G_OFILES=\
       j/g/down.o

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
       j/tree.o

BASE_OFILES=$(N_OFILES) $(J_OFILES)

CRE2_OFILES=\
       outside/cre2/src/src/cre2.o

OUT_OFILES=\
       outside/jhttp/http_parser.o

V_OFILES=\
       v/ames.o \
       v/cttp.o \
       v/http.o \
       v/loop.o \
       v/raft.o \
       v/reck.o \
       v/sist.o \
       v/temp.o \
       v/term.o \
       v/time.o \
       v/unix.o \
       v/save.o \
       v/walk.o

MAIN_FILE =\
       v/main.o 

MEME_FILE =\
       w/test.o

VERE_OFILES=\
       $(CRE2_OFILES) \
       $(OUT_OFILES) \
       $(BASE_OFILES) \
       $(MAIN_FILE) \
       $(V_OFILES)

MEME_OFILES=\
       $(CRE2_OFILES) \
       $(OUT_OFILES) \
       $(BASE_OFILES) \
       $(MEME_FILE)

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
LIBUV_MAKEFILE=outside/libuv_0.11/Makefile
LIBUV_MAKEFILE2=outside/libuv_0.11/config.log

LIBUV=outside/libuv_0.11/.libs/libuv.a

LIBRE2=outside/re2/obj/libre2.a

LIBED25519=outside/ed25519/ed25519.a

LIBANACHRONISM=outside/anachronism/build/libanachronism.a

LIBCOMMONMARK=outside/commonmark/build/src/libcmark.a

LIBSCRYPT=outside/scrypt/scrypt.a

all: urbit

.MAKEFILE-VERSION: Makefile .make.conf
	@echo "Makefile update."
	@touch .MAKEFILE-VERSION

.make.conf:
	@echo "# Set custom configuration here, please!" > ".make.conf"

urbit: $(BIN)/urbit
meme: $(BIN)/meme

$(LIBUV_MAKEFILE) $(LIBUV_MAKEFILE2):
	cd outside/libuv_0.11 ; sh autogen.sh ; ./configure  --disable-dtrace

$(LIBUV): $(LIBUV_MAKEFILE) $(LIBUV_MAKEFILE2)
	$(MAKE) -C outside/libuv_0.11 all-am

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

$(CRE2_OFILES): outside/cre2/src/src/cre2.cpp outside/cre2/src/src/cre2.h $(LIBRE2)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(V_OFILES): i/v/vere.h

ifdef NO_SILENT_RULES
$(BIN)/urbit: $(LIBCRE) $(LIBCOMMONMARK) $(VERE_OFILES) $(LIBUV) $(LIBRE2) $(LIBED25519) $(LIBANACHRONISM) $(LIBSCRYPT)
	mkdir -p $(BIN)
	$(CLD) $(CLDOSFLAGS) -o $(BIN)/urbit $(VERE_OFILES) $(LIBUV) $(LIBCRE) $(LIBRE2) $(LIBED25519) $(LIBANACHRONISM) $(LIBS) $(LIBCOMMONMARK) $(LIBSCRYPT)
else
$(BIN)/urbit: $(LIBCRE) $(LIBCOMMONMARK) $(VERE_OFILES) $(LIBUV) $(LIBRE2) $(LIBED25519) $(LIBANACHRONISM) $(LIBSCRYPT)
	@echo "    CCLD  $(BIN)/urbit"
	@mkdir -p $(BIN)
	@$(CLD) $(CLDOSFLAGS) -o $(BIN)/urbit $(VERE_OFILES) $(LIBUV) $(LIBCRE) $(LIBRE2) $(LIBED25519) $(LIBANACHRONISM) $(LIBS) $(LIBCOMMONMARK) $(LIBSCRYPT)
endif

$(BIN)/meme: $(LIBCRE) $(LIBCOMMONMARK) $(MEME_OFILES) $(LIBUV) $(LIBRE2) $(LIBED25519) $(LIBANACHRONISM) $(LIBSCRYPT)
	mkdir -p $(BIN)
	$(CLD) $(CLDOSFLAGS) -o $(BIN)/meme $(MEME_OFILES) $(LIBUV) $(LIBCRE) $(LIBRE2) $(LIBED25519) $(LIBANACHRONISM) $(LIBS) $(LIBCOMMONMARK) $(LIBSCRYPT)

tags:
	ctags -R -f .tags --exclude=root

etags:
	etags -f .etags $$(find -name '*.c' -or -name '*.h')

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
	$(RM) $(VERE_OFILES) $(BIN)/urbit urbit.pkg $(VERE_DFILES)

distclean: clean $(LIBUV_MAKEFILE)
	$(MAKE) -C outside/libuv_0.11 distclean
	$(MAKE) -C outside/re2 clean
	$(MAKE) -C outside/ed25519 clean
	$(MAKE) -C outside/anachronism clean
	$(MAKE) -C outside/scrypt clean

.PHONY: clean debbuild debinstalldistclean etags osxpackage tags
