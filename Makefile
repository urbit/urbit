# A simple makefile.
#

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

LIB=$(PWD)/urb

RM=rm -f
CC=gcc
CXX=g++
CXXFLAGS=$(CFLAGS)
CLD=g++ -L/usr/local/lib -L/opt/local/lib

ifeq ($(OS),osx)
  COSFLAGS=-fno-diagnostics-fixit-info
  CLDOSFLAGS=-bind_at_load
  OSLIBS=-framework CoreServices -framework CoreFoundation
endif
ifeq ($(OS),linux)
  OSLIBS=-lpthread -lrt -lcurses 
  DEFINES=-D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE
endif
ifeq ($(OS),bsd)
  OSLIBS=-lpthread -lncurses -lkvm
endif

CFLAGS=  $(COSFLAGS) \
        -g -msse3 -ffast-math \
	-funsigned-char \
	-I/usr/local/include \
	-I/opt/local/include \
	-I$(INCLUDE) \
	-Ioutside/libuv/include \
	-Ioutside/anachronism/include \
	-Ioutside/bpt \
	-Ioutside/re2 \
	-Ioutside/cre2/src/src \
	-Ioutside/ed25519/src \
	$(DEFINES) \
	$(MDEFINES)

ifeq ($(STATIC),yes)
LIBS=-lssl -lcrypto -lncurses /usr/local/lib/libsigsegv.a /usr/local/lib/libgmp.a $(OSLIBS)
else
LIBS=-lssl -lcrypto -lgmp -lncurses -lsigsegv $(OSLIBS)
endif

INCLUDE=include
MDEFINES=-DU2_OS_$(OS) -DU2_OS_ENDIAN_$(ENDIAN) -D U2_LIB=\"$(LIB)\"

CWFLAGS=-Wall

.c.o:
	 $(CC) -c $(CWFLAGS) $(CFLAGS) -o $@ $<

N_OFILES=\
       n/a.o \
       n/h.o \
       n/i.o \
       n/j.o \
       n/m.o \
       n/n.o \
       n/r.o \
       n/t.o \
       n/x.o \
       n/z.o

F_OFILES=\
       f/rail.o \
       f/loom.o \
       f/wire.o \
       f/chad.o \
       f/cash.o \
       f/nash.o \
       f/coal.o \
       f/hevn.o \
       f/host.o \
       f/benx.o \
       f/trac.o \
       f/bail.o \
       f/dash.o \
       f/unix.o \
       f/nock.o

J_1_OFILES=\
       j/1/add.o \
       j/1/dec.o \
       j/1/div.o \
       j/1/gte.o \
       j/1/gth.o \
       j/1/lte.o \
       j/1/lth.o \
       j/1/mod.o \
       j/1/mul.o \
       j/1/sub.o

J_2_OFILES=\
       j/2/bind.o \
       j/2/clap.o \
       j/2/drop.o \
       j/2/flop.o \
       j/2/lent.o \
       j/2/levy.o \
       j/2/lien.o \
       j/2/need.o \
       j/2/reel.o \
       j/2/roll.o \
       j/2/skim.o \
       j/2/skip.o \
       j/2/scag.o \
       j/2/slag.o \
       j/2/snag.o \
       j/2/sort.o \
       j/2/turn.o \
       j/2/weld.o

J_3_OFILES=\
       j/3/bex.o \
       j/3/can.o \
       j/3/cap.o \
       j/3/cat.o \
       j/3/con.o \
       j/3/cut.o \
       j/3/dor.o \
       j/3/dis.o \
       j/3/end.o \
       j/3/gor.o \
       j/3/hor.o \
       j/3/lsh.o \
       j/3/mas.o \
       j/3/met.o \
       j/3/mix.o \
       j/3/mug.o \
       j/3/peg.o \
       j/3/po.o  \
       j/3/rap.o \
       j/3/rip.o \
       j/3/rsh.o \
       j/3/vor.o

J_4_OFILES=\
       j/4/in_has.o \
       j/4/in_int.o \
       j/4/in_gas.o \
       j/4/in_mer.o \
       j/4/in_put.o \
       j/4/in_tap.o \
       j/4/in_uni.o \
       j/4/by_gas.o \
       j/4/by_get.o \
       j/4/by_has.o \
       j/4/by_int.o \
       j/4/by_put.o \
       j/4/by_uni.o

J_5_OFILES=\
       j/5/aesc.o \
       j/5/cue.o \
       j/5/jam.o \
       j/5/mat.o \
       j/5/mink.o \
       j/5/mule.o \
       j/5/parse.o \
       j/5/rd.o \
       j/5/repg.o \
       j/5/rexp.o \
       j/5/rub.o \
       j/5/shax.o \
       j/5/lore.o \
       j/5/loss.o \
       j/5/trip.o

J_5_OFILES_ED=\
       j/5/ed_puck.o \
       j/5/ed_sign.o \
       j/5/ed_veri.o

J_6_OFILES=\
       j/6/al.o \
       j/6/ap.o \
       j/6/bull.o \
       j/6/cell.o \
       j/6/comb.o \
       j/6/cons.o \
       j/6/core.o \
       j/6/cube.o \
       j/6/face.o \
       j/6/fitz.o \
       j/6/flan.o \
       j/6/flay.o \
       j/6/flip.o \
       j/6/flor.o \
       j/6/fork.o \
       j/6/hike.o \
       j/6/look.o \

J_6_OFILES_UT=\
       j/6/ut.o \
       j/6/ut_burn.o \
       j/6/ut_busk.o \
       j/6/ut_bust.o \
       j/6/ut_conk.o \
       j/6/ut_crop.o \
       j/6/ut_cull.o \
       j/6/ut_find.o \
       j/6/ut_fink.o \
       j/6/ut_fire.o \
       j/6/ut_firm.o \
       j/6/ut_fish.o \
       j/6/ut_fuse.o \
       j/6/ut_gain.o \
       j/6/ut_heal.o \
       j/6/ut_lose.o \
       j/6/ut_mint.o \
       j/6/ut_mull.o \
       j/6/ut_nest.o \
       j/6/ut_park.o \
       j/6/ut_peek.o \
       j/6/ut_play.o \
       j/6/ut_repo.o \
       j/6/ut_rest.o \
       j/6/ut_seek.o \
       j/6/ut_swab.o \
       j/6/ut_tack.o \
       j/6/ut_tock.o \
       j/6/ut_wrap.o

J_OFILES=\
       $(J_1_OFILES) \
       $(J_2_OFILES) \
       $(J_3_OFILES) \
       $(J_4_OFILES) \
       $(J_5_OFILES) \
       $(J_5_OFILES_ED) \
       $(J_6_OFILES) \
       $(J_6_OFILES_UT) \
       j/dash.o

W_OFILES=\
       w/test.o

CRE2_OFILES=\
       outside/cre2/src/src/cre2.o

OUT_OFILES=\
       outside/jhttp/http_parser.o

V_OFILES=\
       v/ames.o \
       v/batz.o \
       v/cttp.o \
       v/http.o \
       v/loop.o \
       v/main.o \
       v/raft.o \
       v/reck.o \
       v/save.o \
       v/sist.o \
       v/time.o \
       v/term.o \
       v/unix.o \
       v/walk.o

VERE_OFILES=\
       $(BASE_OFILES) \
       $(CRE2_OFILES) \
       $(OUT_OFILES) \
       $(V_OFILES)

LIBUV=outside/libuv/libuv.a

LIBRE2=outside/re2/obj/libre2.a

LIBED25519=outside/ed25519/ed25519.a

LIBANACHRONISM=outside/anachronism/build/libanachronism.a

all: $(BIN)/meme

$(LIBUV):
	$(MAKE) -C outside/libuv libuv.a

$(LIBRE2):
	$(MAKE) -C outside/re2 obj/libre2.a

$(LIBED25519):
	$(MAKE) -C outside/ed25519

$(LIBANACHRONISM):
	$(MAKE) -C outside/anachronism static

$(CRE2_OFILES): outside/cre2/src/src/cre2.cpp outside/cre2/src/src/cre2.h $(LIBRE2)
	$(CXX) $(CXXFLAGS) -c $< $(LIBRE2) -o $@

$(V_OFILES) f/loom.o f/trac.o: include/v/vere.h

$(BIN)/vere: $(LIBCRE) $(VERE_OFILES) $(LIBUV) $(LIBRE2) $(LIBED25519) $(LIBANACHRONISM)
	mkdir -p $(BIN)
	$(CLD) $(CLDOSFLAGS) -o $(BIN)/vere $(VERE_OFILES) $(LIBUV) $(LIBCRE) $(LIBRE2) $(LIBED25519) $(BPT_O) $(LIBANACHRONISM) $(LIBS)

meme: $(BIN)/meme

MEME_OFILES=$(N_OFILES) $(J_OFILES) $(W_OFILES) $(CRE2_OFILES)

$(BIN)/meme: $(MEME_OFILES) $(LIBED25519)
	$(CLD) -g $(CLDOSFLAGS) -o $(BIN)/meme $(MEME_OFILES) $(LIBRE2) $(LIBED25519) -lgmp

tags:
	ctags -R -f .tags --exclude=root

etags:
	etags -f .etags $$(find -name '*.c' -or -name '*.h')

osxpackage:
	$(RM) -r inst
	$(MAKE) distclean
	$(MAKE) $(BIN)/vere LIB=/usr/local/lib/urb STATIC=yes
	mkdir -p inst/usr/local/lib/urb inst/usr/local/bin
	cp $(BIN)/vere inst/usr/local/bin
	cp urb/urbit.pill inst/usr/local/lib/urb
	cp -R urb/zod inst/usr/local/lib/urb
	pkgbuild --root inst --identifier org.urbit.vere --version 0.2 vere.pkg

debbuild:
	$(MAKE) $(BIN)/vere LIB=/usr/share/urb

debinstall:
	mkdir -p $(DESTDIR)/usr/bin $(DESTDIR)/usr/share/urb
	install -m755 $(BIN)/vere $(DESTDIR)/usr/bin
	cp urb/urbit.pill $(DESTDIR)/usr/share/urb
	cp -R urb/zod $(DESTDIR)/usr/share/urb

clean:
	$(RM) $(MEME_OFILES) $(BIN)/vere vere.pkg

distclean: clean
	$(MAKE) -C outside/libuv clean
	$(MAKE) -C outside/re2 clean
	$(MAKE) -C outside/ed25519 clean
	$(MAKE) -C outside/anachronism clean

.PHONY: clean debbuild debinstalldistclean etags osxpackage tags
