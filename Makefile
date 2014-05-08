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

LIB=$(PWD)/lib

RM=rm -f
CC=gcc
CXX=g++
CXXFLAGS=$(CFLAGS)
CLD=g++ -O -g -L/usr/local/lib -L/opt/local/lib
YACC=bison -v -b$(GENERATED)/y
LEX=lex

ifeq ($(OS),osx)
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

LIBS=-lssl -lcrypto -lgmp -lncurses -lsigsegv $(OSLIBS)

INCLUDE=include
MDEFINES=-DU2_OS_$(OS) -DU2_OS_ENDIAN_$(ENDIAN) -D U2_LIB=\"$(LIB)\"

CFLAGS= -O -g \
	-I/usr/local/include \
	-I/opt/local/include \
	-I$(INCLUDE) \
	-Ioutside/libuv/include \
	-Ioutside/bpt \
	-Ioutside/re2 \
	-Ioutside/cre2/src/src \
	-Ioutside/ed25519/src \
	$(DEFINES) \
	$(MDEFINES)

CWFLAGS=-Wall

.y.o:
	 mkdir -p $(GENERATED)
	 $(YACC) $<
	 $(CC) -c $(CFLAGS) -o $@ $(GENERATED)/y.tab.c
	 $(RM) $(GENERATED)/y.tab.c

.c.o:
	 $(CC) -c $(CWFLAGS) $(CFLAGS) -o $@ $<

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

J164_1_OFILES=\
       gen164/1/add.o \
       gen164/1/dec.o \
       gen164/1/div.o \
       gen164/1/gte.o \
       gen164/1/gth.o \
       gen164/1/lte.o \
       gen164/1/lth.o \
       gen164/1/mod.o \
       gen164/1/mul.o \
       gen164/1/sub.o

J164_2_OFILES=\
       gen164/2/bind.o \
       gen164/2/clap.o \
       gen164/2/drop.o \
       gen164/2/flop.o \
       gen164/2/lent.o \
       gen164/2/levy.o \
       gen164/2/lien.o \
       gen164/2/need.o \
       gen164/2/reel.o \
       gen164/2/roll.o \
       gen164/2/skim.o \
       gen164/2/skip.o \
       gen164/2/slag.o \
       gen164/2/snag.o \
       gen164/2/sort.o \
       gen164/2/turn.o \
       gen164/2/weld.o

J164_3_OFILES=\
       gen164/3/bex.o \
       gen164/3/can.o \
       gen164/3/cap.o \
       gen164/3/cat.o \
       gen164/3/con.o \
       gen164/3/cut.o \
       gen164/3/dor.o \
       gen164/3/dis.o \
       gen164/3/end.o \
       gen164/3/gor.o \
       gen164/3/hor.o \
       gen164/3/lsh.o \
       gen164/3/mas.o \
       gen164/3/met.o \
       gen164/3/mix.o \
       gen164/3/mug.o \
       gen164/3/peg.o \
       gen164/3/po.o  \
       gen164/3/rap.o \
       gen164/3/rip.o \
       gen164/3/rsh.o \
       gen164/3/vor.o

J164_4_OFILES=\
       gen164/4/in.o \
       gen164/4/by.o \
       gen164/4/in_has.o \
       gen164/4/in_gas.o \
       gen164/4/in_put.o \
       gen164/4/in_tap.o \
       gen164/4/by_gas.o \
       gen164/4/by_get.o \
       gen164/4/by_has.o \
       gen164/4/by_put.o \
       gen164/4/by_uni.o

J164_5_OFILES=\
       gen164/5/aesc.o \
       gen164/5/cue.o \
       gen164/5/ed.o \
       gen164/5/jam.o \
       gen164/5/mat.o \
       gen164/5/mink.o \
       gen164/5/parse.o \
       gen164/5/repg.o \
       gen164/5/rexp.o \
       gen164/5/rub.o \
       gen164/5/shax.o \
       gen164/5/lore.o \
       gen164/5/loss.o \
       gen164/5/tape.o \
       gen164/5/trip.o

J164_5_OFILES_ED=\
       gen164/5/ed_puck.o \
       gen164/5/ed_sign.o \
       gen164/5/ed_veri.o

J164_6_OFILES=\
       gen164/6/al.o \
       gen164/6/ap.o \
       gen164/6/bull.o \
       gen164/6/cell.o \
       gen164/6/comb.o \
       gen164/6/cons.o \
       gen164/6/core.o \
       gen164/6/cube.o \
       gen164/6/face.o \
       gen164/6/fitz.o \
       gen164/6/flan.o \
       gen164/6/flay.o \
       gen164/6/flip.o \
       gen164/6/flor.o \
       gen164/6/fork.o \
       gen164/6/hike.o \
       gen164/6/look.o \
       gen164/6/ut.o

J164_6_OFILES_UT=\
       gen164/6/ut_burn.o \
       gen164/6/ut_busk.o \
       gen164/6/ut_bust.o \
       gen164/6/ut_conk.o \
       gen164/6/ut_crop.o \
       gen164/6/ut_cull.o \
       gen164/6/ut_find.o \
       gen164/6/ut_fink.o \
       gen164/6/ut_fire.o \
       gen164/6/ut_firm.o \
       gen164/6/ut_fish.o \
       gen164/6/ut_fuse.o \
       gen164/6/ut_gain.o \
       gen164/6/ut_heal.o \
       gen164/6/ut_lose.o \
       gen164/6/ut_mint.o \
       gen164/6/ut_moot.o \
       gen164/6/ut_mull.o \
       gen164/6/ut_nest.o \
       gen164/6/ut_park.o \
       gen164/6/ut_peek.o \
       gen164/6/ut_play.o \
       gen164/6/ut_repo.o \
       gen164/6/ut_rest.o \
       gen164/6/ut_seek.o \
       gen164/6/ut_sift.o \
       gen164/6/ut_swab.o \
       gen164/6/ut_tack.o \
       gen164/6/ut_tock.o \
       gen164/6/ut_wrap.o

J164_OFILES=\
       $(J164_1_OFILES) \
       $(J164_2_OFILES) \
       $(J164_3_OFILES) \
       $(J164_4_OFILES) \
       $(J164_5_OFILES) \
       $(J164_5_OFILES_ED) \
       $(J164_6_OFILES) \
       $(J164_6_OFILES_UT) \
       gen164/watt.o

BASE_OFILES=\
       $(F_OFILES) \
       $(J164_OFILES)

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
BPT_O=outside/bpt/bitmapped_patricia_tree.o

all: $(BIN)/vere

$(LIBUV):
	$(MAKE) -C outside/libuv libuv.a

$(LIBRE2):
	$(MAKE) -C outside/re2 obj/libre2.a

$(LIBED25519):
	$(MAKE) -C outside/ed25519

$(BPT_O): outside/bpt/bitmapped_patricia_tree.c
	$(CC) -g -O2 -o $@ -c $<

$(CRE2_OFILES): outside/cre2/src/src/cre2.cpp outside/cre2/src/src/cre2.h $(LIBRE2)
	$(CXX) $(CXXFLAGS) -c $< $(LIBRE2) -o $@

$(V_OFILES) f/loom.o f/trac.o: include/v/vere.h

$(BIN)/vere: $(LIBCRE) $(VERE_OFILES) $(LIBUV) $(LIBRE2) $(LIBED25519) $(BPT_O)
	mkdir -p $(BIN)
	$(CLD) $(CLDOSFLAGS) -o $(BIN)/vere $(VERE_OFILES) $(LIBUV) $(LIBCRE) $(LIBRE2) $(LIBED25519) $(BPT_O) $(LIBS)

tags:
	ctags -R -f .tags --exclude=root

etags:
	etags -f .etags $$(find -name '*.c' -or -name '*.h')

clean:
	$(RM) $(VERE_OFILES) $(BIN)/vere

distclean: clean
	$(MAKE) -C outside/libuv clean
	$(MAKE) -C outside/re2 clean
	$(MAKE) -C outside/ed25519 clean
	rm $(BPT_O)
