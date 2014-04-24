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

# Library directory - from whence resources are now loaded.
#
LIB=$(PWD)/lib

RM=rm -f
CC=gcc
CXX=g++
CXXFLAGS=$(CFLAGS)
CLD=g++ -O2 -g -L/usr/local/lib -L/opt/local/lib
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
GENERATED=generated
MDEFINES=-DU2_OS_$(OS) -DU2_OS_ENDIAN_$(ENDIAN) -D U2_LIB=\"$(LIB)\"

CFLAGS= -O2 -g \
	-I/usr/local/include \
	-I/opt/local/include \
	-I$(INCLUDE)  \
	-Ioutside/libuv/include \
	-Ioutside/re2 \
	-Ioutside/cre2/src/src \
	-I $(GENERATED) \
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


J191_1_OFILES=\
       gen191/1/add.o \
       gen191/1/dec.o \
       gen191/1/div.o \
       gen191/1/gte.o \
       gen191/1/gth.o \
       gen191/1/lte.o \
       gen191/1/lth.o \
       gen191/1/mod.o \
       gen191/1/mul.o \
       gen191/1/sub.o

J191_2_OFILES=\
       gen191/2/bind.o \
       gen191/2/clap.o \
       gen191/2/drop.o \
       gen191/2/flop.o \
       gen191/2/lent.o \
       gen191/2/levy.o \
       gen191/2/lien.o \
       gen191/2/need.o \
       gen191/2/reel.o \
       gen191/2/roll.o \
       gen191/2/skim.o \
       gen191/2/skip.o \
       gen191/2/slag.o \
       gen191/2/snag.o \
       gen191/2/sort.o \
       gen191/2/turn.o \
       gen191/2/weld.o

J191_3_OFILES=\
       gen191/3/bex.o \
       gen191/3/can.o \
       gen191/3/cap.o \
       gen191/3/cat.o \
       gen191/3/con.o \
       gen191/3/cut.o \
       gen191/3/dor.o \
       gen191/3/dis.o \
       gen191/3/end.o \
       gen191/3/gor.o \
       gen191/3/hor.o \
       gen191/3/lsh.o \
       gen191/3/mas.o \
       gen191/3/met.o \
       gen191/3/mix.o \
       gen191/3/mug.o \
       gen191/3/peg.o \
       gen191/3/po.o  \
       gen191/3/rap.o \
       gen191/3/rip.o \
       gen191/3/rsh.o \
       gen191/3/vor.o

J191_4_OFILES=\
       gen191/4/in.o \
       gen191/4/by.o \
       gen191/4/in_has.o \
       gen191/4/in_gas.o \
       gen191/4/in_put.o \
       gen191/4/in_tap.o \
       gen191/4/by_gas.o \
       gen191/4/by_get.o \
       gen191/4/by_has.o \
       gen191/4/by_put.o 

J191_5_OFILES=\
       gen191/5/cue.o \
       gen191/5/jam.o \
       gen191/5/mat.o \
       gen191/5/mink.o \
       gen191/5/parse.o \
       gen191/5/rub.o \
       gen191/5/shax.o \
       gen191/5/lore.o \
       gen191/5/loss.o \
       gen191/5/tape.o \
       gen191/5/trip.o

J191_6_OFILES=\
       gen191/6/al.o \
       gen191/6/ap.o \
       gen191/6/bull.o \
       gen191/6/cell.o \
       gen191/6/comb.o \
       gen191/6/cons.o \
       gen191/6/core.o \
       gen191/6/cube.o \
       gen191/6/face.o \
       gen191/6/fitz.o \
       gen191/6/flan.o \
       gen191/6/flay.o \
       gen191/6/flip.o \
       gen191/6/flor.o \
       gen191/6/fork.o \
       gen191/6/hike.o \
       gen191/6/look.o \
       gen191/6/ut.o

J191_6_OFILES_UT=\
       gen191/6/ut_burn.o \
       gen191/6/ut_busk.o \
       gen191/6/ut_bust.o \
       gen191/6/ut_conk.o \
       gen191/6/ut_crop.o \
       gen191/6/ut_cull.o \
       gen191/6/ut_find.o \
       gen191/6/ut_fink.o \
       gen191/6/ut_fire.o \
       gen191/6/ut_firm.o \
       gen191/6/ut_fish.o \
       gen191/6/ut_fuse.o \
       gen191/6/ut_gain.o \
       gen191/6/ut_heal.o \
       gen191/6/ut_lose.o \
       gen191/6/ut_mint.o \
       gen191/6/ut_moot.o \
       gen191/6/ut_mull.o \
       gen191/6/ut_nest.o \
       gen191/6/ut_park.o \
       gen191/6/ut_peek.o \
       gen191/6/ut_play.o \
       gen191/6/ut_repo.o \
       gen191/6/ut_rest.o \
       gen191/6/ut_seek.o \
       gen191/6/ut_sift.o \
       gen191/6/ut_swab.o \
       gen191/6/ut_tack.o \
       gen191/6/ut_tock.o \
       gen191/6/ut_wrap.o

J191_OFILES=\
       $(J191_1_OFILES) \
       $(J191_2_OFILES) \
       $(J191_3_OFILES) \
       $(J191_4_OFILES) \
       $(J191_5_OFILES) \
       $(J191_6_OFILES) \
       $(J191_6_OFILES_UT) \
       gen191/watt.o


J190_1_OFILES=\
       gen190/1/add.o \
       gen190/1/dec.o \
       gen190/1/div.o \
       gen190/1/gte.o \
       gen190/1/gth.o \
       gen190/1/lte.o \
       gen190/1/lth.o \
       gen190/1/mod.o \
       gen190/1/mul.o \
       gen190/1/sub.o

J190_2_OFILES=\
       gen190/2/bind.o \
       gen190/2/clap.o \
       gen190/2/drop.o \
       gen190/2/flop.o \
       gen190/2/lent.o \
       gen190/2/levy.o \
       gen190/2/lien.o \
       gen190/2/need.o \
       gen190/2/reel.o \
       gen190/2/roll.o \
       gen190/2/skim.o \
       gen190/2/skip.o \
       gen190/2/slag.o \
       gen190/2/snag.o \
       gen190/2/sort.o \
       gen190/2/turn.o \
       gen190/2/weld.o

J190_3_OFILES=\
       gen190/3/bex.o \
       gen190/3/can.o \
       gen190/3/cap.o \
       gen190/3/cat.o \
       gen190/3/con.o \
       gen190/3/cut.o \
       gen190/3/dor.o \
       gen190/3/dis.o \
       gen190/3/end.o \
       gen190/3/gor.o \
       gen190/3/hor.o \
       gen190/3/lsh.o \
       gen190/3/mas.o \
       gen190/3/met.o \
       gen190/3/mix.o \
       gen190/3/mug.o \
       gen190/3/peg.o \
       gen190/3/po.o  \
       gen190/3/rap.o \
       gen190/3/rip.o \
       gen190/3/rsh.o \
       gen190/3/vor.o

J190_4_OFILES=\
       gen190/4/in.o \
       gen190/4/by.o \
       gen190/4/in_has.o \
       gen190/4/in_gas.o \
       gen190/4/in_put.o \
       gen190/4/in_tap.o \
       gen190/4/by_gas.o \
       gen190/4/by_get.o \
       gen190/4/by_has.o \
       gen190/4/by_put.o 

J190_5_OFILES=\
       gen190/5/cue.o \
       gen190/5/jam.o \
       gen190/5/mat.o \
       gen190/5/mink.o \
       gen190/5/parse.o \
       gen190/5/rub.o \
       gen190/5/shax.o \
       gen190/5/lore.o \
       gen190/5/loss.o \
       gen190/5/tape.o \
       gen190/5/trip.o

J190_6_OFILES=\
       gen190/6/al.o \
       gen190/6/ap.o \
       gen190/6/bull.o \
       gen190/6/cell.o \
       gen190/6/comb.o \
       gen190/6/cons.o \
       gen190/6/core.o \
       gen190/6/cube.o \
       gen190/6/face.o \
       gen190/6/fitz.o \
       gen190/6/flan.o \
       gen190/6/flay.o \
       gen190/6/flip.o \
       gen190/6/flor.o \
       gen190/6/fork.o \
       gen190/6/hike.o \
       gen190/6/look.o \
       gen190/6/ut.o

J190_6_OFILES_UT=\
       gen190/6/ut_burn.o \
       gen190/6/ut_busk.o \
       gen190/6/ut_bust.o \
       gen190/6/ut_conk.o \
       gen190/6/ut_crop.o \
       gen190/6/ut_cull.o \
       gen190/6/ut_find.o \
       gen190/6/ut_fink.o \
       gen190/6/ut_fire.o \
       gen190/6/ut_firm.o \
       gen190/6/ut_fish.o \
       gen190/6/ut_fuse.o \
       gen190/6/ut_gain.o \
       gen190/6/ut_heal.o \
       gen190/6/ut_lose.o \
       gen190/6/ut_mint.o \
       gen190/6/ut_moot.o \
       gen190/6/ut_mull.o \
       gen190/6/ut_nest.o \
       gen190/6/ut_park.o \
       gen190/6/ut_peek.o \
       gen190/6/ut_play.o \
       gen190/6/ut_repo.o \
       gen190/6/ut_rest.o \
       gen190/6/ut_seek.o \
       gen190/6/ut_sift.o \
       gen190/6/ut_swab.o \
       gen190/6/ut_tack.o \
       gen190/6/ut_tock.o \
       gen190/6/ut_wrap.o

J190_OFILES=\
       $(J190_1_OFILES) \
       $(J190_2_OFILES) \
       $(J190_3_OFILES) \
       $(J190_4_OFILES) \
       $(J190_5_OFILES) \
       $(J190_6_OFILES) \
       $(J190_6_OFILES_UT) \
       gen190/watt.o

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
       gen164/5/cue.o \
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

all: $(BIN)/vere

$(LIBUV):
	$(MAKE) -C outside/libuv libuv.a

$(LIBRE2):
	$(MAKE) -C outside/re2 obj/libre2.a

$(CRE2_OFILES): outside/cre2/src/src/cre2.cpp outside/cre2/src/src/cre2.h $(LIBRE2)
	$(CXX) $(CXXFLAGS) -c $< $(LIBRE2) -o $@

$(V_OFILES) f/loom.o f/trac.o: include/v/vere.h

$(BIN)/vere: $(LIBCRE) $(VERE_OFILES) $(LIBUV) $(LIBRE2)
	mkdir -p $(BIN)
	$(CLD) $(CLDOSFLAGS) -o $(BIN)/vere $(VERE_OFILES) $(LIBUV) $(LIBCRE) $(LIBRE2) $(LIBS)

tags:
	ctags -R -f .tags --exclude=root

etags:
	etags -f .etags $$(find -name '*.c' -or -name '*.h')

clean:
	 $(RM) $(VERE_OFILES) $(BIN)/vere $(BIN)/eyre 

distclean: clean
	$(MAKE) -C outside/libuv clean
	$(MAKE) -C outside/re2 clean
