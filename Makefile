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
CLD=gcc -O3 -L/usr/local/lib -L/opt/local/lib
YACC=bison -v -b$(GENERATED)/y
LEX=lex

ifeq ($(OS),osx)
  CLDOSFLAGS=-bind_at_load
  OSLIBS=-framework CoreServices -framework CoreFoundation
endif
ifeq ($(OS),linux)
  OSLIBS=-lcrypto -lpthread -lrt -lcurses
endif
ifeq ($(OS),bsd)
  OSLIBS=-lcrypto -lpthread -lncurses -lkvm
endif

LIBS=-lgmp -lncurses -lsigsegv $(OSLIBS)

INCLUDE=include
GENERATED=generated
DEFINES=-DU2_OS_$(OS) -DU2_OS_ENDIAN_$(ENDIAN) -D U2_LIB=\"$(LIB)\"

CFLAGS=-O3 \
	-I/usr/local/include \
	-I/opt/local/include \
	-I$(INCLUDE)  \
	-Ioutside/libuv/include \
	-I $(GENERATED) \
	$(DEFINES)

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
       gen191/6/ap.o \
       gen191/6/cell.o \
       gen191/6/comb.o \
       gen191/6/cons.o \
       gen191/6/core.o \
       gen191/6/cube.o \
       gen191/6/face.o \
       gen191/6/fine.o \
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
       gen191/6/ut_bust.o \
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
       gen191/6/ut_snap.o \
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

BASE_OFILES=\
       $(F_OFILES) \
       $(J191_OFILES)

OUT_OFILES=\
       outside/jhttp/http_parser.o

VERE_OFILES=\
       v/ames.o \
       v/batz.o \
       v/http.o \
       v/loop.o \
       v/main.o \
       v/reck.o \
       v/save.o \
       v/time.o \
       v/term.o \
       v/unix.o \
       v/walk.o \
       $(BASE_OFILES) \
       $(OUT_OFILES)

LIBUV=outside/libuv/libuv.a

all: $(BIN)/vere

$(LIBUV): 
	$(MAKE) -C outside/libuv

$(BIN)/vere: $(VERE_OFILES) $(LIBUV)
	mkdir -p $(BIN)
	$(CLD) $(CLDOSFLAGS) -o $(BIN)/vere $(VERE_OFILES) $(LIBUV) $(LIBS)

tags:
	ctags -R -f .tags --exclude=root

etags:
	etags -f .etags $$(find -name '*.c' -or -name '*.h')

clean:
	 $(RM) $(VERE_OFILES) $(BIN)/vere $(BIN)/eyre 
	$(MAKE) -C outside/libuv clean

