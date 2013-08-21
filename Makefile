# A simple makefile.
#

# Pick one of:
#   linux
#   osx
#
#OS=linux
OS=osx

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
CLD=gcc -O3 -L/usr/local/lib
YACC=bison -v -b$(GENERATED)/y
LEX=lex

ifeq ($(OS),osx)
  CLDOSFLAGS=-bind_at_load
endif
ifeq ($(OS),linux)
  OSLIBS=-lcrypto -lpthread
endif

LIBS=-lgmp -ltermcap -lsigsegv outside/libuv/libuv.a $(OSLIBS)

INCLUDE=include
GENERATED=generated
DEFINES=-DU2_OS_$(OS) -DU2_OS_ENDIAN_$(ENDIAN) -D U2_LIB=\"$(LIB)\"

CFLAGS=-O3 \
       	-I/usr/local/include \
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

J195_1_OFILES=\
       gen195/1/add.o \
       gen195/1/dec.o \
       gen195/1/div.o \
       gen195/1/gte.o \
       gen195/1/gth.o \
       gen195/1/lte.o \
       gen195/1/lth.o \
       gen195/1/mod.o \
       gen195/1/mul.o \
       gen195/1/sub.o

J195_2_OFILES=\
       gen195/2/bind.o \
       gen195/2/clap.o \
       gen195/2/drop.o \
       gen195/2/flop.o \
       gen195/2/lent.o \
       gen195/2/levy.o \
       gen195/2/lien.o \
       gen195/2/need.o \
       gen195/2/reel.o \
       gen195/2/roll.o \
       gen195/2/skim.o \
       gen195/2/skip.o \
       gen195/2/slag.o \
       gen195/2/snag.o \
       gen195/2/sort.o \
       gen195/2/turn.o \
       gen195/2/weld.o

J195_3_OFILES=\
       gen195/3/bex.o \
       gen195/3/can.o \
       gen195/3/cap.o \
       gen195/3/cat.o \
       gen195/3/con.o \
       gen195/3/cut.o \
       gen195/3/dor.o \
       gen195/3/dis.o \
       gen195/3/end.o \
       gen195/3/gor.o \
       gen195/3/hor.o \
       gen195/3/lsh.o \
       gen195/3/mas.o \
       gen195/3/met.o \
       gen195/3/mix.o \
       gen195/3/mug.o \
       gen195/3/peg.o \
       gen195/3/rap.o \
       gen195/3/rip.o \
       gen195/3/rsh.o \
       gen195/3/vor.o

J195_4_OFILES=\
       gen195/4/in.o \
       gen195/4/by.o \
       gen195/4/in_has.o \
       gen195/4/in_gas.o \
       gen195/4/in_put.o \
       gen195/4/in_tap.o \
       gen195/4/by_gas.o \
       gen195/4/by_get.o \
       gen195/4/by_has.o \
       gen195/4/by_put.o 

J195_5_OFILES=\
       gen195/5/cue.o \
       gen195/5/jam.o \
       gen195/5/mat.o \
       gen195/5/mock.o \
       gen195/5/parse.o \
       gen195/5/rub.o \
       gen195/5/shax.o \
       gen195/5/trip.o

J195_6_OFILES=\
       gen195/6/ap.o \
       gen195/6/cell.o \
       gen195/6/comb.o \
       gen195/6/cons.o \
       gen195/6/core.o \
       gen195/6/cube.o \
       gen195/6/face.o \
       gen195/6/fine.o \
       gen195/6/fitz.o \
       gen195/6/flan.o \
       gen195/6/flay.o \
       gen195/6/flip.o \
       gen195/6/flor.o \
       gen195/6/fork.o \
       gen195/6/hike.o \
       gen195/6/look.o \
       gen195/6/ut.o

J195_6_OFILES_UT=\
       gen195/6/ut_burn.o \
       gen195/6/ut_bust.o \
       gen195/6/ut_crop.o \
       gen195/6/ut_cull.o \
       gen195/6/ut_find.o \
       gen195/6/ut_fink.o \
       gen195/6/ut_fire.o \
       gen195/6/ut_firm.o \
       gen195/6/ut_fish.o \
       gen195/6/ut_fuse.o \
       gen195/6/ut_gain.o \
       gen195/6/ut_heal.o \
       gen195/6/ut_lose.o \
       gen195/6/ut_mint.o \
       gen195/6/ut_moot.o \
       gen195/6/ut_mull.o \
       gen195/6/ut_nest.o \
       gen195/6/ut_park.o \
       gen195/6/ut_peek.o \
       gen195/6/ut_play.o \
       gen195/6/ut_repo.o \
       gen195/6/ut_rest.o \
       gen195/6/ut_seek.o \
       gen195/6/ut_snap.o \
       gen195/6/ut_swab.o \
       gen195/6/ut_tack.o \
       gen195/6/ut_tock.o \
       gen195/6/ut_wrap.o

J195_OFILES=\
       $(J195_1_OFILES) \
       $(J195_2_OFILES) \
       $(J195_3_OFILES) \
       $(J195_4_OFILES) \
       $(J195_5_OFILES) \
       $(J195_6_OFILES) \
       $(J195_6_OFILES_UT) \
       gen195/watt.o

J194_1_OFILES=\
       gen194/1/add.o \
       gen194/1/dec.o \
       gen194/1/div.o \
       gen194/1/gte.o \
       gen194/1/gth.o \
       gen194/1/lte.o \
       gen194/1/lth.o \
       gen194/1/mod.o \
       gen194/1/mul.o \
       gen194/1/sub.o

J194_2_OFILES=\
       gen194/2/bind.o \
       gen194/2/clap.o \
       gen194/2/drop.o \
       gen194/2/flop.o \
       gen194/2/lent.o \
       gen194/2/levy.o \
       gen194/2/lien.o \
       gen194/2/need.o \
       gen194/2/reel.o \
       gen194/2/roll.o \
       gen194/2/skim.o \
       gen194/2/skip.o \
       gen194/2/slag.o \
       gen194/2/snag.o \
       gen194/2/sort.o \
       gen194/2/turn.o \
       gen194/2/weld.o

J194_3_OFILES=\
       gen194/3/bex.o \
       gen194/3/can.o \
       gen194/3/cap.o \
       gen194/3/cat.o \
       gen194/3/con.o \
       gen194/3/cut.o \
       gen194/3/dor.o \
       gen194/3/dis.o \
       gen194/3/end.o \
       gen194/3/gor.o \
       gen194/3/hor.o \
       gen194/3/lsh.o \
       gen194/3/mas.o \
       gen194/3/met.o \
       gen194/3/mix.o \
       gen194/3/mog.o \
       gen194/3/peg.o \
       gen194/3/rap.o \
       gen194/3/rip.o \
       gen194/3/rsh.o \
       gen194/3/vor.o

J194_4_OFILES=\
       gen194/4/in.o \
       gen194/4/by.o \
       gen194/4/in_has.o \
       gen194/4/in_gas.o \
       gen194/4/in_put.o \
       gen194/4/in_tap.o \
       gen194/4/by_gas.o \
       gen194/4/by_get.o \
       gen194/4/by_has.o \
       gen194/4/by_put.o 

J194_5_OFILES=\
       gen194/5/cue.o \
       gen194/5/jam.o \
       gen194/5/mat.o \
       gen194/5/mock.o \
       gen194/5/parse.o \
       gen194/5/rub.o \
       gen194/5/shax.o \
       gen194/5/trip.o

J194_6_OFILES=\
       gen194/6/ap.o \
       gen194/6/cell.o \
       gen194/6/comb.o \
       gen194/6/cons.o \
       gen194/6/core.o \
       gen194/6/cube.o \
       gen194/6/face.o \
       gen194/6/fine.o \
       gen194/6/fitz.o \
       gen194/6/flan.o \
       gen194/6/flay.o \
       gen194/6/flip.o \
       gen194/6/flor.o \
       gen194/6/fork.o \
       gen194/6/hike.o \
       gen194/6/look.o \
       gen194/6/ut.o

J194_6_OFILES_UT=\
       gen194/6/ut_burn.o \
       gen194/6/ut_bust.o \
       gen194/6/ut_crop.o \
       gen194/6/ut_cull.o \
       gen194/6/ut_find.o \
       gen194/6/ut_fink.o \
       gen194/6/ut_fire.o \
       gen194/6/ut_firm.o \
       gen194/6/ut_fish.o \
       gen194/6/ut_fuse.o \
       gen194/6/ut_gain.o \
       gen194/6/ut_heal.o \
       gen194/6/ut_lose.o \
       gen194/6/ut_mint.o \
       gen194/6/ut_moot.o \
       gen194/6/ut_mull.o \
       gen194/6/ut_nest.o \
       gen194/6/ut_park.o \
       gen194/6/ut_peek.o \
       gen194/6/ut_play.o \
       gen194/6/ut_repo.o \
       gen194/6/ut_rest.o \
       gen194/6/ut_seek.o \
       gen194/6/ut_snap.o \
       gen194/6/ut_swab.o \
       gen194/6/ut_tack.o \
       gen194/6/ut_tock.o \
       gen194/6/ut_wrap.o

J194_OFILES=\
       $(J194_1_OFILES) \
       $(J194_2_OFILES) \
       $(J194_3_OFILES) \
       $(J194_4_OFILES) \
       $(J194_5_OFILES) \
       $(J194_6_OFILES) \
       $(J194_6_OFILES_UT) \
       gen194/watt.o

J193_1_OFILES=\
       gen193/1/add.o \
       gen193/1/dec.o \
       gen193/1/div.o \
       gen193/1/gte.o \
       gen193/1/gth.o \
       gen193/1/lte.o \
       gen193/1/lth.o \
       gen193/1/mod.o \
       gen193/1/mul.o \
       gen193/1/sub.o

J193_2_OFILES=\
       gen193/2/bind.o \
       gen193/2/clap.o \
       gen193/2/drop.o \
       gen193/2/flop.o \
       gen193/2/lent.o \
       gen193/2/levy.o \
       gen193/2/lien.o \
       gen193/2/need.o \
       gen193/2/reel.o \
       gen193/2/roll.o \
       gen193/2/skim.o \
       gen193/2/skip.o \
       gen193/2/slag.o \
       gen193/2/snag.o \
       gen193/2/sort.o \
       gen193/2/turn.o \
       gen193/2/weld.o

J193_3_OFILES=\
       gen193/3/bex.o \
       gen193/3/can.o \
       gen193/3/cap.o \
       gen193/3/cat.o \
       gen193/3/con.o \
       gen193/3/cut.o \
       gen193/3/dor.o \
       gen193/3/dis.o \
       gen193/3/end.o \
       gen193/3/gor.o \
       gen193/3/hor.o \
       gen193/3/lsh.o \
       gen193/3/mas.o \
       gen193/3/met.o \
       gen193/3/mix.o \
       gen193/3/mog.o \
       gen193/3/peg.o \
       gen193/3/rap.o \
       gen193/3/rip.o \
       gen193/3/rsh.o \
       gen193/3/vor.o

J193_4_OFILES=\
       gen193/4/in.o \
       gen193/4/by.o \
       gen193/4/in_has.o \
       gen193/4/in_gas.o \
       gen193/4/in_put.o \
       gen193/4/in_tap.o \
       gen193/4/by_gas.o \
       gen193/4/by_get.o \
       gen193/4/by_has.o \
       gen193/4/by_put.o 

J193_5_OFILES=\
       gen193/5/cue.o \
       gen193/5/jam.o \
       gen193/5/mat.o \
       gen193/5/mock.o \
       gen193/5/parse.o \
       gen193/5/rub.o \
       gen193/5/shax.o \
       gen193/5/trip.o

J193_6_OFILES=\
       gen193/6/ap.o \
       gen193/6/cell.o \
       gen193/6/comb.o \
       gen193/6/cons.o \
       gen193/6/core.o \
       gen193/6/cube.o \
       gen193/6/face.o \
       gen193/6/fine.o \
       gen193/6/fitz.o \
       gen193/6/flan.o \
       gen193/6/flay.o \
       gen193/6/flip.o \
       gen193/6/flor.o \
       gen193/6/fork.o \
       gen193/6/hike.o \
       gen193/6/look.o \
       gen193/6/ut.o

J193_6_OFILES_UT=\
       gen193/6/ut_burn.o \
       gen193/6/ut_bust.o \
       gen193/6/ut_crop.o \
       gen193/6/ut_cull.o \
       gen193/6/ut_find.o \
       gen193/6/ut_fink.o \
       gen193/6/ut_fire.o \
       gen193/6/ut_firm.o \
       gen193/6/ut_fish.o \
       gen193/6/ut_fuse.o \
       gen193/6/ut_gain.o \
       gen193/6/ut_heal.o \
       gen193/6/ut_lose.o \
       gen193/6/ut_mint.o \
       gen193/6/ut_moot.o \
       gen193/6/ut_mull.o \
       gen193/6/ut_nest.o \
       gen193/6/ut_park.o \
       gen193/6/ut_peek.o \
       gen193/6/ut_play.o \
       gen193/6/ut_repo.o \
       gen193/6/ut_rest.o \
       gen193/6/ut_seek.o \
       gen193/6/ut_snap.o \
       gen193/6/ut_swab.o \
       gen193/6/ut_tack.o \
       gen193/6/ut_tock.o \
       gen193/6/ut_wrap.o

J193_OFILES=\
       $(J193_1_OFILES) \
       $(J193_2_OFILES) \
       $(J193_3_OFILES) \
       $(J193_4_OFILES) \
       $(J193_5_OFILES) \
       $(J193_6_OFILES) \
       $(J193_6_OFILES_UT) \
       gen193/watt.o

J192_1_OFILES=\
       gen192/1/add.o \
       gen192/1/dec.o \
       gen192/1/div.o \
       gen192/1/gte.o \
       gen192/1/gth.o \
       gen192/1/lte.o \
       gen192/1/lth.o \
       gen192/1/mod.o \
       gen192/1/mul.o \
       gen192/1/sub.o

J192_2_OFILES=\
       gen192/2/bind.o \
       gen192/2/clap.o \
       gen192/2/drop.o \
       gen192/2/flop.o \
       gen192/2/lent.o \
       gen192/2/levy.o \
       gen192/2/lien.o \
       gen192/2/need.o \
       gen192/2/reel.o \
       gen192/2/roll.o \
       gen192/2/skim.o \
       gen192/2/skip.o \
       gen192/2/slag.o \
       gen192/2/snag.o \
       gen192/2/sort.o \
       gen192/2/turn.o \
       gen192/2/weld.o

J192_3_OFILES=\
       gen192/3/bex.o \
       gen192/3/can.o \
       gen192/3/cap.o \
       gen192/3/cat.o \
       gen192/3/con.o \
       gen192/3/cut.o \
       gen192/3/dor.o \
       gen192/3/dis.o \
       gen192/3/end.o \
       gen192/3/gor.o \
       gen192/3/hor.o \
       gen192/3/lsh.o \
       gen192/3/mas.o \
       gen192/3/met.o \
       gen192/3/mix.o \
       gen192/3/mug.o \
       gen192/3/peg.o \
       gen192/3/rap.o \
       gen192/3/rip.o \
       gen192/3/rsh.o \
       gen192/3/vor.o

J192_4_OFILES=\
       gen192/4/in.o \
       gen192/4/by.o \
       gen192/4/in_has.o \
       gen192/4/in_gas.o \
       gen192/4/in_put.o \
       gen192/4/in_tap.o \
       gen192/4/by_gas.o \
       gen192/4/by_get.o \
       gen192/4/by_has.o \
       gen192/4/by_put.o 

J192_5_OFILES=\
       gen192/5/cue.o \
       gen192/5/jam.o \
       gen192/5/mat.o \
       gen192/5/mink.o \
       gen192/5/parse.o \
       gen192/5/rub.o \
       gen192/5/shax.o \
       gen192/5/trip.o

J192_6_OFILES=\
       gen192/6/ap.o \
       gen192/6/cell.o \
       gen192/6/comb.o \
       gen192/6/cons.o \
       gen192/6/core.o \
       gen192/6/cube.o \
       gen192/6/face.o \
       gen192/6/fine.o \
       gen192/6/fitz.o \
       gen192/6/flan.o \
       gen192/6/flay.o \
       gen192/6/flip.o \
       gen192/6/flor.o \
       gen192/6/fork.o \
       gen192/6/hike.o \
       gen192/6/look.o \
       gen192/6/ut.o

J192_6_OFILES_UT=\
       gen192/6/ut_burn.o \
       gen192/6/ut_bust.o \
       gen192/6/ut_crop.o \
       gen192/6/ut_cull.o \
       gen192/6/ut_find.o \
       gen192/6/ut_fink.o \
       gen192/6/ut_fire.o \
       gen192/6/ut_firm.o \
       gen192/6/ut_fish.o \
       gen192/6/ut_fuse.o \
       gen192/6/ut_gain.o \
       gen192/6/ut_heal.o \
       gen192/6/ut_lose.o \
       gen192/6/ut_mint.o \
       gen192/6/ut_moot.o \
       gen192/6/ut_mull.o \
       gen192/6/ut_nest.o \
       gen192/6/ut_park.o \
       gen192/6/ut_peek.o \
       gen192/6/ut_play.o \
       gen192/6/ut_repo.o \
       gen192/6/ut_rest.o \
       gen192/6/ut_seek.o \
       gen192/6/ut_snap.o \
       gen192/6/ut_swab.o \
       gen192/6/ut_tack.o \
       gen192/6/ut_tock.o \
       gen192/6/ut_wrap.o

J192_OFILES=\
       $(J192_1_OFILES) \
       $(J192_2_OFILES) \
       $(J192_3_OFILES) \
       $(J192_4_OFILES) \
       $(J192_5_OFILES) \
       $(J192_6_OFILES) \
       $(J192_6_OFILES_UT) \
       gen192/watt.o

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
       v/behn.o \
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

all: $(BIN)/vere

$(BIN)/vere: $(VERE_OFILES)
	mkdir -p $(BIN)
	$(CLD) $(CLDOSFLAGS) -o $(BIN)/vere $(VERE_OFILES) $(LIBS)

tags:
	ctags -R -f .tags --exclude=root

clean:
	 $(RM) $(VERE_OFILES) $(BIN)/vere $(BIN)/eyre
