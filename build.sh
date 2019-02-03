#!/usr/bin/env bash

set -ex

jets_a_src=" \
  jets/a/add.c \
  jets/a/dec.c \
  jets/a/div.c \
  jets/a/gte.c \
  jets/a/gth.c \
  jets/a/lte.c \
  jets/a/lth.c \
  jets/a/mod.c \
  jets/a/mul.c \
  jets/a/sub.c \
"

jets_b_src=" \
  jets/b/bind.c \
  jets/b/clap.c \
  jets/b/drop.c \
  jets/b/flop.c \
  jets/b/lent.c \
  jets/b/levy.c \
  jets/b/lien.c \
  jets/b/murn.c \
  jets/b/need.c \
  jets/b/reap.c \
  jets/b/reel.c \
  jets/b/roll.c \
  jets/b/skid.c \
  jets/b/skim.c \
  jets/b/skip.c \
  jets/b/scag.c \
  jets/b/slag.c \
  jets/b/snag.c \
  jets/b/sort.c \
  jets/b/turn.c \
  jets/b/weld.c \
"

jets_c_src=" \
  jets/c/bex.c \
  jets/c/xeb.c \
  jets/c/can.c \
  jets/c/cap.c \
  jets/c/cat.c \
  jets/c/con.c \
  jets/c/cut.c \
  jets/c/dor.c \
  jets/c/dvr.c \
  jets/c/dis.c \
  jets/c/end.c \
  jets/c/gor.c \
  jets/c/lsh.c \
  jets/c/mas.c \
  jets/c/met.c \
  jets/c/mix.c \
  jets/c/mor.c \
  jets/c/mug.c \
  jets/c/muk.c \
  jets/c/peg.c \
  jets/c/po.c \
  jets/c/pow.c \
  jets/c/rap.c \
  jets/c/rep.c \
  jets/c/rev.c \
  jets/c/rip.c \
  jets/c/rsh.c \
  jets/c/swp.c \
  jets/c/sqt.c \
"

jets_d_src=" \
  jets/d/in_has.c \
  jets/d/in_int.c \
  jets/d/in_gas.c \
  jets/d/in_mer.c \
  jets/d/in_put.c \
  jets/d/in_tap.c \
  jets/d/in_uni.c \
  jets/d/in_wyt.c \
  jets/d/in_bif.c \
  jets/d/in_dif.c \
  jets/d/in_del.c \
  jets/d/by_del.c \
  jets/d/by_gas.c \
  jets/d/by_get.c \
  jets/d/by_has.c \
  jets/d/by_int.c \
  jets/d/by_jab.c \
  jets/d/by_put.c \
  jets/d/by_uni.c \
  jets/d/by_bif.c \
  jets/d/by_dif.c \
"

jets_e_src=" \
  jets/e/aes_ecb.c \
  jets/e/aes_cbc.c \
  jets/e/aesc.c \
  jets/e/argon2.c \
  jets/e/blake.c \
  jets/e/cue.c \
  jets/e/fl.c \
  jets/e/hmac.c \
  jets/e/jam.c \
  jets/e/mat.c \
  jets/e/mink.c \
  jets/e/mule.c \
  jets/e/parse.c \
  jets/e/rd.c \
  jets/e/rq.c \
  jets/e/rs.c \
  jets/e/rh.c \
  jets/e/rub.c \
  jets/e/scr.c \
  jets/e/secp.c \
  jets/e/shax.c \
  jets/e/sha1.c \
  jets/e/lore.c \
  jets/e/loss.c \
  jets/e/lune.c \
  jets/e/trip.c \
  jets/e/ripe.c \
"

jets_e_ed_src=" \
  jets/e/ed_puck.c \
  jets/e/ed_sign.c \
  jets/e/ed_veri.c \
  jets/e/ed_shar.c \
"

jets_f_src=" \
  jets/f/ap.c \
  jets/f/cell.c \
  jets/f/comb.c \
  jets/f/cons.c \
  jets/f/core.c \
  jets/f/face.c \
  jets/f/fitz.c \
  jets/f/flan.c \
  jets/f/flip.c \
  jets/f/flor.c \
  jets/f/fork.c \
  jets/f/hint.c \
  jets/f/hike.c \
  jets/f/look.c \
  jets/f/loot.c \
"

jets_f_ut_src=" \
  jets/f/ut.c \
  jets/f/ut_buss.c \
  jets/f/ut_crop.c \
  jets/f/ut_find.c \
  jets/f/ut_fire.c \
  jets/f/ut_fish.c \
  jets/f/ut_fuse.c \
  jets/f/ut_gain.c \
  jets/f/ut_lose.c \
  jets/f/ut_mint.c \
  jets/f/ut_mull.c \
  jets/f/ut_nest.c \
  jets/f/ut_peek.c \
  jets/f/ut_peel.c \
  jets/f/ut_play.c \
  jets/f/ut_repo.c \
  jets/f/ut_rest.c \
  jets/f/ut_tack.c \
  jets/f/ut_toss.c \
  jets/f/ut_wrap.c \
"

jets_src=" \
  jets/tree.c \
"

jets_all_src=" \
  $jets_a_src \
  $jets_b_src \
  $jets_c_src \
  $jets_d_src \
  $jets_e_src \
  $jets_e_ed_src \
  $jets_f_src \
  $jets_f_ut_src \
  $jets_src \
"

noun_src=" \
  noun/allocate.c \
  noun/events.c \
  noun/hashtable.c \
  noun/imprison.c \
  noun/jets.c \
  noun/manage.c \
  noun/nock.c \
  noun/retrieve.c \
  noun/trace.c \
  noun/vortex.c \
  noun/xtract.c \
  noun/zave.c \
"

vere_sans_main=" \
  vere/ames.c \
  vere/behn.c \
  vere/cttp.c \
  vere/dawn.c \
  vere/http.c \
  vere/loop.c \
  vere/raft.c \
  vere/reck.c \
  vere/save.c \
  vere/sist.c \
  vere/term.c \
  vere/time.c \
  vere/unix.c \
  vere/walk.c \
"

vere_src=" \
  $vere_sans_main \
  vere/main.c \
"

src_list=" \
  $vere_src \
  $noun_src \
  $jets_all_src \
"

sources=$src_list

incdir=./include

CFLAGS="$CFLAGS -funsigned-char -ffast-math"
CFLAGS="$CFLAGS -DURBIT_VERSION=\"0.7.0\" -funsigned-char -ffast-math"

[ -n "$MEMORY_DEBUG" ]     && CFLAGS="$CFLAGS -DU3_MEMORY_DEBUG=1"
[ -n "$CPU_DEBUG" ]        && CFLAGS="$CFLAGS -DU3_CPU_DEBUG=1"
[ -n "$EVENT_TIME_DEBUG" ] && CFLAGS="$CFLAGS -DU3_EVENT_TIME_DEBUG=1"

case $(sed 'y/A-Z/a-z/' <<<"$host") in
  *linux*)
     CFLAGS="$CFLAGS -DU3_OS_linux=1"
     ;;
  *darwin*)
     CFLAGS="$CFLAGS -DU3_OS_osx=1"
     ;;
  *freebsd*)
     CFLAGS="$CFLAGS -DU3_OS_bsd=1"
     LDFLAGS="$LDFLAGS -lkvm"
     ;;
  *openbsd*)
     CFLAGS="$CFLAGS -DU3_OS_bsd=1"
esac

CFLAGS="$CFLAGS -DU3_OS_ENDIAN_little=1"

# TODO Error if $host is a big-endian CPU.

rm -f include/config.h && touch include/config.h

# TODO Might need to change structure of these defines.
## #pragma once
##
## #define URBIT_VERSION
##
## #define U3_OS_linux
## #define U3_OS_bsd
## #define U3_OS_osx
##
## #define U3_OS_ENDIAN_little=0
## #define U3_OS_ENDIAN_big=1
##
## #define U3_MEMORY_DEBUG=0
## #define U3_CPU_DEBUG=0
## #define U3_EVENT_TIME_DEBUG=0

LDFLAGS="$LDFLAGS -lcurl -lcrypto -lssl -lgmp -lsigsegv"
LDFLAGS="$LDFLAGS -largon2 -led25519 -lent -lh2o -lscrypt -lsni -luv -lmurmur3"
LDFLAGS="$LDFLAGS -lsecp256k1 -lsoftfloat3"

for src in $sources
do obj=$(sed 's/\.c/.o/g' <<<"$src")
   $CC -I./include $CFLAGS -c $src -o $obj
done

objs=$(sed 's/\.c/.o/g' <<<"$sources")

$CC $LDFLAGS $objs -o urbit
