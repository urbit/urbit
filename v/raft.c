/* v/raft.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <uv.h>

#include "all.h"
#include "v/vere.h"

/* _raft_readname(): parse a raft host:port peer name.
*/
static u2_bean
_raft_readname(u2_ropt* rop_u, const c3_c* str_c, c3_w siz_w)
{
  u2_rnam* nam_u = malloc(sizeof(*nam_u));
  c3_c*    col_c;
  c3_w     por_w;
  c3_w     nam_w;

  nam_u->str_c = malloc(siz_w + 1);
  strncpy(nam_u->str_c, str_c, siz_w);
  nam_u->str_c[siz_w] = '\0';
  //fprintf(stderr, "raft: peer %s\n", nam_u->str_c);

  if ( 0 == (col_c = strchr(nam_u->str_c, ':')) ) {
    fprintf(stderr, "raft: invalid name %s\n", str_c);
    return u2_no;
  }
  else {
    nam_w = col_c - nam_u->str_c;
    nam_u->nam_c = malloc(nam_w + 1);
    strncpy(nam_u->nam_c, nam_u->str_c, nam_w);
    nam_u->nam_c[nam_w] = '\0';

    por_w = atol(col_c + 1);
    if ( !(por_w > 0 && por_w < 65536) ) {
      fprintf(stderr, "raft: invalid port '%s'\n", col_c + 1);
      return u2_no;
    }
    else nam_u->por_s = por_w;
    //fprintf(stderr, "raft: peer %s:%d\n", nam_u->nam_c, nam_u->por_s);

    nam_u->nex_u = rop_u->nam_u;
    rop_u->nam_u = nam_u;
    return u2_yes;
  }
}

u2_bean
u2_raft_readopt(u2_ropt* rop_u, const c3_c* arg_c)
{
  c3_c* com_c;

  while ( 0 != (com_c = strchr(arg_c, ',')) ) {
    if ( u2_no == _raft_readname(rop_u, arg_c, com_c - arg_c) ) {
      return u2_no;
    } else arg_c = com_c + 1;
  }
  return _raft_readname(rop_u, arg_c, strlen(arg_c));
}

/* _raft_election_rand(): pseudorandom component of election timeout.
*/
static c3_w
_raft_election_rand()
{
  return ((float) rand() / RAND_MAX) * 150;
}

static void
_raft_listen_cb(uv_stream_t* wax_u, c3_i sas_i)
{
}

static void
_raft_time_cb(uv_timer_t* tim_u, c3_i sas_i)
{
  //u2_raft* raf_u = tim_u->data;
  //uL(fprintf(uH, "raft: time\n"));
}

/* _raft_foll_init(): begin, follower mode.
*/
static void
_raft_foll_init(u2_raft* raf_u)
{
  uL(fprintf(uH, "raft: starting follower\n"));

  if ( 0 != uv_tcp_init(u2L, &raf_u->wax_u) ) {
    uL(fprintf(uH, "raft: init: %s\n", uv_strerror(uv_last_error(u2L))));
    c3_assert(0);
  }

  // Bind the listener.
  {
    struct sockaddr_in add_u;

    memset(&add_u, 0, sizeof(add_u));
    add_u.sin_family = AF_INET;
    add_u.sin_addr.s_addr = htonl(INADDR_ANY);
    add_u.sin_port = htons(u2_Host.ops_u.rop_u.por_s);

    if ( 0 != uv_tcp_bind(&raf_u->wax_u, add_u) ) {
      uL(fprintf(uH, "raft: bind: %s\n", uv_strerror(uv_last_error(u2L))));
      c3_assert(0);
    }
    else {
      if ( 0 != uv_listen((uv_stream_t*)&raf_u->wax_u, 16, _raft_listen_cb) ) {
        uL(fprintf(uH, "raft: listen: %s\n", uv_strerror(uv_last_error(u2L))));
        c3_assert(0);
      }
      else {
        uL(fprintf(uH, "raft: on TCP %d\n", u2_Host.ops_u.rop_u.por_s));
      }
    }
  }

  // Start the initial election timeout.
  {
    uv_timer_init(u2L, &raf_u->tim_u);
    raf_u->tim_u.data = raf_u;
    uv_timer_start(&raf_u->tim_u, _raft_time_cb, _raft_election_rand(), 0);
  }
}

/* _raft_lone_init(): begin, single-instance mode.
*/
static void
_raft_lone_init(u2_raft* raf_u)
{
  uL(fprintf(uH, "raft: single-instance mode\n"));
}

void
u2_raft_io_init()
{
  u2_raft* raf_u = u2R;

  if ( 0 == u2_Host.ops_u.rop_u.por_s ) {
    _raft_lone_init(raf_u);
  }
  else {
    _raft_foll_init(raf_u);
  }
}

/* _raft_sure(): apply and save an input ovum and its result.
*/
static void
_raft_sure(u2_reck* rec_u, u2_noun ovo, u2_noun vir, u2_noun cor)
{
  //  Whatever worked, save it.  (XX - should be concurrent with execute.)
  //  We'd like more events that don't change the state but need work here.
  {
    u2_mug(cor);
    u2_mug(rec_u->roc);

    if ( u2_no == u2_sing(cor, rec_u->roc) ) {
      rec_u->roe = u2nc(u2nc(u2_nul, ovo), rec_u->roe);

      u2z(rec_u->roc);
      rec_u->roc = cor;
    }
    else {
      u2z(ovo);
      rec_u->roe = u2nc(u2_nul, rec_u->roe);

      u2z(cor);
    }
  }

  rec_u->vir = u2nc(vir, rec_u->vir);
}

/* _raft_lame(): handle an application failure.
*/
static void
_raft_lame(u2_reck* rec_u, u2_noun ovo, u2_noun why, u2_noun tan)
{
  u2_noun bov, gon;

#if 1
  {
    c3_c* oik_c = u2_cr_string(u2h(u2t(ovo)));

    // uL(fprintf(uH, "lame: %s\n", oik_c));
    free(oik_c);
  }
#endif

  //  Formal error in a network packet generates a hole card.
  //
  //  There should be a separate path for crypto failures,
  //  to prevent timing attacks, but isn't right now.  To deal
  //  with a crypto failure, just drop the packet.
  //
  if ( (c3__exit == why) && (c3__hear == u2h(u2t(ovo))) ) {
    u2_lo_punt(2, u2_ckb_flop(u2k(tan)));

    bov = u2nc(u2k(u2h(ovo)), u2nc(c3__hole, u2k(u2t(u2t(ovo)))));
    u2z(why);
  }
  else {
    bov = u2nc(u2k(u2h(ovo)), u2nt(c3__crud, why, u2k(tan)));
    u2_hevn_at(lad) = u2_nul;
  }
  // u2_lo_show("data", u2k(u2t(u2t(ovo))));

  u2z(ovo);

  gon = u2_lo_soft(rec_u, 0, u2_reck_poke, u2k(bov));
  if ( u2_blip == u2h(gon) ) {
    _raft_sure(rec_u, bov, u2k(u2h(u2t(gon))), u2k(u2t(u2t(gon))));

    u2z(gon);
  }
  else {
    u2z(gon);
    {
      u2_noun vab = u2nc(u2k(u2h(bov)),
                         u2nc(c3__warn, u2_ci_tape("crude crash!")));
      u2_noun nog = u2_lo_soft(rec_u, 0, u2_reck_poke, u2k(vab));

      if ( u2_blip == u2h(nog) ) {
        _raft_sure(rec_u, vab, u2k(u2h(u2t(nog))), u2k(u2t(u2t(nog))));
        u2z(nog);
      }
      else {
        u2z(nog);
        u2z(vab);

        uL(fprintf(uH, "crude: all delivery failed!\n"));
      }
    }
  }
}

/* _raft_punk(): insert and apply an input ovum (unprotected).
*/
static void
_raft_punk(u2_reck* rec_u, u2_noun ovo)
{
  // c3_c* txt_c = u2_cr_string(u2h(u2t(ovo)));
  c3_w sec_w;
  // static c3_w num_w;
  u2_noun gon;

  // uL(fprintf(uH, "punk: %s: %d\n", u2_cr_string(u2h(u2t(ovo))), num_w++));

  //  XX this is wrong - the timer should be on the original hose.
  //
  if ( (c3__term == u2h(u2t(u2h(ovo)))) ||
       (c3__batz == u2h(u2t(u2h(ovo)))) ) {
    sec_w = 0;
  } else sec_w = 60;

  //  Control alarm loops.
  //
  if ( c3__wake != u2h(u2t(ovo)) ) {
    u2_Host.beh_u.run_w = 0;
  }

  gon = u2_lo_soft(rec_u, sec_w, u2_reck_poke, u2k(ovo));

  if ( u2_blip != u2h(gon) ) {
    u2_noun why = u2k(u2h(gon));
    u2_noun tan = u2k(u2t(gon));

    u2z(gon);
    _raft_lame(rec_u, ovo, why, tan);
  }
  else {
    u2_noun vir = u2k(u2h(u2t(gon)));
    u2_noun cor = u2k(u2t(u2t(gon)));
    u2_noun nug;

    u2z(gon);
    nug = u2_reck_nick(rec_u, vir, cor);

    if ( u2_blip != u2h(nug) ) {
      u2_noun why = u2k(u2h(nug));
      u2_noun tan = u2k(u2t(nug));

      u2z(nug);
      _raft_lame(rec_u, ovo, why, tan);
    }
    else {
      vir = u2k(u2h(u2t(nug)));
      cor = u2k(u2t(u2t(nug)));

      u2z(nug);
      _raft_sure(rec_u, ovo, vir, cor);
    }
  }
  // uL(fprintf(uH, "punk oot %s\n", txt_c));
}

static void
_raft_comm(u2_reck* rec_u, c3_w bid_w)
{
  u2_cart* egg_u;

  u2_lo_open();

  egg_u = rec_u->ova.egg_u;
  while ( egg_u ) {
    if ( egg_u->ent_w <= bid_w ) {
      egg_u->did = u2_yes;
      egg_u->cit = u2_yes;
    } else break;
    egg_u = egg_u->nex_u;
  }
  u2_lo_shut(u2_no);
}

static c3_w
_raft_push(u2_raft* raf_u, c3_w* bob_w, c3_w len_w)
{
  static c3_w bid_w = 0;

  if ( 0 != bob_w ) {
    c3_assert(0 < len_w);
    bid_w = u2_sist_pack(u2A, c3__ov, bob_w, len_w);
  }
  else c3_assert(0 == len_w);

  return bid_w;
}

/* u2_raft_work(): work in rec_u.
*/
void
u2_raft_work(u2_reck* rec_u)
{
  u2_cart* egg_u;
  u2_noun  ova;
  u2_noun  vir;
  u2_noun  nex;

  //  Apply effects from just-committed events, and delete finished events.
  //
  while ( rec_u->ova.egg_u ) {
    egg_u = rec_u->ova.egg_u;

    if ( u2_yes == egg_u->did ) {
      vir = egg_u->vir;

      if ( egg_u == rec_u->ova.geg_u ) {
        c3_assert(egg_u->nex_u == 0);
        rec_u->ova.geg_u = rec_u->ova.egg_u = 0;
        free(egg_u);
      }
      else {
        c3_assert(egg_u->nex_u != 0);
        rec_u->ova.egg_u = egg_u->nex_u;
        free(egg_u);
      }

      if ( u2_yes == egg_u->cit ) {
        while ( u2_nul != vir ) {
          u2_noun ovo = u2k(u2h(vir));
          nex = u2k(u2t(vir));
          u2z(vir); vir = nex;

          u2_reck_kick(rec_u, ovo);
        }
      }
      else {
        //  We poked an event, but Raft failed to persist it.
        //  TODO: gracefully recover.
        uL(fprintf(uH, "vere: event executed but not persisted\n"));
        c3_assert(0);
      }
    }
    else break;
  }

  //  Poke pending events, leaving the poked events and errors on rec_u->roe.
  //
  {
    if ( 0 == u2R->lug_u.len_d ) {
      return;
    }
    ova = u2_ckb_flop(rec_u->roe);
    rec_u->roe = u2_nul;

    c3_assert(rec_u->vir == u2_nul);

    while ( u2_nul != ova ) {
      _raft_punk(rec_u, u2k(u2t(u2h(ova))));
      nex = u2k(u2t(ova));
      u2z(ova); ova = nex;
    }
  }

  //  Cartify, jam, and encrypt this batch of events. Take a number, Raft will
  //  be with you shortly.
  {
    c3_w    bid_w;
    c3_w    len_w;
    c3_w*   bob_w;
    u2_noun ron;
    u2_noun ovo;

    ova = u2_ckb_flop(rec_u->roe);
    vir = u2_ckb_flop(rec_u->vir);
    rec_u->roe = u2_nul;
    rec_u->vir = u2_nul;

    while ( u2_nul != ova ) {
      c3_assert(u2_nul != vir);
      egg_u = malloc(sizeof(*egg_u));
      egg_u->nex_u = 0;
      egg_u->cit = u2_no;
      egg_u->did = u2_no;

      ovo = u2k(u2h(ova));
      nex = u2k(u2t(ova));
      u2z(ova); ova = nex;

      egg_u->vir = u2k(u2h(vir));
      nex = u2k(u2t(vir));
      u2z(vir); vir = nex;

      if ( u2_nul != ovo ) {
        ron = u2_cke_jam(u2k(u2t(ovo)));
        u2z(ovo);
        c3_assert(rec_u->key);
        ron = u2_dc("en:crya", u2k(rec_u->key), ron);

        len_w = u2_cr_met(5, ron);
        bob_w = malloc(len_w * 4L);
        u2_cr_words(0, len_w, bob_w, ron);
        u2z(ron);

        bid_w = _raft_push(u2R, bob_w, len_w);
        egg_u->ent_w = bid_w;
      }
      else {    //  XX
        egg_u->ent_w = _raft_push(u2R, 0, 0);
      }

      if ( 0 == rec_u->ova.geg_u ) {
        c3_assert(0 == rec_u->ova.egg_u);
        rec_u->ova.geg_u = rec_u->ova.egg_u = egg_u;
      }
      else {
        c3_assert(0 == rec_u->ova.geg_u->nex_u);
        rec_u->ova.geg_u->nex_u = egg_u;
        rec_u->ova.geg_u = egg_u;
      }
      //  TODO remove this
      _raft_comm(u2A, bid_w);
    }
  }
}
