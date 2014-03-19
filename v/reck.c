/* v/reck.c
**
**  This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <dirent.h>
#include <stdint.h>
#include <uv.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "f/coal.h"
#include "v/vere.h"

/* _reck_mung(): formula wrapper with gate and sample.
*/
  static u2_noun
  _reck_mung_in(u2_reck* rec_u, u2_noun gam)
  {
    u2_noun pro = u2_cn_mung(u2k(u2h(gam)), u2k(u2t(gam)));

    u2z(gam); return pro;
  }
static u2_noun
_reck_mung(u2_reck* rec_u, c3_w sec_w, u2_noun gat, u2_noun sam)
{
  u2_noun gam = u2nc(gat, sam);

  return u2_lo_soft(rec_u, 0, _reck_mung_in, gam);
}

/* u2_reck_pike(): poke with floating core.
*/
u2_noun
u2_reck_pike(u2_reck* rec_u, u2_noun ovo, u2_noun cor)
{
  u2_noun fun = u2_cn_nock(cor, u2k(u2_cx_at(42, cor)));
  u2_noun sam = u2nc(u2k(rec_u->now), ovo);

  return _reck_mung(rec_u, 0, fun, sam);
}

/* u2_reck_nick(): transform enveloped packets, [vir cor].
*/
u2_noun
u2_reck_nick(u2_reck* rec_u, u2_noun vir, u2_noun cor)
{
  if ( u2_nul == vir ) {
    return u2nt(u2_blip, vir, cor);
  }
  else {
    u2_noun i_vir = u2h(vir);
    u2_noun pi_vir, qi_vir;
    u2_noun vix;

    if ( (u2_yes == u2_cr_cell((i_vir=u2h(vir)), &pi_vir, &qi_vir)) &&
         (u2_yes == u2du(qi_vir)) &&
         (c3__hear == u2h(qi_vir)) )
    {
      u2_noun gon;

      gon = u2_reck_pike(rec_u, u2k(i_vir), cor);
      if ( u2_blip != u2h(gon) ) {
        u2z(vir);
        return gon;
      }
      else {
        u2_noun viz;

        vix = u2k(u2h(u2t(gon)));
        cor = u2k(u2t(u2t(gon)));
        u2z(gon);

        viz = u2_ckb_weld(vix, u2k(u2t(vir)));
        u2z(vir);

        return u2_reck_nick(rec_u, viz, cor);
      }
    }
    else {
      u2_noun nez = u2_reck_nick(rec_u, u2k(u2t(vir)), cor);

      if ( u2_blip != u2h(nez) ) {
        u2z(vir);
        return nez;
      } else {
        u2_noun viz;

        viz = u2nc(u2k(i_vir), u2k(u2h(u2t(nez))));
        cor = u2k(u2t(u2t(nez)));

        u2z(vir);
        u2z(nez);

        return u2nt(u2_blip, viz, cor);
      }
    }
  }
}


/* _reck_spat(): spat with toy.
*/
static u2_noun
_reck_spat(u2_reck* rec_u, u2_noun pox)
{
  return u2_do("spat", pox);
}

/* _reck_nock_poke(): call poke through hardcoded interface.
*/
static u2_noun
_reck_nock_poke(u2_reck* rec_u, u2_noun ovo)
{
  u2_noun fun = u2_cn_nock(u2k(rec_u->roc), u2k(u2_cx_at(42, rec_u->roc)));
  u2_noun sam, pro;

  sam = u2nc(u2k(rec_u->now), ovo);
#if 0
  {
    c3_c*   ovi_c = u2_cr_string(u2h(u2t(ovo)));
    u2_noun tox = _reck_spat(rec_u, u2k(u2h(ovo)));
    c3_c*   tox_c = u2_cr_string(tox);

    uL(fprintf(uH, "poke: %%%s on %s\n", ovi_c, tox_c));

    free(tox_c); free(ovi_c); u2z(tox);
  }
#endif

  pro = u2_cn_mung(fun, sam);
  return pro;
}

/* _reck_nock_peek(): call peek through hardcoded interface.
*/
static u2_noun
_reck_nock_peek(u2_reck* rec_u, u2_noun hap)
{
  u2_noun fun = u2_cn_nock(u2k(rec_u->roc), u2k(u2_cx_at(87, rec_u->roc)));
  u2_noun sam = u2nc(u2k(rec_u->now), hap);

  return u2_cn_mung(fun, sam);
}

/* _reck_nock_keep(): call wait through hardcoded interface.
*/
static u2_noun
_reck_nock_keep(u2_reck* rec_u, u2_noun hap)
{
  u2_noun fun = u2_cn_nock(u2k(rec_u->roc), u2k(u2_cx_at(4, rec_u->roc)));
  u2_noun sam = u2nc(u2k(rec_u->now), hap);

  return u2_cn_mung(fun, sam);
}

/* _reck_nock_wish(): call wish through hardcoded interface.
*/
static u2_noun
_reck_nock_wish(u2_reck* rec_u, u2_noun txt)
{
  u2_noun fun = u2_cn_nock(u2k(rec_u->roc), u2k(u2_cx_at(20, rec_u->roc)));

  return u2_cn_mung(fun, txt);
}

/* u2_reck_gate(): load a kernel function.
*/
u2_noun
u2_reck_gate(const c3_c* txt_c)
{
  u2_noun txt = u2_ci_string(txt_c);
  u2_weak gat = u2_ckd_by_get(u2k(u2_Arv->yot), u2k(txt));

  if ( u2_none == gat ) {
    gat = _reck_nock_wish(u2_Arv, u2k(txt));
    u2_Arv->yot = u2_ckd_by_put(u2_Arv->yot, u2k(txt), u2k(gat));
  }
  u2z(txt);
  return gat;
}

/* u2_reck_do(): use a kernel function.
*/
u2_noun
u2_reck_do(const c3_c* txt_c, u2_noun arg)
{
  return u2_cn_mung(u2_reck_gate(txt_c), arg);
}

/* u2_reck_wish(): noun from expression
*/
u2_noun
u2_reck_wish(u2_reck* rec_u, c3_c* str_c)
{
  return _reck_nock_wish(rec_u, u2_ci_string(str_c));
}

/* _reck_scot(): print atom.
*/
static u2_noun
_reck_scot(u2_reck* rec_u, u2_noun dim)
{
  return u2_do("scot", dim);
}

#if 0
/* _reck_spoo(): noun path from c, kind of a hack.
*/
static u2_noun
_reck_spoo(c3_c* pax_c)
{
  if ( !*pax_c ) {
    return u2_nul;
  } else {
    c3_c* ash_c = strchr(pax_c, '/');

    if ( !ash_c ) {
      return u2nc(u2_ci_string(pax_c), u2_nul);
    }
    else {
      u2_noun pan;

      *ash_c = 0;
      pan = u2_ci_string(pax_c);
      *ash_c = '/';

      return u2nc(pan, _reck_spoo(ash_c + 1));
    }
  }
}
#endif

/* _reck_load_arvo(): read an arvo file.
*/
static u2_noun
_reck_load_arvo(u2_reck* rec_u, c3_c* pax_c)
{
  c3_c ful_c[2048];

  snprintf(ful_c, 2048, "%s/%d/arvo/%s.hoon", u2_System, rec_u->kno_w, pax_c);

  return u2_walk_load(ful_c);
}

/* u2_reck_time(): set the reck time.
*/
void
u2_reck_time(u2_reck* rec_u)
{
  struct timeval tim_tv;

  gettimeofday(&tim_tv, 0);
  u2z(rec_u->now);
  rec_u->now = u2_time_in_tv(&tim_tv);

  u2z(rec_u->wen);
  rec_u->wen = _reck_scot(rec_u, u2nc(c3__da, u2k(rec_u->now)));
}

/* u2_reck_wind(): set the reck time artificially.
*/
void
u2_reck_wind(u2_reck* rec_u, u2_noun now)
{
  u2z(rec_u->now);
  rec_u->now = now;

  u2z(rec_u->wen);
  rec_u->wen = _reck_scot(rec_u, u2nc(c3__da, u2k(rec_u->now)));
}

/* u2_reck_numb(): set the instance number.
*/
void
u2_reck_numb(u2_reck* rec_u)
{
  struct timeval tp;

  gettimeofday(&tp, 0);
  rec_u->sev_l = 0x7fffffff &
                 ( ((c3_w) tp.tv_sec) ^
                   u2_mug(0x7fffffff & ((c3_w) tp.tv_usec)) ^
                   u2_mug(getpid()));
  u2z(rec_u->sen);
  rec_u->sen = _reck_scot(rec_u, u2nc(c3__uv, rec_u->sev_l));
}

#if 0
/* _reck_time_bump(): advance the reck time by a small increment.
*/
static void
_reck_time_bump(u2_reck* rec_u)
{
  c3_d bum_d = (1ULL << 48ULL);

  rec_u->now = u2_cka_add(rec_u->now, u2_ci_chubs(1, &bum_d));
}
#endif

/* u2_reck_peek(): query the reck namespace (protected).
*/
u2_noun
u2_reck_peek(u2_reck* rec_u, u2_noun hap)
{
  return _reck_nock_peek(rec_u, hap);
}

/* u2_reck_keep(): measure timer.
*/
u2_noun
u2_reck_keep(u2_reck* rec_u, u2_noun hap)
{
  return _reck_nock_keep(rec_u, hap);
}

/* _reck_init_veer(): install vane with direct poke.
*/
static void
_reck_init_veer(u2_reck* rec_u, u2_noun nam, u2_noun pax, u2_noun txt)
{
  u2_noun hoe;

  u2_rl_leap(u2_Wire, c3__rock);
  if ( 0 != (hoe = u2_cm_trap()) ) {
    u2_rl_fall(u2_Wire);

    u2_lo_sway(2, u2_ckb_flop(u2k(u2t(hoe))));
    u2z(hoe);

    exit(1);
  }
  else {
    {
      u2_noun ovo, pro, rog;

      pax = u2nc(c3__arvo, pax);
      ovo = u2nc(u2nc(c3__gold, u2_nul), u2nc(c3__veer, u2nt(nam, pax, txt)));

      pro = _reck_nock_poke(rec_u, ovo);

      u2_cm_done();

      u2_rl_fall(u2_Wire);
      rog = u2_rl_take(u2_Wire, u2t(pro));
      u2_rl_flog(u2_Wire);

      u2z(rec_u->roc);
      rec_u->roc = rog;
    }
  }
}

/* u2_reck_cold(): load full arvo with vanes, from new solid state.
*/
void
u2_reck_cold(u2_reck* rec_u, c3_w kno_w)
{
  memset(rec_u, 0, sizeof(*rec_u));
  rec_u->kno_w = kno_w;
  rec_u->rno_w = 0;
  rec_u->ent_w = 1;

  rec_u->own = 0;
  rec_u->now = 0;
  rec_u->wen = 0;
  rec_u->sen = 0;
  rec_u->roe = 0;
  rec_u->key = 0;
  rec_u->yot = 0;

  {
    u2_noun pot, eng;
    c3_c    ful_c[2048];

    if ( u2_yes == u2_Host.ops_u.nuu ) {
      snprintf(ful_c, 2048, "%s/urbit.pill", u2_Host.ops_u.hom_c);
    } else {
      snprintf(ful_c, 2048, "%s/urbit.pill", u2_Host.ops_u.cpu_c);
    }
    printf("loading %s\n", ful_c);

    if ( 0 == (pot = u2_walk_load(ful_c)) ) {
      perror(ful_c);
      exit(1);
    }

    //  Copy into basket to propitiate jet gods.
    {
      u2_rail bas_r = u2_wire_bas_r(u2_Wire);
      u2_noun tup;

      tup = u2_cke_cue(pot);
      eng = u2_rx(bas_r, tup);
      u2z(tup);
    }

    rec_u->ken = u2h(eng);
    rec_u->roc = u2t(eng);

    //  Execute from kernel to propitiate jet gods.
    {
      u2_noun fak;

      fak = u2_cn_nock(0, rec_u->ken);
      u2z(fak);
    }
  }

  //  Gates called deep when memory is wrapped.
  {
    u2z(u2_reck_gate("wash"));
    u2z(u2_reck_gate("mook"));
  }

#if 0
  rec_u->toy.rain = u2_reck_wish(rec_u, "rain");
  rec_u->toy.ream = u2_reck_wish(rec_u, "ream");
  rec_u->toy.slay = u2_reck_wish(rec_u, "slay");
  rec_u->toy.slaw = u2_reck_wish(rec_u, "slaw");
  rec_u->toy.slam = u2_reck_wish(rec_u, "slam");
  rec_u->toy.slap = u2_reck_wish(rec_u, "slap");
  rec_u->toy.slop = u2_reck_wish(rec_u, "slop");
  rec_u->toy.scot = u2_reck_wish(rec_u, "scot");
  rec_u->toy.spat = u2_reck_wish(rec_u, "spat");
  rec_u->toy.stab = u2_reck_wish(rec_u, "stab");
  rec_u->toy.turf = u2_reck_wish(rec_u, "turf");
  rec_u->toy.tuft = u2_reck_wish(rec_u, "tuft");
  rec_u->toy.wash = u2_reck_wish(rec_u, "wash");
  rec_u->toy.hoof = u2_reck_wish(rec_u, "hoof");
  rec_u->toy.mook = u2_reck_wish(rec_u, "mook");

  rec_u->toy.sham = u2_reck_wish(rec_u, "sham");
  rec_u->toy.shen = u2_reck_wish(rec_u, "en:crya");
  rec_u->toy.shed = u2_reck_wish(rec_u, "de:crya");
  rec_u->toy.cyst = u2_reck_wish(rec_u, "cyst");
  rec_u->toy.lump = u2_reck_wish(rec_u, "lump");
#endif

  u2_reck_time(rec_u);

  u2_reck_numb(rec_u);
  {
    c3_c* dyt_c = u2_cr_string(rec_u->wen);

    printf("time: %s\n", dyt_c);
    free(dyt_c);
  }
}

/* u2_reck_init(): load the reck engine, from old staged kernel.
 *
*/
void
u2_reck_init(u2_reck* rec_u, c3_w kno_w, u2_noun ken)
{
  memset(rec_u, 0, sizeof(*rec_u));
  rec_u->kno_w = kno_w;
  rec_u->rno_w = 0;

  rec_u->own = 0;
  rec_u->now = 0;
  rec_u->wen = 0;
  rec_u->sen = 0;
  rec_u->roe = 0;
  rec_u->key = 0;

  if ( kno_w > 191 ) {
    c3_assert(!"old kernel not supported");
  }
  else {
    rec_u->ken = ken;
    rec_u->roc = u2_cn_nock(0, u2k(ken));
#if 0
    rec_u->toy.rain = u2_reck_wish(rec_u, "rain");
    rec_u->toy.ream = u2_reck_wish(rec_u, "ream");
    rec_u->toy.slay = u2_reck_wish(rec_u, "slay");
    rec_u->toy.slaw = u2_reck_wish(rec_u, "slaw");
    rec_u->toy.slam = u2_reck_wish(rec_u, "slam");
    rec_u->toy.slap = u2_reck_wish(rec_u, "slap");
    rec_u->toy.slop = u2_reck_wish(rec_u, "slop");
    rec_u->toy.scot = u2_reck_wish(rec_u, "scot");
    rec_u->toy.spat = u2_reck_wish(rec_u, "spat");
    rec_u->toy.stab = u2_reck_wish(rec_u, "stab");
    rec_u->toy.turf = u2_reck_wish(rec_u, "turf");
    rec_u->toy.tuft = u2_reck_wish(rec_u, "tuft");
    rec_u->toy.wash = u2_reck_wish(rec_u, "wash");
    rec_u->toy.hoof = u2_reck_wish(rec_u, "hoof");
    rec_u->toy.mook = u2_reck_wish(rec_u, "mook");
#endif
    //  Direct poke to install tang/vanes.  Shd be in egz but isnt.
    //
    {
      _reck_init_veer(rec_u, 0,
                             u2nc(c3__zuse, u2_nul),
                             _reck_load_arvo(rec_u, "zuse"));

      _reck_init_veer(rec_u, 'a',
                             u2nc(c3__ames, u2_nul),
                             _reck_load_arvo(rec_u, "ames"));

      _reck_init_veer(rec_u, 'b',
                             u2nc(c3__batz, u2_nul),
                             _reck_load_arvo(rec_u, "batz"));

      _reck_init_veer(rec_u, 'c',
                             u2nc(c3__clay, u2_nul),
                             _reck_load_arvo(rec_u, "clay"));

      _reck_init_veer(rec_u, 'd',
                             u2nc(c3__dill, u2_nul),
                             _reck_load_arvo(rec_u, "dill"));

      _reck_init_veer(rec_u, 'e',
                             u2nc(c3__eyre, u2_nul),
                             _reck_load_arvo(rec_u, "eyre"));
    }
#if 0
    rec_u->toy.sham = u2_reck_wish(rec_u, "sham");
    rec_u->toy.shen = u2_reck_wish(rec_u, "en:crya");
    rec_u->toy.shed = u2_reck_wish(rec_u, "de:crya");
    rec_u->toy.cyst = u2_reck_wish(rec_u, "cyst");
#endif
    u2_reck_time(rec_u);
    u2_reck_numb(rec_u);
    {
      c3_c* dyt_c = u2_cr_string(rec_u->wen);

      printf("time: %s\n", dyt_c);
      free(dyt_c);
    }
  }
}

/* _reck_mole(): parse simple atomic mole.
*/
static u2_bean
_reck_mole(u2_reck* rec_u,
           u2_noun  fot,
           u2_noun  san,
           c3_d*    ato_d)
{
  u2_noun uco = u2_do("slay", san);
  u2_noun p_uco, q_uco, r_uco, s_uco;

  if ( (u2_no == u2_cr_qual(uco, &p_uco, &q_uco, &r_uco, &s_uco)) ||
       (0 != p_uco) ||
       (0 != q_uco) ||
       (u2_no == u2_sing(fot, r_uco)) )
  {
    uL(fprintf(uH, "strange mole %s\n", u2_cr_string(san)));

    u2z(fot); u2z(uco); return u2_no;
  }
  else {
    *ato_d = u2_cr_chub(0, s_uco);

    u2z(fot); u2z(uco); return u2_yes;
  }
}

/* _reck_lily(): parse little atom.
*/
static u2_bean
_reck_lily(u2_reck* rec_u, u2_noun fot, u2_noun txt, c3_l* tid_l)
{
  c3_d ato_d;

  if ( u2_no == _reck_mole(rec_u, fot, txt, &ato_d) ) {
    return u2_no;
  } else {
    if ( ato_d >= 0x80000000ULL ) {
      return u2_no;
    } else {
      *tid_l = (c3_l) ato_d;

      return u2_yes;
    }
  }
}

/* _reck_kick_term(): apply terminal outputs.
*/
static u2_bean
_reck_kick_term(u2_reck* rec_u, u2_noun pox, c3_l tid_l, u2_noun fav)
{
  u2_noun p_fav;

  if ( u2_no == u2du(fav) ) {
    u2z(pox); u2z(fav); return u2_no;
  }
  else switch ( u2h(fav) ) {
    default: u2z(pox); u2z(fav); return u2_no;
    case c3__bbye:
    {
      u2_reck_sync(rec_u);
      // u2_reck_plan(rec_u, pox, u2nc(c3__helo, u2_nul));

      u2z(pox); u2z(fav); return u2_yes;
    } break;

    case c3__blit: p_fav = u2t(fav);
    {
      u2_term_ef_blit(tid_l, u2k(p_fav));

      u2z(pox); u2z(fav); return u2_yes;
    } break;

    case c3__logo:
    {
      u2_Host.liv = u2_no;

      u2z(pox); u2z(fav); return u2_yes;
    } break;

    case c3__init: p_fav = u2t(fav);
    {
      rec_u->own = u2nc(u2k(p_fav), rec_u->own);

      u2_unix_ef_init(u2k(p_fav));

      // uL(fprintf(uH, "kick: init: %d\n", p_fav));
      u2z(pox); u2z(fav); return u2_yes;
    } break;

    case c3__send: {
      u2_noun lan = u2k(u2h(u2t(fav)));
      u2_noun pac = u2k(u2t(u2t(fav)));

      u2_ames_ef_send(lan, pac);
      u2z(pox); u2z(fav); return u2_yes;
    } break;
  }
  c3_assert(!"not reached"); return 0;
}

/* _reck_kick_http(): apply http effects.
*/
static u2_bean
_reck_kick_http(u2_reck* rec_u,
                u2_noun  pox,
                c3_l     sev_l,
                c3_l     coq_l,
                c3_l     seq_l,
                u2_noun  fav)
{
  u2_noun p_fav, q_fav;

  if ( u2_no == u2du(fav) ) {
    u2z(pox); u2z(fav); return u2_no;
  }
  else switch ( u2h(fav) ) {
    default: u2z(pox); u2z(fav); return u2_no;

    case c3__thus: p_fav = u2h(u2t(fav)); q_fav = u2t(u2t(fav));
    {
      u2_cttp_ef_thus(u2_cr_word(0, p_fav), u2k(q_fav));

      u2z(pox); u2z(fav);
      return u2_yes;
    }
    case c3__thou: p_fav = u2t(fav);
    {
      u2_http_ef_thou(sev_l, coq_l, seq_l, u2k(p_fav));

      u2z(pox); u2z(fav);
      return u2_yes;
    } break;
  }
  c3_assert(!"not reached"); return u2_no;
}

/* _reck_kick_sync(): apply sync outputs.
*/
static u2_bean
_reck_kick_sync(u2_reck* rec_u, u2_noun pox, u2_noun fav)
{
  switch ( u2h(fav) ) {
    default: break;
    case c3__ergo: {
      u2_noun who = u2k(u2h(u2t(fav)));
      u2_noun syd = u2k(u2h(u2t(u2t(fav))));
      u2_noun rel = u2k(u2t(u2t(u2t(fav))));

      u2_unix_ef_ergo(who, syd, rel);
      u2z(pox); u2z(fav); return u2_yes;
    } break;
  }

  //  XX obviously not right!
  //
  u2z(pox); u2z(fav); return u2_no;
}

/* _reck_kick_ames(): apply packet network outputs.
*/
static u2_bean
_reck_kick_ames(u2_reck* rec_u, u2_noun pox, u2_noun fav)
{
  u2_noun p_fav;

  switch ( u2h(fav) ) {
    default: break;
    case c3__init: p_fav = u2t(fav);
    {
      rec_u->own = u2nc(u2k(p_fav), rec_u->own);

      u2_unix_ef_init(u2k(p_fav));

      // uL(fprintf(uH, "kick: init: %d\n", p_fav));
      u2z(pox); u2z(fav); return u2_yes;
    }
    case c3__send: {
      u2_noun lan = u2k(u2h(u2t(fav)));
      u2_noun pac = u2k(u2t(u2t(fav)));

      u2_ames_ef_send(lan, pac);
      u2z(pox); u2z(fav); return u2_yes;
    } break;
  }
  u2z(pox); u2z(fav); return u2_no;
}

/* _reck_kick_spec(): apply an effect, by path.
*/
static u2_bean
_reck_kick_spec(u2_reck* rec_u, u2_noun pox, u2_noun fav)
{
  u2_noun i_pox, t_pox;

  if ( (u2_no == u2_cr_cell(pox, &i_pox, &t_pox)) ||
       ((i_pox != c3__gold) && (i_pox != c3__iron) && (i_pox != c3__lead)) )
  {
    u2z(pox); u2z(fav); return u2_no;
  } else {
    u2_noun it_pox, tt_pox;

    if ( (u2_no == u2_cr_cell(t_pox, &it_pox, &tt_pox)) ) {
      u2z(pox); u2z(fav); return u2_no;
    }
    else switch ( it_pox ) {
      default: return u2_no;

      case c3__http: {
        u2_noun pud = tt_pox;
        u2_noun p_pud, t_pud, tt_pud, q_pud, r_pud, s_pud;
        c3_l    sev_l, coq_l, seq_l;

        if ( (u2_no == u2_cr_cell(pud, &p_pud, &t_pud)) ||
             (u2_no == _reck_lily(rec_u, c3__uv, u2k(p_pud), &sev_l)) )
        {
          u2z(pox); u2z(fav); return u2_no;
        }

        if ( u2_nul == t_pud ) {
          coq_l = seq_l = 0;
        }
        else {
          if ( (u2_no == u2_cr_cell(t_pud, &q_pud, &tt_pud)) ||
               (u2_no == _reck_lily(rec_u, c3__ud, u2k(q_pud), &coq_l)) )
          {
            u2z(pox); u2z(fav); return u2_no;
          }

          if ( u2_nul == tt_pud ) {
            seq_l = 0;
          } else {
            if ( (u2_no == u2_cr_cell(tt_pud, &r_pud, &s_pud)) ||
                 (u2_nul != s_pud) ||
                 (u2_no == _reck_lily(rec_u, c3__ud, u2k(r_pud), &seq_l)) )
            {
              u2z(pox); u2z(fav); return u2_no;
            }
          }
        }
        return _reck_kick_http(rec_u, pox, sev_l, coq_l, seq_l, fav);
      } break;

      case c3__clay:
      case c3__sync: {
        return _reck_kick_sync(rec_u, pox, fav);
      } break;

      case c3__ames: {
        if ( (u2_nul != tt_pox) ) {
          u2z(pox); u2z(fav); return u2_no;
        }
        else {
          return _reck_kick_ames(rec_u, pox, fav);
        }
      } break;

      case c3__term: {
        u2_noun pud = tt_pox;
        u2_noun p_pud, q_pud;
        c3_l    tid_l;

        if ( (u2_no == u2_cr_cell(pud, &p_pud, &q_pud)) ||
             (u2_nul != q_pud) ||
             (u2_no == _reck_lily(rec_u, c3__ud, u2k(p_pud), &tid_l)) )
        {
          uL(fprintf(uH, "term: bad tire\n"));
          u2z(pox); u2z(fav); return u2_no;
        } else {
          return _reck_kick_term(rec_u, pox, tid_l, fav);
        }
      } break;
    }
  }
  c3_assert(!"not reached");
  return u2_no;
}

/* _reck_kick_norm(): non path-specific effect handling.
*/
static u2_bean
_reck_kick_norm(u2_reck* rec_u, u2_noun pox, u2_noun fav)
{
  if ( u2_no == u2du(fav) ) {
    u2z(pox); u2z(fav); return u2_no;
  }
  else switch ( u2h(fav) ) {
    default: u2z(pox); u2z(fav); return u2_no;

    case c3__vega:
    {
      // uL(fprintf(uH, "reset\n"));
      u2z(pox); u2z(fav);

      //  u2_ds_wipe(u2_Wire);  //  doesn't work

      return u2_yes;
    }
    case c3__exit:
    {
      uL(fprintf(uH, "<<<goodbye>>>\n"));
      u2_lo_bail(rec_u);

      u2z(pox); u2z(fav); return u2_yes;
    } break;

    case c3__send:
    {
      u2_noun lan = u2k(u2h(u2t(fav)));
      u2_noun pac = u2k(u2t(u2t(fav)));

      u2_ames_ef_send(lan, pac);
      u2z(pox); u2z(fav); return u2_yes;
    } break;
  }
  c3_assert(!"not reached"); return u2_no;
  u2z(pox); u2z(fav); return u2_no;
}

/* u2_reck_kick(): handle effect.
*/
void
u2_reck_kick(u2_reck* rec_u, u2_noun ovo)
{
  if ( (u2_no == _reck_kick_spec(rec_u, u2k(u2h(ovo)), u2k(u2t(ovo)))) &&
       (u2_no == _reck_kick_norm(rec_u, u2k(u2h(ovo)), u2k(u2t(ovo)))) )
  {
    u2_noun tox = _reck_spat(rec_u, u2k(u2h(ovo)));

#if 0
    if ( (c3__warn != u2h(u2t(ovo))) &&
         (c3__text != u2h(u2t(ovo))) &&
         (c3__note != u2h(u2t(ovo))) )
#endif
#if 1
    if ( (c3__crud == u2h(u2t(ovo))) )
#if 0
         (c3__talk == u2h(u2t(ovo))) ||
         (c3__helo == u2h(u2t(ovo))) ||
         (c3__init == u2h(u2t(ovo))) )
#endif
    {
      u2_reck_plan(rec_u, u2nt(c3__gold, c3__term, u2_nul),
                          u2nc(c3__flog, u2k(u2t(ovo))));
    }
    else {
      uL(fprintf(uH, "kick: lost %%%s on %s\n",
                     u2_cr_string(u2h(u2t(ovo))),
                     u2_cr_string(tox)));
#if 0
      if ( c3__hear == u2h(u2t(ovo)) ) {
        c3_assert(0);
      }
#endif
    }
#endif
    u2z(tox);
  }
  u2z(ovo);
}

/* u2_reck_poke(): insert and apply an input ovum (protected).
*/
u2_noun
u2_reck_poke(u2_reck* rec_u, u2_noun ovo)
{
  return _reck_nock_poke(rec_u, ovo);
}

/* u2_reck_sync(): poll and apply sync events (protected).
*/
void
u2_reck_sync(u2_reck* rec_u)
{
#if 0
  u2_reck_plow(rec_u, u2_sync_reck(rec_u));
#endif
}

/* u2_reck_boot(): boot the reck engine (unprotected).
*/
void
u2_reck_boot(u2_reck* rec_u)
{
  u2_noun hoe;

  memset(rec_u, 0, sizeof *rec_u);
  if ( 0 != (hoe = u2_cm_trap()) ) {
    u2_cm_purge();

    u2_lo_sway(2, u2_ckb_flop(u2k(u2t(hoe))));
    u2z(hoe);

    exit(1);
  }
  else {
    u2_reck_cold(rec_u, u2_Host.kno_w);
    u2_cm_done();

    u2_cm_purge();
  }
}

/* u2_reck_http_request(): hear http request on channel (unprotected).
*/
void
u2_reck_http_request(u2_reck* rec_u, u2_bean sec, u2_noun pox, u2_noun req)
{
  // uL(fprintf(uH, "http: request\n"));
  u2_reck_plan(rec_u, pox, u2nq(c3__this, sec, 0, req));
}

/* u2_reck_prick(): query the reck namespace (unprotected).
*/
u2_noun
u2_reck_prick(u2_reck* rec_u, u2_noun hap)
{
  u2_noun hoe;
  u2_noun que;

  if ( 0 != (hoe = u2_cm_trap()) ) {
    u2_cm_purge();

    u2_lo_sway(2, u2_ckb_flop(u2k(u2t(hoe))));
    u2z(hoe);

    return u2_nul;
  }
  else {
    que = u2_reck_peek(rec_u, hap);
    u2_cm_done();

    u2_cm_purge();
  }
  return que;
}

/* u2_reck_plan(): queue ovum (external).
*/
void
u2_reck_plan(u2_reck* rec_u,
             u2_noun  pax,
             u2_noun  fav)
{
  if ( u2_raty_lead == u2R->typ_e ) {
    u2_noun egg = u2nc(pax, fav);
    rec_u->roe = u2nc(u2nc(u2_nul, egg), rec_u->roe);
  }
  else {
    c3_c* hed_c = u2_cr_string(u2h(u2t(pax)));
    uL(fprintf(uH, "reck: dropping roe from %s\n", hed_c));
    free(hed_c);
    u2z(pax); u2z(fav);
  }
}

/* u2_reck_plow(): queue multiple ova (external).
*/
void
u2_reck_plow(u2_reck* rec_u, u2_noun ova)
{
  u2_noun ovi = ova;

  while ( u2_nul != ovi ) {
    u2_noun ovo=u2h(ovi);

    u2_reck_plan(rec_u, u2k(u2h(ovo)), u2k(u2t(ovo)));
    ovi = u2t(ovi);
  }
  u2z(ova);
}
