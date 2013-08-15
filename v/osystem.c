/* v/system.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <stdint.h>
#include <ev.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "v/vere.h"

/* u2_ve_tag: simple string from stage number.
*/
u2_noun
u2_ve_tag(c3_w a_w)
{
  c3_c a_c[] = {0, 0, 0, 0};

  sprintf(a_c, "%d", a_w);
  return u2_ci_string(a_c);
}

/* u2_ve_bone(): direct execution from kernel, using ":!%".
*/
u2_noun
u2_ve_bone(c3_c *bon_c)
{
  c3_w    kno_w = u2_Host.kno_w;

  if ( 0 == u2_Host.ver_e[kno_w].ken ) {
    return u2_cm_foul("bone");
  } else {
    u2_noun ken = u2_ct(u2_Host.ver_e[kno_w].ken);
    u2_noun src, fol, cor;

    //  Obviously heavily syntax dependent.
    //  May vary in future by kernel number.
    {
      c3_c *src_c = alloca(strlen(bon_c) + 9);

      strcpy(src_c, "=>  !%  ");
      strcat(src_c, bon_c);

      src = u2_ci_string(src_c);
    }
    fol = u2_cn_nock(src, ken);
    cor = u2_cn_nock(0, fol);

    return cor;
  }
}

/* u2_ve_seed(): return kernel seed.
*/
u2_noun
u2_ve_seed()
{
  c3_assert(0 != u2_ve_at()->toy.seed);

  return u2_ct(u2_ve_at()->toy.seed);
}

/* u2_ve_slap(): use slap gate. 
*/
u2_noun
u2_ve_slap(u2_noun vax, u2_noun gen)
{
  c3_assert(0 != u2_ve_at()->toy.slap);

  return u2_cn_mung(u2_ct(u2_ve_at()->toy.slap), u2nc(vax, gen));
}

/* u2_ve_slam(): use slam gate. 
*/
u2_noun
u2_ve_slam(u2_noun gat, u2_noun sam)
{
  c3_assert(0 != u2_ve_at()->toy.slam);

  return u2_cn_mung(u2_ct(u2_ve_at()->toy.slam), u2nc(gat, sam));
}

/* u2_ve_slop(): use slop gate. 
*/
u2_noun
u2_ve_slop(u2_noun hed, u2_noun tal)
{
  c3_assert(0 != u2_ve_at()->toy.slop);

  return u2_cn_mung(u2_ct(u2_ve_at()->toy.slop), u2nc(hed, tal));
}

/* u2_ve_sell(): use sell gate. 
*/
u2_noun
u2_ve_sell(u2_noun vax)
{
  c3_assert(0 != u2_ve_at()->toy.sell);

  return u2_cn_mung(u2_ct(u2_ve_at()->toy.sell), vax);
}

/* u2_ve_skol(): use skol gate. 
*/
u2_noun
u2_ve_skol(u2_noun typ)
{
  c3_assert(0 != u2_ve_at()->toy.skol);

  return u2_cn_mung(u2_ct(u2_ve_at()->toy.skol), typ);
}

/* u2_ve_scot(): use scot (atom printer).
*/
u2_noun
u2_ve_scot(u2_noun fom, u2_noun dat)
{
  c3_assert(0 != u2_ve_at()->toy.scot);

  return u2_cn_mung(u2_ct(u2_ve_at()->toy.scot), u2nc(fom, dat));
}

/* u2_ve_ream(): use ream gate.
*/
u2_noun
u2_ve_ream(u2_noun txt)
{
  c3_assert(0 != u2_ve_at()->toy.ream);

  return u2_cn_mung(u2_ct(u2_ve_at()->toy.ream), txt);
}

/* u2_ve_rain(): use rain gate.
*/
u2_noun
u2_ve_rain(u2_noun bon, u2_noun txt)
{
  c3_assert(0 != u2_ve_at()->toy.rain);

  return u2_cn_mung(u2_ct(u2_ve_at()->toy.rain), u2nc(bon, txt));
}

/* u2_ve_slac(): slap with feature as C string.  highly convenient.
*/
u2_noun
u2_ve_slac(u2_noun vax, const c3_c* sam_c)
{
  if ( u2_Host.kno_w >= 214 ) {
    return u2_ve_slap(vax, u2_ci_string(sam_c));
  } else {
    if ( u2_Host.kno_w >= 212 ) {
      return u2_ve_slap(vax, u2nc(c3__cnhs, u2_ci_string(sam_c)));
    } else {
      return u2_ve_slap(vax, u2nc(c3__cnbc, u2_ci_string(sam_c)));
    }
  }
}

/* u2_ve_slan(): slap with gene as C string.
*/
u2_noun
u2_ve_slan(u2_noun vax, const c3_c* sam_c)
{
  return u2_ve_slap(vax, u2_ve_ream(u2_ci_string(sam_c)));
}

/* u2_ve_use(): use specified tool.
*/
u2_noun 
u2_ve_use(const c3_c* wit_c)
{
  u2_steg* ver_e = &u2_Host.ver_e[u2_Host.kno_w];
  u2_noun  wit   = u2_ci_string(wit_c);
  u2_weak  tul   = u2_ckd_by_get(u2_ct(ver_e->tul), wit);

  if ( u2_none == tul ) {
    u2_cm_bean(u2nc(u2_Host.kno_w, u2_ci_string(wit_c)));

    fprintf(stderr, "no %s, %d\n", wit_c, u2_Host.kno_w);
    c3_assert(0);
    return u2_cm_foul("vere-tool");
  }
  else return tul;
}

/* u2_ve_set(): set specified tool.
*/
void
u2_ve_set(const c3_c* wit_c, u2_noun zam)
{
  u2_steg* ver_e = &u2_Host.ver_e[u2_Host.kno_w];
  u2_noun  wit   = u2_ci_string(wit_c);

  ver_e->tul = u2_ckd_by_put(ver_e->tul, wit, zam);
}

/* u2_ve_hard(): use standard tool gate without type check.
*/
u2_noun
u2_ve_hard(const c3_c* wit_c, c3_c* fun_c, u2_noun arg)
{           
  u2_noun tul = u2_ve_use(wit_c);
  u2_noun gat = u2_ve_slac(tul, fun_c);
  u2_noun cor = u2_ct(u2t(gat));

  u2z(gat);
  return u2_cn_mung(cor, arg);
}

/* u2_ve_step(): replace standard tool gate with new core.
*/
void
u2_ve_step(const c3_c* wit_c, u2_noun wip)
{
  u2_steg* ver_e = &u2_Host.ver_e[u2_Host.kno_w];
  u2_noun  wit   = u2_ci_string(wit_c);
  u2_noun  old   = u2_ckd_by_get(u2k(ver_e->tul), u2k(wit));
  u2_noun  wop   = u2nc(u2k(u2h(old)), wip);

  ver_e->tul = u2_ckd_by_put(ver_e->tul, wit, wop);
  u2z(old);
}

/* u2_ve_soft(): use standard tool gate against vase.
*/
u2_noun
u2_ve_soft(const c3_c* wit_c, c3_c* fun_c, u2_noun vos)
{
  u2_noun tul = u2_ve_use(wit_c);
  u2_noun gat = u2_ve_slac(tul, fun_c);

  return u2_ve_slam(gat, vos);
}

/* u2_ve_meat(): return noun of vase.
*/
u2_noun 
u2_ve_meat(u2_noun vos)
{
  u2_noun myt = u2_ct(u2t(vos));

  u2_cz(vos);
  return myt;
}

/* u2_ve_zeus(): prayer to internal file path.  Return unit.
*/
u2_noun
u2_ve_zeus(u2_noun hap)
{
  if ( (u2_no == u2du(hap)) || (c3_s2('.', '~') != u2h(hap)) ) {
    u2z(hap);
    return u2_nul;
  } 
  else {
    u2_noun hat = u2k(u2t(hap));
    u2_noun tah = u2_ckb_flop(hat);
    u2_noun dat = u2_ve_file("watt", tah);

    if ( u2_none == dat ) {
      return u2_nul;
    } else {
      return u2nc(u2_nul, dat);
    }
  }
}
