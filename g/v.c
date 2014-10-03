/* g/v.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include "all.h"

/* _cv_nock_wish(): call wish through hardcoded interface.
*/
static u3_noun
_cv_nock_wish(u3_noun txt)
{
  u3_noun fun = u3_cn_nock_on(u3k(u3A->roc), u3k(u3_cx_at(20, u3A->roc)));

  return u3_cn_slam_on(fun, txt);
}

/* u3_cv_make(): make a new pier and instantiate pill.
*/
void
u3_cv_make(c3_c* pas_c)
{
  u3_noun sys = u3_cke_cue(u3_cm_file(pas_c));

  printf("cv_make: loaded pill %s, as %x\n", pas_c, u3_cr_mug(sys));

  u3A->ken = u3k(u3h(sys));
  u3A->roc = u3k(u3t(sys));

  printf("cv_make: kernel %x, core %x\n", 
         u3_cr_mug(u3A->ken), u3_cr_mug(u3A->roc));
}

/* u3_cv_jack(): execute kernel formula to bind jets.
*/
void
u3_cv_jack(void)
{
  u3_noun cor; 
  
  printf("cv_jack: activating kernel %x\n", u3_cr_mug(u3A->ken));
  cor = u3_cn_nock_on(0, u3k(u3A->ken));
  printf("cv_jack: activated\n");

  u3z(cor);
}

/* u3_cv_start(): start time.
*/
void
u3_cv_start(u3_noun now)
{
  u3_cv_time(now);
  u3_cv_numb();

  {
    c3_c* wen_c = u3_cr_string(u3A->wen);

    printf("cv_start: time: %s\n", wen_c);
    free(wen_c);
  }
}

/* u3_cv_gate(): load a kernel function.
*/
u3_noun
u3_cv_gate(const c3_c* txt_c)
{
  u3_cs_road* rod_u;
  u3_noun     gat;

  rod_u = u3R;
  u3R = &u3H->rod_u;
  {
    u3_noun txt = u3_ci_string(txt_c);
    
    gat = u3_ckdb_get(u3k(u3A->yot), u3k(txt));

    if ( u3_none == gat ) {
      gat = _cv_nock_wish(u3k(txt));
      u3A->yot = u3_ckdb_put(u3A->yot, u3k(txt), u3k(gat));
    }
    u3z(txt);
  }
  u3R = rod_u;
  return gat;
}

/* _cv_mung(): formula wrapper with gate and sample.
*/
  static u3_noun
  _cv_mung_in(u3_noun gam)
  {
    u3_noun pro = u3_cn_slam_on(u3k(u3h(gam)), u3k(u3t(gam)));

    u3z(gam); return pro;
  }
static u3_noun
_cv_mung(c3_w sec_w, u3_noun gat, u3_noun sam)
{
  u3_noun gam = u3nc(gat, sam);

  return u3_cm_soft(0, _cv_mung_in, gam);
}

/* u3_cv_pike(): poke with floating core.
*/
u3_noun
u3_cv_pike(u3_noun ovo, u3_noun cor)
{
  u3_noun fun = u3_cn_nock_on(cor, u3k(u3_cx_at(42, cor)));
  u3_noun sam = u3nc(u3k(u3A->now), ovo);

  return _cv_mung(0, fun, sam);
}

/* u3_cv_nick(): transform enveloped packets, [vir cor].
*/
u3_noun
u3_cv_nick(u3_noun vir, u3_noun cor)
{
  if ( u3_nul == vir ) {
    return u3nt(u3_blip, vir, cor);
  }
  else {
    u3_noun i_vir = u3h(vir);
    u3_noun pi_vir, qi_vir;
    u3_noun vix;

    if ( (u3_yes == u3_cr_cell((i_vir=u3h(vir)), &pi_vir, &qi_vir)) &&
         (u3_yes == u3du(qi_vir)) &&
         (c3__hear == u3h(qi_vir)) )
    {
      u3_noun gon;

      gon = u3_cv_pike(u3k(i_vir), cor);
      if ( u3_blip != u3h(gon) ) {
        u3z(vir);
        return gon;
      }
      else {
        u3_noun viz;

        vix = u3k(u3h(u3t(gon)));
        cor = u3k(u3t(u3t(gon)));
        u3z(gon);

        viz = u3_ckb_weld(vix, u3k(u3t(vir)));
        u3z(vir);

        return u3_cv_nick(viz, cor);
      }
    }
    else {
      u3_noun nez = u3_cv_nick(u3k(u3t(vir)), cor);

      if ( u3_blip != u3h(nez) ) {
        u3z(vir);
        return nez;
      } else {
        u3_noun viz;

        viz = u3nc(u3k(i_vir), u3k(u3h(u3t(nez))));
        cor = u3k(u3t(u3t(nez)));

        u3z(vir);
        u3z(nez);

        return u3nt(u3_blip, viz, cor);
      }
    }
  }
}

/* _cv_nock_poke(): call poke through hardcoded interface.
*/
static u3_noun
_cv_nock_poke(u3_noun ovo)
{
  u3_noun fun = u3_cn_nock_on(u3k(u3A->roc), u3k(u3_cx_at(42, u3A->roc)));
  u3_noun sam, pro;

  sam = u3nc(u3k(u3A->now), ovo);
#if 0
  {
    c3_c*   ovi_c = u3_cr_string(u3h(u3t(ovo)));
    u3_noun tox = u3_do("spat", u3k(u3h(ovo)));
    c3_c*   tox_c = u3_cr_string(tox);

    printf("poke: %%%s on %s\r\n", ovi_c, tox_c);
    free(tox_c); free(ovi_c); u3z(tox);
  }
#endif

  pro = u3_cn_slam_on(fun, sam);

#if 0
  {
    c3_c*   ovi_c = u3_cr_string(u3h(u3t(ovo)));

    printf("poked: %s\r\n", ovi_c);

    free(ovi_c);
  }
#endif

  return pro;
}

/* _cv_nock_peek(): call peek through hardcoded interface.
*/
static u3_noun
_cv_nock_peek(u3_noun hap)
{
  u3_noun fun = u3_cn_nock_on(u3k(u3A->roc), u3k(u3_cx_at(87, u3A->roc)));
  u3_noun sam = u3nc(u3k(u3A->now), hap);

  return u3_cn_slam_on(fun, sam);
}

/* _cv_nock_keep(): call wait through hardcoded interface.
*/
static u3_noun
_cv_nock_keep(u3_noun hap)
{
  u3_noun fun = u3_cn_nock_on(u3k(u3A->roc), u3k(u3_cx_at(4, u3A->roc)));
  u3_noun sam = u3nc(u3k(u3A->now), hap);

  return u3_cn_slam_on(fun, sam);
}

/* u3_cv_do(): use a kernel function.
*/
u3_noun
u3_cv_do(const c3_c* txt_c, u3_noun arg)
{
  // printf("cv_do: fn %s\r\n", txt_c);
  return u3_cn_slam_on(u3_cv_gate(txt_c), arg);
}

/* u3_cv_wish(): noun from expression
*/
u3_noun
u3_cv_wish(c3_c* str_c)
{
  return _cv_nock_wish(u3_ci_string(str_c));
}

/* _cv_scot(): print atom.
*/
static u3_noun
_cv_scot(u3_noun dim)
{
  return u3_do("scot", dim);
}

/* u3_cv_time(): set the reck time.
*/
void
u3_cv_time(u3_noun now)
{
  u3z(u3A->now);
  u3A->now = now;

  u3z(u3A->wen);
  u3A->wen = _cv_scot(u3nc(c3__da, u3k(u3A->now)));
}

/* u3_cv_numb(): set the instance number.
*/
void
u3_cv_numb()
{
  u3A->sev_l = u3_cr_mug(u3A->now);
  u3z(u3A->sen);
  u3A->sen = _cv_scot(u3nc(c3__uv, u3A->sev_l));
}

#if 0
/* _cv_time_bump(): advance the reck time by a small increment.
*/
static void
_cv_time_bump(u3_reck* rec_u)
{
  c3_d bum_d = (1ULL << 48ULL);

  u3A->now = u3_cka_add(u3A->now, u3_ci_chubs(1, &bum_d));
}
#endif

/* u3_cv_peek(): query the reck namespace (protected).
*/
u3_noun
u3_cv_peek(u3_noun hap)
{
  return _cv_nock_peek(hap);
}

/* u3_cv_keep(): measure timer.
*/
u3_noun
u3_cv_keep(u3_noun hap)
{
  return _cv_nock_keep(hap);
}

#if 0
/* _cv_mole(): parse simple atomic mole.
*/
static u3_bean
_cv_mole(u3_noun  fot,
         u3_noun  san,
         c3_d*    ato_d)
{
  u3_noun uco = u3_do("slay", san);
  u3_noun p_uco, q_uco, r_uco, s_uco;

  if ( (u3_no == u3_cr_qual(uco, &p_uco, &q_uco, &r_uco, &s_uco)) ||
       (0 != p_uco) ||
       (0 != q_uco) ||
       (u3_no == u3_sing(fot, r_uco)) )
  {
    uL(fprintf(uH, "strange mole %s\n", u3_cr_string(san)));

    u3z(fot); u3z(uco); return u3_no;
  }
  else {
    *ato_d = u3_cr_chub(0, s_uco);

    u3z(fot); u3z(uco); return u3_yes;
  }
}

/* _cv_lily(): parse little atom.
*/
static u3_bean
_cv_lily(u3_noun fot, u3_noun txt, c3_l* tid_l)
{
  c3_d ato_d;

  if ( u3_no == _cv_mole(fot, txt, &ato_d) ) {
    return u3_no;
  } else {
    if ( ato_d >= 0x80000000ULL ) {
      return u3_no;
    } else {
      *tid_l = (c3_l) ato_d;

      return u3_yes;
    }
  }
}
#endif

/* u3_cv_poke(): insert and apply an input ovum (protected).
*/
u3_noun
u3_cv_poke(u3_noun ovo)
{
  return _cv_nock_poke(ovo);
}

/* u3_cv_http_request(): hear http request on channel (unprotected).
*/
void
u3_cv_http_request(u3_bean sec, u3_noun pox, u3_noun req)
{
  // uL(fprintf(uH, "http: request\n"));
  u3_cv_plan(pox, u3nq(c3__this, sec, 0, req));
}

/* u3_cv_tank(): dump single tank.
*/
void
u3_cv_tank(u3_noun blu, c3_l tab_l, u3_noun tac)
{
  u3_cv_punt(blu, tab_l, u3nc(tac, u3_nul));
}

/* u3_cv_punt(): dump tank list.
*/
void
u3_cv_punt(u3_noun blu, c3_l tab_l, u3_noun tac)
{
#if 0
  u3_noun blu   = u3_term_get_blew(0);
#endif
  c3_l    col_l = u3h(blu);
  u3_noun cat   = tac;

  //  We are calling nock here, but hopefully need no protection.
  //
  while ( u3_yes == u3_cr_du(cat) ) {
    u3_noun wol = u3_dc("wash", u3nc(tab_l, col_l), u3k(u3h(cat)));

    u3_cm_wall(wol);
    cat = u3t(cat);
  }
  u3z(tac);
  u3z(blu);
}

/* u3_cv_sway(): print trace.
*/
void
u3_cv_sway(u3_noun blu, c3_l tab_l, u3_noun tax)
{
  u3_noun mok = u3_dc("mook", 2, tax);

  u3_cv_punt(blu, tab_l, u3k(u3t(mok)));
  u3z(mok);
}

/* u3_cv_plan(): queue ovum (external).
*/
void
u3_cv_plan(u3_noun pax, u3_noun fav)
{
  u3_noun egg = u3nc(pax, fav);
  u3A->roe = u3nc(u3nc(u3_nul, egg), u3A->roe);
}

/* u3_cv_plow(): queue multiple ova (external).
*/
void
u3_cv_plow(u3_noun ova)
{
  u3_noun ovi = ova;

  while ( u3_nul != ovi ) {
    u3_noun ovo=u3h(ovi);

    u3_cv_plan(u3k(u3h(ovo)), u3k(u3t(ovo)));
    ovi = u3t(ovi);
  }
  u3z(ova);
}

/* u3_cv_louse(): last-minute deviltry upon a bail.
*/
void
u3_cv_louse(c3_m how_m)
{
#if 0
  if ( c3__exit == how_m ) {
    printf("louse: nocks: %d\n", NOX);
    printf("louse: washing kernel %x %d\n", u3A->ken, u3_co_is_dog(u3A->ken));
    u3_cm_wash(u3A->ken); 

    printf("kernel %x; washed mug %x\n", u3A->ken, u3_cr_mug(u3A->ken));
  }
#endif
}
