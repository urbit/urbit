/* g/v.c
**
*/
#include <stdio.h>
#include "all.h"

int WISH;
/* _cv_nock_wish(): call wish through hardcoded interface.
*/
static u3_noun
_cv_nock_wish(u3_noun txt)
{
  u3_noun fun, pro;

  WISH = 1;
  fun = u3n_nock_on(u3k(u3A->roc), u3k(u3x_at(20, u3A->roc)));
  pro = u3n_slam_on(fun, txt);
  WISH = 0;

  return pro;
}

/* u3v_make(): make a new pier and instantiate pill.
*/
void
u3v_make(c3_c* pas_c)
{
  u3_noun sys = u3ke_cue(u3m_file(pas_c));

  u3A->ken = u3k(u3h(sys));
  u3A->roc = u3k(u3t(sys));

  u3z(sys);
}

/* u3v_jack(): execute kernel formula to bind jets.
*/
void
u3v_jack(void)
{
  u3_noun cor; 

  cor = u3n_nock_on(0, u3k(u3A->ken));
  u3z(cor);
}

/* u3v_hose(): clear initial ovum queue.
*/
void
u3v_hose(void)
{
  u3p(u3v_cart) egg_p = u3A->ova.egg_p;

  while ( egg_p ) {
    u3v_cart*     egg_u = u3to(u3v_cart, egg_p);
    u3p(u3v_cart) nex_p = egg_u->nex_p;

    u3a_lose(egg_u->vir);
    u3a_free(egg_u);

    egg_p = nex_p;
  }
  u3A->ova.egg_p = u3A->ova.geg_p = 0;
  u3z(u3A->roe);
  u3A->roe = u3_nul;
}

/* u3v_start(): start time.
*/
void
u3v_start(u3_noun now)
{
  u3v_time(now);
  u3v_numb();
  u3A->sac = u3_nul;

  {
    c3_c* wen_c = u3r_string(u3A->wen);

    printf("arvo: time: %s\n", wen_c);
    free(wen_c);
  }
}

/* u3v_wish(): text expression with cache.
*/
u3_noun
u3v_wish(const c3_c* str_c)
{
  u3_noun exp;

  if ( u3R == &u3H->rod_u ) {
    u3_noun txt = u3i_string(str_c);
    
    exp = u3kdb_get(u3k(u3A->yot), u3k(txt));

    if ( u3_none == exp ) {
      exp = _cv_nock_wish(u3k(txt));
      u3A->yot = u3kdb_put(u3A->yot, u3k(txt), u3k(exp));
    }
    u3z(txt);
    return exp;
  }
  else {
    //  It's probably not a good idea to use u3v_wish() 
    //  outside the top level...
    //
    u3_noun txt = u3i_string(str_c);
    u3_noun exp = u3kdb_get(u3A->yot, u3k(txt));
    
    if ( u3_none != exp ) {
      u3z(txt);
      return exp;
    }
    else return _cv_nock_wish(u3i_string(str_c));
  }
}

/* _cv_mung(): formula wrapper with gate and sample.
*/
  static u3_noun
  _cv_mung_in(u3_noun gam)
  {
    u3_noun pro = u3n_slam_on(u3k(u3h(gam)), u3k(u3t(gam)));

    u3z(gam); return pro;
  }
static u3_noun
_cv_mung(c3_w sec_w, u3_noun gat, u3_noun sam)
{
  u3_noun gam = u3nc(gat, sam);

  return u3m_soft(0, _cv_mung_in, gam);
}

/* u3v_pike(): poke with floating core.
*/
u3_noun
u3v_pike(u3_noun ovo, u3_noun cor)
{
  u3_noun fun = u3n_nock_on(cor, u3k(u3x_at(42, cor)));
  u3_noun sam = u3nc(u3k(u3A->now), ovo);

  return _cv_mung(0, fun, sam);
}

/* u3v_nick(): transform enveloped packets, [vir cor].
*/
u3_noun
u3v_nick(u3_noun vir, u3_noun cor)
{
  if ( u3_nul == vir ) {
    return u3nt(u3_blip, vir, cor);
  }
  else {
    u3_noun i_vir = u3h(vir);
    u3_noun pi_vir, qi_vir;
    u3_noun vix;

    if ( (c3y == u3r_cell((i_vir=u3h(vir)), &pi_vir, &qi_vir)) &&
         (c3y == u3du(qi_vir)) &&
         (c3__hear == u3h(qi_vir)) )
    {
      u3_noun gon;

      gon = u3v_pike(u3k(i_vir), cor);
      if ( u3_blip != u3h(gon) ) {
        u3z(vir);
        return gon;
      }
      else {
        u3_noun viz;

        vix = u3k(u3h(u3t(gon)));
        cor = u3k(u3t(u3t(gon)));
        u3z(gon);

        viz = u3kb_weld(vix, u3k(u3t(vir)));
        u3z(vir);

        return u3v_nick(viz, cor);
      }
    }
    else {
      u3_noun nez = u3v_nick(u3k(u3t(vir)), cor);

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
  u3_noun fun = u3n_nock_on(u3k(u3A->roc), u3k(u3x_at(42, u3A->roc)));
  u3_noun sam, pro;

  sam = u3nc(u3k(u3A->now), ovo);
#if 0
  {
    c3_c*   ovi_c = u3r_string(u3h(u3t(ovo)));
    u3_noun tox = u3do("spat", u3k(u3h(ovo)));
    c3_c*   tox_c = u3r_string(tox);

    printf("poke: %%%s (%x) on %s\r\n", ovi_c, u3r_mug(ovo), tox_c);
    free(tox_c); free(ovi_c); u3z(tox);
  }
#endif

  u3a_lush(u3h(u3t(ovo)));
  pro = u3n_slam_on(fun, sam);
  u3a_lop(0);

#if 0
  {
    c3_c*   ovi_c = u3r_string(u3h(u3t(ovo)));

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
  u3_noun fun = u3n_nock_on(u3k(u3A->roc), u3k(u3x_at(87, u3A->roc)));
  u3_noun sam = u3nc(u3k(u3A->now), hap);

  return u3n_slam_on(fun, sam);
}

/* _cv_nock_keep(): call wait through hardcoded interface.
*/
static u3_noun
_cv_nock_keep(u3_noun hap)
{
  u3_noun fun = u3n_nock_on(u3k(u3A->roc), u3k(u3x_at(4, u3A->roc)));
  u3_noun sam = u3nc(u3k(u3A->now), hap);

  return u3n_slam_on(fun, sam);
}

/* u3v_do(): use a kernel gate.
*/
u3_noun
u3v_do(const c3_c* txt_c, u3_noun sam)
{
  u3_noun gat = u3v_wish(txt_c);
  u3_noun pro;

#if 0
  if ( &u3H->rod_u == u3R ) {
    pro = u3m_soft_slam(gat, sam);
  }
  else {
    pro = u3n_slam_on(gat, sam);
  }
#else
  pro = u3n_slam_on(gat, sam);
#endif

  return pro;
}

/* _cv_scot(): print atom.
*/
static u3_noun
_cv_scot(u3_noun dim)
{
  return u3do("scot", dim);
}

/* u3v_time(): set the reck time.
*/
void
u3v_time(u3_noun now)
{
  u3z(u3A->now);
  u3A->now = now;

  u3z(u3A->wen);
  u3A->wen = _cv_scot(u3nc(c3__da, u3k(u3A->now)));
}

/* u3v_numb(): set the instance number.
*/
void
u3v_numb()
{
  u3A->sev_l = u3r_mug(u3A->now);
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

  u3A->now = u3ka_add(u3A->now, u3i_chubs(1, &bum_d));
}
#endif

/* u3v_peek(): query the reck namespace (protected).
*/
u3_noun
u3v_peek(u3_noun hap)
{
  return u3m_soft_sure(_cv_nock_peek, hap);
}

/* u3v_keep(): measure timer.
*/
u3_noun
u3v_keep(u3_noun hap)
{
  return u3m_soft_sure(_cv_nock_keep, hap);
}

#if 0
/* _cv_mole(): parse simple atomic mole.
*/
static c3_o
_cv_mole(u3_noun  fot,
         u3_noun  san,
         c3_d*    ato_d)
{
  u3_noun uco = u3do("slay", san);
  u3_noun p_uco, q_uco, r_uco, s_uco;

  if ( (c3n == u3r_qual(uco, &p_uco, &q_uco, &r_uco, &s_uco)) ||
       (0 != p_uco) ||
       (0 != q_uco) ||
       (c3n == u3_sing(fot, r_uco)) )
  {
    uL(fprintf(uH, "strange mole %s\n", u3r_string(san)));

    u3z(fot); u3z(uco); return c3n;
  }
  else {
    *ato_d = u3r_chub(0, s_uco);

    u3z(fot); u3z(uco); return c3y;
  }
}

/* _cv_lily(): parse little atom.
*/
static c3_o
_cv_lily(u3_noun fot, u3_noun txt, c3_l* tid_l)
{
  c3_d ato_d;

  if ( c3n == _cv_mole(fot, txt, &ato_d) ) {
    return c3n;
  } else {
    if ( ato_d >= 0x80000000ULL ) {
      return c3n;
    } else {
      *tid_l = (c3_l) ato_d;

      return c3y;
    }
  }
}
#endif

/* u3v_poke(): insert and apply an input ovum (protected).
*/
u3_noun
u3v_poke(u3_noun ovo)
{
  return _cv_nock_poke(ovo);
}

/* u3v_http_request(): hear http request on channel (unprotected).
*/
void
u3v_http_request(c3_o sec, u3_noun pox, u3_noun req)
{
  // uL(fprintf(uH, "http: request\n"));
  u3v_plan(pox, u3nq(c3__this, sec, 0, req));
}

/* u3v_tank(): dump single tank.
*/
void
u3v_tank(u3_noun blu, c3_l tab_l, u3_noun tac)
{
  u3v_punt(blu, tab_l, u3nc(tac, u3_nul));
}

/* u3v_punt(): dump tank list.
*/
void
u3v_punt(u3_noun blu, c3_l tab_l, u3_noun tac)
{
#if 0
  u3_noun blu   = u3_term_get_blew(0);
#endif
  c3_l    col_l = u3h(blu);
  u3_noun cat   = tac;

  //  We are calling nock here, but hopefully need no protection.
  //
  while ( c3y == u3r_du(cat) ) {
    u3_noun wol = u3dc("wash", u3nc(tab_l, col_l), u3k(u3h(cat)));

    u3m_wall(wol);
    cat = u3t(cat);
  }
  u3z(tac);
  u3z(blu);
}

/* u3v_sway(): print trace.
*/
void
u3v_sway(u3_noun blu, c3_l tab_l, u3_noun tax)
{
  u3_noun mok = u3dc("mook", 2, tax);

  u3v_punt(blu, tab_l, u3k(u3t(mok)));
  u3z(mok);
}

/* u3v_plan(): queue ovum (external).
*/
void
u3v_plan(u3_noun pax, u3_noun fav)
{
  u3_noun egg = u3nc(pax, fav);
  u3A->roe = u3nc(u3nc(u3_nul, egg), u3A->roe);
}

/* u3v_plow(): queue multiple ova (external).
*/
void
u3v_plow(u3_noun ova)
{
  u3_noun ovi = ova;

  while ( u3_nul != ovi ) {
    u3_noun ovo=u3h(ovi);

    u3v_plan(u3k(u3h(ovo)), u3k(u3t(ovo)));
    ovi = u3t(ovi);
  }
  u3z(ova);
}

/* u3v_louse(): last-minute deviltry upon a bail.
*/
void
u3v_louse(c3_m how_m)
{
#if 0
  if ( c3__exit == how_m ) {
    printf("louse: nocks: %d\n", NOX);
    printf("louse: washing kernel %x %d\n", u3A->ken, u3a_is_dog(u3A->ken));
    u3m_wash(u3A->ken); 

    printf("kernel %x; washed mug %x\n", u3A->ken, u3r_mug(u3A->ken));
  }
#endif
}

/* _cv_mark_ova(): mark ova queue.
*/
c3_w
_cv_mark_ova(u3p(u3v_cart) egg_p)
{
  c3_w tot_w = 0;

  while ( egg_p ) {
    u3v_cart* egg_u = u3to(u3v_cart, egg_p);

    tot_w += u3a_mark_mptr(egg_u);
    tot_w += u3a_mark_noun(egg_u->vir);

    egg_p = egg_u->nex_p;
  }

  return tot_w;
}

/* u3v_mark(): mark arvo kernel.
*/
c3_w
u3v_mark(void)
{
  c3_w tot_w = 0;

  u3v_arvo* arv_u = &(u3H->arv_u);

  tot_w += u3a_mark_noun(arv_u->yot);
  tot_w += u3a_mark_noun(arv_u->now);
  tot_w += u3a_mark_noun(arv_u->wen);
  tot_w += u3a_mark_noun(arv_u->sen);
  tot_w += u3a_mark_noun(arv_u->own);

  tot_w += u3a_mark_noun(arv_u->roe);
  tot_w += u3a_mark_noun(arv_u->key);

  tot_w += u3a_mark_noun(arv_u->ken);
  tot_w += u3a_mark_noun(arv_u->roc);

  tot_w += _cv_mark_ova(arv_u->ova.egg_p);

  return tot_w;
}
