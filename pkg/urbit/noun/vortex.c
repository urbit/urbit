/* g/v.c
**
*/
#include <stdio.h>
#include "all.h"

#define _CVX_WISH 22
#define _CVX_POKE 47
#define _CVX_PEEK 46

/* _cv_life(): execute initial lifecycle, producing Arvo core.
*/
static u3_noun
_cv_life(u3_noun eve)
{
  u3_noun lyf = u3nt(2, u3nc(0, 3), u3nc(0, 2));
  u3_noun gat = u3n_nock_on(eve, lyf);
  u3_noun cor = u3k(u3x_at(7, gat));

  u3z(gat);
  return cor;
}

/* u3v_boot(): evaluate boot sequence, making a kernel
*/
c3_o
u3v_boot(u3_noun eve)
{
  //  ensure zero-initialized kernel
  //
  u3A->roc = 0;

  {
    u3_noun pro = u3m_soft(0, _cv_life, eve);

    if ( u3_blip != u3h(pro) ) {
      u3z(pro);
      return c3n;
    }

    u3A->roc = u3k(u3t(pro));
    u3z(pro);
  }

  return c3y;
}

/* _cv_lite(): load lightweight, core-only pill.
*/
static u3_noun
_cv_lite(u3_noun pil)
{
  u3_noun arv = u3ke_cue(pil);
  u3_noun eve, pro;

  u3x_trel(arv, &eve, 0, 0);

  u3l_log("lite: arvo formula %x\r\n", u3r_mug(arv));
  pro = _cv_life(u3k(eve));
  u3l_log("lite: core %x\r\n", u3r_mug(pro));

  u3z(arv);
  return pro;
}

/* u3v_boot_lite(): light bootstrap sequence, just making a kernel.
*/
c3_o
u3v_boot_lite(u3_atom lit)
{
  //  ensure zero-initialized kernel
  //
  u3A->roc = 0;

  {
    u3_noun pro = u3m_soft(0, _cv_lite, lit);

    if ( u3_blip != u3h(pro) ) {
      u3z(pro);
      return c3n;
    }

    u3A->roc = u3k(u3t(pro));
    u3z(pro);
  }

  u3l_log("lite: final state %x\r\n", u3r_mug(u3A->roc));

  return c3y;
}

/* _cv_nock_wish(): call wish through hardcoded interface.
*/
static u3_noun
_cv_nock_wish(u3_noun txt)
{
  u3_noun fun, pro;

  fun = u3n_nock_on(u3k(u3A->roc), u3k(u3x_at(_CVX_WISH, u3A->roc)));
  pro = u3n_slam_on(fun, txt);

  return pro;
}

/* u3v_wish(): text expression with cache.
*/
u3_noun
u3v_wish(const c3_c* str_c)
{
  u3t_event_trace("u3v_wish", 'b');
  u3_noun txt = u3i_string(str_c);
  u3_weak exp = u3kdb_get(u3k(u3A->yot), u3k(txt));

  if ( u3_none == exp ) {
    exp = _cv_nock_wish(u3k(txt));

    //  It's probably not a good idea to use u3v_wish()
    //  outside the top level... (as the result is uncached)
    //
    if ( u3R == &u3H->rod_u ) {
      u3A->yot = u3kdb_put(u3A->yot, u3k(txt), u3k(exp));
    }
  }

  u3t_event_trace("u3v_wish", 'e');

  u3z(txt);
  return exp;
}

/* _cv_nock_poke(): call poke through hardcoded interface.
*/
static u3_noun
_cv_nock_poke(u3_noun ovo)
{
  u3_noun fun = u3n_nock_on(u3k(u3A->roc), u3k(u3x_at(_CVX_POKE, u3A->roc)));
  u3_noun sam, pro;
  u3_noun cod_w;

  sam = u3nc(u3k(u3A->now), ovo);
#if 0
  {
    c3_c*   ovi_c = u3r_string(u3h(u3t(ovo)));
    u3_noun tox = u3do("spat", u3k(u3h(ovo)));
    c3_c*   tox_c = u3r_string(tox);

    u3l_log("poke: %%%s (%x) on %s\r\n", ovi_c, u3r_mug(ovo), tox_c);
    c3_free(tox_c); c3_free(ovi_c); u3z(tox);
  }
#endif

  cod_w = u3a_lush(u3h(u3t(ovo)));
  pro = u3n_slam_on(fun, sam);
  u3a_lop(cod_w);

#if 0
  {
    c3_c*   ovi_c = u3r_string(u3h(u3t(ovo)));

    if ( u3_nul == u3h(pro) ) {
      u3l_log("  blank: %s\r\n", ovi_c);
    } else {
      u3l_log("  happy: %s: %d\r\n", ovi_c, u3kb_lent(u3k(u3h(pro))));
    }
    c3_free(ovi_c);
  }
#endif

  return pro;
}

/* _cv_nock_peek(): call peek through hardcoded interface.
*/
static u3_noun
_cv_nock_peek(u3_noun hap)
{
  u3_noun fun = u3n_nock_on(u3k(u3A->roc), u3k(u3x_at(_CVX_PEEK, u3A->roc)));
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
       (c3n == u3r_sing(fot, r_uco)) )
  {
    u3l_log("strange mole %s\n", u3r_string(san)));

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

/* u3v_mark(): mark arvo kernel.
*/
c3_w
u3v_mark(FILE* fil_u)
{
  u3v_arvo* arv_u = &(u3H->arv_u);
  c3_w tot_w = 0;

  tot_w += u3a_maid(fil_u, "  kernel", u3a_mark_noun(arv_u->roc));
  tot_w += u3a_maid(fil_u, "  date", u3a_mark_noun(arv_u->now));
  tot_w += u3a_maid(fil_u, "  formatted date", u3a_mark_noun(arv_u->wen));
  tot_w += u3a_maid(fil_u, "  instance string", u3a_mark_noun(arv_u->sen));
  tot_w += u3a_maid(fil_u, "  wish cache", u3a_mark_noun(arv_u->yot));
  return   u3a_maid(fil_u, "total arvo stuff", tot_w);
}
