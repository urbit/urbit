/* g/v.c
**
*/
#include <stdio.h>
#include "all.h"

#define _CVX_LOAD  4
#define _CVX_PEEK 22
#define _CVX_POKE 23
#define _CVX_WISH 10

/* u3v_life(): execute initial lifecycle, producing Arvo core.
*/
u3_noun
u3v_life(u3_noun eve)
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
    u3_noun pro = u3m_soft(0, u3v_life, eve);

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
  u3_noun eve, pro;

  {
    u3_noun hed, tal;
    u3x_cell(pil, &hed, &tal);
    u3_assent( u3r_sing_c("ivory", hed) );
    eve = tal;
  }

  u3l_log("lite: arvo formula %x\r\n", u3r_mug(pil));
  pro = u3v_life(u3k(eve));
  u3l_log("lite: core %x\r\n", u3r_mug(pro));

  u3z(pil);
  return pro;
}

/* u3v_boot_lite(): light bootstrap sequence, just making a kernel.
*/
c3_o
u3v_boot_lite(u3_noun pil)
{
  //  ensure zero-initialized kernel
  //
  u3A->roc = 0;

  {
    u3_noun pro = u3m_soft(0, _cv_lite, pil);

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
u3v_peek(u3_noun sam)
{
  u3_noun fun = u3n_nock_on(u3k(u3A->roc), u3k(u3x_at(_CVX_PEEK, u3A->roc)));
  return u3n_slam_on(fun, sam);
}

/* u3v_soft_peek(): softly query the reck namespace.
*/
u3_noun
u3v_soft_peek(c3_w mil_w, u3_noun sam)
{
  u3_noun gon = u3m_soft(mil_w, u3v_peek, sam);
  u3_noun pro;

  {
    u3_noun tag, dat;
    u3x_cell(gon, &tag, &dat);

    //  read succeeded, produce result
    //
    if ( u3_blip == tag ) {
      pro = u3nc(c3y, u3k(dat));
      u3z(gon);
    }
    //  read failed, produce trace
    //
    //    NB, reads should *not* fail deterministically
    //
    else {
      pro = u3nc(c3n, gon);
    }
  }

  return pro;
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
  u3_noun fun = u3n_nock_on(u3k(u3A->roc), u3k(u3x_at(_CVX_POKE, u3A->roc)));
  u3_noun sam = u3nc(u3k(u3A->now), ovo);
  u3_noun pro;

  {
    c3_w cod_w = u3a_lush(u3h(u3t(ovo)));
    pro = u3n_slam_on(fun, sam);
    u3a_lop(cod_w);
  }

  return pro;
}

static u3_noun
_cv_poke_eve(u3_noun sam)
{
  u3_noun fun = u3n_nock_on(u3k(u3A->roc), u3k(u3x_at(_CVX_POKE, u3A->roc)));
  u3_noun pro;

  {
    // c3_w cod_w = u3a_lush(u3h(u3t(u3t(sam)))); // XX MEMORY_DEBUG
    pro = u3n_slam_on(fun, sam);
    // u3a_lop(cod_w);
  }

  return pro;
}

/* u3v_poke_sure(): inject an event, saving new state if successful.
*/
c3_o
u3_poke_sure(c3_w mil_w, u3_noun eve, u3_noun* pro)
{
  u3_noun gon = u3m_soft(mil_w, _cv_poke_eve, eve);
  u3_noun tag, dat;
  u3x_cell(gon, &tag, &dat);

  //  event succeeded, persist state and produce effects
  //
  if ( u3_blip == tag ) {
    u3_noun vir, cor;
    u3x_cell(dat, &vir, &cor);

    u3z(u3A->roc);
    u3A->roc = u3k(cor);
    u3A->eve_d++;

    *pro = u3k(vir);
    u3z(gon);
    return c3y;
  }
  //  event failed, produce trace
  //
  else {
    *pro = gon;
    return c3n;
  }
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
  tot_w += u3a_maid(fil_u, "  wish cache", u3a_mark_noun(arv_u->yot));
  return   u3a_maid(fil_u, "total arvo stuff", tot_w);
}

/* u3v_reclaim(): clear ad-hoc persistent caches to reclaim memory.
*/
void
u3v_reclaim(void)
{
  //  clear the u3v_wish cache
  //
  //    NB: this would leak if not on the home road
  //
  if ( &(u3H->rod_u) == u3R ) {
    u3z(u3A->yot);
    u3A->yot = u3_nul;
  }
}

/* u3v_rewrite_compact(): rewrite arvo kernel for compaction.
*/
void
u3v_rewrite_compact()
{
  u3v_arvo* arv_u = &(u3H->arv_u);

  u3a_rewrite_noun(arv_u->roc);
  u3a_rewrite_noun(arv_u->now);
  u3a_rewrite_noun(arv_u->yot);

  arv_u->roc = u3a_rewritten_noun(arv_u->roc);
  arv_u->now = u3a_rewritten_noun(arv_u->now);
  arv_u->yot = u3a_rewritten_noun(arv_u->yot);
}

