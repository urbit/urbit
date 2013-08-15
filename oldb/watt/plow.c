/* watt/plow.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /**   XX:   This entire file is a hack and will be deleted.
  **/
#if 1
  /**   Global variables.
  **/
    static struct _u4_plow PlowStructure;
    static struct _u4_plow *PlowPointer;

    /* PlowInit(): initialize global plow.
    **
    ** This saves memoization results, most notably the type check
    ** of scry, which is very slow ri
    */
    static struct _u4_plow *
    PlowInit(u4_lane lan)
    {
      if ( !PlowPointer ) {
        u4_plow_init(&PlowStructure, lan);
        PlowPointer = &PlowStructure;
      }
      return PlowPointer;
    }
#endif

/* trap:plow
*/
void
_plow_trap(u4_plow p,
           const char *msg)
{
  u4_lane lan = p->lan;

  u4_atom hal  = u4_k_atom_c(lan, msg);

  p->meb = u4_k_cell(lan, u4_k_cell(lan, hal, u4_nul), p->meb);
}

/* trac:plow
*/

/* fail:plow
*/
u4_noun
_plow_fail(u4_plow p,
           const char *msg)
{
  _plow_trap(p, msg);
  {
    while ( !u4_n_zero(p->meb) ) {
      u4_noun i_meb = u4_ch(p->meb);
      u4_noun hal = u4_ch(i_meb);
      u4_noun jup = u4_ct(i_meb);

      if ( !u4_n_zero(hal) ) {
        u4_sb sb_hal = u4_a_bin(hal, 3);
        u4_cl *cl_hal = alloca(sb_hal + 1);

        u4_a_bytes(hal, (u4_xb *)cl_hal, 0, sb_hal + 1);
        printf("msg: %s\n", cl_hal);
      }

      if ( !u4_n_zero(jup) ) {
        u4_atom pp_jup = u4_ch(u4_ch(jup));
        u4_atom qp_jup = u4_ct(u4_ch(jup));
        u4_atom pq_jup = u4_ch(u4_ct(jup));
        u4_atom qq_jup = u4_ct(u4_ct(jup));
        {
          u4_xw xw_flin = u4_a_wtrip(pp_jup);
          u4_xw xw_fcol = u4_a_wtrip(qp_jup);
          u4_xw xw_llin = u4_a_wtrip(pq_jup);
          u4_xw xw_lcol = u4_a_wtrip(qq_jup);

          printf("spot: %d:%d - %d:%d\n", 
                 xw_flin, xw_fcol, xw_llin, xw_lcol);
        }
      }
      p->meb = u4_ct(p->meb);
    }
  }
  return u4_exit;
  // return u4_trip;
}

/* rake:plow 
*/
u4_rope
_plow_rake(u4_plow p,
           u4_gene gen)
{
  u4_lane lan = p->lan;
  u4_gene p_gen, q_gen;

  if ( u4_b_pq(gen, u4_atom_mack, &p_gen, &q_gen) ) {
    if ( !u4_n_zero(q_gen) ) {
      return _plow_fail(p, "rope");
    }
    else return p_gen;
  }
  else if ( u4_b_p(gen, u4_atom_frag, &p_gen) ) {
    return u4_kl(lan, gen);
  }
  else if ( u4_b_p(gen, u4_atom_pane, &p_gen) ) {
    return u4_kl(lan, gen);
  }
  else if ( u4_b_pq(gen, u4_atom_zemp, &p_gen, &q_gen) ) {
    return _plow_rake(p, q_gen);
  }
  else if ( u4_b_p(gen, u4_atom_zush, &p_gen) ) {
    return _plow_rake(p, p_gen);
  }
  else {
    u4_err(lan, "rake", gen);
    return _plow_fail(p, "rope");
  }
}

/* init:plow (fake)
*/
void
u4_plow_init(u4_plow p,
             u4_lane lan)
{
  p->lan = lan;
  p->prf = 0;
  p->prg = 0;
  p->prh = 0;
  p->pri = 0;

  p->fan = u4_nul;
  p->ver = u4_nul;
  p->bug = u4_nul;
  p->meb = u4_nul;
  p->vus = u4_nul;
  p->tyc = u4_nul;
  p->gam = u4_nul;
  p->hos = u4_nul;
  p->zor = u4_nul;
  p->niq = u4_nul;
  p->fac = u4_nul;
  p->vom = u4_nul;
  p->pon = u4_nul;
  p->fin = u4_nul;
  p->huf = u4_nul;
}

/* make:plow (fake)
*/
u4_noun
u4_plow_make(u4_lane lan,
             u4_type sut,
             u4_gene gen)
{
  u4_plow p = PlowInit(lan);

  return _rose_make(p, sut, gen);
}

/* play:plow (fake)
*/
u4_noun
u4_plow_play(u4_lane lan,
             u4_type sut,
             u4_gene gen)
{
  u4_plow p = PlowInit(lan);

  return _rose_play(p, sut, gen);
}

/* show:plow (fake)
*/
u4_noun
u4_plow_show(u4_lane lan,
             u4_type sut,
             u4_gene gen)
{
  u4_plow p = PlowInit(lan);

  return _rose_show(p, sut, gen);
}

/* pass:plow (fake)
*/
u4_noun
u4_plow_pass(u4_lane lan,
             u4_type sut,
             u4_gene gen)
{
  u4_plow p = PlowInit(lan);

  if ( !u4_so(_rose_show(p, sut, gen)) ) {
    return u4_exit;
  }
  else return _rose_make(p, sut, gen);
}

/* shop:plow (fake)
*/
u4_noun
u4_plow_shop(u4_lane lan,
             u4_type sut,
             u4_gene gen)
{
  u4_plow p = PlowInit(lan);

  if ( !u4_so(_rose_show(p, sut, gen)) ) {
    return u4_exit;
  }
  else return _rose_play(p, sut, gen);
}

/* wish:plow (fake)
*/
u4_noun
u4_plow_wish(u4_lane lan,
             u4_type sut,
             u4_gene gen)
{
  u4_plow p = PlowInit(lan);

  return 
    u4_kc(lan, _rose_play(p, sut, gen),
               _rose_make(p, sut, gen));
}

/* shop:plow (fake)
*/
u4_noun
u4_plow_mill(u4_lane lan,
             u4_type sut,
             u4_gene gen)
{
  u4_plow p = PlowInit(lan);

  if ( !u4_so(_rose_show(p, sut, gen)) ) {
    return u4_exit;
  }
  else return 
    u4_kc(lan, _rose_play(p, sut, gen),
               _rose_make(p, sut, gen));
}

#if 0
/* mill:plow (fake)
*/
u4_noun
u4_plow_mill(u4_lane lan,
             u4_type sut,
             u4_gene gen)
{
  struct _u4_plow plow;

  u4_plow_init(&plow, lan);

  printf(":"); fflush(stdout);
  {
    u4_flag boz;
    u4_type typ;
    u4_tool tol;

    tol = _rose_make(&plow, sut, gen);
    printf(":"); fflush(stdout);

#if 0
    boz = _rose_show(&plow, sut, gen);
    if ( !u4_so(boz) ) {
      printf("type error\n");
      return u4_exit;
    }
    printf(":"); fflush(stdout);
#else
    printf("!"); fflush(stdout);
#endif

    typ = _rose_play(&plow, sut, gen);
    printf(":\n");

    // u4_err(lan, "size", _dump_size(lan, tol));

    // printf("prf: %d\n", plow.prf);
    // printf("prg: %d\n", plow.prg);
    // printf("prh: %d\n", plow.prh);
    // printf("pri: %d\n", plow.pri);

    return u4_kc(lan, typ, tol);
  }
}
#endif
