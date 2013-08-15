/* mill/make.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

#define _make_p(flot) \
  if ( u4_b_p(gen, u4_atom_##flot, &p_gen) ) { \
    u4_loaf _mill_m_##flot(u4_milr, u4_noun, u4_mold); \
    return _mill_m_##flot(m, p_gen, tip); \
  }

#define _make_pq(flot) \
  if ( u4_b_pq(gen, u4_atom_##flot, &p_gen, &q_gen) ) { \
    u4_loaf _mill_m_##flot(u4_milr, u4_noun, u4_noun, u4_mold); \
    return _mill_m_##flot(m, p_gen, q_gen, tip); \
  }

#define _make_pqr(flot) \
  if ( u4_b_pqr(gen, u4_atom_##flot, &p_gen, &q_gen, &r_gen) ) {\
    u4_loaf _mill_m_##flot(u4_milr, u4_noun, u4_noun, u4_noun, u4_mold); \
    return _mill_m_##flot(m, p_gen, q_gen, r_gen, tip); \
  }

/* _make_main(): internal of _mill_make().
*/
u4_loaf
_make_main(u4_milr m,
           u4_gene gen,
           u4_mold tip)
{
  u4_noun p_gen, q_gen, r_gen;

  _make_p  (bail);    // !!
  _make_pq (cast);    // ^-
  _make_p  (cage);    // :.
  _make_p  (drag);    // |$
  _make_p  (dbug);
  _make_pq (home);
  _make_pq (kick);    // :=
  _make_pq (like);    // ?= 
  _make_pq (link);    // ~>
  _make_p  (load);    // |+
  _make_pq (mang);    // :~
  _make_pq (name);    // :`
  _make_pq (nock);
  _make_pqr(quiz);    // ?:
  _make_p  (rock);
  _make_pq (spot);
  _make_pqr(sure);    // ^=

  {
    u4_noun rex = _mill_open(m, gen);

    if ( u4_n_eq(rex, gen) ) {
      u4_err(m->lane, "rex", rex);
      return u4_trip;
    }
    else {
      return _mill_make(m, rex, tip);
    }
  }
}

/* _mill_make(): mold inference, top level.
*/
u4_loaf
_mill_make(u4_milr m,
           u4_gene gen,
           u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_noun fid  = u4_k_cell(lane, gen, tip);
  u4_nopt mig  = u4_tab_get(fid, m->zor);
  u4_nopt dum  = u4_tab_get(fid, m->niq);

  if ( (u4_bull == mig) && (u4_bull != dum) ) {
    mig = _mill_play(m, gen, tip);

    return u4_k_cell(lane, mig, dum);
  }
  else if ( (u4_bull != mig) && (u4_bull == dum) ) {
    dum = _mill_bake(m, gen, tip);

    return u4_k_cell(lane, mig, dum);
  }
  else {
    u4_loaf gel = _make_main(m, gen, tip);

    m->zor = u4_tab_add(lane, fid, u4_ch(gel), m->zor);
    m->niq = u4_tab_add(lane, fid, u4_ct(gel), m->niq);

    return gel;
  }
}
