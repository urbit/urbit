/* mill/bake.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

#define _bake_p(flot) \
  if ( u4_b_p(gen, u4_atom_##flot, &p_gen) ) { \
    u4_nock _mill_b_##flot(u4_milr, u4_noun, u4_mold); \
    return _mill_b_##flot(m, p_gen, tip); \
  }

#define _bake_pq(flot) \
  if ( u4_b_pq(gen, u4_atom_##flot, &p_gen, &q_gen) ) { \
    u4_nock _mill_b_##flot(u4_milr, u4_noun, u4_noun, u4_mold); \
    return _mill_b_##flot(m, p_gen, q_gen, tip); \
  }

#define _bake_pqr(flot) \
  if ( u4_b_pqr(gen, u4_atom_##flot, &p_gen, &q_gen, &r_gen) ) {\
    u4_nock _mill_b_##flot(u4_milr, u4_noun, u4_noun, u4_noun, u4_mold); \
    return _mill_b_##flot(m, p_gen, q_gen, r_gen, tip); \
  }

/* _bake_main(): internal of _mill_bake().
*/
u4_nock
_bake_main(u4_milr m,
           u4_gene gen,
           u4_mold tip)
{
  u4_noun p_gen, q_gen, r_gen;

  _bake_p  (bail);    // !!
  _bake_pq (cast);    // ^-
  _bake_p  (cage);    // :.
  _bake_p  (drag);    // |$
  _bake_p  (dbug);
  _bake_pq (home);
  _bake_pq (kick);    // :=
  _bake_pq (like);    // ?= 
  _bake_pq (link);    // ~>
  _bake_p  (load);    // |@
  _bake_pq (mang);    // :~
  _bake_pq (name);    // :`
  _bake_pq (nock);
  _bake_pqr(quiz);    // ?:
  _bake_p  (rock);
  _bake_pq (spot);
  _bake_pqr(sure);    // ^=

  {
    u4_noun rex = _mill_open(m, gen);

    if ( u4_n_eq(rex, gen) ) {
      u4_err(m->lane, "rex", rex);
      return u4_trip;
    }
    else {
      return _mill_bake(m, rex, tip);
    }
  }
}

/* _mill_bake(): mold inference, top level.
*/
u4_nock
_mill_bake(u4_milr m,
           u4_gene gen,
           u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_noun fid  = u4_k_cell(lane, gen, tip);
  u4_nopt dum  = u4_tab_get(fid, m->niq);

  if ( u4_bull == dum ) {
    dum = _bake_main(m, gen, tip);
    m->niq = u4_tab_add(lane, fid, dum, m->niq);
  }
  return dum;
}
