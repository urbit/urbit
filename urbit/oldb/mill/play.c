/* mill/play.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

#define _play_p(flot) \
  if ( u4_b_p(gen, u4_atom_##flot, &p_gen) ) { \
    u4_mold _mill_p_##flot(u4_milr, u4_noun, u4_mold); \
    return _mill_p_##flot(m, p_gen, tip); \
  }

#define _play_pq(flot) \
  if ( u4_b_pq(gen, u4_atom_##flot, &p_gen, &q_gen) ) { \
    u4_mold _mill_p_##flot(u4_milr, u4_noun, u4_noun, u4_mold); \
    return _mill_p_##flot(m, p_gen, q_gen, tip); \
  }

#define _play_pqr(flot) \
  if ( u4_b_pqr(gen, u4_atom_##flot, &p_gen, &q_gen, &r_gen) ) {\
    u4_mold _mill_p_##flot(u4_milr, u4_noun, u4_noun, u4_noun, u4_mold); \
    return _mill_p_##flot(m, p_gen, q_gen, r_gen, tip); \
  }

/* _play_main(): internal of _mill_play().
*/
u4_mold
_play_main(u4_milr m,
           u4_gene gen,
           u4_mold tip)
{
  u4_noun p_gen, q_gen, r_gen;

  _play_p  (bail);    // !!
  _play_pq (cast);    // ^-
  _play_p  (cage);    // :.
  _play_p  (dbug);
  _play_p  (drag);    // |$
  _play_pq (home);
  _play_pq (kick);    // :=
  _play_pq (like);    // ?= 
  _play_pq (link);    // ~>
  _play_p  (load);    // |+
  _play_pq (mang);    // :~
  _play_pq (name);    // :`
  _play_pq (nock);
  _play_pqr(quiz);    // ?:
  _play_p  (rock);
  _play_pq (spot);
  _play_pqr(sure);    // ^=

  {
    u4_noun rex = _mill_open(m, gen);

    if ( u4_n_eq(rex, gen) ) {
      u4_err(m->lane, "rex", rex);
      return u4_trip;
    }
    else {
      return _mill_play(m, rex, tip);
    }
  }
}

/* _mill_play(): mold inference, top level.
*/
u4_mold
_mill_play(u4_milr m,
           u4_gene gen,
           u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_noun fid  = u4_k_cell(lane, gen, tip);
  u4_nopt mig  = u4_tab_get(fid, m->zor);

  if ( u4_bull == mig ) {
    mig = _play_main(m, gen, tip);

    if ( !u4_n_atom(mig) && u4_n_eq(u4_noun_3, u4_ch(mig)) ) {
      u4_err(lane, "gen", gen);
      return u4_trip;
    }

    m->zor = u4_tab_add(lane, fid, mig, m->zor);
  }
  return mig;
}
