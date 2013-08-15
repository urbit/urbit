/* mill/x/cage.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_p_cage()::
*/
u4_mold
_mill_p_cage(u4_milr m,
             u4_noun fut,
             u4_mold pex)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(fut) ) {
    return _mill_fail(m, "empty cage");
  }
  else if ( u4_n_zero(u4_ct(fut)) ) {
    return _mill_play(m, u4_ch(fut), pex);
  }
  else {
    u4_mold gon = _mill_play(m, u4_ch(fut), pex);
    u4_mold zuf = _mill_p_cage(m, u4_ct(fut), pex);

    return u4_k_trel(lane, u4_atom_cell, gon, zuf);
  }
}

/* _mill_b_cage()::
*/
u4_nock
_mill_b_cage(u4_milr m,
             u4_noun fut,
             u4_mold pex)
{
  if ( u4_n_zero(fut) ) {
    return _mill_fail(m, "empty cage");
  }
  else if ( u4_n_zero(u4_ct(fut)) ) {
    return _mill_bake(m, u4_ch(fut), pex);
  }
  else {
    u4_nock fiq = _mill_bake(m, u4_ch(fut), pex);
    u4_nock nux = _mill_b_cage(m, u4_ct(fut), pex);

    return _mill_cons(m, fiq, nux);
  }
}

/* _mill_m_cage()::
*/
u4_loaf
_mill_m_cage(u4_milr m,
             u4_noun fut,
             u4_mold pex)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(fut) ) {
    return _mill_fail(m, "empty cage");
  }
  else if ( u4_n_zero(u4_ct(fut)) ) {
    return _mill_make(m, u4_ch(fut), pex);
  }
  else {
    u4_loaf deg = _mill_make(m, u4_ch(fut), pex);
    u4_loaf har = _mill_m_cage(m, u4_ct(fut), pex);

    return u4_k_cell
      (lane, u4_k_trel(lane, u4_atom_cell, u4_ch(deg), u4_ch(har)),
             _mill_cons(m, u4_ct(deg), u4_ct(har)));
  }
}
