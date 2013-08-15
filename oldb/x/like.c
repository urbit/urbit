/* mill/x/like.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_p_like()::
*/
u4_mold
_mill_p_like(u4_milr m, 
             u4_rope rid,
             u4_gene bul, 
             u4_mold tip)
{
  u4_lane lane = m->lane;

  return u4_k_qual
          (lane, 
           u4_atom_forq, 
           u4_k_cell(lane, u4_atom_cube, u4_noun_0),
           u4_k_cell(lane, u4_atom_cube, u4_noun_1),
           u4_noun_0);
}

/* _mill_b_like()::
*/
u4_nock
_mill_b_like(u4_milr m, 
             u4_rope rid,
             u4_gene bul, 
             u4_mold tip)
{
  u4_loaf fod  = _mill_look(m, rid, tip);
  u4_mold gan  = _mill_play(m, bul, tip);
  u4_axis axe;

  if ( !u4_b_p(u4_ct(fod), u4_noun_0, &axe) ) {
    return _mill_fail(m, "fat like");
  }
  else {
    return _mill_fish(m, axe, gan);
  }
}

/* _mill_m_like()::
*/
u4_loaf
_mill_m_like(u4_milr m, 
             u4_rope rid,
             u4_gene bul, 
             u4_mold tip)
{
  u4_lane lane = m->lane;

  return u4_k_cell
    (lane, _mill_p_like(m, rid, bul, tip),
           _mill_b_like(m, rid, bul, tip));
}
