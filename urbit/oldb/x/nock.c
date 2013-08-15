/* mill/x/nock.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_p_nock()::
*/
u4_mold
_mill_p_nock(u4_milr m, 
             u4_atom fiz,
             u4_gene dil, 
             u4_mold tip)
{
  u4_lane lane  = m->lane;

  if ( u4_n_eq(u4_noun_3, fiz) ) {
    return u4_atom_blur;
  }
  else if ( u4_n_eq(u4_noun_4, fiz) || u4_n_eq(u4_noun_6, fiz) ) {
    return u4_k_qual
      (lane, u4_atom_forq,
             u4_k_cell(lane, u4_atom_cube, u4_noun_0),
             u4_k_cell(lane, u4_atom_cube, u4_noun_1),
             u4_noun_0);
  }
  else if ( u4_n_eq(u4_noun_5, fiz) ) {
    return u4_atom_atom;
  }
  else return _mill_fail(m, "odd nock");
}

/* _mill_b_nock()::
*/
u4_nock
_mill_b_nock(u4_milr m, 
             u4_atom fiz,
             u4_gene dil, 
             u4_mold tip)
{
  u4_lane lane = m->lane;

  // XX: test safety
  //
  return u4_k_cell(lane, fiz, _mill_bake(m, dil, tip));
}

/* _mill_m_nock()::
*/
u4_loaf
_mill_m_nock(u4_milr m, 
             u4_atom fiz,
             u4_gene dil, 
             u4_mold tip)
{
  u4_lane lane = m->lane;

  return u4_k_cell
    (lane, _mill_p_nock(m, fiz, dil, tip), 
           _mill_b_nock(m, fiz, dil, tip));
}
