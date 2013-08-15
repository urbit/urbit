/* mill/x/bail.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_p_bail()::
*/
u4_mold
_mill_p_bail(u4_milr m, 
             u4_noun guk)
{
  return u4_atom_blot;
}

/* _mill_b_bail()::
*/
u4_nock
_mill_b_bail(u4_milr m, 
             u4_noun guk)
{
  u4_lane lane  = m->lane;

  return u4_k_cell(lane, u4_noun_0, u4_noun_0);
}

/* _mill_m_bail()::
*/
u4_loaf
_mill_m_bail(u4_milr m, 
             u4_noun guk)
{
  u4_lane lane  = m->lane;

  u4_assert(u4_noun_0 == guk);

  return u4_k_cell
      (lane, _mill_p_bail(m, guk),
             _mill_b_bail(m, guk));
}
