/* mill/eith.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_eith(): forq pair.
*/
u4_mold
_mill_eith(u4_milr m,
           u4_mold lef,
           u4_mold gap)
{
  u4_lane lane = m->lane;

  if ( u4_n_eq(lef, gap) ) {
    return lef;
  }
  else if ( u4_n_eq(u4_atom_blur, lef) )  {
    return lef;
  }
  else if ( u4_n_eq(u4_atom_blot, lef) ) {
    return gap;
  }
  else if ( u4_n_eq(u4_atom_blur, gap) ) {
    return gap;
  }
  else if ( u4_n_eq(u4_atom_blot, gap) ) {
    return lef;
  }
  else {
    u4_noun p_lef, p_gap;

    if ( !u4_b_p(lef, u4_atom_forq, &p_lef) ) {
      p_lef = u4_k_cell(lane, lef, u4_noun_0);
    }
    if ( !u4_b_p(gap, u4_atom_forq, &p_gap) ) {
      p_gap = u4_k_cell(lane, gap, u4_noun_0);
    }
    return u4_k_cell(lane, u4_atom_forq, u4_log_cat(lane, p_lef, p_gap));
  }
}
