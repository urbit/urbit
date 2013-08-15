/* mill/q/rack.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _rack_in():
*/
u4_noun
_rack_in(u4_milr m,
         u4_noun der)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(der) ) {
    return u4_k_cell
      (lane, u4_k_cell(lane, u4_atom_rock, u4_noun_0), u4_noun_0);
  }
  else {
    return u4_k_cell(lane, u4_ch(der), _rack_in(m, u4_ct(der)));
  }
}

/* _mill_q_rack():
*/
u4_noun
_mill_q_rack(u4_milr m, 
             u4_noun der)
{
  u4_lane lane = m->lane;

  return u4_k_cell(lane, u4_atom_cage, _rack_in(m, der));
}
