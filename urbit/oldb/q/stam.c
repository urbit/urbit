/* mill/q/stam.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_stam():
*/
u4_noun
_mill_q_stam(u4_milr m, 
             u4_gene feg,
             u4_noun bem)
{
  u4_lane lane = m->lane;

  //  :*
  //    %trop
  //    feg
  //    :*
  //      %gril
  //      [%frag 2]
  //      bem
  //    ==
  //  ==
  //
  return u4_k_trel
   (lane, 
    u4_atom_trop,
    feg,
    u4_k_trel
     (lane,
      u4_atom_gril,
      u4_k_cell(lane, u4_k_cell(lane, u4_atom_frag, u4_noun_2), u4_noun_0),
      bem));
}
