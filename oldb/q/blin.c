/* mill/q/blin.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_blin():
*/
u4_noun
_mill_q_blin(u4_milr m, 
             u4_gene bov,
             u4_noun vab)
{
  u4_lane lane = m->lane;

  //  :*
  //    %link
  //    [%malk vab]
  //    bov
  //  ==
  
  return u4_k_trel
    (lane,
     u4_atom_link,
     u4_k_cell(lane, u4_atom_load, vab),
     bov);
}
