/* mill/q/mang.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_mang():
*/
u4_noun
_mill_q_mang(u4_milr m, 
             u4_gene buz,
             u4_gene lep)
{
  u4_lane lane = m->lane;

  return u4_k_qual
    (lane, u4_atom_grad, 
           u4_k_cell(lane, u4_noun_0, u4_noun_0),
           buz, lep);
}
