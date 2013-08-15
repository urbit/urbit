/* mill/q/grad.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_grad():
*/
u4_noun
_mill_q_grad(u4_milr m, 
             u4_wire luq,
             u4_gene buz,
             u4_gene lep)
{
  u4_lane lane = m->lane;

  return u4_k_trel
    (lane, 
     u4_atom_trop,
     buz,
     u4_k_trel
      (lane,
       u4_atom_kick,
       u4_k_cell
        (lane, 
         u4_k_cell(lane, u4_atom_frag, u4_noun_2),
         luq),
       u4_k_cell
        (lane,
         u4_k_cell
          (lane,
           u4_k_cell
            (lane, u4_k_cell(lane, u4_atom_frag, u4_noun_4), u4_noun_0),
           u4_k_trel
            (lane, 
             u4_atom_link, 
             u4_k_cell(lane, u4_atom_frag, u4_noun_3), 
             lep)),
         u4_noun_0)));
}
