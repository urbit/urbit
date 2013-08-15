/* mill/q/marn.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_marn():
*/
u4_noun
_mill_q_marn(u4_milr m, 
             u4_gene mut,
             u4_form tep,
             u4_gene von)
{
  u4_lane lane = m->lane;

  return u4_k_qual
    (lane, u4_atom_trop,
           _mill_lump(m, tep),
           u4_atom_drag,
           u4_k_cell
            (lane, 
             u4_k_cell
              (lane, 
               u4_noun_0, 
               u4_k_trel(lane, u4_atom_cast, mut, von)),
             u4_noun_0));
}
