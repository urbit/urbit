/* mill/q/trop.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_trop():
*/
u4_noun
_mill_q_trop(u4_milr m, 
             u4_gene hig,
             u4_gene muc)
{
  u4_lane lane = m->lane;

  return u4_k_trel
    (lane, u4_atom_link,
           u4_k_qual
            (lane, u4_atom_cage, 
                   hig,
                   u4_k_cell(lane, u4_atom_frag, u4_noun_1),
                   u4_noun_0),
           muc);
}
