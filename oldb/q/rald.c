/* mill/q/rald.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_rald():
*/
u4_noun
_mill_q_rald(u4_milr m, 
             u4_gene rum)
{
  u4_lane lane = m->lane;

  // [%cast [%nock 3 %rock 0] rum]
  
  return u4_k_trel
    (lane, u4_atom_cast,
           u4_k_qual(lane, u4_atom_nock, u4_noun_3, u4_atom_rock, u4_noun_0),
           rum);
}
