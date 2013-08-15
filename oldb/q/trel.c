/* mill/q/trel.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_trel():
*/
u4_noun
_mill_q_trel(u4_milr m, 
             u4_noun ruv,
             u4_noun sel,
             u4_noun gar)
{
  u4_lane lane = m->lane;

  return u4_k_quil
    (lane, u4_atom_cage, ruv, sel, gar, u4_noun_0);
}
