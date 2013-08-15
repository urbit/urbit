/* mill/q/qual.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_qual():
*/
u4_noun
_mill_q_qual(u4_milr m, 
             u4_noun dor,
             u4_noun vit,
             u4_noun lum, 
             u4_noun sag)
{
  u4_lane lane = m->lane;

  return u4_k_cell
    (lane, u4_atom_cage, 
           u4_k_quil(lane, dor, vit, lum, sag, u4_noun_0));
}
