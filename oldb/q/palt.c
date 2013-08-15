/* mill/q/palt.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_palt():
*/
u4_noun
_mill_q_palt(u4_milr m, 
             u4_noun daq)
{
  u4_lane lane = m->lane;

  return u4_k_trel
    (lane, u4_atom_cast, 
           u4_k_qual(lane, u4_atom_nock, u4_noun_5, u4_atom_rock, u4_noun_0),
           u4_k_cell(lane, u4_atom_rock, daq));
}
