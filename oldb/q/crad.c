/* mill/q/crad.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_crad():
*/
u4_noun
_mill_q_crad(u4_milr m, 
             u4_noun daq)
{
  u4_lane lane = m->lane;

  return u4_k_trel(lane, u4_atom_rald, u4_atom_rock, daq);
}
