/* mill/q/read.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_read():
*/
u4_noun
_mill_q_read(u4_milr m, 
             u4_wire ved)
{
  u4_lane lane = m->lane;

  return u4_k_trel(lane, u4_atom_kick, ved, u4_noun_0);
}
