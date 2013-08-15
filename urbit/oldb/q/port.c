/* mill/q/port.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_port():
*/
u4_noun
_mill_q_port(u4_milr m, 
             u4_atom tic,
             u4_mark vix)
{
  u4_lane lane = m->lane;

  return u4_k_trel
    (lane, u4_atom_kick,
           u4_k_cell(lane, u4_k_trel(lane, u4_atom_port, tic, vix), u4_noun_0),
           u4_noun_0);
}
