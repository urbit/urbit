/* mill/q/cell.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_cell():
*/
u4_noun
_mill_q_cell(u4_milr m, 
             u4_noun yeg,
             u4_noun nal)
{
  u4_lane lane = m->lane;

  return u4_k_qual
    (lane, u4_atom_cage, yeg, nal, u4_noun_0);
}
