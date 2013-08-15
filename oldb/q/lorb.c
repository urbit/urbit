/* mill/q/lorb.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_lorb():
*/
u4_noun
_mill_q_lorb(u4_milr m, 
             u4_form tul,
             u4_gene rum)
{
  u4_lane lane = m->lane;

  return u4_k_trel(lane, u4_atom_cast, _mill_lump(m, tul), rum);
}
