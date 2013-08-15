/* mill/q/gril.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_q_gril():
*/
u4_noun
_mill_q_gril(u4_milr m, 
             u4_rope mox, 
             u4_noun bem)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(bem) ) {
    return u4_k_cell(lane, u4_atom_bail, u4_noun_0);
  }
  else {
    u4_pike i_bem = u4_ch(bem);
    u4_noun t_bem = u4_ct(bem);
    u4_gene tes;
    u4_gene bif;

    _mill_pike(m, mox, i_bem, &tes, &bif);

    return u4_k_qual
      (lane,
       u4_atom_quiz, 
       tes, 
       bif,
       _mill_q_gril(m, mox, t_bem));
  }
}
