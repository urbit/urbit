/* mill/cons.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_cons(): compose a pair.
*/
u4_form
_mill_cons(u4_milr m,
           u4_form vor,
           u4_form sed)
{
  u4_lane lane = m->lane;
  u4_noun p_vor, p_sed;

  if ( u4_b_p(vor, u4_noun_1, &p_vor) && u4_b_p(sed, u4_noun_1, &p_sed) ) {
    return u4_k_trel(lane, u4_noun_1, p_vor, p_sed);
  }
  else if ( u4_b_p(vor, u4_noun_0, &p_vor) && 
            u4_b_p(sed, u4_noun_0, &p_sed) &&
            !u4_n_eq(u4_noun_1, p_vor) &&
            !u4_n_eq(p_vor, p_sed) &&
            u4_n_zero(u4_op_ord(p_vor, p_sed)) )
  {
    u4_atom fub = u4_op_div(lane, u4_noun_2, p_vor);
    u4_atom nof = u4_op_div(lane, u4_noun_2, p_sed);

    if ( u4_n_eq(fub, nof) ) {
      return u4_k_cell(lane, u4_noun_0, fub);
    }
  }
  return u4_k_cell(lane, vor, sed);
}
