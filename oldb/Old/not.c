/* mill/not.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_not(): invert boolean.
*/
u4_type
_mill_not(u4_milr m,
          u4_form zet)
{
  u4_lane lane = m->lane;

  if ( u4_n_eq(u4_noun_1, u4_ch(zet)) ) {
    if ( u4_n_eq(u4_noun_0, u4_ct(zet)) ) {
      return u4_k_cell(lane, u4_noun_1, u4_noun_1);
    }
    else {
      u4_assert(u4_n_eq(u4_noun_1, u4_ct(zet)));

      return u4_k_cell(lane, u4_noun_1, u4_noun_0);
    }
  }
  else {
    return u4_k_qual
      (lane, u4_noun_2, 
             zet, 
             u4_k_cell(lane, u4_noun_1, u4_noun_1), 
             u4_k_cell(lane, u4_noun_1, u4_noun_0));
  }
}
