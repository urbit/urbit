/* mill/or.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_or(): make or.
*/
u4_form
_mill_or(u4_milr m,
         u4_form bos,
         u4_form nif)
{
  u4_lane lane = m->lane;

  if ( u4_n_eq(u4_noun_1, u4_ch(bos)) ) {
    if ( u4_n_zero(u4_ct(bos)) ) {
      return bos;
    }
    else return nif;
  }
  else {
    if ( u4_n_eq(u4_noun_1, u4_ch(nif)) ) {
      if ( u4_n_zero(u4_ct(nif)) ) {
        return nif;
      }
      else return bos;
    }
    else {
      return u4_k_qual
        (lane, u4_noun_2, bos, u4_k_cell(lane, u4_noun_1, u4_noun_0), nif);
    }
  }
}
