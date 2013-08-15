/* mill/and.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_and(): make and.
*/
u4_nock
_mill_and(u4_milr m,
          u4_nock zeg,
          u4_nock dac)
{
  u4_lane lane = m->lane;

  if ( u4_n_eq(u4_noun_1, u4_ch(zeg)) ) {
    if ( u4_n_zero(u4_ct(zeg)) ) {
      return dac;
    }
    else return zeg;
  }
  else {
    if ( u4_n_eq(u4_noun_1, u4_ch(dac)) ) {
      if ( u4_n_zero(u4_ct(dac)) ) {
        return zeg;
      }
      else return dac;
    }
    else {
      return u4_k_qual
        (lane, u4_noun_2, zeg, dac, u4_k_cell(lane, u4_noun_1, u4_noun_1));
    }
  }
}
