/* mill/cond.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_cond(): compose an if.
*/
u4_form
_mill_cond(u4_milr m,
           u4_form nef,
           u4_form buz,
           u4_form dav)
{
  if ( u4_n_eq(u4_ch(nef), u4_noun_1) ) {
    if ( u4_n_eq(u4_ct(nef), u4_noun_0) ) {
      return buz;
    }
    else return dav;
  }
  else {
    return u4_k_qual(m->lane, u4_noun_2, nef, buz, dav);
  }
}
