/* j/2/drop.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqb_drop(u2_noun a)
  {
    if ( 0 == a ) {
      return u2_nul;
    }
    else {
      return u2nc(0, u2k(u2t(a)));
    }
  }
  u2_noun
  u2_cwb_drop(u2_noun cor)
  {
    u2_noun a;

    if ( u2_none == (a = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_none;
    } else {
      return u2_cqb_drop(a);
    }
  }

