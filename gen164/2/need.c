/* j/2/need.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqb_need(
                    u2_noun a)
  {
    if ( 0 == a ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      return u2k(u2t(a));
    }
  }
  u2_noun
  u2_cwb_need(
                   u2_noun cor)
  {
    u2_noun a;

    if ( u2_none == (a = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_need(a);
    }
  }

