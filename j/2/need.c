/* j/2/need.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqb_need(
                    u3_noun a)
  {
    if ( 0 == a ) {
      return u3_cm_bail(c3__exit);
    }
    else {
      return u3k(u3t(a));
    }
  }
  u3_noun
  u3_cwb_need(
                   u3_noun cor)
  {
    u3_noun a;

    if ( u3_none == (a = u3_cr_at(u3_cv_sam, cor)) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqb_need(a);
    }
  }

