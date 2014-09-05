/* j/2/lent.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqb_lent(u3_noun a)
  {
    u3_noun len = 0;

    while ( 1 ) {
      if ( 0 == a ) {
        return len;
      }
      else if ( u3_no == u3du(a) ) {
        u3z(len);
        return u3_cm_bail(c3__exit);
      }
      else {
        len = u3_ci_vint(len);
        a = u3t(a);
      }
    }
  }
  u3_noun
  u3_cwb_lent(u3_noun cor)
  {
    u3_noun a;

    if ( u3_none == (a = u3_cr_at(u3_cv_sam, cor)) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqb_lent(a);
    }
  }
  u3_noun
  u3_ckb_lent(u3_noun a)
  {
    u3_noun b = u3_cqb_lent(a);

    u3z(a);
    return b;
  }

