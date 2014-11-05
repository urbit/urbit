/* j/2/flop.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqb_flop(u3_noun a)
  {
    u3_noun b = 0;

    while ( 1 ) {
      if ( u3_nul == a ) {
        return b;
      }
      else if ( c3n == u3du(a) ) {
        u3z(b);

        return u3_cm_bail(c3__exit);
      }
      else {
        b = u3nc(u3k(u3h(a)), b);
        a = u3t(a);
      }
    }
  }
  u3_noun
  u3_cwb_flop(u3_noun cor)
  {
    u3_noun a;

    if ( u3_none == (a = u3_cr_at(u3_cv_sam, cor)) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqb_flop(a);
    }
  }
  u3_noun
  u3_ckb_flop(u3_noun a)
  {
    u3_noun b = u3_cqb_flop(a);

    u3z(a);
    return b;
  }

