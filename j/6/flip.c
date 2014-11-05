/* j/6/flip.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqf_flip(
                    u3_noun hel)
  {
    if ( c3y == u3_cr_sing(1, u3h(hel)) ) {
      if ( (c3y == u3t(hel)) ) {
        return u3nc(1, c3n);
      }
      else {
        c3_assert((c3n == u3t(hel)));

        return u3nc(1, c3y);
      }
    }
    else {
      return u3nq
        (6,
               u3k(hel),
               u3nc(1, c3n),
               u3nc(1, c3y));
    }
  }
  u3_noun
  u3_cwf_flip(
                   u3_noun cor)
  {
    u3_noun hel;

    if ( u3_none == (hel = u3_cr_at(u3_cv_sam, cor)) ) {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqf_flip(hel);
    }
  }
