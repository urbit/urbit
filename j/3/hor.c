/* j/3/hor.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_hor(u3_noun a,
                   u3_noun b)
  {
    if ( c3y == u3ud(a) ) {
      if ( c3y == u3ud(b) ) {
        return u3_cqc_gor(a, b);
      } else {
        return c3y;
      }
    } else {
      if ( c3y == u3ud(b) ) {
        return c3n;
      }
      else {
        u3_noun h_a = u3h(a);
        u3_noun h_b = u3h(b);

        if ( c3y == u3_cr_sing(h_a, h_b) ) {
          return u3_cqc_gor(u3t(a), u3t(b));
        } else {
          return u3_cqc_gor(h_a, h_b);
        }
      }
    }
  }
  u3_noun
  u3_cwc_hor(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0)) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqc_hor(a, b);
    }
  }

