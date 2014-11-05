/* j/2/skip.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqb_skip(
                    u3_noun a,
                    u3_noun b)
  {
    if ( 0 == a ) {
      return a;
    }
    else if ( c3n == u3du(a) ) {
      return c3nne;
    } else {
      u3_noun hoz = u3_cn_slam_on(u3k(b), u3k(u3h(a)));
      u3_noun vyr = u3_cqb_skip(u3t(a), b);

      switch ( hoz ) {
        case c3y:  return vyr;
        case c3n:   return u3nc(u3k(u3h(a)), vyr);
        default:      u3z(hoz);
                      u3z(vyr);
                      return c3nne;
      }
    }
  }
  u3_noun
  u3_cwb_skip(
                   u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ) {
      return c3nne;
    } else {
      return u3_cqb_skip(a, b);
    }
  }

