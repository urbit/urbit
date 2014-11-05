/* j/2/skim.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqb_skim(
                    u3_noun a,
                    u3_noun b)
  {
    if ( 0 == a ) {
      return a;
    }
    else if ( c3n == u3du(a) ) {
      return u3_cm_bail(c3__exit);
    } else {
      u3_noun hoz = u3_cn_slam_on(u3k(b), u3k(u3h(a)));
      u3_noun vyr = u3_cqb_skim(u3t(a), b);

      switch ( hoz ) {
        case c3y:  return u3nc(u3k(u3h(a)), vyr);
        case c3n:   return vyr;
        default:      u3z(hoz);
                      u3z(vyr);
                      return u3_cm_bail(c3__exit);
      }
    }
  }
  u3_noun
  u3_cwb_skim(
                   u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqb_skim(a, b);
    }
  }

