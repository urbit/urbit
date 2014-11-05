/* j/2/levy.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqb_levy(
                    u3_noun a,
                    u3_noun b)
  {
    if ( 0 == a ) {
      return c3y;
    } else {
      u3_noun loz;

      if ( c3n == u3du(a) ) {
        return u3_cm_bail(c3__exit);
      }
      else switch ( (loz = u3_cn_slam_on(u3k(b), u3k(u3h(a)))) ) {
        case c3y:  return u3_cqb_levy(u3t(a), b);
        case c3n:   return c3n;
        default:      u3z(loz);
                      return u3_cm_bail(c3__exit);
      }
    }
  }
  u3_noun
  u3_cwb_levy(
                   u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqb_levy(a, b);
    }
  }
