/* j/2/clap.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqb_clap(u3_noun a,
                    u3_noun b,
                    u3_noun c)
  {
    if ( 0 == a ) {
      return u3k(b);
    }
    else if ( 0 == b ) {
      return u3k(a);
    }
    else {
      return u3nc(0, u3_cn_slam_on(u3k(c), u3nc(u3k(u3t(a)), u3k(u3t(b)))));
    }
  }
  u3_noun
  u3_cwb_clap(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam_2, &a,
                               u3_cv_sam_6, &b,
                               u3_cv_sam_7, &c, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqb_clap(a, b, c);
    }
  }
