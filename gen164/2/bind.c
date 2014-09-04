/* j/2/bind.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqb_bind(
                    u2_noun a,
                    u2_noun b)
  {
    if ( 0 == a ) {
      return 0;
    } else {
      return u2nc(0, u2_cn_slam_on(u2k(b), u2k(u2t(a))));
    }
  }
  u2_noun
  u2_cwb_bind(
                   u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_bind(a, b);
    }
  }

