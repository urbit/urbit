/* j/2/reel.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqb_reel(
                    u2_noun a,
                    u2_noun b)
  {
    if ( 0 == a ) {
      return u2k(u2_cr_at(u2_cv_sam_3, b));
    }
    else if ( u2_no == u2du(a) ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      u2_noun gim = u2k(u2h(a));
      u2_noun hur = u2_cqb_reel(u2t(a), b);

      return u2_cn_slam_on(u2k(b), u2nc(gim, hur));
    }
  }
  u2_noun
  u2_cwb_reel(
                   u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_reel(a, b);
    }
  }
