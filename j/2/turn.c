/* j/2/turn.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqb_turn(u2_noun a,
                    u2_noun b)
  {
    if ( 0 == a ) {
      return a;
    }
    else if ( u2_no == u2du(a) ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      u2_noun one = u2_cn_slam_on(b, u2k(u2h(a)));
      u2_noun two = u2_cqb_turn(u2t(a), b);

      return u2nc(one, two);
    }
  }
  u2_noun
  u2_cwb_turn(
                   u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_turn(a, b);
    }
  }

