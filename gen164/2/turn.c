/* j/2/turn.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, turn)(u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( 0 == a ) {
      return a;
    }
    else if ( u2_no == u2du(a) ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      u2_noun one = u2_cn_slam_on(b, u2k(u2h(a)));
      u2_noun two = j2_mbc(Pt2, turn)(u2t(a), b);

      return u2nc(one, two);
    }
  }
  u2_noun                                                         // transfer
  j2_mb(Pt2, turn)(
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt2, turn)(a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt2, turn)[] = {
    { ".2", c3__lite, j2_mb(Pt2, turn), Tier2, u2_none, u2_none },
    { }
  };
