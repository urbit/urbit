/* j/2/reel.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, reel)(
                    u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( 0 == a ) {
      return u2k(u2_cr_at(u2_cv_sam_3, b));
    }
    else if ( u2_no == u2du(a) ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      u2_weak gim = u2k(u2h(a));
      u2_weak hur = j2_mbc(Pt2, reel)(u2t(a), b);

      return u2_cn_slam_on(u2k(b), u2nc(gim, hur));
    }
  }
  u2_noun                                                         // transfer
  j2_mb(Pt2, reel)(
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt2, reel)(a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt2, reel)[] = {
    { ".2", c3__lite, j2_mb(Pt2, reel), Tier2, u2_none, u2_none },
    { }
  };

