/* j/2/bind.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, bind)(
                    u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( 0 == a ) {
      return 0;
    } else {
      return u2nc(0, u2_cn_slam_on(u2k(b), u2k(u2t(a))));
    }
  }
  u2_noun                                                         // transfer
  j2_mb(Pt2, bind)(
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt2, bind)(a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt2, bind)[] = {
    { ".2", c3__lite, j2_mb(Pt2, bind), Tier2, u2_none, u2_none },
    { }
  };

