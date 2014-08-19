/* j/2/need.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, need)(
                    u2_noun a)                                    //  retain
  {
    if ( 0 == a ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      return u2k(u2t(a));
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt2, need)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun a;

    if ( u2_none == (a = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt2, need)(a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt2, need)[] = {
    { ".2", c3__lite, j2_mb(Pt2, need), Tier2, u2_none, u2_none },
    { }
  };
