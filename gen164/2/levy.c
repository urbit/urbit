/* j/2/levy.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, levy)(
                    u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( 0 == a ) {
      return u2_yes;
    } else {
      u2_weak loz;

      if ( u2_no == u2du(a) ) {
        return u2_cm_bail(c3__exit);
      }
      else switch ( (loz = u2_cn_slam_on(u2k(b), u2k(u2h(a)))) ) {
        case u2_yes:  return j2_mbc(Pt2, levy)(u2t(a), b);
        case u2_no:   return u2_no;
        default:      u2z(loz);
                      return u2_cm_bail(c3__exit);
      }
    }
  }
  u2_noun                                                         // transfer
  j2_mb(Pt2, levy)(
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt2, levy)(a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt2, levy)[] = {
    { ".2", c3__lite, j2_mb(Pt2, levy), Tier2, u2_none, u2_none },
    { }
  };
