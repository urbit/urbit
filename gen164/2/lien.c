/* j/2/lien.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  u2_cqb_lien(u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( 0 == a ) {
      return u2_no;
    } else {
      u2_weak loz;

      if ( u2_no == u2du(a) ) {
        return u2_cm_bail(c3__exit);
      }
      else switch ( (loz = u2_cn_slam_on(u2k(b), u2k(u2h(a)))) ) {
        case u2_yes:  return u2_yes;
        case u2_no:   return u2_cqb_lien(u2t(a), b);
        default:      u2z(loz);
                      return u2_cm_bail(c3__exit);
      }
    }
  }
  u2_noun                                                         // transfer
  u2_cwb_lien(u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_lien(a, b);
    }
  }
