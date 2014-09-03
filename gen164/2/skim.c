/* j/2/skim.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  u2_cqb_skim(
                    u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( 0 == a ) {
      return a;
    }
    else if ( u2_no == u2du(a) ) {
      return u2_cm_bail(c3__exit);
    } else {
      u2_weak hoz = u2_cn_slam_on(u2k(b), u2k(u2h(a)));
      u2_weak vyr = u2_cqb_skim(u2t(a), b);

      switch ( hoz ) {
        case u2_yes:  return u2nc(u2k(u2h(a)), vyr);
        case u2_no:   return vyr;
        default:      u2z(hoz);
                      u2z(vyr);
                      return u2_cm_bail(c3__exit);
      }
    }
  }
  u2_noun                                                         // transfer
  u2_cwb_skim(
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_skim(a, b);
    }
  }

