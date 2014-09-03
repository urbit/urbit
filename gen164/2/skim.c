/* j/2/skim.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun
  u2_cqb_skim(
                    u2_noun a,
                    u2_noun b)
  {
    if ( 0 == a ) {
      return a;
    }
    else if ( u2_no == u2du(a) ) {
      return u2_cm_bail(c3__exit);
    } else {
      u2_noun hoz = u2_cn_slam_on(u2k(b), u2k(u2h(a)));
      u2_noun vyr = u2_cqb_skim(u2t(a), b);

      switch ( hoz ) {
        case u2_yes:  return u2nc(u2k(u2h(a)), vyr);
        case u2_no:   return vyr;
        default:      u2z(hoz);
                      u2z(vyr);
                      return u2_cm_bail(c3__exit);
      }
    }
  }
  u2_noun
  u2_cwb_skim(
                   u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_skim(a, b);
    }
  }

