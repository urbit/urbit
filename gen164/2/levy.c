/* j/2/levy.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun
  u2_cqb_levy(
                    u2_noun a,
                    u2_noun b)
  {
    if ( 0 == a ) {
      return u2_yes;
    } else {
      u2_noun loz;

      if ( u2_no == u2du(a) ) {
        return u2_cm_bail(c3__exit);
      }
      else switch ( (loz = u2_cn_slam_on(u2k(b), u2k(u2h(a)))) ) {
        case u2_yes:  return u2_cqb_levy(u2t(a), b);
        case u2_no:   return u2_no;
        default:      u2z(loz);
                      return u2_cm_bail(c3__exit);
      }
    }
  }
  u2_noun
  u2_cwb_levy(
                   u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_levy(a, b);
    }
  }
