/* j/3/hor.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  u2_cqc_hor(u2_noun a,                                     //  retain
                   u2_noun b)                                     //  retain
  {
    if ( u2_yes == u2ud(a) ) {
      if ( u2_yes == u2ud(b) ) {
        return u2_cqc_gor(a, b);
      } else {
        return u2_yes;
      }
    } else {
      if ( u2_yes == u2ud(b) ) {
        return u2_no;
      }
      else {
        u2_noun h_a = u2h(a);
        u2_noun h_b = u2h(b);

        if ( u2_yes == u2_cr_sing(h_a, h_b) ) {
          return u2_cqc_gor(u2t(a), u2t(b));
        } else {
          return u2_cqc_gor(h_a, h_b);
        }
      }
    }
  }
  u2_weak                                                         //  transfer
  u2_cwc_hor(u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqc_hor(a, b);
    }
  }

