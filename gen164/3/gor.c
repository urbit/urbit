/* j/3/gor.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  u2_cqc_gor(u2_noun a,                                     //  retain
                   u2_noun b)                                     //  retain
  {
    c3_w c_w = u2_cr_mug(a);
    c3_w d_w = u2_cr_mug(b);

    if ( c_w == d_w ) {
      return u2_cqc_dor(a, b);
    }
    else return (c_w < d_w) ? u2_yes : u2_no;
  }
  u2_weak                                                         //  transfer
  u2_cwc_gor(u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqc_gor(a, b);
    }
  }

