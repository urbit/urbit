/* j/3/vor.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_vor(u3_atom a,
                   u3_atom b)
  {
    c3_w c_w = u3_cr_mug(u3_cr_mug(a));
    c3_w d_w = u3_cr_mug(u3_cr_mug(b));

    if ( c_w == d_w ) {
      return u3_cqc_dor(a, b);
    }
    else return (c_w < d_w) ? u3_yes : u3_no;
  }
  u3_noun
  u3_cwc_vor(u3_noun cor)
  {
    u3_noun a, b;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0)) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqc_vor(a, b);
    }
  }
