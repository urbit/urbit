/* j/2/sort.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqb_sort(
                    u2_noun a,
                    u2_noun b)
  {
    //  must think about
    //
    return u2_cm_bail(c3__fail);
  }
  u2_noun
  u2_cwb_sort(
                   u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_sort(a, b);
    }
  }

