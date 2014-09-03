/* j/2/sort.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  u2_cqb_sort(
                    u2_noun a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    //  must think about
    //
    return u2_cm_bail(c3__fail);
  }
  u2_noun                                                         // transfer
  u2_cwb_sort(
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqb_sort(a, b);
    }
  }

