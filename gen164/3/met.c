/* j/3/met.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqc_met(
                   u2_atom a,
                   u2_atom b)
  {
    if ( u2_ne(u2_co_is_cat(a)) || (a >= 32) ) {
      if ( 0 == b ) {
        return 0;
      } else return 1;
    }
    else {
      c3_w met_w = u2_cr_met(a, b);

      if ( u2_ne(u2_co_is_cat(met_w)) ) {
        return u2_ci_words(1, &met_w);
      }
      else return u2_cr_met(a, b);
    }
  }
  u2_noun
  u2_cwc_met(
                  u2_noun cor)
  {
    u2_noun a, b;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ||
         (u2_no == u2ud(a)) ||
         (u2_no == u2ud(b)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqc_met(a, b);
    }
  }

