/* j/3/met.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_met(
                   u3_atom a,
                   u3_atom b)
  {
    if ( u3_ne(u3_co_is_cat(a)) || (a >= 32) ) {
      if ( 0 == b ) {
        return 0;
      } else return 1;
    }
    else {
      c3_w met_w = u3_cr_met(a, b);

      if ( u3_ne(u3_co_is_cat(met_w)) ) {
        return u3_ci_words(1, &met_w);
      }
      else return u3_cr_met(a, b);
    }
  }
  u3_noun
  u3_cwc_met(
                  u3_noun cor)
  {
    u3_noun a, b;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0)) ||
         (u3_no == u3ud(a)) ||
         (u3_no == u3ud(b)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqc_met(a, b);
    }
  }

