/* j/3/end.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_end(
                   u3_atom a,
                   u3_atom b,
                   u3_atom c)
  {
    if ( u3_ne(u3_ca_is_cat(a)) || (a >= 32) ) {
      return u3_cm_bail(c3__fail);
    }
    else if ( u3_ne(u3_ca_is_cat(b)) ) {
      return u3k(c);
    }
    else {
      c3_g a_g   = a;
      c3_w b_w   = b;
      c3_w len_w = u3_cr_met(a_g, c);

      if ( 0 == b_w ) {
        return 0;
      }
      else if ( b_w >= len_w ) {
        return u3k(c);
      }
      else {
        c3_w* sal_w = u3_ca_slaq(a_g, b_w);

        if ( 0 == sal_w ) {
          return u3_cm_bail(c3__fail);
        }
        u3_cr_chop(a_g, 0, b_w, 0, sal_w, c);

        return u3_ca_malt(sal_w);
      }
    }
  }
  u3_noun
  u3_cwc_end(
                  u3_noun cor)
  {
    u3_noun a, b, c;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a,
                                u3_cv_sam_6, &b,
                                u3_cv_sam_7, &c, 0)) ||
         (u3_no == u3ud(a)) ||
         (u3_no == u3ud(b)) ||
         (u3_no == u3ud(c)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqc_end(a, b, c);
    }
  }

