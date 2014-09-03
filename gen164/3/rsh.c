/* j/3/rsh.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun
  u2_cqc_rsh(
                   u2_atom a,
                   u2_atom b,
                   u2_atom c)
  {
    if ( u2_ne(u2_co_is_cat(a)) || (a >= 32) ) {
      return u2_cm_bail(c3__fail);
    }
    else if ( u2_ne(u2_co_is_cat(b)) ) {
      return 0;
    }
    else {
      c3_g a_g   = a;
      c3_w b_w   = b;
      c3_w len_w = u2_cr_met(a_g, c);

      if ( b_w >= len_w ) {
        return 0;
      }
      else {
        c3_w* sal_w = u2_ca_slaq(a_g, (len_w - b_w));

        if ( 0 == sal_w ) {
          return u2_cm_bail(c3__fail);
        }
        u2_cr_chop(a_g, b_w, (len_w - b_w), 0, sal_w, c);

        // return u2_ca_moot(sal_w);
        return u2_ca_malt(sal_w);
      }
    }
  }
  u2_noun
  u2_cwc_rsh(
                  u2_noun cor)
  {
    u2_noun a, b, c;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a,
                                u2_cv_sam_6, &b,
                                u2_cv_sam_7, &c, 0)) ||
         (u2_no == u2ud(a)) ||
         (u2_no == u2ud(b)) ||
         (u2_no == u2ud(c)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqc_rsh(a, b, c);
    }
  }

