/* j/3/rsh.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_rsh(
                   u3_atom a,
                   u3_atom b,
                   u3_atom c)
  {
    if ( u3_ne(u3_ca_is_cat(a)) || (a >= 32) ) {
      return u3_cm_bail(c3__fail);
    }
    else if ( u3_ne(u3_ca_is_cat(b)) ) {
      return 0;
    }
    else {
      c3_g a_g   = a;
      c3_w b_w   = b;
      c3_w len_w = u3_cr_met(a_g, c);

      if ( b_w >= len_w ) {
        return 0;
      }
      else {
        c3_w* sal_w = u3_ca_slaq(a_g, (len_w - b_w));

        if ( 0 == sal_w ) {
          return u3_cm_bail(c3__fail);
        }
        u3_cr_chop(a_g, b_w, (len_w - b_w), 0, sal_w, c);

        // return u3_ca_moot(sal_w);
        return u3_ca_malt(sal_w);
      }
    }
  }
  u3_noun
  u3_cwc_rsh(
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
      return u3_cqc_rsh(a, b, c);
    }
  }
  u3_noun
  u3_ckc_rsh(u3_noun a, u3_noun b, u3_noun c)
  {
    u3_noun d = u3_cqc_rsh(a, b, c);

    u3z(a); u3z(b); u3z(c);
    return d;
  }

