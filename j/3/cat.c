/* j/3/cat.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_cat(u3_atom a,
                   u3_atom b,
                   u3_atom c)
  {
    if ( !_(u3_ca_is_cat(a)) || (a >= 32) ) {
      return u3_cm_bail(c3__fail);
    }
    else {
      c3_g   a_g = a;
      c3_w   lew_w = u3_cr_met(a_g, b);
      c3_w   ler_w = u3_cr_met(a_g, c);
      c3_w   all_w = (lew_w + ler_w);

      if ( 0 == all_w ) {
        return 0;
      } else {
        c3_w* sal_w = u3_ca_slaq(a_g, all_w);

        if ( 0 == sal_w ) {
          return u3_cm_bail(c3__fail);
        }
        else {
          u3_cr_chop(a_g, 0, lew_w, 0, sal_w, b);
          u3_cr_chop(a_g, 0, ler_w, lew_w, sal_w, c);
        }
        // return u3_ca_moot(sal_w);
        return u3_ca_malt(sal_w);
      }
    }
  }

  u3_noun
  u3_cwc_cat(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( (c3n == u3_cr_mean(cor, u3_cv_sam_2, &a,
                                u3_cv_sam_6, &b,
                                u3_cv_sam_7, &c, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) ||
         (c3n == u3ud(c)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqc_cat(a, b, c);
    }
  }

