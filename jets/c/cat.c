/* j/3/cat.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_cat(u3_atom a,
           u3_atom b,
           u3_atom c)
  {
    if ( !_(u3a_is_cat(a)) || (a >= 32) ) {
      return u3m_bail(c3__fail);
    }
    else {
      c3_g   a_g = a;
      c3_w   lew_w = u3r_met(a_g, b);
      c3_w   ler_w = u3r_met(a_g, c);
      c3_w   all_w = (lew_w + ler_w);

      if ( 0 == all_w ) {
        return 0;
      } else {
        c3_w* sal_w = u3a_slaq(a_g, all_w);

        if ( 0 == sal_w ) {
          return u3m_bail(c3__fail);
        }
        else {
          u3r_chop(a_g, 0, lew_w, 0, sal_w, b);
          u3r_chop(a_g, 0, ler_w, lew_w, sal_w, c);
        }
        // return u3a_moot(sal_w);
        return u3a_malt(sal_w);
      }
    }
  }

  u3_noun
  u3wc_cat(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a,
                                u3x_sam_6, &b,
                                u3x_sam_7, &c, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) ||
         (c3n == u3ud(c)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_cat(a, b, c);
    }
  }

