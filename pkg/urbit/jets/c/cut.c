/* j/3/cut.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_cut(u3_atom a,
           u3_atom b,
           u3_atom c,
           u3_atom d)
  {
    if ( !_(u3a_is_cat(a)) || (a >= 32) ) {
      return u3m_bail(c3__fail);
    }
    if ( !_(u3a_is_cat(b)) ) {
      return 0;
    }
    if ( !_(u3a_is_cat(c)) ) {
      c = 0x7fffffff;
    }

    {
      c3_g a_g   = a;
      c3_w b_w   = b;
      c3_w c_w   = c;
      c3_w len_w = u3r_met(a_g, d);

      if ( (0 == c_w) || (b_w >= len_w) ) {
        return 0;
      }
      if ( b_w + c_w > len_w ) {
        c_w = (len_w - b_w);
      }
      if ( (b_w == 0) && (c_w == len_w) ) {
        return u3k(d);
      }
      else {
        c3_w* sal_w = u3a_slaq(a_g, c_w);

        if ( 0 == sal_w ) {
          return u3m_bail(c3__fail);
        }
        u3r_chop(a_g, b_w, c_w, 0, sal_w, d);

        return u3a_malt(sal_w);
      }
    }
  }
  u3_noun
  u3wc_cut(u3_noun cor)
  {
    u3_noun a, b, c, d;

    if ( (c3n == u3r_mean(cor, u3x_sam_2,  &a,
                                u3x_sam_12, &b,
                                u3x_sam_13, &c,
                                u3x_sam_7,  &d, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) ||
         (c3n == u3ud(c)) ||
         (c3n == u3ud(d)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_cut(a, b, c, d);
    }
  }

