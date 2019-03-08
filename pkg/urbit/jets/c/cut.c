#include "all.h"

u3_noun u3qc_cut(u3_atom a, u3_atom b, u3_atom c, u3_atom d) {
  c3_w a_w = u3a_get_cat31(a);

  if ( (a_w >= 32) ) {
    return u3m_bail(c3__fail);
  }

  c3_g a_g = a_w;

  if ( !_(u3a_is_cat(b)) ) {
    return UNSAFECAT(0);
  }

  c3_w b_w   = u3a_get_cat31(b);
  c3_w c_w   = _(u3a_is_cat(c)) ? u3a_get_cat31(c) : 0x7fffffff;
  c3_w len_w = u3r_met(a_g, d);

  if ( (0 == c_w) || (b_w >= len_w) ) {
    return UNSAFECAT(0);
  }

  if ( b_w + c_w > len_w ) {
    c_w = (len_w - b_w);
  }

  if ( (b_w == 0) && (c_w == len_w) ) {
    return u3k(d);
  }

  c3_w* sal_w = u3a_slaq(a_g, c_w);

  if ( 0 == sal_w ) {
    return u3m_bail(c3__fail);
  }

  u3r_chop(a_g, b_w, c_w, 0, sal_w, d);

  return u3a_malt(sal_w);
}

u3_noun u3wc_cut(u3_noun cor) {
  u3_noun a, b, c, d;

  if ( !_(u3r_mean(cor, u3x_sam_2,  &a,
                        u3x_sam_12, &b,
                        u3x_sam_13, &c,
                        u3x_sam_7,  &d, 0)) ||
       !_(u3ud(a)) ||
       !_(u3ud(b)) ||
       !_(u3ud(c)) ||
       !_(u3ud(d)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_cut(a, b, c, d);
}
