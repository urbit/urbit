#include "all.h"

u3_noun u3qc_rsh(u3_atom a, u3_atom b, u3_atom c) {
  c3_w a_w = u3a_get_cat31(a);

  if ( (a_w >= 32) ) {
    return u3m_bail(c3__fail);
  }

  if ( !_(u3a_is_cat(b)) ) {
    return UNSAFECAT(0);
  }

  c3_g a_g   = (c3_g)u3a_get_cat31(a);
  c3_w b_w   = u3a_get_cat31(b);
  c3_w len_w = u3r_met(a_g, c);

  if ( b_w >= len_w ) {
    return UNSAFECAT(0);
  }

  c3_w* sal_w = u3a_slaq(a_g, (len_w - b_w));

  if ( 0 == sal_w ) {
    return u3m_bail(c3__fail);
  }

  u3r_chop(a_g, b_w, (len_w - b_w), 0, sal_w, c);

  // return u3a_moot(sal_w);
  return u3a_malt(sal_w);
}

u3_noun u3wc_rsh(u3_noun cor) {
  u3_noun a, b, c;

  if ( !_(u3r_mean(cor, u3x_sam_2, &a,
                        u3x_sam_6, &b,
                        u3x_sam_7, &c, 0)) ||
       !_(u3ud(a)) ||
       !_(u3ud(b)) ||
       !_(u3ud(c)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_rsh(a, b, c);
}

u3_noun u3kc_rsh(u3_noun a, u3_noun b, u3_noun c) {
  u3_noun d = u3qc_rsh(a, b, c);

  u3z(a); u3z(b); u3z(c);
  return d;
}

