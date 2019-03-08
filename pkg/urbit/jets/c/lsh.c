#include "all.h"

u3_noun u3qc_lsh(u3_atom a, u3_atom b, u3_atom c) {
  c3_w a_w = u3a_get_cat31(a);
  c3_w b_w = u3a_get_cat31(b);

  if ( a_w >= 32 ) {
    return u3m_bail(c3__fail);
  }

  c3_g a_g   = a_w;
  c3_w len_w = u3r_met(a_g, c);

  if ( 0 == len_w ) {
    return UNSAFECAT(0);
  }

  if ( (b_w + len_w) < len_w ) {
    return u3m_bail(c3__exit);
  }

  c3_w* sal_w = u3a_slaq(a_g, (b_w + len_w));

  if ( 0 == sal_w ) {
    return u3m_bail(c3__fail);
  }

  u3r_chop(a_g, 0, len_w, b_w, sal_w, c);

  // return u3a_moot(sal_w);
  return u3a_malt(sal_w);
}

u3_noun u3wc_lsh(u3_noun cor) {
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

  return u3qc_lsh(a, b, c);
}

u3_noun u3kc_lsh(u3_noun a, u3_noun b, u3_noun c) {
  u3_noun d = u3qc_lsh(a, b, c);

  u3z(a);
  u3z(b);
  u3z(c);
  return d;
}
