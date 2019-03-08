#include "all.h"

u3_noun u3qc_cat(u3_atom a, u3_atom b, u3_atom c) {
  c3_w a_w = u3a_get_cat31(a);

  if ( a_w >= 32 ) {
    return u3m_bail(c3__fail);
  }

  c3_g   a_g = a_w;
  c3_w   lew_w = u3r_met(a_g, b);
  c3_w   ler_w = u3r_met(a_g, c);
  c3_w   all_w = (lew_w + ler_w);

  if ( 0 == all_w ) {
    return UNSAFECAT(0);
  }

  c3_w* sal_w = u3a_slaq(a_g, all_w);

  if ( 0 == sal_w ) {
    return u3m_bail(c3__fail);
  }

  u3r_chop(a_g, 0, lew_w, 0, sal_w, b);
  u3r_chop(a_g, 0, ler_w, lew_w, sal_w, c);

  return u3a_malt(sal_w);
}

u3_noun u3wc_cat(u3_noun cor)
{
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

  return u3qc_cat(a, b, c);
}

