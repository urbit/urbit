#include "all.h"

u3_noun u3qc_dis(u3_atom a, u3_atom b) {
  c3_w lna_w = u3r_met(5, a);
  c3_w lnb_w = u3r_met(5, b);

  if ( (lna_w == 0) && (lnb_w == 0) ) {
    return UNSAFECAT(0);
  } else {
    c3_w  len_w = c3_max(lna_w, lnb_w);
    c3_w* sal_w = u3a_slab(len_w);

    if ( 0 == sal_w ) {
      return u3m_bail(c3__fail);
    }
    else {
      c3_w i_w;

      u3r_chop(5, 0, lna_w, 0, sal_w, a);

      for ( i_w = 0; i_w < len_w; i_w++ ) {
        sal_w[i_w] &= (i_w >= lnb_w) ? 0 : u3r_word(i_w, b);
      }
      return u3a_malt(sal_w);
    }
  }
}

u3_noun u3wc_dis(u3_noun cor) {
  u3_noun a, b;

  if ( !_(u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
       !_(u3ud(a)) ||
       !_(u3ud(b)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qc_dis(a, b);
  }
}
