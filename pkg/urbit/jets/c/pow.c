#include "all.h"

u3_noun u3qc_pow(u3_atom a, u3_atom b) {
  c3_w b_w = u3a_get_cat31(b);

  mpz_t a_mp;

  u3r_mp(a_mp, a);
  mpz_pow_ui(a_mp, a_mp, b_w);

  return u3i_mp(a_mp);
}

u3_noun u3wc_pow(u3_noun cor) {
  u3_noun a, b;

  if ( !_(u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
       !_(u3ud(a)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_pow(a, b);
}

