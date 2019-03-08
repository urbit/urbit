////////////////////////////////////////////////////////////////////////////////
// j/3/bex.c
////////////////////////////////////////////////////////////////////////////////

#include "all.h"

// u3qc_bex: Compute 2**a
u3_noun u3qc_bex(u3_atom a) {
  mpz_t a_mp;

  if ( !_(u3a_is_cat(a)) ) {
    return u3m_bail(c3__fail);
  }

  c3_w a_w = u3a_get_cat31(a);
  mpz_init_set_ui(a_mp, 1);
  mpz_mul_2exp(a_mp, a_mp, a_w);

  return u3i_mp(a_mp);
}

// u3wc_bex: Compute 2**a
u3_noun u3wc_bex(u3_noun cor) {
  u3_noun a = u3r_at(u3x_sam, cor);

  if ( _(u3a_is_none(a)) || !_(u3ud(a)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_bex(a);
}
