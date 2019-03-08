////////////////////////////////////////////////////////////////////////////////
// jets/c/peg.c
////////////////////////////////////////////////////////////////////////////////

#include "all.h"

u3_noun u3qc_peg(u3_atom a, u3_atom b) {
  if ( _(u3a_cat31_equals_noun(1, b)) ) {
    return u3k(a);
  }

  u3_atom c = u3a_cat31(u3r_met(0, b));
  u3_atom d = u3qa_dec(c);
  u3_atom e = u3qc_lsh(UNSAFECAT(0), d, UNSAFECAT(1));
  u3_atom f = u3qa_sub(b, e);
  u3_atom g = u3qc_lsh(UNSAFECAT(0), d, a);
  u3_atom h = u3qa_add(f, g);

  u3z(c);
  u3z(d);
  u3z(e);
  u3z(f);
  u3z(g);

  return h;
}

u3_noun u3wc_peg(u3_noun cor) {
  u3_noun a, b;

  if ( !_(u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
       _(u3a_is_nil(a)) ||
       _(u3a_is_nil(b)) ||
       !_(u3ud(b)) ||
       (!_(u3ud(a)) && !_(u3a_cat31_equals_noun(1,b))) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_peg(a, b);
}
