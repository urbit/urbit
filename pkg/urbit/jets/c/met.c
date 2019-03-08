#include "all.h"

u3_noun u3qc_met(u3_atom a, u3_atom b) {
  if ( _(u3a_cat31_equals_noun(0, b)) ) {
    return UNSAFECAT(0);
  }

  if ( !_(u3a_is_cat(a)) ) {
    return UNSAFECAT(1);
  }

  c3_w a_w = u3a_get_cat31(a);

  if ( a_w >= 32 ) {
    return UNSAFECAT(1);
  }

  c3_w met_w = u3r_met(a_w, b);

  // If the result is too big
  return u3a_word_to_atom(met_w);
}

u3_noun u3wc_met(u3_noun cor) {
  u3_noun a, b;

  if ( !_(u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
       !_(u3ud(b)) ||
       (!_(u3ud(a)) &&
        !_(u3a_cat31_equals_noun(0,b))) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_met(a, b);
}
