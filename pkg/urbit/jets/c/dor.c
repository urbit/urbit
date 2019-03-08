#include "all.h"

u3_noun u3qc_dor(u3_atom a, u3_atom b) {
  if ( _(u3r_sing(a, b)) ) {
    return loobean_cat(c3y);
  }

  if ( _(u3ud(a)) ) {
    if ( _(u3ud(b)) ) {
      return u3qa_lth(a, b);
    }

    return loobean_cat(c3y);
  }

  if ( _(u3ud(b)) ) {
    return loobean_cat(c3n);
  }

  if ( _(u3r_sing(u3h(a), u3h(b))) ) {
    return u3qc_dor(u3t(a), u3t(b));
  }

  return u3qc_dor(u3h(a), u3h(b));
}

u3_noun u3wc_dor(u3_noun cor) {
  u3_noun a, b;

  if ( !_(u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ) {
    return u3m_bail(c3__exit);
  }

  return u3qc_dor(a, b);
}
