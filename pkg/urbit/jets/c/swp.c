#include "all.h"

u3_noun u3qc_swp(u3_atom a, u3_atom b) {
  //XX write a proper c-style swp, maybe
  return u3kc_rep(u3k(a), u3kb_flop(u3qc_rip(a, b)));
}

u3_noun u3wc_swp(u3_noun cor) {
  u3_noun a, b;

  if ( !_(u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
       !_(u3ud(a)) ||
       !_(u3ud(b)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_swp(a, b);
}

u3_noun u3kc_swp(u3_atom a, u3_atom b) {
  u3_noun res = u3qc_swp(a, b);
  u3z(a); u3z(b);
  return res;
}
