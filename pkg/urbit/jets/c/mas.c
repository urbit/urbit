////////////////////////////////////////////////////////////////////////////////
// j/3/mas.c
////////////////////////////////////////////////////////////////////////////////

#include "all.h"

u3_noun u3qc_mas(u3_atom a) {
  c3_w b_w = u3r_met(0, a);

  if ( b_w < 2 ) {
    return u3m_bail(c3__exit);
  }

  u3_atom c = u3qc_bex(UNSAFECAT(b_w - 1));
  u3_atom d = u3qc_bex(UNSAFECAT(b_w - 2));
  u3_atom e = u3qa_sub(a, c);
  u3_atom f = u3qc_con(e, d);

  u3z(c);
  u3z(d);
  u3z(e);

  return f;
}


u3_noun u3wc_mas(u3_noun cor) {
  u3_noun a = u3r_at(u3x_sam, cor);

  if ( _(u3a_is_none(a)) || !_(u3ud(a)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_mas(a);
}
