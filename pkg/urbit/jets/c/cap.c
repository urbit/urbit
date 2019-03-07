////////////////////////////////////////////////////////////////////////////////
// j/3/cap.c
////////////////////////////////////////////////////////////////////////////////

#include "all.h"

u3_noun u3qc_cap(u3_atom a) {
  c3_w met_w = u3r_met(0, a);

  if ( met_w < 2 ) {
    return u3m_bail(c3__exit);
  }

  if ( (1 == u3r_bit((met_w - 2), a)) ) {
    return u3a_cat31_unsafe(3);
  }

  return u3a_cat31_unsafe(2);
}

u3_noun u3wc_cap(u3_noun cor) {
  u3_noun a = u3r_at(u3x_sam, cor);

  if ( u3a_is_none(a) || (c3n == u3ud(a)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_cap(a);
}

