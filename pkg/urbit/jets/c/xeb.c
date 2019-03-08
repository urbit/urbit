#include "all.h"

u3_noun u3qc_xeb(u3_atom a) {
  return u3a_word_to_atom(u3r_met(0, a));
}

u3_noun u3wc_xeb(u3_noun cor) {
  u3_noun a = u3r_at(u3x_sam, cor);

  if ( _(u3a_is_none(a)) ||
       !_(u3ud(a)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_xeb(a);
}
