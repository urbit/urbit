#include "all.h"

c3_o
u3qe_lex(u3_atom p, u3_atom k, u3_atom q, u3_atom l)
{
  return ( p == q ) ? u3qa_lth(k, l) : u3qa_lth(p, q);
}

u3_noun
u3we_lex(u3_noun cor)
{
  u3_noun p, k, q, l;

  if ( (c3n == u3r_mean(cor, u3x_sam_4, &p, u3x_sam_5, &k, u3x_sam_6, &q,
                             u3x_sam_7, &l, 0)) ||
       (c3n == u3ud(p)) ||
       (c3n == u3ud(k)) ||
       (c3n == u3ud(q)) ||
       (c3n == u3ud(l)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qe_lex(p, k, q, l);
  }
}

