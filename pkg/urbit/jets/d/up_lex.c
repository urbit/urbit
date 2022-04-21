#include "all.h"

c3_o
u3qdu_lex(u3_atom p, u3_noun k, u3_atom q, u3_noun l)
{
  return ( c3y == u3r_sing(p, q) ) ? u3qc_gor(k, l) : u3qa_lth(p, q);
}

u3_noun
u3wdu_lex(u3_noun cor)
{
  u3_noun p, k, q, l;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &p, u3x_sam_6, &k, u3x_sam_14, &q,
                             u3x_sam_15, &l, 0)) ||
       (c3n == u3ud(p)) ||
       (c3n == u3ud(q)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_lex(p, k, q, l);
  }
}

