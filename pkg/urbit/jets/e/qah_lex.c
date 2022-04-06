#include "all.h"

c3_o
u3qe_qah_lex(u3_atom p, u3_atom k, u3_atom q, u3_atom l)
{
  if (c3y == u3r_sing(p, q)) {
    u3_atom h = u3r_mug(k);
    u3_atom i = u3r_mug(l);

    u3_atom pro = u3qa_lth(h, i);

    u3z(h);
    u3z(i);

    return pro;
  }
  else {
    return u3qa_lth(p, q);
  }
}

u3_noun
u3we_qah_lex(u3_noun cor)
{
  u3_noun p, k, q, l;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &p, u3x_sam_6, &k, u3x_sam_14, &q,
                             u3x_sam_15, &l, 0)) ||
       (c3n == u3ud(p)) ||
       (c3n == u3ud(q)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qe_qah_lex(p, k, q, l);
  }
}

