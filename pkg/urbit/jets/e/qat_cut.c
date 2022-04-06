#include "all.h"

u3_noun
u3qe_qat_cut(u3_noun a)
{
  u3_noun val = u3qe_qat_bot(a);

  if (u3_nul == val) {
    return u3k(a);
  }
  else {
    u3_noun p, q, r, s;
    u3x_qual(u3t(val), &p, &q, &r, &s);

    u3_noun pro = u3k(s);

    u3z(val);

    return pro;
  }
}

u3_noun
u3we_qat_cut(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qe_qat_cut(a);
  }
}


