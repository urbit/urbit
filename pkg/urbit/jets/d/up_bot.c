#include "all.h"

u3_noun
u3qdu_bot(u3_noun a)
{
  u3_noun val = u3qdu_qat_jib(a);

  u3_noun pv, qv;
  u3x_cell(val, &pv, &qv);

  if (u3_nul == pv) {
    u3z(val);
    return u3_nul;
  }
  else {
    u3_noun ppv, qpv, rpv;
    u3x_trel(u3t(pv), &ppv, &qpv, &rpv);

    u3_noun pro = u3nq(u3_nul, u3k(ppv), u3k(qpv), u3nc(u3k(rpv), u3k(qv)));

    u3z(val);

    return pro;
  }
}

u3_noun
u3wdu_bot(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_bot(a);
  }
}


