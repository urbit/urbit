#include "all.h"

u3_noun
u3qdl_del(u3_noun a, u3_noun k)
{
  u3_noun cap_a, siz_a, tic_a, pri_a;
  u3x_qual(a, &cap_a, &siz_a, &tic_a, &pri_a);

  u3_noun ded = u3qdu_dew(pri_a, k);

  if (u3_nul == ded) {
    return u3_nul;
  }
  else {
    u3_noun pd, qd, rd;
    u3x_trel(u3t(ded), &pd, &qd, &rd);

    u3_noun pro = u3nq(u3k(cap_a), u3qa_dec(siz_a), u3k(tic_a), u3k(rd));

    u3z(ded);

    return pro;
  }
}

u3_noun
u3wdl_del(u3_noun cor)
{
  u3_noun a, k;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &k, 0)) ||
       (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdl_del(a, k);
  }
}

