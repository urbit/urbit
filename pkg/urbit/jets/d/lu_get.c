#include "all.h"

u3_noun
u3qdl_get(u3_noun a, u3_noun k)
{
  u3_noun cap_a, siz_a, tic_a, pri_a;
  u3x_qual(a, &cap_a, &siz_a, &tic_a, &pri_a);

  u3_noun val = u3qdu_see(pri_a, k, tic_a);

  u3_noun pv, qv;
  u3x_cell(val, &pv, &qv);

  if (u3_nul == pv) {
    u3z(val);

    return u3_nul;
  }
  else {
    u3_noun ppv, qpv;
    u3x_cell(u3t(pv), &ppv, &qpv);

    u3_noun pru = u3nq(u3k(cap_a), u3k(siz_a), u3qa_inc(tic_a), u3k(qv));
    u3_noun pre = u3kdl_ebb(pru);
    u3_noun pro = u3nt(u3_nul, u3k(qpv), pre);

    u3z(val);

    return pro;
  }
}

u3_noun
u3wdl_get(u3_noun cor)
{
  u3_noun a, k;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &k, 0)) ||
       (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdl_get(a, k);
  }
}

