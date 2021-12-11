#include "all.h"

u3_noun
u3qdl_put(u3_noun a, u3_noun k, u3_noun v)
{
  u3_noun cap_a, siz_a, tic_a, pri_a;
  u3x_qual(a, &cap_a, &siz_a, &tic_a, &pri_a);

  u3_noun vue = u3qdu_gun(pri_a, k, tic_a, v);

  u3_noun pv, qv;
  u3x_cell(vue, &pv, &qv);

  u3_noun siz = (u3_nul == pv) ? u3qa_inc(siz_a) : u3k(siz_a);

  u3_noun pre = u3nq(u3k(cap_a), siz, u3qa_inc(tic_a), u3k(qv));
  u3_noun pro = u3kdl_ebb(pre);

  u3z(vue);
  u3z(siz);

  return pro;
}

u3_noun
u3wdl_put(u3_noun cor)
{
  u3_noun a, k, v;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6,
                        &k, u3x_sam_7, &v, 0)) ||
       (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdl_put(a, k, v);
  }
}

