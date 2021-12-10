#include "all.h"

u3_noun
u3qdl_ebb(u3_noun a)
{
  mpz_t bon;
  mpz_init(bon);

  mpz_set_str(bon, "7fffffffffffffff", 16);

  u3_atom b = u3i_mp(bon);

  u3_noun cap_a, siz_a, tic_a, pri_a;
  u3x_qual(a, &cap_a, &siz_a, &tic_a, &pri_a);

  if (c3y == u3qa_gte(tic_a, b)) {
    u3z(b);

    return u3nq(u3k(cap_a), 0, 0, u3_nul);
  }
  else if (c3y == u3qa_gth(siz_a, cap_a)) {
    u3z(b);

    return u3nq(u3k(cap_a), u3qa_dec(siz_a), u3k(tic_a), u3qdu_cut(pri_a));
  }
  else {
    u3z(b);

    return u3k(a);
  }
}

u3_noun
u3kdl_ebb(u3_noun a)
{
  u3_noun pro = u3qdl_ebb(a);

  u3z(a);
  return pro;
}

u3_noun
u3wdl_ebb(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) ||
       (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdl_ebb(a);
  }
}


