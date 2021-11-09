#include "all.h"

u3_noun
u3qdu_lbal(u3_noun a)
{
  u3_noun n_a, l_a, m_a, r_a;

  u3x_qual(a, &n_a, &l_a, &m_a, &r_a);

  if ( c3y == u3qa_lth(u3qa_add(u3qdu_size(l_a), u3qdu_size(r_a)), 2) ) {
    return u3qdu_llos(n_a, l_a, m_a, r_a);
  }
  else if ( c3y == u3qa_gth(u3qdu_size(r_a), u3qa_mul(4, u3qdu_size(l_a))) ) {
    return u3qdu_llbal(a);
  }
  else if ( c3y == u3qa_gth(u3qdu_size(l_a), u3qa_mul(4, u3qdu_size(r_a))) ) {
    return u3qdu_lrbal(a);
  }
  else {
    return u3qdu_llos(n_a, l_a, m_a, r_a);
  }
}

u3_noun
u3wdu_lbal(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0 )) ||
       (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_lbal(a);
  }
}

