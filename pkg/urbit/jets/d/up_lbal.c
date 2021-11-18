#include "all.h"

u3_noun
u3qdu_lbal(u3_noun n_a, u3_noun l_a, u3_noun m_a, u3_noun r_a)
{
  if ( c3y == u3qa_lth(u3qa_add(u3qdu_size(l_a), u3qdu_size(r_a)), 2) ) {
    return u3qdu_llos(u3k(n_a), u3k(l_a), u3k(m_a), u3k(r_a));
  }
  else if ( c3y == u3qa_gth(u3qdu_size(r_a), u3qa_mul(4, u3qdu_size(l_a))) ) {
    return u3qdu_llbal(u3k(n_a), u3k(l_a), u3k(m_a), u3k(r_a));
  }
  else if ( c3y == u3qa_gth(u3qdu_size(l_a), u3qa_mul(4, u3qdu_size(r_a))) ) {
    return u3qdu_lrbal(u3k(n_a), u3k(l_a), u3k(m_a), u3k(r_a));
  }
  else {
    return u3qdu_llos(u3k(n_a), u3k(l_a), u3k(m_a), u3k(r_a));
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
    u3_noun n, l, m, r;
    u3x_qual(a, &n, &l, &m, &r);

    if ( (c3n == u3du(n)) || (c3n == u3ud(m)) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qdu_lbal(n, l, m, r);
    }
  }
}

