#include "all.h"

c3_o
u3qdu_win(u3_atom p, u3_noun q, u3_atom r, u3_noun s)
{
  if  ( c3y == u3qa_lth(p, r) ) {
    return c3y;
  }
  else if ( (p == r) && (c3y == u3qc_gor(q, s)) ) {
    return c3y;
  }
  else {
    return c3n;
  }
}

u3_noun
u3wdu_win(u3_noun cor)
{
  u3_atom p, r;
  u3_noun q, s;

  if ( (c3n == u3r_mean(cor, u3x_sam_4, &p, u3x_sam_5, &q, u3x_sam_6, &r,
                             u3x_sam_7, &s, 0)) ||
       (c3n == u3ud(p)) ||
       (c3n == u3ud(r)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_win(p, q, r, s);
  }
}

