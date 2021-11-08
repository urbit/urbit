#include "all.h"

u3_atom
u3qdu_size(u3_noun t)
{
  if ( u3_nul == t ) {
    return 0;
  }
  else {
    u3_noun st = u3h(u3t(t));

    if ( c3n == u3du(st) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return st;
    }
  }
}

u3_atom
u3wdu_size(u3_noun cor)
{
  u3_noun t;

  if ( (c3n == u3r_mean(cor, u3x_sam, &t, 0)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_size(t);
  }
}


