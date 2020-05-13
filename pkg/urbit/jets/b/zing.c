/* j/2/zing.c
**
*/
#include "all.h"


/* functions
*/
u3_noun
u3qb_zing(u3_noun a)
{
  u3_noun top = u3qb_flop(a);
  u3_noun top_orig = top;
  u3_noun l = 0;

  while ( u3_nul != top ) {
    u3_noun f = u3qb_flop(u3h(top));
    u3_noun f_orig = f;

    while ( u3_nul != f ) {
      l = u3nc(u3k(u3h(f)), l);
      f = u3t(f);
    }

    u3z(f_orig);
    top = u3t(top);
  }

  u3z(top_orig);

  return l;
}

u3_noun
u3wb_zing(u3_noun cor)
{
  u3_noun a;

  if ( c3n == u3r_mean(cor, u3x_sam, &a, 0) ) {
    return u3m_bail(c3__exit);
  } else {
    return u3qb_zing(a);
  }
}
