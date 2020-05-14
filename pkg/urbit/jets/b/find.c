/* j/2/find.c
**
*/
#include "all.h"


/* functions
*/
u3_noun
u3qb_find(u3_noun nedl, u3_noun hstk)
{
  u3_atom i = 0;

  while ( 1 ) {
    if ( u3_nul == nedl) {
      return u3_nul;
    } else if ( u3_nul == hstk ) {
      return u3_nul;
    }

    u3_noun n = nedl;
    u3_noun h = hstk;
    while (c3y == u3r_sing(u3h(n), u3h(h))) {
      if ( u3_nul == u3t(n) ) {
        return u3nc(0, i);
      }

      n = u3t(n);
      h = u3t(h);
    }

    i = u3i_vint(i);
    hstk = u3t(hstk);
  }
}

u3_noun
u3wb_find(u3_noun cor)
{
  u3_noun a, b;

  if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
    return u3m_bail(c3__exit);
  } else {
    return u3qb_find(a, b);
  }
}
