/* j/4/by_has.c
**
*/
#include "all.h"

/* functions
*/
u3_noun
u3qdb_has(u3_noun a,
          u3_noun b)
{
  if ( u3_nul == a ) {
    return c3n;
  }
  else {
    u3_noun n_a, lr_a;
    u3_noun pn_a;
    u3x_cell(a, &n_a, &lr_a);
    u3x_cell(n_a, &pn_a, 0);

    if ( (c3y == u3r_sing(b, pn_a)) ) {
      return c3y;
    }
    else {
      return ( c3y == u3qc_gor(b, pn_a) ) ? u3qdb_has(u3h(lr_a), b)
                                          : u3qdb_has(u3t(lr_a), b);
    }
  }
}

u3_noun
u3wdb_has(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdb_has(a, b);
}

u3_noun
u3kdb_has(u3_noun a,
          u3_noun b)
{
  u3_noun c = u3qdb_has(a, b);
  u3z(a); u3z(b);
  return c;
}
