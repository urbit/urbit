/* j/4/by_get.c
**
*/
#include "all.h"

/* functions
*/
u3_noun
u3qdb_get(u3_noun a,
          u3_noun b)
{
  if ( u3_nul == a ) {
    return u3_nul;
  }
  else {
    u3_noun n_a, lr_a;
    u3_noun pn_a, qn_a;
    u3x_cell(a, &n_a, &lr_a);
    u3x_cell(n_a, &pn_a, &qn_a);

    if ( (c3y == u3r_sing(b, pn_a)) ) {
      return u3nc(u3_nul, u3k(qn_a));
    }
    else {
      return ( c3y == u3qc_gor(b, pn_a) ) ? u3qdb_get(u3h(lr_a), b)
                                          : u3qdb_get(u3t(lr_a), b);
    }
  }
}

u3_noun
u3wdb_get(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdb_get(a, b);
}

u3_weak
u3kdb_get(u3_noun a,
          u3_noun b)
{
  u3_noun c = u3qdb_get(a, b);
  u3z(a); u3z(b);

  if ( c3n == u3r_du(c) ) {
    u3z(c);
    return u3_none;
  }
  else {
    u3_noun pro = u3k(u3t(c));
    u3z(c);
    return pro;
  }
}

u3_noun
u3kdb_got(u3_noun a,
          u3_noun b)
{
  return u3x_good(u3kdb_get(a, b));
}
