/* j/4/orm_get.c
 *
**
*/
#include "all.h"

u3_noun
u3qdo_get_inner(u3j_site cmp,
          u3_noun a,
          u3_noun b);


u3_noun
u3qdo_get(
    u3_noun cmp,
    u3_noun a,
    u3_noun b)
{
  u3_noun res;
  u3j_site sit_u;
  u3j_gate_prep(&sit_u, u3k(cmp));
  res = u3qdo_get_inner(sit_u, a, b);
  u3j_gate_lose(&sit_u);
  return res;
}

/* functions
*/
u3_noun
u3qdo_get_inner(u3j_site cmp,
                u3_noun a,
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
      return ( c3y == u3j_gate_slam(&cmp, u3nc(b, pn_a)))
          ? u3qdo_get_inner(cmp, u3h(lr_a), b)
          : u3qdo_get_inner(cmp, u3t(lr_a), b);
    }
  }
}

u3_noun
u3wdo_get(u3_noun cor)
{
  u3_noun a, b, cmp;
  u3x_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, u3x_con_sam, &cmp, 0);
  return u3qdo_get(cmp, a, b);
}

u3_weak
u3kdo_get(u3_noun cmp,
          u3_noun a,
          u3_noun b)
{
  u3_noun c = u3qdo_get(cmp, a, b);
  u3z(a); u3z(b); u3z(cmp);

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
u3kdo_got(u3_noun cmp,
          u3_noun a,
          u3_noun b)
{
  return u3x_good(u3kdo_get(cmp, a, b));
}
