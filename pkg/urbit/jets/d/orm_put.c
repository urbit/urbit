

#include "all.h"

u3_noun
u3qdo_put_inner(u3_noun cmp,
                u3_noun a,
                u3_noun b,
                u3_noun c);

u3_noun
u3qdo_put(u3_noun cmp,
          u3_noun a,
          u3_noun b,
          u3_noun c)
{
  u3_noun res;
  u3j_site sit_u;
  //u3j_gate_prep(&sit_u, u3k(cmp));
  res = u3qdo_put_inner(cmp, a, b, c);
  //u3j_gate_lose(&sit_u);
  return res;
}



u3_noun
u3qdo_put_inner(u3_noun cmp,
                u3_noun a,
                u3_noun b,
                u3_noun c)
{
  if( u3_nul == a ) {
    return u3nt(u3nc(u3k(b), u3k(c)),
                u3_nul,
                u3_nul);
  }
  u3_noun n_a, l_a, r_a;
  u3_noun pn_a, qn_a;
  u3x_trel(a, &n_a, &l_a, &r_a);
  u3x_cell(n_a, &pn_a, &qn_a);

  if( c3y == u3r_sing(pn_a, b) ) {
    //  performance optimisation from by_put.c
    //  is this worth it?
    if ( c3y == u3r_sing(qn_a, c) ) {
      return u3k(a);
    }
    return u3nt(u3nc(u3k(b), u3k(c)),
                u3k(l_a),
                u3k(r_a));
  }
  u3_noun d, n_d, l_d, r_d;

  if ( c3y == u3n_slam_on(cmp, u3nc(b, pn_a)) ) {
    d = u3qdo_put_inner(cmp, l_a, b, c);
    if( c3y == u3qc_mor(pn_a, u3h(u3h(d))) ) {
      return u3nt(u3k(n_a), d, u3k(r_a));
    }
    u3x_trel(d, &n_d, &l_d, &r_d);
    u3_noun e = u3nt(u3k(n_d),
                     u3k(l_d),
                     u3nt(u3k(n_a),
                          u3k(r_d),
                          u3k(r_a)));
    u3z(d);
    return e;
  }

  d = u3qdo_put_inner(cmp, r_a, b, c);
  if( c3y == u3qc_mor(pn_a, u3h(u3h(d))) ) {
    return u3nt(u3k(n_a), u3k(l_a), d);
  }
  u3x_trel(d, &n_d, &l_d, &r_d);
  u3_noun e = u3nt(u3k(n_d),
                   u3nt(u3k(n_a),
                        u3k(l_a),
                        u3k(l_d)),
                   u3k(r_d));
  u3z(d);
  return e;
}

u3_noun
u3wdo_put(u3_noun cor)
{
  u3_noun cmp, a, b, c;
  u3x_mean(cor, u3x_sam_2,   &a,
                u3x_sam_6,   &b,
                u3x_sam_7,   &c,
                u3x_con_sam, &cmp, 0);
  return u3qdo_put(cmp, a, b, c);
}

u3_noun
u3kdo_put(u3_noun cmp,
          u3_noun a,
          u3_noun b,
          u3_noun c)
{
  u3_noun pro = u3qdo_put(cmp, a, b, c);
  u3z(cmp); u3z(a); u3z(b); u3z(c);
  return pro;
}

