/* j/4/put.c
**
*/
#include "all.h"

/* functions
*/
u3_noun
u3qdb_put(u3_noun a,
          u3_noun b,
          u3_noun c)
{
  if ( u3_nul == a ) {
    return u3nt(u3nc(u3k(b), u3k(c)),
                u3_nul,
                u3_nul);
  }
  else {
    u3_noun n_a, l_a, r_a;
    u3_noun pn_a, qn_a;
    u3x_trel(a, &n_a, &l_a, &r_a);
    u3x_cell(n_a, &pn_a, &qn_a);

    if ( c3y == u3r_sing(pn_a, b) ) {
      if ( c3y == u3r_sing(qn_a, c) ) {
        return u3k(a);
      }
      else {
        return u3nt(u3nc(u3k(b), u3k(c)),
                    u3k(l_a),
                    u3k(r_a));
      }
    }
    else {
      u3_noun d, n_d, l_d, r_d;

      if ( c3y == u3qc_gor(b, pn_a) ) {
        d = u3qdb_put(l_a, b, c);

        if ( c3y == u3qc_mor(pn_a, u3h(u3h(d))) ) {
          return u3nt(u3k(n_a),
                      d,
                      u3k(r_a));
        }
        else {
          u3r_trel(d, &n_d, &l_d, &r_d);

          u3_noun e = u3nt(u3k(n_d),
                           u3k(l_d),
                           u3nt(u3k(n_a),
                                u3k(r_d),
                                u3k(r_a)));

          u3z(d);
          return e;
        }
      }
      else {
        d = u3qdb_put(r_a, b, c);

        if ( c3y == u3qc_mor(pn_a, u3h(u3h(d))) ) {
          return u3nt(u3k(n_a),
                      u3k(l_a),
                      d);
        }
        else {
          u3r_trel(d, &n_d, &l_d, &r_d);

          u3_noun e = u3nt(u3k(n_d),
                           u3nt(u3k(n_a),
                                u3k(l_a),
                                u3k(l_d)),
                            u3k(r_d));

          u3z(d);
          return e;
        }
      }
    }
  }
}

u3_noun
u3wdb_put(u3_noun cor)
{
  u3_noun a, b, c;
  u3x_mean(cor, u3x_sam_2,   &b,
                u3x_sam_3,   &c,
                u3x_con_sam, &a, 0);
  return u3qdb_put(a, b, c);
}

u3_noun
u3kdb_put(u3_noun a,
          u3_noun b,
          u3_noun c)
{
  u3_noun pro = u3qdb_put(a, b, c);
  u3z(a); u3z(b); u3z(c);
  return pro;
}
