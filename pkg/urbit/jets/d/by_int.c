/* j/4/by_int.c
** XXX THIS IS DISABLED
** specifically, s/hor/gor/g (already done?)
** and eliminate memory leaks
*/
#include "all.h"

/* functions
*/
u3_noun
u3qdb_int(u3_noun a,
          u3_noun b)
{
  if ( u3_nul == a ) {
    return u3k(u3_nul);
  }
  else if ( u3_nul == b ) {
    return u3k(u3_nul);
  }
  else {
    u3_noun n_a, l_a, r_a;
    u3_noun n_b, l_b, r_b;
    u3_noun p_n_a, q_n_a;
    u3_noun p_n_b, q_n_b;
    u3x_trel(a, &n_a, &l_a, &r_a);
    u3x_trel(b, &n_b, &l_b, &r_b);
    u3x_cell(n_a, &p_n_a, &q_n_a);
    u3x_cell(n_b, &p_n_b, &q_n_b);

    if ( c3y == u3qc_mor(p_n_a, p_n_b) ) {
      if ( c3y == u3r_sing(p_n_a, p_n_b) ) {
        return u3nt(u3k(n_b),
                    u3qdb_int(u3k(l_a), u3k(l_b)),
                    u3qdb_int(u3k(r_a), u3k(r_b)));
      }
      else if ( c3y == u3qc_gor(p_n_b, p_n_a) ) {
        return u3qdb_uni(u3qdb_int(u3k(l_a),
                                   u3nt(u3k(n_b),
                                        u3k(l_b),
                                        u3k(u3_nul))),
                         u3qdb_int(u3k(a),
                                   u3k(r_b)));
      }
      else {
        return u3qdb_uni(u3qdb_int(u3k(r_a),
                                   u3nt(u3k(n_b),
                                        u3k(u3_nul),
                                        u3k(r_b))),
                         u3qdb_int(u3k(a),
                                   u3k(l_b)));
      }
    }
    else if ( c3y == u3r_sing(p_n_b, p_n_a) ) {
      return u3nt(u3k(n_b),
                  u3qdb_int(u3k(l_b), u3k(l_a)),
                  u3qdb_int(u3k(r_b), u3k(r_a)));
    }
    else if ( c3y == u3qc_gor(p_n_a, p_n_b) ) {
      return u3qdb_uni(u3qdb_int(u3k(l_b),
                                 u3nt(u3k(n_a),
                                      u3k(l_a),
                                      u3k(u3_nul))),
                       u3qdb_int(u3k(a),
                                 u3k(r_a)));
    }
    else {
      return u3qdb_uni(u3qdb_int(u3k(r_b),
                                 u3nt(u3k(n_a),
                                      u3k(u3_nul),
                                      u3k(r_a))),
                       u3qdb_int(u3k(a),
                                 u3k(l_a)));
    }
  }
}

//  XX disabled in tree.c, reference counts presumed wrong
//
u3_noun
u3wdb_int(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdb_int(a, b);
}
