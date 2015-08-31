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
      u3_noun l_a, n_a, r_a, lr_a, p_n_a, q_n_a;
      u3_noun l_b, n_b, r_b, lr_b, p_n_b, q_n_b;

      if ( (c3n == u3r_cell(a, &n_a, &lr_a)) ) {
        return u3m_bail(c3__exit);
      }
      else if ( (c3n == u3r_cell(b, &n_b, &lr_b)) ) {
        return u3m_bail(c3__exit);
      }
      else if ( c3n == u3r_cell(lr_a, &l_a, &r_a) ) {
        return u3m_bail(c3__exit);
      }
      else if ( c3n == u3r_cell(lr_b, &l_b, &r_b) ) {
        return u3m_bail(c3__exit);
      }
      else if ( c3n == u3r_cell(n_a, &p_n_a, &q_n_a) ) {
        return u3m_bail(c3__exit);
      }
      else if ( c3n == u3r_cell(n_b, &p_n_b, &q_n_b) ) {
        return u3m_bail(c3__exit);
      }
      else if ( c3y == u3qc_vor(p_n_a, p_n_b) ) {
        if ( c3y == u3r_sing(p_n_a, p_n_b) ) {
          return u3nt(
            u3k(n_b),
            u3qdb_int(u3k(l_a), u3k(l_b)),
            u3qdb_int(u3k(r_a), u3k(r_b)));
        }
        else if ( c3y == u3qc_gor(p_n_b, p_n_a) ) {
          return u3qdb_uni(
            u3qdb_int(u3k(l_a),
                      u3nt(u3k(n_b),
                           u3k(l_b),
                           u3k(u3_nul))),
            u3qdb_int(u3k(a),
                      u3k(r_b)));
        }
        else {
          return u3qdb_uni(
            u3qdb_int(u3k(r_a),
                      u3nt(u3k(n_b),
                           u3k(u3_nul),
                           u3k(r_b))),
            u3qdb_int(u3k(a),
                      u3k(l_b)));
        }
      }
      else if ( c3y == u3r_sing(p_n_b, p_n_a) ) {
        return u3nt(
          u3k(n_b),
          u3qdb_int(u3k(l_b), u3k(l_a)),
          u3qdb_int(u3k(r_b), u3k(r_a)));
      }
      else if ( c3y == u3qc_gor(p_n_a, p_n_b) ) {
        return u3qdb_uni(
          u3qdb_int(u3k(l_b),
                    u3nt(u3k(n_a),
                         u3k(l_a),
                         u3k(u3_nul))),
          u3qdb_int(u3k(a),
                    u3k(r_a)));
      }
      else {
        return u3qdb_uni(
          u3qdb_int(u3k(r_b),
                    u3nt(u3k(n_a),
                         u3k(u3_nul),
                         u3k(r_a))),
          u3qdb_int(u3k(a),
                    u3k(l_a)));
      }
    }
  }
  u3_noun
  u3wdb_int(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qdb_int(a, b);
    }
  }
