/* j/4/by_uni.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqdb_uni(
                       u3_noun a,
                       u3_noun b)
  {
    if ( u3_nul == a ) {
      return u3k(b);
    }
    else if ( u3_nul == b ) {
      return u3k(a);
    }
    else {
      u3_noun l_a, n_a, r_a, lr_a, p_n_a, q_n_a;
      u3_noun l_b, n_b, r_b, lr_b, p_n_b, q_n_b;

      if ( (c3n == u3_cr_cell(a, &n_a, &lr_a)) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( (c3n == u3_cr_cell(b, &n_b, &lr_b)) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( (c3n == u3_cr_cell(n_a, &p_n_a, &q_n_a)) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( (c3n == u3_cr_cell(n_b, &p_n_b, &q_n_b)) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( c3n == u3_cr_cell(lr_a, &l_a, &r_a) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( c3n == u3_cr_cell(lr_b, &l_b, &r_b) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( c3y == u3_cqc_vor(p_n_a, p_n_b) ) {
        if ( c3y == u3_cr_sing(p_n_a, p_n_b) ) {
          return u3nt(u3k(n_b),
                              u3_cqdb_uni(
                                u3k(l_a), u3k(l_b)),
                              u3_cqdb_uni(
                                u3k(r_a), u3k(r_b)));
        }
        else if ( c3y == u3_cqc_hor(p_n_b, p_n_a) ) {
          return u3_cqdb_uni(

            u3nt(
                  u3k(n_a),
                  u3_cqdb_uni(
                                      u3k(l_a),
                                      u3nt(
                                            u3k(n_b),
                                            u3k(l_b),
                                            u3k(u3_nul))),
                  u3k(r_a)),
            u3k(r_b));
        }
        else {
          return u3_cqdb_uni(

            u3nt(
                  u3k(n_a),
                  u3k(l_a),
                  u3_cqdb_uni(
                                      u3k(r_a),
                                      u3nt(
                                            u3k(n_b),
                                            u3k(u3_nul),
                                            u3k(r_b)))),
            u3k(l_b));
        }
      }
      else if ( c3y == u3_cr_sing(p_n_b, p_n_a) ) {
        return u3nt(

          u3k(n_b),
          u3_cqdb_uni(u3k(r_b), u3k(r_a)),
          u3_cqdb_uni(u3k(l_b), u3k(l_a)));
      }
      else if ( c3y == u3_cqc_hor(p_n_a, p_n_b) ) {
        return u3_cqdb_uni(

          u3k(r_a),
          u3nt(
                u3k(n_b),
                u3_cqdb_uni(
                                    u3nt(
                                          u3k(n_a),
                                          u3k(l_a),
                                          u3k(u3_nul)),
                                    u3k(l_b)),
                u3k(r_b)));
      }
      else {
        return u3_cqdb_uni(

          u3k(l_a),
          u3nt(
                u3k(n_b),
                u3k(l_b),
                u3_cqdb_uni(
                                    u3k(r_b),
                                    u3nt(
                                          u3k(n_a),
                                          u3k(u3_nul),
                                          u3k(r_a)))));
      }
    }
  }
  u3_noun
  u3_cwdb_uni(
                      u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam, &b, u3_cv_con_sam, &a, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqdb_uni(a, b);
    }
  }
