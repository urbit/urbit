/* j/4/by_int.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqdb_int(
                       u3_noun a,
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

      if ( (u3_no == u3_cr_cell(a, &n_a, &lr_a)) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( (u3_no == u3_cr_cell(b, &n_b, &lr_b)) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( u3_no == u3_cr_cell(lr_a, &l_a, &r_a) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( u3_no == u3_cr_cell(lr_b, &l_b, &r_b) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( u3_no == u3_cr_cell(n_a, &p_n_a, &q_n_a) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( u3_no == u3_cr_cell(n_b, &p_n_b, &q_n_b) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( u3_yes == u3_cqc_vor(p_n_a, p_n_b) ) {
        if ( u3_yes == u3_cr_sing(p_n_a, p_n_b) ) {
          return u3nt(

            u3k(n_b),
            u3_cqdb_int(u3k(l_a), u3k(l_b)),
            u3_cqdb_int(u3k(r_a), u3k(r_b)));
        }
        else if ( u3_yes == u3_cqc_hor(p_n_b, p_n_a) ) {
          return u3_cqdb_uni(

            u3_cqdb_int(
                                u3k(l_a),
                                u3nt(
                                      u3k(n_b),
                                      u3k(l_b),
                                      u3k(u3_nul))),
            u3_cqdb_int(
                                u3k(a),
                                u3k(r_b)));
        }
        else {
          return u3_cqdb_uni(

            u3_cqdb_int(
                                u3k(r_a),
                                u3nt(
                                      u3k(n_b),
                                      u3k(u3_nul),
                                      u3k(r_b))),
            u3_cqdb_int(
                                u3k(a),
                                u3k(l_b)));
        }
      }
      else if ( u3_yes == u3_cr_sing(p_n_b, p_n_a) ) {
        return u3nt(

          u3k(n_b),
          u3_cqdb_int(u3k(l_b), u3k(l_a)),
          u3_cqdb_int(u3k(r_b), u3k(r_a)));
      }
      else if ( u3_yes == u3_cqc_hor(p_n_a, p_n_b) ) {
        return u3_cqdb_uni(

          u3_cqdb_int(
                              u3k(l_b),
                              u3nt(
                                    u3k(n_a),
                                    u3k(l_a),
                                    u3k(u3_nul))),
          u3_cqdb_int(
                              u3k(a),
                              u3k(r_a)));
      }
      else {
        return u3_cqdb_uni(

          u3_cqdb_int(
                              u3k(r_b),
                              u3nt(
                                    u3k(n_a),
                                    u3k(u3_nul),
                                    u3k(r_a))),
          u3_cqdb_int(
                              u3k(a),
                              u3k(l_a)));
      }
    }
  }
  u3_noun
  u3_cwdb_int(
                      u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam, &b, u3_cv_con_sam, &a, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqdb_int(a, b);
    }
  }
