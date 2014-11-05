/* j/4/in_uni.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqdi_uni(
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
      u3_noun l_a, n_a, r_a, lr_a;
      u3_noun l_b, n_b, r_b, lr_b;

      if ( (u3_no == u3_cr_cell(a, &n_a, &lr_a)) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( (u3_no == u3_cr_cell(b, &n_b, &lr_b)) ) {
        return u3_cm_bail(c3__exit);
      }
      else {
        if ( u3_yes == u3_cqc_vor(n_a, n_b) ) {
          if ( u3_no == u3_cr_cell(lr_a, &l_a, &r_a) ) {
            return u3_cm_bail(c3__exit);
          }
          else if ( u3_no == u3_cr_cell(lr_b, &l_b, &r_b) ) {
            return u3_cm_bail(c3__exit);
          }
          else if ( u3_yes == u3_cr_sing(n_a, n_b) ) {
            return u3nt(

              u3k(n_b),
              u3_cqdi_uni(u3k(l_a), u3k(l_b)),
              u3_cqdi_uni(u3k(r_a), u3k(r_b)));
          }
          else if ( u3_yes == u3_cqc_hor(n_b, n_a) ) {
            return u3_cqdi_uni(

              u3nt(
                    u3k(n_a),
                    u3_cqdi_uni(
                                         u3k(l_a),
                                         u3nt(
                                               u3k(n_b),
                                               u3k(l_b),
                                               u3k(u3_nul))),
                    u3k(r_a)),
              u3k(r_b));
          }
          else {
            return u3_cqdi_uni(

              u3nt(
                    u3k(n_a),
                    u3k(l_a),
                    u3_cqdi_uni(
                                         u3k(r_a),
                                         u3nt(
                                               u3k(n_b),
                                               u3k(u3_nul),
                                               u3k(r_b)))),
              u3k(l_b));
          }
        }
        else if ( u3_no == u3_cr_cell(lr_b, &l_b, &r_b) ) {
          return u3_cm_bail(c3__exit);
        }
        else if ( u3_no == u3_cr_cell(lr_a, &l_a, &r_a) ) {
          return u3_cm_bail(c3__exit);
        }
        else if ( u3_yes == u3_cr_sing(n_b, n_a) ) {
          return u3nt(

            u3k(n_b),
            u3_cqdi_uni(u3k(r_b), u3k(r_a)),
            u3_cqdi_uni(u3k(l_b), u3k(l_a)));
        }
        else if ( u3_yes == u3_cqc_hor(n_a, n_b) ) {
          return u3_cqdi_uni(

            u3k(r_a),
            u3nt(
                  u3k(n_b),
                  u3_cqdi_uni(
                                      u3nt(
                                            u3k(n_a),
                                            u3k(l_a),
                                            u3_nul),
                                      u3k(l_b)),
                  u3k(r_b)));
        }
        else {
          return u3_cqdi_uni(

            u3k(l_a),
            u3nt(
                  u3k(n_b),
                  u3k(l_b),
                  u3_cqdi_uni(
                                      u3nt(
                                            u3k(n_a),
                                            u3k(u3_nul),
                                            u3k(r_a)),
                                      u3k(r_b))));
        }
      }
    }
  }
  u3_noun
  u3_cwdi_uni(
                      u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam, &b, u3_cv_con_sam, &a, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqdi_uni(a, b);
    }
  }

