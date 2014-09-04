/* j/4/in_int.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqdi_int(
                       u2_noun a,
                       u2_noun b)
  {
    if ( u2_nul == a ) {
      return u2k(u2_nul);
    }
    else if ( u2_nul == b ) {
      return u2k(u2_nul);
    }
    else {
      u2_noun l_a, n_a, r_a, lr_a;
      u2_noun l_b, n_b, r_b, lr_b;
      u2_noun c;

      if ( (u2_no == u2_cr_cell(a, &n_a, &lr_a)) ) {
        return u2_cm_bail(c3__exit);
      }
      else if ( (u2_no == u2_cr_cell(b, &n_b, &lr_b)) ) {
        return u2_cm_bail(c3__exit);
      }
      else {
        if ( u2_yes == u2_cqc_vor(n_b, n_a) ) {
          c = a;    a = b;       b = c;
          c = n_a;  n_a = n_b;   n_b = c;
          c = lr_a; lr_a = lr_b; lr_b = c;
        }
        if ( u2_no == u2_cr_cell(lr_a, &l_a, &r_a) ) {
          return u2_cm_bail(c3__exit);
        }
        else if ( u2_no == u2_cr_cell(lr_b, &l_b, &r_b) ) {
          return u2_cm_bail(c3__exit);
        }
        else if ( u2_yes == u2_cr_sing(n_a, n_b) ) {
          return u2nt(u2k(n_a),
                              u2_cqdi_int(l_a, l_b),
                              u2_cqdi_int(r_a, r_b));
        }
        else if ( u2_yes == u2_cqc_hor(n_b, n_a) ) {
          return u2_cqdi_uni(
                                      u2_cqdi_int(
                                                          l_a,
                                                          u2nt(
                                                                n_b,
                                                                l_b,
                                                                u2_nul)),
                                      u2_cqdi_int(
                                                          a,
                                                          r_b));
        }
        else {
          return u2_cqdi_uni(
                                      u2_cqdi_int(
                                                          r_a,
                                                          u2nt(
                                                                n_b,
                                                                u2_nul,
                                                                r_b)),
                                      u2_cqdi_int(
                                                          a,
                                                          l_b));
        }
      }
    }
  }
  u2_noun
  u2_cwdi_int(
                      u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqdi_int(a, b);
    }
  }
