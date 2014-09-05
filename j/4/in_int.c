/* j/4/in_int.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqdi_int(
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
      u3_noun l_a, n_a, r_a, lr_a;
      u3_noun l_b, n_b, r_b, lr_b;
      u3_noun c;

      if ( (u3_no == u3_cr_cell(a, &n_a, &lr_a)) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( (u3_no == u3_cr_cell(b, &n_b, &lr_b)) ) {
        return u3_cm_bail(c3__exit);
      }
      else {
        if ( u3_yes == u3_cqc_vor(n_b, n_a) ) {
          c = a;    a = b;       b = c;
          c = n_a;  n_a = n_b;   n_b = c;
          c = lr_a; lr_a = lr_b; lr_b = c;
        }
        if ( u3_no == u3_cr_cell(lr_a, &l_a, &r_a) ) {
          return u3_cm_bail(c3__exit);
        }
        else if ( u3_no == u3_cr_cell(lr_b, &l_b, &r_b) ) {
          return u3_cm_bail(c3__exit);
        }
        else if ( u3_yes == u3_cr_sing(n_a, n_b) ) {
          return u3nt(u3k(n_a),
                              u3_cqdi_int(l_a, l_b),
                              u3_cqdi_int(r_a, r_b));
        }
        else if ( u3_yes == u3_cqc_hor(n_b, n_a) ) {
          return u3_cqdi_uni(
                                      u3_cqdi_int(
                                                          l_a,
                                                          u3nt(
                                                                n_b,
                                                                l_b,
                                                                u3_nul)),
                                      u3_cqdi_int(
                                                          a,
                                                          r_b));
        }
        else {
          return u3_cqdi_uni(
                                      u3_cqdi_int(
                                                          r_a,
                                                          u3nt(
                                                                n_b,
                                                                u3_nul,
                                                                r_b)),
                                      u3_cqdi_int(
                                                          a,
                                                          l_b));
        }
      }
    }
  }
  u3_noun
  u3_cwdi_int(
                      u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam, &b, u3_cv_con_sam, &a, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqdi_int(a, b);
    }
  }
