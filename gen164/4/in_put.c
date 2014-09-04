/* j/4/in_put.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqdi_put(
                       u2_noun a,
                       u2_noun b)
  {
    if ( u2_nul == a ) {
      return u2nt(u2k(b), u2_nul, u2_nul);
    }
    else {
      u2_noun l_a, n_a, r_a, lr_a;  //  XX copy tree boilerplate to other pt4
      u2_noun c, l_c, n_c, r_c;

      if ( (u2_no == u2_cr_cell(a, &n_a, &lr_a)) ) {
        return u2_cm_bail(c3__exit);
      }
      else if ( u2_yes == u2_cr_sing(n_a, b) ) {
        return u2k(a);
      }
      else if ( u2_no == u2_cr_cell(lr_a, &l_a, &r_a) ) {
        return u2_cm_bail(c3__exit);
      }
      else {
        if ( u2_yes == u2_cqc_hor(b, n_a) ) {
          c = u2_cqdi_put(l_a, b);

          if ( u2_yes == u2_cqc_vor(n_a, u2h(c)) ) {
            return u2nt(u2k(n_a),
                                c,
                                u2k(r_a));
          }
          else {
            u2_cr_trel(c, &n_c, &l_c, &r_c);
            {
              u2_noun d = u2nt
                (u2k(n_c),
                        u2k(l_c),
                        u2nt(
                              u2k(n_a),
                              u2k(r_c),
                              u2k(r_a)));

              u2z(c);
              return d;
            }
          }
        }
        else {
          c = u2_cqdi_put(r_a, b);

          if ( u2_yes == u2_cqc_vor(n_a, u2h(c)) ) {
            return u2nt(u2k(n_a),
                                u2k(l_a),
                                c);
          }
          else {
            u2_cr_trel(c, &n_c, &l_c, &r_c);
            {
              u2_noun d = u2nt
                (u2k(n_c),
                        u2nt(
                              u2k(n_a),
                              u2k(l_a),
                              u2k(l_c)),
                        u2k(r_c));

              u2z(c);
              return d;
            }
          }
        }
      }
    }
  }
  u2_noun
  u2_cwdi_put(
                      u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqdi_put(a, b);
    }
  }

