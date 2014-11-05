/* j/4/in_put.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqdi_put(
                       u3_noun a,
                       u3_noun b)
  {
    if ( u3_nul == a ) {
      return u3nt(u3k(b), u3_nul, u3_nul);
    }
    else {
      u3_noun l_a, n_a, r_a, lr_a;  //  XX copy tree boilerplate to other pt4
      u3_noun c, l_c, n_c, r_c;

      if ( (c3n == u3_cr_cell(a, &n_a, &lr_a)) ) {
        return u3_cm_bail(c3__exit);
      }
      else if ( c3y == u3_cr_sing(n_a, b) ) {
        return u3k(a);
      }
      else if ( c3n == u3_cr_cell(lr_a, &l_a, &r_a) ) {
        return u3_cm_bail(c3__exit);
      }
      else {
        if ( c3y == u3_cqc_hor(b, n_a) ) {
          c = u3_cqdi_put(l_a, b);

          if ( c3y == u3_cqc_vor(n_a, u3h(c)) ) {
            return u3nt(u3k(n_a),
                                c,
                                u3k(r_a));
          }
          else {
            u3_cr_trel(c, &n_c, &l_c, &r_c);
            {
              u3_noun d = u3nt
                (u3k(n_c),
                        u3k(l_c),
                        u3nt(
                              u3k(n_a),
                              u3k(r_c),
                              u3k(r_a)));

              u3z(c);
              return d;
            }
          }
        }
        else {
          c = u3_cqdi_put(r_a, b);

          if ( c3y == u3_cqc_vor(n_a, u3h(c)) ) {
            return u3nt(u3k(n_a),
                                u3k(l_a),
                                c);
          }
          else {
            u3_cr_trel(c, &n_c, &l_c, &r_c);
            {
              u3_noun d = u3nt
                (u3k(n_c),
                        u3nt(
                              u3k(n_a),
                              u3k(l_a),
                              u3k(l_c)),
                        u3k(r_c));

              u3z(c);
              return d;
            }
          }
        }
      }
    }
  }
  u3_noun
  u3_cwdi_put(
                      u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam, &b, u3_cv_con_sam, &a, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqdi_put(a, b);
    }
  }
  u3_weak
  u3_ckdi_put(u3_noun a, u3_noun b)
  {
    // Bizarre asymmetry in old jets.
    //
    // (Mysterious comment in old glue code.)
    //
    u3_noun pro = u3_cqdi_put(a, b);

    u3z(a); u3z(b);
    return pro;
  }


