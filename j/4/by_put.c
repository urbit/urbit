/* j/4/put.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqdb_put(
                       u3_noun a,
                       u3_noun b,
                       u3_noun c)
  {
    if ( u3_nul == a ) {
      return u3nt(
                   u3nc(u3k(b),
                                u3k(c)),
                   u3_nul,
                   u3_nul);
    }
    else {
      u3_noun l_a, n_a, r_a, pn_a, qn_a;
      u3_noun d, l_d, n_d, r_d;

      if ( (u3_no == u3_cr_trel(a, &n_a, &l_a, &r_a)) ||
           (u3_no == u3_cr_cell(n_a, &pn_a, &qn_a)) )
      {
        return u3_cm_bail(c3__exit);
      }
      else if ( u3_yes == u3_cr_sing(pn_a, b) ) {
        if ( u3_yes == u3_cr_sing(qn_a, c) ) {
          return u3k(a);
        } else {
          return u3nt
            (u3nc(u3k(b), u3k(c)),
                    u3k(l_a),
                    u3k(r_a));
        }
      }
      else {
        if ( u3_yes == u3_cqc_gor(b, pn_a) ) {
          d = u3_cqdb_put(l_a, b, c);

          if ( u3_yes == u3_cqc_vor(pn_a, u3h(u3h(d))) ) {
            return u3nt(u3k(n_a),
                                d,
                                u3k(r_a));
          }
          else {
            u3_cr_trel(d, &n_d, &l_d, &r_d);
            {
              u3_noun e = u3nt
                (u3k(n_d),
                        u3k(l_d),
                        u3nt(
                              u3k(n_a),
                              u3k(r_d),
                              u3k(r_a)));

              u3z(d);
              return e;
            }
          }
        }
        else {
          d = u3_cqdb_put(r_a, b, c);

          if ( u3_yes == u3_cqc_vor(pn_a, u3h(u3h(d))) ) {
            return u3nt(u3k(n_a),
                                u3k(l_a),
                                d);
          }
          else {
            u3_cr_trel(d, &n_d, &l_d, &r_d);
            {
              u3_noun e = u3nt
                (u3k(n_d),
                        u3nt(
                              u3k(n_a),
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
  }
  u3_noun
  u3_cwdb_put(
                      u3_noun cor)
  {
    u3_noun a, b, c;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam_2,   &b,
                               u3_cv_sam_3,   &c,
                               u3_cv_con_sam, &a, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqdb_put(a, b, c);
    }
  }
  u3_weak
  u3_ckdb_put(u3_noun a, u3_noun b, u3_noun c)
  {
    // Bizarre asymmetry in old jets.
    //
    // (Mysterious comment in old glue code.)
    //
    u3_noun pro = u3_cqdb_put(a, b, c);

    u3z(a); u3z(b); u3z(c);
    return pro;
  }

