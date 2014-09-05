/* j/4/put.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqdb_put(
                       u2_noun a,
                       u2_noun b,
                       u2_noun c)
  {
    if ( u2_nul == a ) {
      return u2nt(
                   u2nc(u2k(b),
                                u2k(c)),
                   u2_nul,
                   u2_nul);
    }
    else {
      u2_noun l_a, n_a, r_a, pn_a, qn_a;
      u2_noun d, l_d, n_d, r_d;

      if ( (u2_no == u2_cr_trel(a, &n_a, &l_a, &r_a)) ||
           (u2_no == u2_cr_cell(n_a, &pn_a, &qn_a)) )
      {
        return u2_cm_bail(c3__exit);
      }
      else if ( u2_yes == u2_cr_sing(pn_a, b) ) {
        if ( u2_yes == u2_cr_sing(qn_a, c) ) {
          return u2k(a);
        } else {
          return u2nt
            (u2nc(u2k(b), u2k(c)),
                    u2k(l_a),
                    u2k(r_a));
        }
      }
      else {
        if ( u2_yes == u2_cqc_gor(b, pn_a) ) {
          d = u2_cqdb_put(l_a, b, c);

          if ( u2_yes == u2_cqc_vor(pn_a, u2h(u2h(d))) ) {
            return u2nt(u2k(n_a),
                                d,
                                u2k(r_a));
          }
          else {
            u2_cr_trel(d, &n_d, &l_d, &r_d);
            {
              u2_noun e = u2nt
                (u2k(n_d),
                        u2k(l_d),
                        u2nt(
                              u2k(n_a),
                              u2k(r_d),
                              u2k(r_a)));

              u2z(d);
              return e;
            }
          }
        }
        else {
          d = u2_cqdb_put(r_a, b, c);

          if ( u2_yes == u2_cqc_vor(pn_a, u2h(u2h(d))) ) {
            return u2nt(u2k(n_a),
                                u2k(l_a),
                                d);
          }
          else {
            u2_cr_trel(d, &n_d, &l_d, &r_d);
            {
              u2_noun e = u2nt
                (u2k(n_d),
                        u2nt(
                              u2k(n_a),
                              u2k(l_a),
                              u2k(l_d)),
                        u2k(r_d));

              u2z(d);
              return e;
            }
          }
        }
      }
    }
  }
  u2_noun
  u2_cwdb_put(
                      u2_noun cor)
  {
    u2_noun a, b, c;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2,   &b,
                               u2_cv_sam_3,   &c,
                               u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqdb_put(a, b, c);
    }
  }
  u2_weak
  u2_ckdb_put(u2_noun a, u2_noun b, u2_noun c)
  {
    // Bizarre asymmetry in old jets.
    //
    // (Mysterious comment in old glue code.)
    //
    u2_noun pro = u2_cqdb_put(a, b, c);

    u2z(a); u2z(b); u2z(c);
    return pro;
  }

