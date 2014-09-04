/* j/4/in_tap.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  static u2_noun
  _tap_in(
          u2_noun a,
          u2_noun b)
  {
    if ( u2_nul == a ) {
      return b;
    } else {
      u2_noun l_a, n_a, r_a;

      if ( (u2_no == u2_cr_trel(a, &n_a, &l_a, &r_a)) ) {
        u2z(b);
        return u2_cm_bail(c3__exit);
      } else {
        return _tap_in
          (r_a,
                  u2nc(u2k(n_a),
                               _tap_in(l_a, b)));
      }
    }
  }

  u2_noun
  u2_cqdi_tap(
                       u2_noun a,
                       u2_noun b)
  {
    return _tap_in(a, u2k(b));
  }
  u2_noun
  u2_cwdi_tap(
                      u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqdi_tap(a, b);
    }
  }
