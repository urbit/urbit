/* j/4/gas.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun
  u2_cqdb_gas(
                       u2_noun a,
                       u2_noun b)
  {
    if ( u2_nul == b ) {
      return u2k(a);
    }
    else {
      if ( u2_no == u2du(b) ) {
        return u2_cm_bail(c3__exit);
      } else {
        u2_noun i_b = u2h(b);
        u2_noun t_b = u2t(b);

        if ( u2_no == u2du(i_b) ) {
          return u2_cm_bail(c3__exit);
        } else {
          u2_noun pi_b = u2h(i_b);
          u2_noun qi_b = u2t(i_b);
          u2_noun c;

          if ( u2_none == (c = u2_cqdb_put(a, pi_b, qi_b)) ) {
            return u2_cm_bail(c3__exit);
          } else {
            u2_noun d = u2_cqdb_gas(c, t_b);

            u2z(c);
            return d;
          }
        }
      }
    }
  }
  u2_noun
  u2_cwdb_gas(
                      u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqdb_gas(a, b);
    }
  }
