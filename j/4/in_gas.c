/* j/4/gas.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u2_noun
  u2_cqdi_gas(
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
        u2_noun c;

        if ( u2_none == (c = u2_cqdi_put(a, i_b)) ) {
          return u2_cm_bail(c3__exit);
        } else {
          u2_noun d = u2_cqdi_gas(c, t_b);

          u2z(c);
          return d;
        }
      }
    }
  }
  u2_noun
  u2_cwdi_gas(
                      u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqdi_gas(a, b);
    }
  }
  u2_noun
  u2_ckdi_gas(u2_noun a, u2_noun b)
  {
    u2_weak c = u2_cqdi_gas(a, b);

    u2z(a); u2z(b);
    if ( u2_none == c ) {
      return u2_cm_bail(c3__exit);
    }
    else return c;
  }

