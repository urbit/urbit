/* j/4/gas.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqdi_gas(
                       u3_noun a,
                       u3_noun b)
  {
    if ( u3_nul == b ) {
      return u3k(a);
    }
    else {
      if ( c3n == u3du(b) ) {
        return u3_cm_bail(c3__exit);
      } else {
        u3_noun i_b = u3h(b);
        u3_noun t_b = u3t(b);
        u3_noun c;

        if ( u3_none == (c = u3_cqdi_put(a, i_b)) ) {
          return u3_cm_bail(c3__exit);
        } else {
          u3_noun d = u3_cqdi_gas(c, t_b);

          u3z(c);
          return d;
        }
      }
    }
  }
  u3_noun
  u3_cwdi_gas(
                      u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam, &b, u3_cv_con_sam, &a, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqdi_gas(a, b);
    }
  }
  u3_noun
  u3_ckdi_gas(u3_noun a, u3_noun b)
  {
    u3_weak c = u3_cqdi_gas(a, b);

    u3z(a); u3z(b);
    if ( u3_none == c ) {
      return u3_cm_bail(c3__exit);
    }
    else return c;
  }

