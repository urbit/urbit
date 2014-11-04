/* j/4/in_has.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_bean
  u3_cqdi_has(u3_noun a, u3_noun b)
  {
    if ( u3_nul == a ) {
      return u3_no;
    }
    else {
      u3_noun l_a, n_a, r_a;

      if ( (u3_no == u3_cr_mean(a, 2, &n_a, 6, &l_a, 7, &r_a, 0)) ) {
        return u3_cm_bail(c3__exit);
      }
      else {
        if ( (u3_yes == u3_cr_sing(b, n_a)) ) {
          return u3_yes;
        }
        else {
          if ( u3_yes == u3_cqc_hor(b, n_a) ) {
            return u3_cqdi_has(l_a, b);
          }
          else return u3_cqdi_has(r_a, b);
        }
      }
    }
  }
  u3_noun
  u3_cwdi_has(u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam, &b, u3_cv_con_sam, &a, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqdi_has(a, b);
    }
  }
  u3_bean
  u3_ckdi_has(u3_noun a, u3_noun b)
  {
    u3_weak c = u3_cqdi_has(a, b);

    u3z(a); u3z(b);
    if ( u3_none == c ) {
      return u3_cm_bail(c3__exit);
    }
    else return c;
  }

