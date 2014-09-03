/* j/4/in_has.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_bean
  u2_cqdi_has(
                       u2_noun a,
                       u2_noun b)
  {
    if ( u2_nul == a ) {
      return u2_no;
    }
    else {
      u2_noun l_a, n_a, r_a;

      if ( (u2_no == u2_cr_mean(a, 2, &n_a, 6, &l_a, 7, &r_a, 0)) ) {
        return u2_cm_bail(c3__exit);
      }
      else {
        if ( (u2_yes == u2_cr_sing(b, n_a)) ) {
          return u2_yes;
        }
        else {
          if ( u2_yes == u2_cqc_hor(b, n_a) ) {
            return u2_cqdi_has(l_a, b);
          }
          else return u2_cqdi_has(r_a, b);
        }
      }
    }
  }
  u2_noun
  u2_cwdi_has(
                      u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqdi_has(a, b);
    }
  }
