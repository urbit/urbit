/* j/4/by_has.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_bean
  u2_cqdb_has(
                       u2_noun a,
                       u2_noun b)
  {
    if ( u2_nul == a ) {
      return u2_no;
    }
    else {
      u2_noun l_a, n_a, r_a;
      u2_noun pn_a, qn_a;

      if ( (u2_no == u2_cr_trel(a, &n_a, &l_a, &r_a)) ||
           (u2_no == u2_cr_cell(n_a, &pn_a, &qn_a)) )
      {
        return u2_cm_bail(c3__exit);
      }
      else {
        if ( (u2_yes == u2_cr_sing(b, pn_a)) ) {
          return u2_yes;
        }
        else {
          if ( u2_yes == u2_cqc_gor(b, pn_a) ) {
            return u2_cqdb_has(l_a, b);
          }
          else return u2_cqdb_has(r_a, b);
        }
      }
    }
  }
  u2_noun
  u2_cwdb_has(
                      u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqdb_has(a, b);
    }
  }

