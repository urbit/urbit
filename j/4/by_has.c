/* j/4/by_has.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_bean
  u3_cqdb_has(
                       u3_noun a,
                       u3_noun b)
  {
    if ( u3_nul == a ) {
      return u3_no;
    }
    else {
      u3_noun l_a, n_a, r_a;
      u3_noun pn_a, qn_a;

      if ( (u3_no == u3_cr_trel(a, &n_a, &l_a, &r_a)) ||
           (u3_no == u3_cr_cell(n_a, &pn_a, &qn_a)) )
      {
        return u3_cm_bail(c3__exit);
      }
      else {
        if ( (u3_yes == u3_cr_sing(b, pn_a)) ) {
          return u3_yes;
        }
        else {
          if ( u3_yes == u3_cqc_gor(b, pn_a) ) {
            return u3_cqdb_has(l_a, b);
          }
          else return u3_cqdb_has(r_a, b);
        }
      }
    }
  }
  u3_noun
  u3_cwdb_has(
                      u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam, &b, u3_cv_con_sam, &a, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqdb_has(a, b);
    }
  }
  u3_bean
  u3_ckdb_has(u3_noun a, u3_noun b)
  {
    u3_weak c = u3_cqdb_has(a, b);

    u3z(a); u3z(b);
    if ( u3_none == c ) {
      return u3_cm_bail(c3__exit);
    }
    else return c;
  }

