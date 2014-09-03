/* j/4/by_get.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  u2_cqdb_get(
                       u2_noun a,                                 //  retain
                       u2_noun b)                                 //  retain
  {
    if ( u2_nul == a ) {
      return u2_nul;
    }
    else {
      u2_noun l_a, n_a, r_a;
      u2_noun pn_a, qn_a;

      if ( (u2_no == u2_cr_trel(a, &n_a, &l_a, &r_a)) ||
           (u2_no == u2_cr_cell(n_a, &pn_a, &qn_a) ) )
      {
        return u2_cm_bail(c3__exit);
      }
      else {
        if ( (u2_yes == u2_cr_sing(b, pn_a)) ) {
          return u2nc(u2_nul, u2k(qn_a));
        }
        else {
          if ( u2_yes == u2_cqc_gor(b, pn_a) ) {
            return u2_cqdb_get(l_a, b);
          }
          else return u2_cqdb_get(r_a, b);
        }
      }
    }
  }
  u2_weak                                                         //  transfer
  u2_cwdb_get(
                      u2_noun cor)                                //  retain
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &b, u2_cv_con_sam, &a, 0) ) {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqdb_get(a, b);
    }
  }
