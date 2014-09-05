/* j/4/by_get.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqdb_get(
                       u3_noun a,
                       u3_noun b)
  {
    if ( u3_nul == a ) {
      return u3_nul;
    }
    else {
      u3_noun l_a, n_a, r_a;
      u3_noun pn_a, qn_a;

      if ( (u3_no == u3_cr_trel(a, &n_a, &l_a, &r_a)) ||
           (u3_no == u3_cr_cell(n_a, &pn_a, &qn_a) ) )
      {
        return u3_cm_bail(c3__exit);
      }
      else {
        if ( (u3_yes == u3_cr_sing(b, pn_a)) ) {
          return u3nc(u3_nul, u3k(qn_a));
        }
        else {
          if ( u3_yes == u3_cqc_gor(b, pn_a) ) {
            return u3_cqdb_get(l_a, b);
          }
          else return u3_cqdb_get(r_a, b);
        }
      }
    }
  }
  u3_noun
  u3_cwdb_get(
                      u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam, &b, u3_cv_con_sam, &a, 0) ) {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqdb_get(a, b);
    }
  }
  u3_weak
  u3_ckdb_get(u3_noun a, u3_noun b)
  {
    u3_noun c = u3_cqdb_get(a, b);

    u3z(a); u3z(b);
    if ( u3_no == u3_cr_du(c) ) {
      u3z(c);
      return u3_none;
    } else {
      u3_noun pro = u3k(u3t(c));

      u3z(c);
      return pro;
    }
  }
  u3_noun
  u3_ckdb_got(u3_noun a, u3_noun b)
  {
    u3_weak c = u3_ckdb_get(a, b);

    if ( u3_none == c ) {
      return u3_cm_bail(c3__exit);
    }
    else return c;
  }

