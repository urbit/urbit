/* j/3/met.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, met)(
                   u2_atom a,                                     //  retain
                   u2_atom b)                                     //  retain
  {
    if ( u2_ne(u2_co_is_cat(a)) || (a >= 32) ) {
      if ( 0 == b ) {
        return 0;
      } else return 1;
    }
    else {
      c3_w met_w = u2_cr_met(a, b);

      if ( u2_ne(u2_co_is_cat(met_w)) ) {
        return u2_ci_words(1, &met_w);
      }
      else return u2_cr_met(a, b);
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, met)(
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ||
         (u2_no == u2ud(a)) ||
         (u2_no == u2ud(b)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt3, met)(a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt3, met)[] = {
    { ".2", c3__lite, j2_mb(Pt3, met), Tier3, u2_none, u2_none },
    { }
  };
