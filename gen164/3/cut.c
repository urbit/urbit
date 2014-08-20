/* j/3/cut.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, cut)(
                   u2_atom a,                                     //  retain
                   u2_atom b,                                     //  retain
                   u2_atom c,                                     //  retain
                   u2_atom d)                                     //  retain
  {
    if ( u2_ne(u2_co_is_cat(a)) || (a >= 32) ) {
      return u2_cm_bail(c3__fail);
    }
    if ( u2_ne(u2_co_is_cat(b)) ) {
      return 0;
    }
    if ( u2_ne(u2_co_is_cat(c)) ) {
      c = 0x7fffffff;
    }

    {
      c3_g a_g   = a;
      c3_w b_w   = b;
      c3_w c_w   = c;
      c3_w len_w = u2_cr_met(a_g, d);

      if ( (0 == c_w) || (b_w >= len_w) ) {
        return 0;
      }
      if ( b_w + c_w > len_w ) {
        c_w = (len_w - b_w);
      }
      if ( (b_w == 0) && (c_w == len_w) ) {
        return u2k(d);
      }
      else {
        c3_w* sal_w = u2_ca_slaq(a_g, c_w);

        if ( 0 == sal_w ) {
          return u2_cm_bail(c3__fail);
        }
        u2_cr_chop(a_g, b_w, c_w, 0, sal_w, d);

        return u2_ca_malt(sal_w);
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, cut)(
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b, c, d;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2,  &a,
                                u2_cv_sam_12, &b,
                                u2_cv_sam_13, &c,
                                u2_cv_sam_7,  &d, 0)) ||
         (u2_no == u2ud(a)) ||
         (u2_no == u2ud(b)) ||
         (u2_no == u2ud(c)) ||
         (u2_no == u2ud(d)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt3, cut)(a, b, c, d);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt3, cut)[] = {
    { ".2", c3__lite, j2_mb(Pt3, cut), Tier3, u2_none, u2_none },
    { }
  };
