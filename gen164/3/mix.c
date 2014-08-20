/* j/3/mix.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, mix)(u2_atom a,                                     //  retain
                   u2_atom b)                                     //  retain
  {
    c3_w lna_w = u2_cr_met(5, a);
    c3_w lnb_w = u2_cr_met(5, b);

    if ( (lna_w == 0) && (lnb_w == 0) ) {
      return 0;
    } else {
      c3_w  len_w = c3_max(lna_w, lnb_w);
      c3_w* sal_w = u2_ca_slab(len_w);

      if ( 0 == sal_w ) {
        return u2_cm_bail(c3__fail);
      }
      else {
        c3_w i_w;

        u2_cr_chop(5, 0, lna_w, 0, sal_w, a);

        for ( i_w = 0; i_w < lnb_w; i_w++ ) {
          sal_w[i_w] ^= u2_cr_word(i_w, b);
        }
        return u2_ca_malt(sal_w);
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, mix)(u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ||
         (u2_no == u2ud(a)) ||
         (u2_no == u2ud(b)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt3, mix)(a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt3, mix)[] = {
    { ".2", c3__lite, j2_mb(Pt3, mix), Tier3, u2_none, u2_none },
    { }
  };
