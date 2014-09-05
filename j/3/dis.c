/* j/3/dis.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_dis(u3_atom a,
                   u3_atom b)
  {
    c3_w lna_w = u3_cr_met(5, a);
    c3_w lnb_w = u3_cr_met(5, b);

    if ( (lna_w == 0) && (lnb_w == 0) ) {
      return 0;
    } else {
      c3_w  len_w = c3_max(lna_w, lnb_w);
      c3_w* sal_w = u3_ca_slab(len_w);

      if ( 0 == sal_w ) {
        return u3_cm_bail(c3__fail);
      }
      else {
        c3_w i_w;

        u3_cr_chop(5, 0, lna_w, 0, sal_w, a);

        for ( i_w = 0; i_w < len_w; i_w++ ) {
          sal_w[i_w] &= (i_w >= lnb_w) ? 0 : u3_cr_word(i_w, b);
        }
        return u3_ca_malt(sal_w);
      }
    }
  }
  u3_noun
  u3_cwc_dis(u3_noun cor)
  {
    u3_noun a, b;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0)) ||
         (u3_no == u3ud(a)) ||
         (u3_no == u3ud(b)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqc_dis(a, b);
    }
  }

