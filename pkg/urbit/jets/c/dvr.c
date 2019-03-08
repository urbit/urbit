/* j/3/dvr.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_dvr(u3_atom a,
           u3_atom b)
  {
    if ( u3a_is_nil(b) ) {
      return u3m_bail(c3__exit);
    }
    else {
      if ( _(u3a_is_cat(a)) && _(u3a_is_cat(b)) ) {
        c3_w a_w = u3a_get_cat31(a);
        c3_w b_w = u3a_get_cat31(b);
        return u3nc(UNSAFECAT(a_w / b_w), UNSAFECAT(a_w % b_w));
      }
      else {
        mpz_t a_mp, b_mp;

        u3r_mp(a_mp, a);
        u3r_mp(b_mp, b);

        mpz_tdiv_qr(a_mp, b_mp, a_mp, b_mp);

        return u3nc(u3k(u3i_mp(a_mp)), u3k(u3i_mp(b_mp)));
      }
    }
  }
  u3_noun
  u3wc_dvr(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_dvr(a, b);
    }
  }

