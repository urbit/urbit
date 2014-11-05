/* j/1/mod.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqa_mod(
                   u3_atom a,
                   u3_atom b)
  {
    if ( 0 == b ) {
      return u3_cm_bail(c3__exit);
    } else {
      mpz_t a_mp, b_mp;

      u3_cr_mp(a_mp, a);
      u3_cr_mp(b_mp, b);

      mpz_tdiv_r(a_mp, a_mp, b_mp);
      mpz_clear(b_mp);

      return u3_ci_mp(a_mp);
    }
  }

  u3_noun
  u3_cwa_mod(u3_noun cor)
  {
    u3_noun a, b;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0)) ||
         (u3_no == u3ud(a)) ||
         (u3_no == u3ud(b)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqa_mod(a, b);
    }
  }
