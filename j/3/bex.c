/* j/3/bex.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqc_bex(u3_atom a)
  {
    mpz_t a_mp;

    if ( !_(u3_ca_is_cat(a)) ) {
      return u3_cm_bail(c3__fail);
    }
    else {
      mpz_init_set_ui(a_mp, 1);
      mpz_mul_2exp(a_mp, a_mp, a);

      return u3_ci_mp(a_mp);
    }
  }
  u3_noun
  u3_cwc_bex(
                  u3_noun cor)
  {
    u3_noun a;

    if ( (c3nne == (a = u3_cr_at(u3_cv_sam, cor))) ||
         (c3n == u3ud(a)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqc_bex(a);
    }
  }

