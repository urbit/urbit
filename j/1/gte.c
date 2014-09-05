/* j/1/gte.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqa_gte(
                   u3_atom a,
                   u3_atom b)
  {
    if ( u3_co_is_cat(a) && u3_co_is_cat(b) ) {
      return u3_say(a >= b);
    }
    else {
      mpz_t   a_mp, b_mp;
      u3_bean cmp;

      u3_cr_mp(a_mp, a);
      u3_cr_mp(b_mp, b);

      cmp = (mpz_cmp(a_mp, b_mp) >= 0) ? u3_yes : u3_no;

      mpz_clear(a_mp);
      mpz_clear(b_mp);

      return cmp;
    }
  }
  u3_noun
  u3_cwa_gte(
                  u3_noun cor)
  {
    u3_noun a, b;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0)) ||
         (u3_no == u3ud(a)) ||
         (u3_no == u3ud(b)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqa_gte(a, b);
    }
  }
