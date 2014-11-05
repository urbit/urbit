/* j/1/add.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqa_add(u3_atom a,
                   u3_atom b)
  {
    if ( u3_so(u3_ca_is_cat(a)) && u3_so(u3_ca_is_cat(b)) ) {
      c3_w c = a + b;

      return u3_ci_words(1, &c);
    }
    else {
      mpz_t a_mp, b_mp;

      u3_cr_mp(a_mp, a);
      u3_cr_mp(b_mp, b);

      mpz_add(a_mp, a_mp, b_mp);
      mpz_clear(b_mp);

      return u3_ci_mp(a_mp);
    }
  }
  u3_noun
  u3_cwa_add(u3_noun cor)
  {
    u3_noun a, b;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0)) ||
         (u3_no == u3ud(a)) ||
         (u3_no == u3ud(b)) )
    {
      return u3_cm_bail(c3__exit);
    } else {
      return u3_cqa_add(a, b);
    }
  }

  u3_noun
  u3_cka_add(u3_noun a, u3_noun b)
  {
    u3_noun c = u3_cqa_add(a, b);

    u3z(a); u3z(b);
    return c;
  }
