/* j/1/dec.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun
  u2_cqa_inc(u2_atom a)
  {
    return u2_ci_vint(u2k(a));
  }

  u2_noun
  u2_cqa_dec(u2_atom a)
  {
    if ( 0 == a ) {
      return u2_cm_error("decrement-underflow");
    }
    else {
      if ( u2_so(u2_co_is_cat(a)) ) {
        return a - 1;
      }
      else {
        mpz_t a_mp;

        u2_cr_mp(a_mp, a);
        mpz_sub_ui(a_mp, a_mp, 1);

        return u2_ci_mp(a_mp);
      }
    }
  }
  u2_noun
  u2_cwa_dec(u2_noun cor)
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ||
         (u2_no == u2ud(a)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return u2_cqa_dec(a);
    }
  }
