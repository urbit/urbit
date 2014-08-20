/* j/3/bex.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, bex)(u2_atom a)                                     //  retain
  {
    mpz_t a_mp;

    if ( u2_ne(u2_co_is_cat(a)) ) {
      return u2_cm_bail(c3__fail);
    }
    else {
      mpz_init_set_ui(a_mp, 1);
      mpz_mul_2exp(a_mp, a_mp, a);

      return u2_ci_mp(a_mp);
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, bex)(
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ||
         (u2_no == u2ud(a)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt3, bex)(a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt3, bex)[] = {
    { ".2", c3__lite, j2_mb(Pt3, bex), Tier3, u2_none, u2_none },
    { }
  };
