/* j/1/dec.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt1, inc)(u2_atom a)                                     //  retain
  {
    return u2_ci_vint(u2k(a));
  }

  u2_weak                                                         //  transfer
  j2_mbc(Pt1, dec)(u2_atom a)                                     //  retain
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
  u2_weak                                                         //  transfer
  j2_mb(Pt1, dec)(u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ||
         (u2_no == u2ud(a)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt1, dec)(a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt1, dec)[] = {
    { ".2", c3__lite, j2_mb(Pt1, dec), u2_jet_live, u2_none, u2_none },
    { }
  };
