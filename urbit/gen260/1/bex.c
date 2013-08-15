/* j/1/bex.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt1, bex)(u2_wire wir_r, 
                   u2_atom a)                                     //  retain
  {
    mpz_t a_mp;

    if ( !u2_fly_is_cat(a) ) {
      return u2_none;
    }
    else {
      mpz_init_set_ui(a_mp, 1);
      mpz_mul_2exp(a_mp, a_mp, a);

      return u2_rl_mp(wir_r, a_mp);
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt1, bex)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_frag(u2_cv_sam, cor))) ||
         (u2_no == u2_stud(a)) ) 
    {
      return u2_none;
    } else {
      return j2_mbc(Pt1, bex)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt1, bex)[] = { 
    { ".3", c3__lite, j2_mb(Pt1, bex), Tier1, u2_none, u2_none },
    { }
  };
