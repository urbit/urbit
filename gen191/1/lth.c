/* j/1/lth.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt1, lth)(u2_wire wir_r, 
                   u2_atom a,                                     //  retain
                   u2_atom b)                                     //  retain
  {
    mpz_t   a_mp, b_mp;
    u2_bean cmp;

    u2_mp(a_mp, a);
    u2_mp(b_mp, b);

    cmp = (mpz_cmp(a_mp, b_mp) < 0) ? u2_yes : u2_no;

    mpz_clear(a_mp);
    mpz_clear(b_mp);

    return cmp;
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt1, lth)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ||
         (u2_no == u2_stud(a)) ||
         (u2_no == u2_stud(b)) )
    {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbc(Pt1, lth)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt1, lth)[] = {
    { ".2", c3__lite, j2_mb(Pt1, lth), Tier1, u2_none, u2_none },
   { }
  };
