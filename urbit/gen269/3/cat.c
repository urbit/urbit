/* j/3/cat.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pit, cat)(u2_wire wir_r, 
                   u2_atom a,                                     //  retain
                   u2_atom b,                                     //  retain
                   u2_atom c)                                     //  retain
  {
    if ( !u2_fly_is_cat(a) || (a >= 32) ) {
      return u2_none;
    }
    else {
      c3_g   a_g = a;
      c3_w   lew_w = u2_met(a_g, b);
      c3_w   ler_w = u2_met(a_g, c); 
      u2_ray sal_r = u2_rl_slaq(wir_r, a_g, (lew_w + ler_w));

      if ( 0 == sal_r ) {
        return u2_none;
      } 
      else {
        u2_chop(a_g, 0, lew_w, 0, sal_r, b);
        u2_chop(a_g, 0, ler_w, lew_w, sal_r, c);
      }
      return u2_rl_moot(wir_r, sal_r);
    }
  }

  u2_weak                                                         //  transfer
  j2_mb(Pit, cat)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b, c;

    if ( (u2_no == u2_mean(cor, 8, &a, 18, &b, 19, &c, 0)) ||
         (u2_no == u2_stud(a)) ||
         (u2_no == u2_stud(b)) ||
         (u2_no == u2_stud(c)) )
    {
      return u2_none;
    } else {
      return j2_mbc(Pit, cat)(wir_r, a, b, c);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, cat)[] = {
    { ".3", c3__lite, j2_mb(Pit, cat), Tier3, u2_none, u2_none },
    { }
  };

