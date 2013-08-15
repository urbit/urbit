/* j/3/cut.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pit, cut)(u2_wire wir_r, 
                   u2_atom a,                                     //  retain
                   u2_atom b,                                     //  retain
                   u2_atom c,                                     //  retain
                   u2_atom d)                                     //  retain
  {
    if ( !u2_fly_is_cat(a) || (a >= 32) ) {
      return u2_none;
    }
    if ( !u2_fly_is_cat(b) ) {
      return _0;
    }
    if ( !u2_fly_is_cat(c) ) {
      c = 0x7fffffff;
    }

    {
      c3_g a_g   = a;
      c3_w b_w   = b;
      c3_w c_w   = c;
      c3_w len_w = u2_met(a_g, d);

      if ( (_0 == c_w) || (b_w >= len_w) ) {
        return _0;
      }
      if ( b_w + c_w > len_w ) {
        c_w = (len_w - b_w);
      }
      if ( (b_w == 0) && (c_w == len_w) ) {
        return u2_rx(wir_r, d);
      }
      else {
        u2_ray sal_r = u2_rl_slaq(wir_r, a_g, c_w);

        if ( 0 == sal_r ) {
          return u2_none;
        }
        u2_chop(a_g, b_w, c_w, 0, sal_r, d);

        return u2_rl_malt(wir_r, sal_r);
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pit, cut)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b, c, d;

    if ( (u2_no == u2_mean(cor, 8, &a, 36, &b, 37, &c, 19, &d, 0)) ||
         (u2_no == u2_stud(a)) ||
         (u2_no == u2_stud(b)) ||
         (u2_no == u2_stud(c)) ||
         (u2_no == u2_stud(d)) )
    {
      return u2_none;
    } else {
      return j2_mbc(Pit, cut)(wir_r, a, b, c, d);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, cut)[] = {
    { ".3", j2_mb(Pit, cut), u2_yes, u2_none, u2_none },
    { }
  };
