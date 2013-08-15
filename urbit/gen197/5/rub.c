/* j/3/rub.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  produce
  j2_mby(Pt5, rub)(u2_wire wir_r, 
                   u2_atom a,                                     //  retain
                   u2_atom b)                                     //  retain
  {
    u2_atom c, d, e;
    u2_atom w, x, y, z;
    u2_atom p, q;

    //  Compute c and d.
    {
      x = u2_rx(wir_r, a);

      while ( _0 == j2_mbc(Pt3, cut)(wir_r, _0, x, _1, b) ) {
        u2_atom y = j2_mbc(Pt1, inc)(wir_r, x);

        u2_rz(wir_r, x);
        x = y;
      }
      if ( u2_yes == u2_sing(x, a) ) {
        u2_rz(wir_r, x);
        return u2_bc(wir_r, _1, _0);
      }
      c = j2_mbc(Pt1, sub)(wir_r, x, a);
      d = j2_mbc(Pt1, inc)(wir_r, x);

      u2_rz(wir_r, x);
    }
 
    //  Compute e, p, q.
    {
      x = j2_mbc(Pt1, dec)(wir_r, c);
      y = j2_mbc(Pt3, bex)(wir_r, x);
      z = j2_mbc(Pt3, cut)(wir_r, _0, d, x, b);

      e = j2_mbc(Pt1, add)(wir_r, y, z);
      u2_rz(wir_r, y); u2_rz(wir_r, z);

      w = j2_mbc(Pt1, add)(wir_r, c, c);
      y = j2_mbc(Pt1, add)(wir_r, w, e);
      z = j2_mbc(Pt1, add)(wir_r, d, x);

      p = j2_mbc(Pt1, add)(wir_r, w, e);
      q = j2_mbc(Pt3, cut)(wir_r, _0, z, e, b);

      u2_rz(wir_r, w); u2_rz(wir_r, x); u2_rz(wir_r, y); u2_rz(wir_r, z);

      return u2_bc(wir_r, p, q);
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt5, rub)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ||
         (u2_no == u2_stud(a)) ||
         (u2_no == u2_stud(b)) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt5, rub)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt5, rub)[] = {
    { ".2", c3__hevy, j2_mb(Pt5, rub), Tier3, u2_none, u2_none },
    { }
  };
