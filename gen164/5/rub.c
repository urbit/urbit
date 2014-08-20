/* j/3/rub.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  produce
  j2_mby(Pt5, rub)(
                   u2_atom a,                                     //  retain
                   u2_atom b)                                     //  retain
  {
    u2_atom c, d, e;
    u2_atom w, x, y, z;
    u2_atom p, q;

    u2_atom m = j2_mbc(Pt1, add)(a, u2_cr_met(0, b));

    //  Compute c and d.
    {
      x = u2k(a);

      while ( 0 == j2_mbc(Pt3, cut)(0, x, 1, b) ) {
        u2_atom y = j2_mbc(Pt1, inc)(x);

        //  Sanity check: crash if decoding more bits than available
        if ( u2_yes == j2_mbc(Pt1, gth)(x, m)) {
          //  fprintf(stderr, "[%%rub-hard %d %d %d]\r\n", a, x, m);
          return u2_cm_bail(c3__exit);
        }

        u2z(x);
        x = y;
      }
      if ( u2_yes == u2_cr_sing(x, a) ) {
        u2z(x);
        return u2nc(1, 0);
      }
      c = j2_mbc(Pt1, sub)(x, a);
      d = j2_mbc(Pt1, inc)(x);

      u2z(x);
    }

    //  Compute e, p, q.
    {
      x = j2_mbc(Pt1, dec)(c);
      y = j2_mbc(Pt3, bex)(x);
      z = j2_mbc(Pt3, cut)(0, d, x, b);

      e = j2_mbc(Pt1, add)(y, z);
      u2z(y); u2z(z);

      w = j2_mbc(Pt1, add)(c, c);
      y = j2_mbc(Pt1, add)(w, e);
      z = j2_mbc(Pt1, add)(d, x);

      p = j2_mbc(Pt1, add)(w, e);
      q = j2_mbc(Pt3, cut)(0, z, e, b);

      u2z(w); u2z(x); u2z(y); u2z(z);

      return u2nc(p, q);
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt5, rub)(
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ||
         (u2_no == u2ud(a)) ||
         (u2_no == u2ud(b)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mby(Pt5, rub)(a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt5, rub)[] = {
    { ".2", c3__hevy, j2_mb(Pt5, rub), Tier5, u2_none, u2_none },
    { }
  };
