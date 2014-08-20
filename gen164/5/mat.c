/* j/3/mat.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  produce
  j2_mby(Pt5, mat)(
                   u2_atom a)                                     //  retain
  {
    if ( 0 == a ) {
      return u2nc(1, 1);
    } else {
      u2_atom b = j2_mbc(Pt3, met)(0, a);
      u2_atom c = j2_mbc(Pt3, met)(0, b);
      u2_atom u, v, w, x, y, z;
      u2_atom p, q;

      u = j2_mbc(Pt1, dec)(c);
      v = j2_mbc(Pt1, add)(c, c);
      w = j2_mbc(Pt3, bex)(c);
      x = j2_mbc(Pt3, end)(0, u, b);
      y = j2_mbc(Pt3, lsh)(0, u, a);
      z = j2_mbc(Pt3, mix)(x, y);

      p = j2_mbc(Pt1, add)(v, b);
      q = j2_mbc(Pt3, cat)(0, w, z);

      u2z(u);
      u2z(v);
      u2z(w);
      u2z(x);
      u2z(y);
      u2z(z);

      return u2nc(p, q);
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt5, mat)(
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mby(Pt5, mat)(a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt5, mat)[] = {
    { ".2", c3__hevy, j2_mb(Pt5, mat), Tier3, u2_none, u2_none },
    { }
  };
