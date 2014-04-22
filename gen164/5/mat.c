/* j/3/mat.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  produce
  j2_mby(Pt5, mat)(u2_wire wir_r,
                   u2_atom a)                                     //  retain
  {
    if ( _0 == a ) {
      return u2_bc(wir_r, 1, 1);
    } else {
      u2_atom b = j2_mbc(Pt3, met)(wir_r, 0, a);
      u2_atom c = j2_mbc(Pt3, met)(wir_r, 0, b);
      u2_atom u, v, w, x, y, z;
      u2_atom p, q;

      u = j2_mbc(Pt1, dec)(wir_r, c);
      v = j2_mbc(Pt1, add)(wir_r, c, c);
      w = j2_mbc(Pt3, bex)(wir_r, c);
      x = j2_mbc(Pt3, end)(wir_r, _0, u, b);
      y = j2_mbc(Pt3, lsh)(wir_r, _0, u, a);
      z = j2_mbc(Pt3, mix)(wir_r, x, y);

      p = j2_mbc(Pt1, add)(wir_r, v, b);
      q = j2_mbc(Pt3, cat)(wir_r, _0, w, z);

      u2_rz(wir_r, u);
      u2_rz(wir_r, v);
      u2_rz(wir_r, w);
      u2_rz(wir_r, x);
      u2_rz(wir_r, y);
      u2_rz(wir_r, z);

      return u2_bc(wir_r, p, q);
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt5, mat)(u2_wire wir_r,
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_frag(u2_cv_sam, cor))) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt5, mat)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt5, mat)[] = {
    { ".2", c3__hevy, j2_mb(Pt5, mat), Tier3, u2_none, u2_none },
    { }
  };
