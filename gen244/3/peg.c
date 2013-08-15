/* j/3/peg.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, peg)(u2_wire wir_r, 
                   u2_atom a,                                     //  retain
                   u2_atom b)                                     //  retain
  {
    u2_atom c, d, e, f, g, h;

    c = u2_met(0, b);
    d = j2_mbc(Pt1, dec)(wir_r, c);
    e = j2_mbc(Pt3, lsh)(wir_r, _0, d, 1);
    f = j2_mbc(Pt1, sub)(wir_r, b, e);
    g = j2_mbc(Pt3, lsh)(wir_r, _0, d, a);
    h = j2_mbc(Pt1, add)(wir_r, f, g);

    u2_rl_lose(wir_r, c);
    u2_rl_lose(wir_r, d);
    u2_rl_lose(wir_r, e);
    u2_rl_lose(wir_r, f);
    u2_rl_lose(wir_r, g);

    return h;
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, peg)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, u2_cw_sam_2, &a, u2_cw_sam_3, &b, 0)) ||
         (u2_no == u2_stud(a)) ||
         (u2_no == u2_stud(b)) ||
         (0 == a) ||
         (0 == b) )
    {
      return u2_none;
    } else {
      return j2_mbc(Pt3, peg)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt3, peg)[] = {
    { ".3", c3__lite, j2_mb(Pt3, peg), Tier3, u2_none, u2_none },
    { }
  };
