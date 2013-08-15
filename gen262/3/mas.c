/* j/3/mas.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, mas)(u2_wire wir_r, 
                   u2_atom a)                                     //  retain
  {
    c3_w b_w;
    u2_atom c, d, e, f;

    b_w = u2_met(0, a);
    if ( b_w < 2 ) {
      return u2_none;
    } 
    else {
      c = j2_mbc(Pt1, bex)(wir_r, (b_w - 1));
      d = j2_mbc(Pt1, bex)(wir_r, (b_w - 2));
      e = j2_mbc(Pt1, sub)(wir_r, a, c);
      f = j2_mbc(Pt3, con)(wir_r, e, d); 
 
      u2_rl_lose(wir_r, c);
      u2_rl_lose(wir_r, d);
      u2_rl_lose(wir_r, e);

      return f;
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, mas)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_frag(4, cor))) ||
         (u2_no == u2_stud(a)) )
    {
      return u2_none;
    } else {
      return j2_mbc(Pt3, mas)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt3, mas)[] = {
    { ".3", c3__lite, j2_mb(Pt3, mas), Tier3, u2_none, u2_none },
    { }
  };
