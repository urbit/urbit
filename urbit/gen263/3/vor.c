/* j/3/vor.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pit, vor)(u2_wire wir_r, 
                   u2_atom a,                                     //  retain
                   u2_atom b)                                     //  retain
  {
    c3_w c_w = u2_mug(u2_mug(a));
    c3_w d_w = u2_mug(u2_mug(b));

    if ( c_w == d_w ) {
      return j2_mbc(Pit, dor)(wir_r, a, b);
    }
    else return (c_w < d_w) ? u2_yes : u2_no;
  }
  u2_weak                                                         //  transfer
  j2_mb(Pit, vor)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, 8, &a, 9, &b, 0)) ) {
      return u2_none;
    } else {
      return j2_mbc(Pit, vor)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, vor)[] = {
    { ".3", c3__lite, j2_mb(Pit, vor), Tier3, u2_none, u2_none },
    { }
  };
