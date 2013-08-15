/* j/3/trip.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt5, trip)(u2_wire wir_r, 
                    u2_atom a)                                    //  retain
  {
    return j2_mbc(Pt3, rip)(wir_r, _3, a);
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt5, trip)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_frag(u2_cw_sam, cor))) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt5, trip)(wir_r, a);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt5, trip)[] = {
    { ".3", c3__hevy, j2_mb(Pt5, trip), Tier3, u2_none, u2_none },
    { }
  };
