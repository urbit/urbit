/* j/3/trip.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt5, trip)(
                    u2_atom a)                                    //  retain
  {
    if ( u2_no == u2ud(a) ) {
      return u2_cm_bail(c3__exit);
    }
    return j2_mbc(Pt3, rip)(3, a);
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt5, trip)(
                   u2_noun cor)                                    //  retain
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mby(Pt5, trip)(a);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt5, trip)[] = {
    { ".2", c3__hevy, j2_mb(Pt5, trip), Tier3, u2_none, u2_none },
    { }
  };
