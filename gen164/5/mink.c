/* j/5/mink.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  u2_weak                                                         //  produce
  j2_mb(Pt5, mink)(u2_wire wir_r,
                   u2_noun cor)                                   //  retain
  {
    u2_noun bus, fol, fly;

    if ( u2_no == u2_mean(cor, u2_cv_sam_4, &bus,
                               u2_cv_sam_5, &fol,
                               u2_cv_sam_3, &fly,
                               0) )
    {
      return u2_bl_bail(wir_r, c3__exit);
    }
    else {
      return u2_cn_mink(u2k(bus), u2k(fol), u2k(fly));
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt5, mink)[] = {
    { ".2", c3__lite, j2_mb(Pt5, mink), Tier5, u2_none, u2_none },
    { }
  };
