/* j/5/mule.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  u2_weak                                                         //  produce
  j2_mb(Pt5, mule)(u2_noun cor)                                   //  retain
  {
    u2_noun tap;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &tap, 0) ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      u2_noun hok = u2_cn_hook(u2k(cor), "mute");

      /* this takes advantage of the fact that mute's result, at the typeless
       * C/Nock level, is identical to what a typed mule would produce,
       * without running the formula twice.
       */
      return u2_cn_slam_on(hok, u2k(tap));;
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt5, mule)[] = {
    { ".2", c3__lite, j2_mb(Pt5, mule), Tier5, u2_none, u2_none },
    { }
  };
