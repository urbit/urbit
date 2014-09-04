/* j/5/mule.c
**
** This file is in the public domain.
*/
#include "all.h"


  u2_noun
  u2_cwe_mule(u2_noun cor)
  {
    u2_noun tap;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam, &tap, 0) ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      u2_noun hok = u2_cj_hook(u2k(cor), "mute");

      /* this takes advantage of the fact that mute's result, at the typeless
       * C/Nock level, is identical to what a typed mule would produce,
       * without running the formula twice.
       */
      return u2_cn_slam_on(hok, u2k(tap));;
    }
  }
