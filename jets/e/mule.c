/* j/5/mule.c
**
*/
#include "all.h"

  u3_noun
  u3we_mule(u3_noun cor)
  {
    u3_noun tap;

    if ( c3n == u3r_mean(cor, u3x_sam, &tap, 0) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun hok = u3j_cook("u3we_mule-mute", u3k(cor), "mute");

      /* this takes advantage of the fact that mute's result, at the typeless
       * C/Nock level, is identical to what a typed mule would produce,
       * without running the formula twice.
       */
      return u3n_slam_on(hok, u3k(tap));
    }
  }
