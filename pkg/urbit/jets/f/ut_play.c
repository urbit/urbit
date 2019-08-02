/* j/6/play.c
**
*/
#include "all.h"

  u3_noun
  u3wfu_play(u3_noun cor)
  {
    u3_noun sut, gen, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &gen, u3x_con, &van, 0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      c3_m    fun_m = 141 + c3__play;
      u3_noun vrf   = u3r_at(u3qfu_van_vrf, van);
      u3_noun pro   = u3z_find_3(fun_m, vrf, sut, gen);

      if ( u3_none != pro ) {
        return pro;
      }
      else {
        pro = u3n_nock_on(u3k(cor), u3k(u3x_at(u3x_bat, cor)));

        return u3z_save_3(fun_m, vrf, sut, gen, pro);
      }
    }
  }
