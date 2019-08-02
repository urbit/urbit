/* j/6/ut_rest.c
**
*/
#include "all.h"

  u3_noun
  u3wfu_rest(u3_noun cor)
  {
    u3_noun sut, leg, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &leg, u3x_con, &van, 0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      c3_m    fun_m = 141 + c3__rest + ((!!u3r_at(u3qfu_van_vet, van)) << 8);
      u3_noun pro   = u3z_find_2(fun_m, sut, leg);

      if ( u3_none != pro ) {
        return pro;
      }
      else {
        pro = u3n_nock_on(u3k(cor), u3k(u3x_at(u3x_bat, cor)));

        return u3z_save_2(fun_m, sut, leg, pro);
      }
    }
  }
