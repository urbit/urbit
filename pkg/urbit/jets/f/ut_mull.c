/* j/6/mull.c
**
*/
#include "all.h"

  u3_noun
  u3wfu_mull(u3_noun cor)
  {
    u3_noun sut, gol, dox, gen, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &gol,
                               u3x_sam_6, &dox,
                               u3x_sam_7, &gen,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      c3_m    fun_m = 141 + c3__mull + ((!!u3r_at(u3qfu_van_vet, van)) << 8);
      u3_noun pro   = u3z_find_4(fun_m, sut, gol, dox, gen);

      if ( u3_none != pro ) {
        return pro;
      }
      else {
        pro = u3n_nock_on(u3k(cor), u3k(u3x_at(u3x_bat, cor)));

        return u3z_save_4(fun_m, sut, gol, dox, gen, pro);
      }
    }
  }
