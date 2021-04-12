/* j/6/ut_rest.c
**
*/
#include "all.h"

u3_noun
u3wfu_rest(u3_noun cor)
{
  u3_noun bat, sut, leg, van;

  if ( (c3n == u3r_mean(cor, u3x_sam, &leg, u3x_con, &van, 0))
     || (u3_none == (bat = u3r_at(u3x_bat, van)))
     || (u3_none == (sut = u3r_at(u3x_sam, van))) )
  {
    return u3m_bail(c3__fail);
  }
  else {
    u3_weak vet = u3r_at(u3qfu_van_vet, van);
    c3_m  fun_m = 141 + c3__rest + ((!!vet) << 8);
    u3_noun key = u3z_key_3(fun_m, sut, leg, bat);
    u3_weak pro = u3z_find(key);

    if ( u3_none != pro ) {
      u3z(key);
      return pro;
    }
    else {
      pro = u3n_nock_on(u3k(cor), u3k(u3x_at(u3x_bat, cor)));
      return u3z_save(key, pro);
    }
  }
}
