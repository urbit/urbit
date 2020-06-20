/* j/6/peek.c
**
*/
#include "all.h"

u3_noun
u3wfu_peek(u3_noun cor)
{
  u3_noun sut, way, axe, van;
  static c3_w zav_w = 0, vaz_w = 0;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &way,
                             u3x_sam_3, &axe,
                             u3x_con, &van,
                             0)) ||
       (c3n == u3ud(axe)) ||
       (u3_none == (sut = u3r_at(u3x_sam, van))) )
  {
    return u3m_bail(c3__fail);
  }
  else {
    c3_m  fun_m = 141 + c3__peek + ((!!u3r_at(u3qfu_van_vet, van)) << 8);
    u3_noun key = u3z_key_3(fun_m, sut, way, axe);
    u3_weak pro = u3z_find(key);

    if ( u3_none != pro ) {
      vaz_w++;
      u3z(key);
      return pro;
    }
    else {
      pro = u3n_nock_on(u3k(cor), u3k(u3x_at(u3x_bat, cor)));
      if ( zav_w++ % 50000 == 1) {
        u3l_log("peek: %d %d %f\r\n", zav_w, vaz_w, (double)vaz_w / (zav_w + vaz_w));
      }
      u3z(key);
      return pro;
      //return u3z_save(key, pro);
    }
  }
}
