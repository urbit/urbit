/* j/6/play.c
**
*/
#include "all.h"

  static u3_noun
  _cqfu_play(u3_noun van,
             u3_noun sut,
             u3_noun gen)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("_cqfu_play-play", von, "play");

    gat = u3i_molt(gat, u3x_sam, u3k(gen), 0);

    return u3n_nock_on(gat, u3k(u3x_at(u3x_bat, gat)));
  }

/* boilerplate
*/
  u3_noun
  u3wfu_play(u3_noun cor)
  {
    u3_noun sut, gen, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &gen, u3x_con, &van, 0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_play(van, sut, gen);
    }
  }

  u3_noun
  u3qfu_play(u3_noun van,
             u3_noun sut,
             u3_noun gen)
  {
#if 1
    c3_m    fun_m = 141 + c3__play;
    u3_noun vrf   = u3r_at(u3qfu_van_vrf, van);
    u3_noun pro   = u3z_find_3(fun_m, vrf, sut, gen);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_play(van, sut, gen);

      return u3z_save_3(fun_m, vrf, sut, gen, pro);
    }
#else
    return _cqfu_play(van, sut, gen);
#endif
  }
