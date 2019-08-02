/* j/6/mull.c
**
*/
#include "all.h"

  static u3_noun
  _cqfu_mull(u3_noun van,
             u3_noun sut,
             u3_noun gol,
             u3_noun dox,
             u3_noun gen)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("_cqfu_mull-mull", von, "mull");

    gat = u3i_molt(gat,
                   u3x_sam_2, u3k(gol),
                   u3x_sam_6, u3k(dox),
                   u3x_sam_7, u3k(gen),
                   0);

    return u3n_nock_on(gat, u3k(u3x_at(u3x_bat, gat)));  }

/* boilerplate
*/
  u3_noun
  u3qfu_mull(u3_noun van,
             u3_noun sut,
             u3_noun gol,
             u3_noun dox,
             u3_noun gen)
  {
    c3_m    fun_m = 141 + c3__mull + ((!!u3r_at(u3qfu_van_vet, van)) << 8);
    u3_noun pro   = u3z_find_4(fun_m, sut, gol, dox, gen);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_mull(van, sut, gol, dox, gen);

      return u3z_save_4(fun_m, sut, gol, dox, gen, pro);
    }
  }

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
      return u3qfu_mull(van, sut, gol, dox, gen);
    }
  }
