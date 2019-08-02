/* j/6/peek.c
**
*/
#include "all.h"

  static u3_noun
  _cqfu_peek(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_atom axe)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("_cqfu_peek-peek", von, "peek");

    gat = u3i_molt(gat, u3x_sam_2, u3k(way), u3x_sam_3, u3k(axe), 0);

    return u3n_nock_on(gat, u3k(u3x_at(u3x_bat, gat)));
  }

/* boilerplate
*/
  u3_noun
  u3qfu_peek(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun axe)
  {
    c3_m    fun_m = 141 + c3__peek + ((!!u3r_at(u3qfu_van_vet, van)) << 8);
    u3_noun pro   = u3z_find_3(fun_m, sut, way, axe);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_peek(van, sut, way, axe);

      return u3z_save_3(fun_m, sut, way, axe, pro);
    }
  }

  u3_noun
  u3wfu_peek(u3_noun cor)
  {
    u3_noun sut, way, axe, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &way,
                               u3x_sam_3, &axe,
                               u3x_con, &van,
                               0)) ||
         (c3n == u3ud(axe)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_peek(van, sut, way, axe);
    }
  }
