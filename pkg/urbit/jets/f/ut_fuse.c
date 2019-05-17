/* j/6/fuse.c
**
*/
#include "all.h"

  static u3_noun
  _cqfu_fuse(u3_noun van,
             u3_noun sut,
             u3_noun ref)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("_cqfu_fuse-fuse", von, "fuse");

    gat = u3i_molt(gat, u3x_sam, u3k(ref), 0);

    return u3n_nock_on(gat, u3k(u3x_at(u3x_bat, gat)));

  }

/* boilerplate
*/
  u3_noun
  u3wfu_fuse(u3_noun cor)
  {
    u3_noun sut, ref, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &ref, u3x_con, &van, 0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_fuse(van, sut, ref);
    }
  }

  u3_noun
  u3qfu_fuse(u3_noun van,
             u3_noun sut,
             u3_noun ref)
  {
    c3_m    fun_m = 141 + c3__fuse + ((!!u3r_at(u3qfu_van_vet, van)) << 8);
    u3_noun pro   = u3z_find_2(fun_m, sut, ref);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_fuse(van, sut, ref);

      return u3z_save_2(fun_m, sut, ref, pro);
    }
  }
