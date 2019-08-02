/* j/6/find.c
**
*/
#include "all.h"

  static u3_noun
  _cqfu_fond(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("_cqfu_fond-fond", von, "fond");

    gat = u3i_molt(gat, u3x_sam_2, u3k(way), u3x_sam_3, u3k(hyp), 0);

    return u3n_nock_on(gat, u3k(u3x_at(u3x_bat, gat)));
  }

  static u3_noun
  _cqfu_find(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("_cqfu_find-find", von, "find");

    gat = u3i_molt(gat, u3x_sam_2, u3k(way), u3x_sam_3, u3k(hyp), 0);

    return u3n_nock_on(gat, u3k(u3x_at(u3x_bat, gat)));
  }

/* boilerplate
*/
  u3_noun
  u3qfu_find(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    c3_m    fun_m = 141 + c3__find + ((!!u3r_at(u3qfu_van_vet, van)) << 8);
    u3_noun pro   = u3z_find_3(fun_m, sut, way, hyp);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_find(van, sut, way, hyp);

      return u3z_save_3(fun_m, sut, way, hyp, pro);
    }
  }

  u3_noun
  u3wfu_find(u3_noun cor)
  {
    u3_noun sut, way, hyp, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &way,
                               u3x_sam_3, &hyp,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_find(van, sut, way, hyp);
    }
  }

/* boilerplate
*/
  u3_noun
  u3qfu_fond(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    c3_m    fun_m = 141 + c3__fond + ((!!u3r_at(u3qfu_van_vet, van)) << 8);
    u3_noun pro   = u3z_find_3(fun_m, sut, way, hyp);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_fond(van, sut, way, hyp);

      return u3z_save_3(fun_m, sut, way, hyp, pro);
    }
  }

  u3_noun
  u3wfu_fond(u3_noun cor)
  {
    u3_noun sut, way, hyp, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &way,
                               u3x_sam_3, &hyp,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_fond(van, sut, way, hyp);
    }
  }

