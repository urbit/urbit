/* j/6/bust.c
**
** This file is in the public domain.
*/
#include "all.h"


/* logic
*/
  u2_noun
  _cqfu_bust(
                        u2_noun van,
                        u2_noun sut,
                        u2_noun dib)
  {
    u2_noun yam = u2_cqfu_burn(van, sut);
    u2_noun yib = u2nc
      (
       u2nc(u2k(u2h(u2h(yam))),
                    u2k(dib)),
       u2k(u2t(yam)));
    u2_noun woo = u2nc(c3__cnzy, u2_blip);
    u2_noun wox = u2_cqfu_mint(van, sut, c3__noun, woo);
    u2_noun ret = u2_cn_nock_on(yib, u2k(u2t(wox)));

    if ( u2_none == ret ) {
      return u2_cm_error("bust-nock");
    }
    u2z(wox);
    u2z(woo);
    u2z(yam);

    return ret;
  }

/* boilerplate
*/
  u2_noun
  u2_cwfu_bust(u2_noun cor)
  {
    u2_noun sut, dib, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &dib, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_bust(van, sut, dib);
    }
  }

  u2_noun
  u2_cqfu_bust(u2_noun van,
                        u2_noun sut,
                        u2_noun dib)
  {
#if 1
    return _cqfu_bust(van, sut, dib);
#else
    c3_m    fun_m = c3__bust;
    u2_noun pro   = u2_cz_find_2(fun_m, sut, dib);

    if ( u2_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_bust(van, sut, dib);

      return u2_cz_save_2(fun_m, sut, dib, pro);
    }
#endif
  }
