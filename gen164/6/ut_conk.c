/* j/6/ut_conk.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_bean
  _cqfu_conk(
                        u2_noun van,
                        u2_noun sut,
                        u2_noun got)
  {
    if ( u2_yes == u2ud(got) ) {
      return u2_cqf_face(got, sut);
    }
    else switch ( u2h(got) ) {
      default: return u2_cm_bail(c3__fail);

      case 0: {
        return u2k(sut);
      }
      case 1: {
        return u2_cqf_face
          (u2h(u2t(got)),
                 _cqfu_conk(van, sut, u2t(u2t(got))));
      }
      case 2: {
        u2_bean vet = u2_cr_at(u2_cqfu_van_vet, van);
        u2_noun hed, tal, ret;

        if ( u2_yes == vet ) {
          u2_noun cel = u2nt(c3__cell, c3__noun, c3__noun);

          if ( u2_no == u2_cqfu_nest(van, cel, u2_yes, sut) ) {
            return u2_cm_bail(c3__fail);
          }
          u2z(cel);
        }
        hed = u2_cqfu_peek(van, sut, c3__both, 2);
        tal = u2_cqfu_peek(van, sut, c3__both, 3);

        ret = u2_cqf_cell
          (
           _cqfu_conk(van, hed, u2h(u2t(got))),
           _cqfu_conk(van, tal, u2t(u2t(got))));

        u2z(hed);
        u2z(tal);

        return ret;
      }
    }
  }

/* boilerplate
*/
  u2_noun
  u2_cwfu_conk(u2_noun cor)
  {
    u2_noun sut, got, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &got,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_conk(van, sut, got);
    }
  }

  u2_noun
  u2_cqfu_conk(u2_noun van,
                        u2_noun sut,
                        u2_noun got)
  {
    return _cqfu_conk(van, sut, got);
  }

