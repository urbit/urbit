/* j/6/wrap.c
**
** This file is in the public domain.
*/
#include "all.h"


  static u2_noun
  _cqfu_wrap(
                        u2_noun van,
                        u2_noun sut,
                        u2_noun yoz)
  {
    u2_noun p_sut, q_sut;

    if ( u2_no == u2du(sut) )  {
      return u2_cm_error("wrap-type");
    }
    else switch ( u2h(sut) ) {
      default: return u2_cm_error("wrap-type");

      case c3__core: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun pq_sut, qq_sut, rq_sut;

        if ( u2_no == u2_cr_trel(q_sut, &pq_sut, &qq_sut, &rq_sut) ) {
          return u2_cm_bail(c3__fail);
        }
        else if ( c3__gold != pq_sut ) {
          return u2_cm_error("wrap-gold");
        }
        else {
          return u2nt(c3__core,
                              u2k(p_sut),
                              u2nt(u2k(yoz),
                                           u2k(qq_sut),
                                           u2k(rq_sut)));
        }
      }
      case c3__fork: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        return u2_cqf_fork
          (_cqfu_wrap(van, p_sut, yoz),
                  _cqfu_wrap(van, q_sut, yoz));
      }
      case c3__hold:
      {
        u2_noun fop = u2_cqfu_repo(van, sut);
        u2_noun pro = _cqfu_wrap(van, fop, yoz);

        u2z(fop);
        return pro;
      }
    }
  }

/* boilerplate
*/
  u2_noun
  u2_cwfu_wrap(
                       u2_noun cor)
  {
    u2_noun sut, yoz, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &yoz, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_wrap(van, sut, yoz);
    }
  }

  u2_noun
  u2_cqfu_wrap(u2_noun van,
                        u2_noun sut,
                        u2_noun yoz)
  {
    return _cqfu_wrap(van, sut, yoz);
  }
