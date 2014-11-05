/* j/6/firm.c
**
** This file is in the public domain.
*/
#include "all.h"


/* logic
*/
  u3_noun
  _cqfu_firm(
                        u3_noun van,
                        u3_noun sut,
                        u3_noun dib)
  {
    u3_noun p_sut, q_sut;

    if ( c3n == u3du(sut) ) switch ( sut ) {
      default: return u3_cm_bail(c3__fail);

      case c3__noun:
      {
        return c3y;
      }
      case c3__void:
      {
        return c3n;
      }
    }
    else switch ( u3h(sut) ) {
      default: return u3_cm_bail(c3__fail);

      case c3__atom: p_sut = u3t(sut);
      {
        return u3ud(dib);
      }
      case c3__bull: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun nun = u3_cr_at(u3h(u3t(u3t(p_sut))), dib);

        if ( c3nne == nun ) {
          return c3n;
        } else {
          return c3a(_cqfu_firm(van, q_sut, dib),
                        _cqfu_firm(van, u3t(u3t(u3t(p_sut))),
                                                          nun));
        }
      }
      case c3__cell: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        return c3a
          (u3du(dib),
           c3a(_cqfu_firm(van, p_sut, u3h(dib)),
                  _cqfu_firm(van, q_sut, u3t(dib))));
      }
      case c3__core: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun pq_sut, qq_sut, rq_sut, prq_sut, qrq_sut;

        u3_cx_trel(q_sut, &pq_sut, &qq_sut, &rq_sut);
        u3_cx_cell(rq_sut, &prq_sut, &qrq_sut);

        return c3a
          (u3du(dib),
           c3a
            (_cqfu_firm(van, p_sut, u3h(dib)),
             ((u3_nul == prq_sut) ? u3_cm_error("firm-core")
                                  : u3_cr_sing(prq_sut, u3t(dib)))));
      }
      case c3__cube: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        return u3_cr_sing(dib, p_sut);
      }
      case c3__face: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        return _cqfu_firm(van, q_sut, dib);
      }
      case c3__fork: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
      {
        return c3o(_cqfu_firm(van, p_sut, dib),
                     _cqfu_firm(van, q_sut, dib));
      }
      case c3__hold:
      {
        u3_noun goy = u3_cqfu_repo(van, sut);
        u3_noun ret = _cqfu_firm(van, goy, dib);

        u3z(goy);
        return ret;
      }
    }
  }

/* boilerplate
*/
  u3_noun
  u3_cwfu_firm(
                       u3_noun cor)
  {
    u3_noun sut, dib, van;

    if ( (c3n == u3_cr_mean(cor, u3_cv_sam, &dib, u3_cv_con, &van, 0)) ||
         (c3nne == (sut = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqfu_firm(van, sut, dib);
    }
  }

  u3_noun
  u3_cqfu_firm(u3_noun van,
                        u3_noun sut,
                        u3_noun dib)
  {
    return _cqfu_firm(van, sut, dib);
  }
