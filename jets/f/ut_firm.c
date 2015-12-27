/* j/6/firm.c
**
*/
#include "all.h"


/* logic
*/
  u3_noun
  _cqfu_firm(u3_noun van,
             u3_noun sut,
             u3_noun dib)
  {
    u3_noun p_sut, q_sut;

    if ( c3n == u3du(sut) ) switch ( sut ) {
      default: return u3m_bail(c3__fail);

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
      default: return u3m_bail(c3__fail);

      case c3__atom: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        return u3ud(dib);
      }
      case c3__cell: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        return c3a(u3du(dib),
                   c3a(_cqfu_firm(van, p_sut, u3h(dib)),
                       _cqfu_firm(van, q_sut, u3t(dib))));
      }
      case c3__core: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun pq_sut, qq_sut, rq_sut, prq_sut, qrq_sut;

        u3x_trel(q_sut, &pq_sut, &qq_sut, &rq_sut);
        u3x_cell(rq_sut, &prq_sut, &qrq_sut);

        return c3a(u3du(dib),
                   c3a(_cqfu_firm(van, p_sut, u3h(dib)),
                       ((u3_nul == prq_sut) ? u3m_error("firm-core")
                                            : u3r_sing(prq_sut, 
                                                       u3t(dib)))));
      }
      case c3__cube: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        return u3r_sing(dib, p_sut);
      }
      case c3__face: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        return _cqfu_firm(van, q_sut, dib);
      }
      case c3__fork: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        return c3o(_cqfu_firm(van, p_sut, dib),
                     _cqfu_firm(van, q_sut, dib));
      }
      case c3__hold:
      {
        u3_noun goy = u3qfu_repo(van, sut);
        u3_noun ret = _cqfu_firm(van, goy, dib);

        u3z(goy);
        return ret;
      }
    }
  }

/* boilerplate
*/
  u3_noun
  u3wfu_firm(u3_noun cor)
  {
    u3_noun sut, dib, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &dib, u3x_con, &van, 0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_firm(van, sut, dib);
    }
  }

  u3_noun
  u3qfu_firm(u3_noun van,
             u3_noun sut,
             u3_noun dib)
  {
    return _cqfu_firm(van, sut, dib);
  }
