/* j/6/ut_burn.c
**
*/
#include "all.h"


/* logic
*/
  static u3_noun
  _burn_in(u3_noun van,
           u3_noun sut,
           u3_noun gil)
  {
    u3_noun p_sut, q_sut, pq_sut, qq_sut, rq_sut, prq_sut, qrq_sut;

    if ( c3n == u3du(sut) ) switch ( sut ) {
      default: return u3m_bail(c3__fail);

      case c3__noun: return 0;
      case c3__void: {
        return u3m_error("burn-void");
      }
    }
    else switch ( u3h(sut) ) {
      default: return u3m_bail(c3__fail);

      case c3__atom: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        if ( c3y == u3du(q_sut) ) {
          return u3k(u3t(q_sut));
        } else {
          return 0;
        }
      }
      case c3__cell: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        return u3nc
          (_burn_in(van, p_sut, gil),
                  _burn_in(van, q_sut, gil));
      }
      case c3__core: u3x_cell(u3t(sut), &p_sut, &q_sut);
                     u3x_trel(q_sut, &pq_sut, &qq_sut, &rq_sut);
                     u3x_cell(rq_sut, &prq_sut, &qrq_sut);
      {
        return u3nc
          (u3k(prq_sut),
                  _burn_in(van, p_sut, gil));
      }
      case c3__face: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        return _burn_in(van, q_sut, gil);
      }
      case c3__fork: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        return _burn_in(van, p_sut, gil);
      }
#if 0
      case c3__frog: p_sut = u3t(sut);
      {
        return _burn_in(van, u3h(p_sut), gil);
      }
#else
      case c3__frog: p_sut = u3t(sut);
      {
        u3_noun fag = u3qf_grof(p_sut);
        u3_noun ret = _burn_in(van, fag, gil);

        u3z(fag);
        return ret;
      }
#endif
      case c3__hold: p_sut = u3t(sut);
      {
        if ( (c3y == u3qdi_has(gil, sut)) ) {
          u3_noun sux = u3qfu_dunq(van, "type", sut);

          u3t_push(u3nc(c3__mean, sux));
          return u3m_error("burn-loop");
        }
        else {
          u3_noun zoc = u3qdi_put(gil, sut);
          u3_noun fop = u3qfu_repo(van, sut);
          u3_noun pro = _burn_in(van, fop, zoc);

          u3z(fop);
          u3z(zoc);

          return pro;
        }
      }
    }
  }

  u3_noun
  _cqfu_burn(u3_noun van,
             u3_noun sut)
  {
    return _burn_in(van, sut, u3_nul);
  }

  u3_noun
  u3qfu_burn(u3_noun van,
             u3_noun sut)
  {
    c3_m    fun_m = c3__burn + !!u3r_at(u3qfu_van_vet, van);
    u3_noun pro   = u3z_find(fun_m, sut);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_burn(van, sut);

      return u3z_save(fun_m, sut, pro);
    }
  }

  u3_noun
  u3wfu_burn(u3_noun cor)
  {
    u3_noun sut;

    if ( u3_none == (sut = u3r_at(u3x_sam, cor)) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_burn(cor, sut);
    }
  }
