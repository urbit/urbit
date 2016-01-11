/* j/6/peek.c
**
*/
#include "all.h"


/* logic
*/
  u3_noun
  _cqfu_peek(u3_noun, u3_noun, u3_noun, u3_atom);

  static u3_noun
  _peek_in(u3_noun, u3_noun, u3_noun, u3_atom, u3_noun);

  static u3_noun
  _peek_frog(u3_noun van, u3_noun p_sut, u3_noun way, u3_noun axe, u3_noun gil)
  {
    if ( u3_nul == p_sut ) {
      return u3_nul;
    } 
    else {
      return u3nc(_peek_in(van, u3h(p_sut), way, axe, gil),
                  _peek_frog(van, u3t(p_sut), way, axe, gil));
    }
  }

  static u3_noun
  _peek_in(u3_noun van,
           u3_noun sut,
           u3_noun way,
           u3_atom axe,
           u3_noun gil)
  {
    u3_noun p_sut, q_sut;
    u3_noun pro;

    if ( (c3n == u3du(sut)) ) switch ( sut ) {
      default: return u3m_bail(c3__fail);

      case c3__noun: {
        return c3__noun;
      }
      case c3__void: {
        return c3__void;
      }
    }
    else switch ( u3h(sut) ) {
      default: return u3m_bail(c3__fail);

      case c3__atom: {
        return c3__void;
      }
      case c3__cell: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          u3_atom tip = u3qc_cap(axe);
          u3_atom tap = u3qc_mas(axe);

          if ( 2 == tip ) {
            pro = _cqfu_peek(van, p_sut, way, tap);
          }
          else {
            pro = _cqfu_peek(van, q_sut, way, tap);
          }
          u3z(tap);
          u3z(tip);

          return pro;
        }
      }
      case c3__core: {
        u3_noun pq_sut, qq_sut, rq_sut;
        u3_noun prq_sut, qrq_sut;

        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ||
             (c3n == u3r_trel(q_sut, &pq_sut, &qq_sut, &rq_sut)) ||
             (c3n == u3r_cell(rq_sut, &prq_sut, &qrq_sut)) )
        {
          return u3m_bail(c3__fail);
        } else {
          u3_atom tip = u3qc_cap(axe);
          u3_atom tap = u3qc_mas(axe);

          if ( 3 == tip ) {
            if ( c3n == u3qfu_park(van, sut, way, tap) )
            {
              // u3_noun dun = u3qfu_dunq(van, "type", sut);
              u3_noun waz = u3qfu_shep
                (van, "axis", 'd', u3k(axe));

              // u3t_push(u3nc(c3__mean, dun));
              u3t_push(u3nc(c3__mean, waz));

              return u3m_error("peek-park");
            }
            else pro = _cqfu_peek(van, p_sut, way, tap);
          }
          else {
            pro = c3__noun;
          }
          u3z(tap);
          u3z(tip);

          return pro;
        }
      }
      case c3__face: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          return _peek_in(van, q_sut, way, axe, gil);
        }
      }
      case c3__frog: {
        p_sut = u3t(sut);

        return u3kf_frog(_peek_frog(van, p_sut, way, axe, gil));
      }
      case c3__hold: {
        p_sut = u3t(sut);
        if ( (c3y == u3qdi_has(gil, sut)) ) {
          return c3__void;
        }
        else {
          u3_noun zoc = u3qdi_put(gil, sut);
          u3_noun fop = u3qfu_repo(van, sut);
          u3_noun pro = _peek_in(van, fop, way, axe, zoc);

          u3z(fop);
          u3z(zoc);

          return pro;
        }
      }
    }
  }

  u3_noun
  _cqfu_peek(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_atom axe)
  {
    if ( 1 == axe ) {
      return u3k(sut);
    }
    else return _peek_in(van, sut, way, axe, u3_nul);
  }

/* boilerplate
*/
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
      return _cqfu_peek(van, sut, way, axe);
    }
  }

  u3_noun
  u3qfu_peek(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun axe)
  {
    c3_m    fun_m = c3__peek + !!u3r_at(u3qfu_van_vet, van);
    u3_noun pro   = u3z_find_3(fun_m, sut, way, axe);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_peek(van, sut, way, axe);

      return u3z_save_3(fun_m, sut, way, axe, pro);
    }
  }
