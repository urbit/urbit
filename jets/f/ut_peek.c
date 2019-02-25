/* j/6/peek.c
**
*/
#include "all.h"


/* logic
*/
  static u3_noun
  _cqfu_peek(u3_noun, u3_noun, u3_noun, u3_atom);

  static u3_noun
  _peek_in(u3_noun, u3_noun, u3_noun, u3_atom, u3_noun);

  static u3_noun
  _peek_fork(u3_noun van, u3_noun p_sut, u3_noun way, u3_noun axe, u3_noun gil)
  {
    if ( u3_nul == p_sut ) {
      return u3_nul;
    }
    else {
      return u3nc(_peek_in(van, u3h(p_sut), way, axe, gil),
                  _peek_fork(van, u3t(p_sut), way, axe, gil));
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
        u3_noun ppq_sut, qpq_sut, rpq_sut;
        u3_noun prq_sut, qrq_sut;

        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ||
             (c3n == u3r_trel(q_sut, &pq_sut, &qq_sut, &rq_sut)) ||
             (c3n == u3r_trel(pq_sut, &ppq_sut, &qpq_sut, &rpq_sut)) ||
             (c3n == u3r_cell(rq_sut, &prq_sut, &qrq_sut)) )
        {
          return u3m_bail(c3__fail);
        } else {
          u3_atom now = u3qc_cap(axe);
          u3_atom lat = u3qc_mas(axe);

          if ( 3 == now ) {
            u3_noun pec = u3qfu_peel(van, sut, way, rpq_sut);
            u3_noun sam = u3h(pec);
            u3_noun con = u3t(pec);
            u3_atom tow;

            if ( 1 == lat ) {
              tow = 1;
            } else {
              tow = u3qc_cap(lat);
            }

            if ( (c3y == c3a(sam, con)) ||
                 ((c3y == sam) && (tow == 2)) ||
                 ((c3y == con) && (tow == 3)) )
            {
              pro = _cqfu_peek(van, p_sut, way, lat);
            }
            else {

              if ( way != c3__read ) {
               return u3m_error("payload-block");
              }

              u3_noun typ;

              {
                u3_noun hed, tal;

                if ( c3n == sam ) {
                  hed = c3__noun;
                } else {
                  hed = _cqfu_peek(van, p_sut, way, 2);
                }

                if ( c3n == con ) {
                  tal = c3__noun;
                } else {
                  tal = _cqfu_peek(van, p_sut, way, 3);
                }

                typ = u3qf_cell(hed, tal);
                u3z(hed);
                u3z(tal);
              }

              pro = _cqfu_peek(van, typ, way, lat);
              u3z(typ);
            }

            u3z(pec);
            u3z(tow);
          }
          else {
            pro = c3__noun;
          }
          u3z(lat);
          u3z(now);

          return pro;
        }
      }
      case c3__hint:
      case c3__face: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          return _peek_in(van, q_sut, way, axe, gil);
        }
      }
      case c3__fork: p_sut = u3t(sut);
      {
        u3_noun yed = u3qdi_tap(p_sut);
        u3_noun ret = u3kf_fork(_peek_fork(van, yed, way, axe, gil));

        u3z(yed);
        return ret;
      }
      case c3__hold: {
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

  static u3_noun
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
