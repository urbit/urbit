/* j/6/ut_nest.c
**
*/
#include "all.h"


/* logic
*/
  /* forward
  */
    static u3_noun
    _nest_sint(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    static u3_noun
    _nest_dext(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);

  static u3_noun
  _nest_cram(u3_noun van,
             u3_noun sut,
             u3_noun tel,
             u3_noun ref,
             u3_noun dab,
             u3_noun hem,
             u3_noun gil)
  {
    if ( u3_nul == dab ) {
      return u3r_sing(u3_nul, hem);
    } else if ( u3_nul == hem ) {
      return c3n;
    } else {
      u3_noun n_dab, l_dab, r_dab;
      u3_noun n_hem, l_hem, r_hem;
      u3_noun pn_hem, qn_hem, pn_dab, qn_dab;

      u3x_trel(dab, &n_dab, &l_dab, &r_dab);
      u3x_trel(hem, &n_hem, &l_hem, &r_hem);

      if ( (c3n == _nest_cram(van, sut, tel, ref, l_dab, l_hem, gil)) ||
           (c3n == _nest_cram(van, sut, tel, ref, r_dab, r_hem, gil)) ) {
        return c3n;
      }
      u3x_cell(n_dab, &pn_dab, &qn_dab);
      u3x_cell(n_hem, &pn_hem, &qn_hem);

      if ( c3n == u3r_sing(pn_dab, pn_hem) ) {
        return c3n;
      } else {
        if ( (c3n == u3du(qn_dab)) || (c3n == u3du(qn_hem)) ) {
          return u3m_bail(c3__fail);
        }
        else switch ( u3h(qn_dab) ) {
          default: return u3m_bail(c3__exit);
          case c3__ash: {
            if ( c3__ash != u3h(qn_hem) ) {
              return c3n;
            } else {
              u3_noun pqn_dab = u3t(qn_dab);
              u3_noun pqn_hem = u3t(qn_hem);
              u3_noun vis = u3qfu_play(van, sut, pqn_dab);
              u3_noun lon = u3qfu_play(van, ref, pqn_hem);
              u3_noun ret = _nest_dext(van, vis, tel, lon, gil);

              u3z(vis);
              u3z(lon);
              return ret;
            }
          }
          case c3__elm: {
            return u3r_sing(qn_dab, qn_hem);
          }
        }
      }
    }
  }

  static u3_noun
  _nest_cong(u3_noun van,
             u3_noun sut,
             u3_noun tel,
             u3_noun ref,
             u3_noun gil)
  {
    u3_noun p_sut, q_sut, p_ref, q_ref;
    u3_noun pq_sut, qq_sut, rq_sut;
    u3_noun pq_ref, qq_ref, rq_ref;
    u3_noun prq_sut, qrq_sut, prq_ref, qrq_ref;
    u3_noun ret;

    u3x_trel(sut, 0, &p_sut, &q_sut);
    u3x_trel(ref, 0, &p_ref, &q_ref);

    u3x_trel(q_sut, &pq_sut, &qq_sut, &rq_sut);
    u3x_trel(q_ref, &pq_ref, &qq_ref, &rq_ref);

    u3x_cell(rq_sut, &prq_sut, &qrq_sut);
    u3x_cell(rq_ref, &prq_ref, &qrq_ref);

    if ( c3y == u3r_sing(q_sut, q_ref) ) {
      return _nest_dext(van, p_sut, tel, p_ref, gil);
    }
    else if ( (c3n == _nest_dext(van, qq_sut, tel, p_sut, gil)) ||
              (c3n == _nest_dext(van, p_sut, tel, qq_sut, gil)) ||
              (c3n == _nest_dext(van, qq_ref, tel, p_ref, gil)) )
    {
      return c3n;
    }
    else {
      if ( (pq_sut != pq_ref) && (c3__gold != pq_ref) ) {
        return c3n;
      }
      else {
        u3_noun hud = u3nc(u3k(sut), u3k(ref));

        if ( (c3y == u3qdi_has(gil, hud)) ) {
          u3z(hud);

          return c3y;
        }
        else {
          u3_noun zoc = u3qdi_put(gil, hud);
          u3_noun tus = u3nt(c3__core,
                             u3k(qq_sut),
                             u3k(q_sut));
          u3_noun fer = u3nt(c3__core,
                             u3k(qq_ref),
                             u3k(q_ref));

          ret = _nest_cram(van, tus, tel, fer, qrq_sut, qrq_ref, zoc);
          u3z(fer);
          u3z(tus);
          u3z(zoc);
          u3z(hud);

          if ( c3n == ret ) {
            return c3n;
          }
          else {
            switch ( pq_sut ) {
              default: return u3m_bail(c3__fail);

              case c3__gold: {
                return
                  c3a(_nest_dext(van, qq_sut, tel, qq_ref, gil),
                      _nest_dext(van, qq_ref, tel, qq_sut, gil));
              }
              case c3__iron: {
                u3_noun s_sam = u3qfu_peek(van, qq_sut, c3__rite, 2);
                u3_noun r_sam = u3qfu_peek(van, qq_ref, c3__rite, 2);
                u3_noun ret = _nest_dext(van, r_sam, tel, s_sam, gil);

                u3z(r_sam);
                u3z(s_sam);
                return ret;
              }
              case c3__lead: {
                return c3y;
              }
              case c3__zinc: {
                u3_noun s_pal = u3qfu_peek(van, qq_sut, c3__read, 2);
                u3_noun r_pal = u3qfu_peek(van, qq_ref, c3__read, 2);
                u3_noun ret = _nest_dext(van, s_pal, tel, r_pal, gil);

                u3z(r_pal);
                u3z(s_pal);

                return ret;
              }
            }
          }
        }
      }
    }
  }

  static u3_noun
  _nest_dext_in(u3_noun van,
                u3_noun sut,
                u3_noun tel,
                u3_noun ref,
                u3_noun gil)
  {
    u3_noun p_sut, q_sut, p_ref, q_ref;

    if ( (c3n == u3du(sut)) ) switch ( sut ) {
      default: return u3m_bail(c3__fail);

      case c3__noun: {
        return c3y;
      }
      case c3__void: {
        return _nest_sint(van, sut, tel, ref, gil);
      }
    }
    else switch ( u3h(sut) ) {
      default: return u3m_bail(c3__fail);

      case c3__atom: {
        if ( (c3y == u3du(ref)) && (c3__atom == u3h(ref)) ) {
          if ( c3n == u3qf_fitz(u3t(sut), u3t(ref)) ) {
            // u3_err("fitz: need", u3t(sut));
            // u3_err("fitz: have", u3t(ref));
            return c3n;
          }
          return c3y;
        }
        else return _nest_sint(van, sut, tel, ref, gil);
      }
      case c3__bull: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          return _nest_dext(van, q_sut, tel, ref, gil);
        }
      }
      case c3__cell: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          if ( c3y == u3r_pq(ref, c3__cell, &p_ref, &q_ref) ) {
            return c3a(_nest_dext(van, p_sut, tel, p_ref, gil),
                       _nest_dext(van, q_sut, tel, q_ref, gil));
          }
          else return _nest_sint(van, sut, tel, ref, gil);
        }
      }
      case c3__core: {
        if ( c3n == u3r_trel(sut, 0, &p_sut, &q_sut) ) {
          return u3m_bail(c3__fail);
        } else {
          if ( (c3y == u3r_pq(ref, c3__core, &p_ref, &q_ref)) ) {
            return _nest_cong(van, sut, tel, ref, gil);
          }
          else return _nest_sint(van, sut, tel, ref, gil);
        }
      }
      case c3__cube: {
        if ( c3n == u3r_trel(sut, 0, &p_sut, &q_sut) ) {
          return u3m_bail(c3__fail);
        } else {
          if ( c3y == u3r_pq(ref, c3__cube, &p_ref, &q_ref) ) {
            return u3r_sing(p_sut, p_ref);
          }
          else return _nest_sint(van, sut, tel, ref, gil);
        }
      }
      case c3__face: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          return _nest_dext(van, q_sut, tel, ref, gil);
        }
      }
      case c3__fork: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        }
        else {
          if ( c3n == u3du(ref) ) switch ( ref ) {
            default: return _nest_sint(van, sut, tel, ref, gil);

            case c3__noun:
              break;
          }
          else switch ( u3h(ref) ) {
            default: return _nest_sint(van, sut, tel, ref, gil);

            case c3__atom:
            case c3__cell:
            case c3__cube:
            case c3__core:
              break;
          }

          return c3o(_nest_dext(van, p_sut, c3n, ref, gil),
                     _nest_dext(van, q_sut, c3n, ref, gil));
        }
      }
      case c3__hold: p_sut = u3t(sut);
      {
        u3_noun hud = u3nc(u3k(sut), u3k(ref));

        if ( (c3y == u3qdi_has(gil, hud)) ) {
          u3z(hud);

          return c3y;
        } else {
          u3_noun zoc = u3qdi_put(gil, hud);
          u3_noun fop = u3qfu_rest(van, sut, p_sut);
          u3_noun hiv = _nest_dext(van, fop, tel, ref, zoc);

          u3z(hud);
          u3z(fop);
          u3z(zoc);

          return hiv;
        }
      }
    }
  }

  static u3_noun
  _nest_dext_to(u3_noun van,
                u3_noun sut,
                u3_noun tel,
                u3_noun ref,
                u3_noun gil)
  {
    u3_noun tyn = _nest_dext_in(van, sut, tel, ref, gil);

    if ( (c3y == tyn) || (c3n == tel) ) {
      return tyn;
    } else {
      // u3_noun dun = u3qfu_dunq(van, "need", sut);
      // u3_noun niz = u3qfu_dunq(van, "have", ref);

      // u3t_push(u3nc(c3__mean, dun));
      // u3t_push(u3nc(c3__mean, niz));

      return u3m_error("type-fail");
    }
  }

  static u3_noun
  _nest_dext(u3_noun van,
             u3_noun sut,
             u3_noun tel,
             u3_noun ref,
             u3_noun gil)
  {

    if ( (c3y == u3r_sing(sut, ref)) ) {
      return c3y;
    }

    {
      c3_m    fun_m = c3__nest;
      u3_noun pro   = u3z_find_2(fun_m, sut, ref);

      if ( u3_none != pro ) {
        return pro;
      }
      else {
        pro = _nest_dext_to(van, sut, tel, ref, gil);

        return u3z_save_2(fun_m, sut, ref, pro);
      }
    }
  }

  static u3_noun
  _nest_sint(u3_noun van,
             u3_noun sut,
             u3_noun tel,
             u3_noun ref,
             u3_noun gil)
  {
    u3_noun p_ref, q_ref;

    if ( (c3n == u3du(ref)) ) {
      switch ( ref ) {
        default: return u3m_bail(c3__fail);

        case c3__noun: return c3n;
        case c3__void: return c3y;
      }
    }
    else {
      switch ( u3h(ref) ) {
        default: {
          return u3m_bail(c3__fail);
        }
        case c3__atom: return c3n;
        case c3__bull: {
          if ( c3n == u3r_trel(ref, 0, &p_ref, &q_ref) ) {
            return u3m_bail(c3__fail);
          } else {
            return _nest_dext(van, sut, tel, q_ref, gil);
          }
        }
        case c3__cell: return c3n;
        case c3__core: {
          u3_noun gam = u3qfu_repo(van, ref);
          u3_noun hiv = _nest_dext(van, sut, tel, gam, gil);

          u3z(gam);
          return hiv;
        }
        case c3__cube: {
          if ( c3n == u3r_trel(ref, 0, &p_ref, &q_ref) ) {
            return u3m_bail(c3__fail);
          } else {
            return _nest_dext(van, sut, tel, q_ref, gil);
          }
        }
        case c3__face: {
          if ( c3n == u3r_trel(ref, 0, &p_ref, &q_ref) ) {
            return u3m_bail(c3__fail);
          } else {
            return _nest_dext(van, sut, tel, q_ref, gil);
          }
        }
        case c3__fork: {
          if ( (c3y == u3r_mean(ref, 6, &p_ref, 7, &q_ref, 0)) ) {
            return c3a(_nest_dext(van, sut, c3n, p_ref, gil),
                       _nest_dext(van, sut, c3n, q_ref, gil));
          }
          else return u3m_bail(c3__fail);
        }
        case c3__hold: {
          p_ref = u3t(ref);
          {
            u3_noun hud = u3nc(u3k(sut),
                               u3k(ref));

            if ( (c3y == u3qdi_has(gil, hud)) ) {
              u3z(hud);

              return c3y;
            } else {
              u3_noun zoc = u3qdi_put(gil, hud);
              u3_noun gam = u3qfu_repo(van, ref);

              {
                u3_noun hiv = _nest_dext(van, sut, tel, gam, zoc);

                u3z(hud);
                u3z(gam);
                u3z(zoc);

                return hiv;
              }
            }
          }
        }
      }
    }
  }

  u3_noun
  _cqfu_nest(u3_noun van,
             u3_noun sut,
             u3_noun tel,
             u3_noun ref)
  {
    return _nest_dext(van, sut, tel, ref, u3_nul);
  }

/* boilerplate
*/
  u3_noun
  u3wfu_nest(u3_noun cor)
  {
    u3_noun sut, tel, ref, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &tel,
                               u3x_sam_3, &ref,
                               u3x_con, &van,
                               0)) ||
         (c3n == u3ud(tel)) || (tel > 1) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_nest(van, sut, tel, ref);
    }
  }

  u3_noun
  u3qfu_nest(u3_noun van,
             u3_noun sut,
             u3_noun tel,
             u3_noun ref)
  {
    return _cqfu_nest(van, sut, tel, ref);
  }

