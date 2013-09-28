/* j/6/ut_nest.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  /* forward
  */
    static u2_bean
    _nest_sint(u2_wire, u2_noun, u2_noun, u2_bean, u2_noun, u2_noun);
    static u2_bean
    _nest_dext(u2_wire, u2_noun, u2_noun, u2_bean, u2_noun, u2_noun);

    u2_ho_jet 
    j2_mcj(Pt6, ut, nest)[];

  static u2_bean
  _nest_cram(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_bean tel,
             u2_noun ref,
             u2_noun dab,
             u2_noun hem, 
             u2_noun gil)
  {
    if ( u2_nul == dab ) {
      return u2_sing(u2_nul, hem);
    } else if ( u2_nul == hem ) {
      return u2_no;
    } else {
      u2_noun n_dab, l_dab, r_dab;
      u2_noun n_hem, l_hem, r_hem;
      u2_noun pn_hem, qn_hem, pn_dab, qn_dab;

      u2_bi_trel(wir_r, dab, &n_dab, &l_dab, &r_dab);
      u2_bi_trel(wir_r, hem, &n_hem, &l_hem, &r_hem);

      if ( (u2_no == _nest_cram
                        (wir_r, van, sut, tel, ref, l_dab, l_hem, gil)) ||
           (u2_no == _nest_cram
                        (wir_r, van, sut, tel, ref, r_dab, r_hem, gil)) ) {
        return u2_no;
      }
      u2_bi_cell(wir_r, n_dab, &pn_dab, &qn_dab);
      u2_bi_cell(wir_r, n_hem, &pn_hem, &qn_hem);

      if ( u2_no == u2_sing(pn_dab, pn_hem) ) {
        return u2_no;
      } else {
        if ( (u2_no == u2_dust(qn_dab)) || (u2_no == u2_dust(qn_hem)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } 
        else switch ( u2_h(qn_dab) ) {
          default: return u2_bl_bail(wir_r, c3__exit);
          case c3__ash: {
            if ( c3__ash != u2_h(qn_hem) ) {
              return u2_no;
            } else {
              u2_noun pqn_dab = u2_t(qn_dab);
              u2_noun pqn_hem = u2_t(qn_hem);
              u2_noun vis = j2_mcy(Pt6, ut, play)(wir_r, van, sut, pqn_dab);
              u2_noun lon = j2_mcy(Pt6, ut, play)(wir_r, van, ref, pqn_hem);
              u2_bean ret = _nest_dext(wir_r, van, vis, tel, lon, gil);

              u2_rz(wir_r, vis);
              u2_rz(wir_r, lon);
              return ret;
            }
          }
          case c3__elm: {
            return u2_sing(qn_dab, qn_hem);
          }
        }
      }
    }
  }
 
  static u2_bean
  _nest_cong(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_bean tel,
             u2_noun ref,
             u2_noun gil)
  {
    u2_noun p_sut, q_sut, p_ref, q_ref;
    u2_noun pq_sut, qq_sut, rq_sut;
    u2_noun pq_ref, qq_ref, rq_ref;
    u2_noun prq_sut, qrq_sut, prq_ref, qrq_ref;
    u2_bean ret;

    u2_bi_trel(wir_r, sut, 0, &p_sut, &q_sut);
    u2_bi_trel(wir_r, ref, 0, &p_ref, &q_ref);

    u2_bi_trel(wir_r, q_sut, &pq_sut, &qq_sut, &rq_sut);
    u2_bi_trel(wir_r, q_ref, &pq_ref, &qq_ref, &rq_ref);

    u2_bi_cell(wir_r, rq_sut, &prq_sut, &qrq_sut);
    u2_bi_cell(wir_r, rq_ref, &prq_ref, &qrq_ref);

    if ( u2_yes == u2_sing(q_sut, q_ref) ) {
      return _nest_dext(wir_r, van, p_sut, tel, p_ref, gil);
    }
    else if ( (u2_no == _nest_dext(wir_r, van, qq_sut, tel, p_sut, gil)) ||
              (u2_no == _nest_dext(wir_r, van, p_sut, tel, qq_sut, gil)) ||
              (u2_no == _nest_dext(wir_r, van, qq_ref, tel, p_ref, gil)) )
    {
      return u2_no;
    }
    else {
      if ( (pq_sut != pq_ref) && (c3__gold != pq_ref) ) {
        return u2_no;
      }
      else {
        u2_noun hud = u2_bc(wir_r, u2_rx(wir_r, sut), u2_rx(wir_r, ref));

        if ( (u2_yes == j2_mcc(Pt4, in, has)(wir_r, gil, hud)) ) {
          u2_rl_lose(wir_r, hud);

          return u2_yes;
        } 
        else {
          u2_noun zoc = j2_mcc(Pt4, in, put)(wir_r, gil, hud);
          u2_noun tus = u2_bt(wir_r, c3__core, 
                                     u2_rx(wir_r, qq_sut), 
                                     u2_rx(wir_r, q_sut));
          u2_noun fer = u2_bt(wir_r, c3__core, 
                                     u2_rx(wir_r, qq_ref), 
                                     u2_rx(wir_r, q_ref));

          ret = _nest_cram(wir_r, van, tus, tel, fer, qrq_sut, qrq_ref, zoc);
          u2_rz(wir_r, fer);
          u2_rz(wir_r, tus);
          u2_rz(wir_r, zoc);
          u2_rz(wir_r, hud);

          if ( u2_no == ret ) {
            return u2_no;
          }
          else {
            switch ( pq_sut ) {
              default: return u2_bl_bail(wir_r, c3__fail);

              case c3__gold: {
                return 
                  u2_and(_nest_dext(wir_r, van, qq_sut, tel, qq_ref, gil),
                         _nest_dext(wir_r, van, qq_ref, tel, qq_sut, gil));
              }
              case c3__iron: {
                u2_noun s_sam = j2_mcy(Pt6, ut, peek)
                                        (wir_r, van, qq_sut, c3__rite, _2);
                u2_noun r_sam = j2_mcy(Pt6, ut, peek)
                                        (wir_r, van, qq_ref, c3__rite, _2);
                u2_bean ret = _nest_dext(wir_r, van, r_sam, tel, s_sam, gil);

                u2_rz(wir_r, r_sam);
                u2_rz(wir_r, s_sam);
                return ret;
              }
              case c3__lead: {
                return u2_yes;
              }
              case c3__zinc: {
                u2_noun s_pal = j2_mcy(Pt6, ut, peek)
                                        (wir_r, van, qq_sut, c3__read, _2);
                u2_noun r_pal = j2_mcy(Pt6, ut, peek)
                                        (wir_r, van, qq_ref, c3__read, _2);
                u2_bean ret = _nest_dext(wir_r, van, s_pal, tel, r_pal, gil);

                u2_rz(wir_r, r_pal);
                u2_rz(wir_r, s_pal);

                return ret;
              }
            }
          }
        }
      }
    }
  }

  static u2_bean
  _nest_dext_in(u2_wire wir_r,
                u2_noun van,
                u2_noun sut,
                u2_bean tel,
                u2_noun ref,
                u2_noun gil)
  {
    u2_noun p_sut, q_sut, r_sut, p_ref, q_ref, r_ref;

    if ( (u2_no == u2_dust(sut)) ) switch ( sut ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__noun: {
        return u2_yes;
      }
      case c3__void: {
        return _nest_sint(wir_r, van, sut, tel, ref, gil);
      }
    }
    else switch ( u2_h(sut) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__atom: {
        if ( (u2_yes == u2_dust(ref)) && (c3__atom == u2_h(ref)) ) {
          if ( u2_no == j2_mby(Pt6, fitz)(wir_r, u2_t(sut), u2_t(ref)) ) {
            // u2_err(wir_r, "fitz: need", u2_t(sut));
            // u2_err(wir_r, "fitz: have", u2_t(ref));
            return u2_no;
          }
          return u2_yes;
        }
        else return _nest_sint(wir_r, van, sut, tel, ref, gil);
      }
      case c3__cell: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          if ( u2_yes == u2_as_pq(ref, c3__cell, &p_ref, &q_ref) ) {
            return u2_and(_nest_dext(wir_r, van, p_sut, tel, p_ref, gil),
                          _nest_dext(wir_r, van, q_sut, tel, q_ref, gil));
          }
          else return _nest_sint(wir_r, van, sut, tel, ref, gil);
        }
      }
      case c3__core: {
        if ( u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          if ( (u2_yes == u2_as_pq(ref, c3__core, &p_ref, &q_ref)) ) {
            return _nest_cong(wir_r, van, sut, tel, ref, gil);
          }
          else return _nest_sint(wir_r, van, sut, tel, ref, gil);
        }
      }
      case c3__cube: {
        if ( u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          if ( u2_yes == u2_as_pq(ref, c3__cube, &p_ref, &q_ref) ) {
            return u2_sing(p_sut, p_ref);
          }
          else return _nest_sint(wir_r, van, sut, tel, ref, gil);
        }
      }
      case c3__face: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else { 
          return _nest_dext(wir_r, van, q_sut, tel, ref, gil);
        }
      }
      case c3__fine: {
        if ( (u2_no == u2_as_qual(sut, 0, &p_sut, &q_sut, &r_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else { 
          if ( (u2_yes == u2_as_pqr(ref, c3__fine, &p_ref, &q_ref, &r_ref)) ) {
            return u2_and
              (u2_sing(p_sut, p_ref),
               u2_and(u2_sing(q_sut, q_ref),
                      _nest_dext(wir_r, van, r_sut, tel, r_ref, gil)));
          }
          else return _nest_sint(wir_r, van, sut, tel, ref, gil);
        }
      }
      case c3__fork: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        else {
          if ( u2_no == u2_dust(ref) ) switch ( ref ) {
            default: return _nest_sint(wir_r, van, sut, tel, ref, gil);

            case c3__noun:
              break;
          }
          else switch ( u2_h(ref) ) {
            default: return _nest_sint(wir_r, van, sut, tel, ref, gil);
            
            case c3__atom:
            case c3__cell: 
            case c3__cube:
            case c3__core:
            case c3__fine:
              break;
          }

          return u2_or(_nest_dext(wir_r, van, p_sut, u2_no, ref, gil),
                       _nest_dext(wir_r, van, q_sut, u2_no, ref, gil));
        }
      } 
      case c3__hold: p_sut = u2_t(sut);
      {
        u2_noun hud = u2_bc(wir_r, u2_rx(wir_r, sut), 
                                   u2_rx(wir_r, ref));

        if ( (u2_yes == j2_mcc(Pt4, in, has)(wir_r, gil, hud)) ) {
          u2_rl_lose(wir_r, hud);

          return u2_yes;
        } else {
          u2_noun zoc = j2_mcc(Pt4, in, put)(wir_r, gil, hud);
          u2_type fop = j2_mcy(Pt6, ut, rest)(wir_r, van, sut, p_sut);
          u2_bean hiv = _nest_dext(wir_r, van, fop, tel, ref, zoc);

          u2_rl_lose(wir_r, hud);
          u2_rl_lose(wir_r, fop);
          u2_rl_lose(wir_r, zoc);

          return hiv;
        }
      }
    }
  }

  static u2_bean
  _nest_dext_to(u2_wire wir_r,
                u2_noun van,
                u2_noun sut,
                u2_bean tel,
                u2_noun ref,
                u2_noun gil)
  {
    u2_bean tyn = _nest_dext_in(wir_r, van, sut, tel, ref, gil);

    if ( (u2_yes == tyn) || (u2_no == tel) ) {
      return tyn;
    } else {
      // u2_noun dun = j2_mcy(Pt6, ut, dunq)(wir_r, van, "need", sut);
      // u2_noun niz = j2_mcy(Pt6, ut, dunq)(wir_r, van, "have", ref);

      // u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dun));
      // u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, niz));

      return u2_bl_error(wir_r, "type-fail");
    }
  }

  static u2_bean
  _nest_dext(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_bean tel,
             u2_noun ref,
             u2_noun gil)
  {
    if ( (u2_yes == u2_sing(sut, ref)) ) {
      return u2_yes;
    }

    {
      u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, nest)[0];

      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return _nest_dext_to(wir_r, van, sut, tel, ref, gil);
      } else {
        c3_m    fun_m = c3__nest;
        u2_noun pro   = u2_rl_find_cell(wir_r, fun_m, sut, ref);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = _nest_dext_to(wir_r, van, sut, tel, ref, gil);

          return u2_rl_save_cell(wir_r, fun_m, sut, ref, pro);
        }
      }
    }
  }

  static u2_bean
  _nest_sint(u2_wire wir_r,
             u2_noun van,
             u2_noun sut, 
             u2_bean tel,
             u2_noun ref,
             u2_noun gil)
  {
    u2_noun p_ref, q_ref, r_ref;

    if ( (u2_no == u2_dust(ref)) ) {
      switch ( ref ) {
        default: return u2_bl_bail(wir_r, c3__fail);

        case c3__noun: return u2_no;
        case c3__void: return u2_yes;
      }
    }
    else {
      switch ( u2_h(ref) ) {
        default: {
          return u2_bl_bail(wir_r, c3__fail);
        }
        case c3__atom: return u2_no;
        case c3__cell: return u2_no;
        case c3__core: {
          u2_type gam = j2_mcy(Pt6, ut, repo)(wir_r, van, ref);
          u2_bean hiv = _nest_dext(wir_r, van, sut, tel, gam, gil);

          u2_rl_lose(wir_r, gam);
          return hiv;
        }
        case c3__cube: {
          if ( u2_no == u2_as_trel(ref, 0, &p_ref, &q_ref) ) {
            return u2_bl_bail(wir_r, c3__fail);
          } else {
            return _nest_dext(wir_r, van, sut, tel, q_ref, gil);
          }
        }
        case c3__face: {
          if ( u2_no == u2_as_trel(ref, 0, &p_ref, &q_ref) ) {
            return u2_bl_bail(wir_r, c3__fail);
          } else {
            return _nest_dext(wir_r, van, sut, tel, q_ref, gil);
          }
        }
        case c3__fine: {
          if ( u2_no == u2_as_qual(ref, 0, &p_ref, &q_ref, &r_ref) ) {
            return u2_bl_bail(wir_r, c3__fail);
          } else {
            return _nest_dext(wir_r, van, sut, tel, r_ref, gil);
          }
        }
        case c3__fork: {
          if ( (u2_yes == u2_mean(ref, 6, &p_ref, 7, &q_ref, 0)) ) {
            return u2_and(_nest_dext(wir_r, van, sut, u2_no, p_ref, gil),
                          _nest_dext(wir_r, van, sut, u2_no, q_ref, gil));
          }
          else return u2_bl_bail(wir_r, c3__fail);
        } 
        case c3__hold: {
          p_ref = u2_t(ref);
          {
            u2_noun hud = u2_bc(wir_r, u2_rx(wir_r, sut), 
                                       u2_rx(wir_r, ref));

            if ( (u2_yes == j2_mcc(Pt4, in, has)(wir_r, gil, hud)) ) {
              u2_rl_lose(wir_r, hud);

              return u2_yes;
            } else {
              u2_noun zoc = j2_mcc(Pt4, in, put)(wir_r, gil, hud);
              u2_type gam = j2_mcy(Pt6, ut, repo)(wir_r, van, ref);

              {
                u2_bean hiv = _nest_dext(wir_r, van, sut, tel, gam, zoc);

                u2_rl_lose(wir_r, hud);
                u2_rl_lose(wir_r, gam);
                u2_rl_lose(wir_r, zoc);

                return hiv;
              }
            }
          }
        } 
      }
    }
  }
  
  u2_bean                                                         //  transfer
  j2_mcx(Pt6, ut, nest)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_bean tel,                              //  retain
                        u2_noun ref)                              //  retain
  {
    return _nest_dext(wir_r, van, sut, tel, ref, u2_nul);
  }

/* boilerplate
*/
  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, nest)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, tel, ref, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &tel, 
                                u2_cv_sam_3, &ref, 
                                u2_cv_con, &van, 
                                0)) ||
         (u2_no == u2_stud(tel)) || (tel > 1) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, nest)(wir_r, van, sut, tel, ref);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, nest)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_bean tel,                              //  retain
                        u2_noun ref)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "nest");

    if ( u2_none == hoc ) {
      c3_assert(!"register nest");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam_2, tel,
                                           u2_cv_sam_3, u2_rx(wir_r, ref), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, nest)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, nest)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, nest)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_bean tel,                              //  retain
                        u2_noun ref)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, nest)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      return j2_mcx(Pt6, ut, nest)(wir_r, van, sut, tel, ref);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, nest)(wir_r, van, sut, tel, ref);
      fol = u2_h(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, nest)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, ref, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_3, &ref, u2_cv_con, &van, &ref, 0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rc(wir_r, u2_rx(wir_r, sut), u2_rx(wir_r, ref));
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, nest)[] = {
    { ".2", c3__hevy, 
        j2_mc(Pt6, ut, nest), 
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, nest), c3__nest,
    },
    { }
  };
