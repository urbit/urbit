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
    static u2_flag
    _nest_sint(u2_wire, u2_noun, u2_noun, u2_noun, u2_noun);
    static u2_flag
    _nest_dext(u2_wire, u2_noun, u2_noun, u2_noun, u2_noun);

    u2_ho_jet 
    j2_mcj(Pt6, ut, nest)[];

  static u2_flag
  _nest_cram(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
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

      if ( (u2_no == _nest_cram(wir_r, van, sut, ref, l_dab, l_hem, gil)) ||
           (u2_no == _nest_cram(wir_r, van, sut, ref, r_dab, r_hem, gil)) ) {
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
        else {
          if ( u2_h(qn_dab) != u2_h(qn_hem) ) {
            return u2_no;
          } else if ( u2_yes == u2_h(qn_dab) ) {
            u2_noun pqn_dab = u2_t(qn_dab);
            u2_noun pqn_hem = u2_t(qn_hem);
            u2_noun qpqn_dab = u2_t(pqn_dab);
            u2_noun qpqn_hem = u2_t(pqn_hem);
            u2_noun vis = j2_mcy(Pt6, ut, play)(wir_r, van, sut, qpqn_dab);
            u2_noun lon = j2_mcy(Pt6, ut, play)(wir_r, van, ref, qpqn_hem);
            u2_flag ret = _nest_dext(wir_r, van, vis, lon, gil);

            u2_rz(wir_r, vis);
            u2_rz(wir_r, lon);
            return ret;
          } else {
            if ( u2_nul == u2_t(qn_dab) ) {
              return u2_yes;
            } else {
              return _nest_cram
                (wir_r, van, sut, ref, u2_t(qn_dab), u2_t(qn_hem), gil);
            }
          }
        }
      }
    }
  }
 
  static u2_flag
  _nest_cong(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_noun ref,
             u2_noun gil)
  {
    u2_noun p_sut, q_sut, p_ref, q_ref;
    u2_noun pq_sut, qq_sut, rq_sut;
    u2_noun pq_ref, qq_ref, rq_ref;
    u2_noun prq_sut, qrq_sut, prq_ref, qrq_ref;
    u2_flag ret;

    u2_bi_trel(wir_r, sut, 0, &p_sut, &q_sut);
    u2_bi_trel(wir_r, ref, 0, &p_ref, &q_ref);

    u2_bi_trel(wir_r, q_sut, &pq_sut, &qq_sut, &rq_sut);
    u2_bi_trel(wir_r, q_ref, &pq_ref, &qq_ref, &rq_ref);

    u2_bi_cell(wir_r, rq_sut, &prq_sut, &qrq_sut);
    u2_bi_cell(wir_r, rq_ref, &prq_ref, &qrq_ref);

    if ( u2_yes == u2_sing(q_sut, q_ref) ) {
      return _nest_dext(wir_r, van, p_sut, p_ref, gil);
    }
    else if ( (u2_no == _nest_dext(wir_r, van, qq_sut, p_sut, gil)) ||
              (u2_no == _nest_dext(wir_r, van, p_sut, qq_sut, gil)) ||
              (u2_no == _nest_dext(wir_r, van, qq_ref, p_ref, gil)) )
    {
      return u2_no;
    }
    else {
      if ( (pq_sut != pq_ref) && (c3__gold != pq_ref) ) {
        return u2_no;
      }
      else {
        u2_noun tus = u2_bt(wir_r, c3__core, 
                                   u2_rx(wir_r, qq_sut), 
                                   u2_rx(wir_r, q_sut));
        u2_noun fer = u2_bt(wir_r, c3__core, 
                                   u2_rx(wir_r, qq_ref), 
                                   u2_rx(wir_r, q_ref));

        ret = _nest_cram(wir_r, van, tus, fer, qrq_sut, qrq_ref, gil);
        u2_rz(wir_r, tus);
        u2_rz(wir_r, fer);

        if ( u2_no == ret ) {
          return u2_no;
        }
        else {
          switch ( pq_sut ) {
            default: return u2_bl_bail(wir_r, c3__fail);

            case c3__gold: {
              return u2_and(_nest_dext(wir_r, van, qq_sut, qq_ref, gil),
                            _nest_dext(wir_r, van, qq_ref, qq_sut, gil));
            }
            case c3__iron: {
              u2_noun s_sam = j2_mcy(Pt6, ut, peek)
                                      (wir_r, van, qq_sut, c3__rite, _3);
              u2_noun r_sam = j2_mcy(Pt6, ut, peek)
                                      (wir_r, van, qq_ref, c3__rite, _3);

              return _nest_dext(wir_r, van, r_sam, s_sam, gil);
            }
            case c3__lead: {
              return u2_yes;
            }
            case c3__zinc: {
              u2_noun s_pal = j2_mcy(Pt6, ut, peek)
                                      (wir_r, van, qq_sut, c3__read, _3);
              u2_noun r_pal = j2_mcy(Pt6, ut, peek)
                                      (wir_r, van, qq_ref, c3__read, _3);

              return _nest_dext(wir_r, van, s_pal, r_pal, gil);
            }
          }
        }
      }
    }
  }

  static u2_flag
  _nest_dext_in(u2_wire wir_r,
                u2_noun van,
                u2_noun sut,
                u2_noun ref,
                u2_noun gil)
  {
    u2_noun p_sut, q_sut, p_ref, q_ref;

    if ( (u2_no == u2_dust(sut)) ) switch ( sut ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__atom: {
        if ( c3__atom == ref ) {
          return u2_yes;
        } else if ( (u2_yes == u2_dust(ref)) && (c3__cube == u2_h(ref)) ) {
          return u2_stud(u2_t(ref));
        }
        else return _nest_sint(wir_r, van, sut, ref, gil);
      }
      case c3__noun: {
        return u2_yes;
      }
      case c3__void: {
        return _nest_sint(wir_r, van, sut, ref, gil);
      }
    }
    else switch ( u2_h(sut) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__cell: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          if ( u2_yes == u2_as_pq(ref, c3__cell, &p_ref, &q_ref) ) {
            return u2_and(_nest_dext(wir_r, van, p_sut, p_ref, gil),
                          _nest_dext(wir_r, van, q_sut, q_ref, gil));
          }
          else return _nest_sint(wir_r, van, sut, ref, gil);
        }
      }
      case c3__core: {
        if ( u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          if ( (u2_yes == u2_as_pq(ref, c3__core, &p_ref, &q_ref)) ) {
            return _nest_cong(wir_r, van, sut, ref, gil);
          }
          else return _nest_sint(wir_r, van, sut, ref, gil);
        }
      }
      case c3__cube: {
        p_sut = u2_t(sut);
        {
          if ( u2_yes == u2_as_p(ref, c3__cube, &p_ref) ) {
            return u2_sing(p_sut, p_ref);
          }
          else return _nest_sint(wir_r, van, sut, ref, gil);
       }
      }
      case c3__face: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else { 
          return _nest_dext(wir_r, van, q_sut, ref, gil);
        }
      }
      case c3__fork: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        else {
          if ( u2_no == u2_dust(ref) ) switch ( ref ) {
            default: return _nest_sint(wir_r, van, sut, ref, gil);

            case c3__atom:
            case c3__noun:
              break;
          }
          else switch ( u2_h(ref) ) {
            default: return _nest_sint(wir_r, van, sut, ref, gil);
            
            case c3__cell: 
            case c3__cube:
            case c3__core:
              break;
          }

          return u2_or(_nest_dext(wir_r, van, p_sut, ref, gil),
                       _nest_dext(wir_r, van, q_sut, ref, gil));
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
          u2_flag hiv = _nest_dext(wir_r, van, fop, ref, zoc);

          u2_rl_lose(wir_r, hud);
          u2_rl_lose(wir_r, fop);
          u2_rl_lose(wir_r, zoc);

          return hiv;
        }
      }
    }
  }

  static u2_flag
  _nest_dext(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_noun ref,
             u2_noun gil)
  {
    if ( (u2_yes == u2_sing(sut, ref)) ) {
      return u2_yes;
    }

    {
      u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, nest)[0];

      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return _nest_dext_in(wir_r, van, sut, ref, gil);
      } else {
        c3_m    fun_m = u2_jet_fun_m(jet_j);
        u2_noun pro   = u2_rl_find_cell(wir_r, fun_m, sut, ref);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = _nest_dext_in(wir_r, van, sut, ref, gil);

          return u2_rl_save_cell(wir_r, fun_m, sut, ref, pro);
        }
      }
    }
  }

  static u2_flag
  _nest_sint(u2_wire wir_r,
             u2_noun van,
             u2_noun sut, 
             u2_noun ref,
             u2_noun gil)
  {
    u2_noun p_ref, q_ref;

    if ( (u2_no == u2_dust(ref)) ) {
      switch ( ref ) {
        default: return u2_bl_bail(wir_r, c3__fail);

        case c3__atom: return u2_no;
        case c3__noun: return u2_no;
        case c3__void: return u2_yes;
      }
    }
    else {
      switch ( u2_h(ref) ) {
        default: {
          u2_err(wir_r, "sint: ref", ref);
          return u2_bl_bail(wir_r, c3__fail);
        }
        case c3__cell: {
          return u2_no;
        }
        case c3__core: {
          u2_type gam = j2_mcy(Pt6, ut, repo)(wir_r, van, ref);
          u2_flag hiv = _nest_dext(wir_r, van, sut, gam, gil);

          u2_rl_lose(wir_r, gam);
          return hiv;
        }
        case c3__cube: {
          p_ref = u2_t(ref);

          if ( u2_no == u2_dust(p_ref) ) {
            return u2_no;
          } else {
            u2_noun fug = u2_bt(wir_r, c3__cell, 
                                       u2_bc(wir_r, c3__cube, u2_h(p_ref)),
                                       u2_bc(wir_r, c3__cube, u2_t(p_ref)));
            u2_flag hiv = _nest_dext(wir_r, van, sut, fug, gil);
 
            u2_rl_lose(wir_r, fug);
            return hiv;
          }
        }
        case c3__face: {
          if ( u2_no == u2_as_trel(ref, 0, &p_ref, &q_ref) ) {
            return u2_bl_bail(wir_r, c3__fail);
          } else {
            return _nest_dext(wir_r, van, sut, q_ref, gil);
          }
        }
        case c3__fork: {
          if ( (u2_yes == u2_mean(ref, 6, &p_ref, 7, &q_ref, 0)) ) {
            return u2_and(_nest_dext(wir_r, van, sut, p_ref, gil),
                          _nest_dext(wir_r, van, sut, q_ref, gil));
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
                u2_flag hiv = _nest_dext(wir_r, van, sut, gam, zoc);

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
  
  u2_flag                                                         //  transfer
  j2_mcx(Pt6, ut, nest)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun ref)                              //  retain
  {
    return _nest_dext(wir_r, van, sut, ref, u2_nul);
  }

/* boilerplate
*/
  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, nest)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, ref, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &ref, 0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, nest)(wir_r, van, sut, ref);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, nest)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun ref)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "nest");

    if ( u2_none == hoc ) {
      c3_assert(!"register nest");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cw_sam, u2_rx(wir_r, ref), 0);

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
                        u2_noun ref)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, nest)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      return j2_mcx(Pt6, ut, nest)(wir_r, van, sut, ref);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, nest)(wir_r, van, sut, ref);
      fol = u2_t(cor);

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

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &ref, 0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
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
    { ".3", c3__hevy, 
        j2_mc(Pt6, ut, nest), 
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, nest)
    },
    { }
  };
