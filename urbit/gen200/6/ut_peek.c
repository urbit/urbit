/* j/6/peek.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun
  j2_mcx(Pt6, ut, peek)(u2_wire, u2_noun, u2_noun, u2_noun, u2_atom);

  static u2_noun                                                  //  produce
  _peek_in(u2_wire wir_r,
           u2_noun van,                                           //  retain
           u2_noun sut,                                           //  retain
           u2_noun way,                                           //  retain
           u2_atom axe,                                           //  retain
           u2_noun gil)                                           //  retain
  {
    u2_noun p_sut, q_sut, r_sut;
    u2_noun pro;

    if ( (u2_no == u2_dust(sut)) ) switch ( sut ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__noun: {
        return c3__noun;
      }
      case c3__void: {
        return c3__void;
      }
    }
    else switch ( u2_h(sut) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__atom: {
        return c3__void;
      }
      case c3__cell: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          u2_atom tip = j2_mbc(Pt3, cap)(wir_r, axe);
          u2_atom tap = j2_mbc(Pt3, mas)(wir_r, axe);

          if ( _2 == tip ) {
            pro = j2_mcx(Pt6, ut, peek)(wir_r, van, p_sut, way, tap);
          }
          else {
            pro = j2_mcx(Pt6, ut, peek)(wir_r, van, q_sut, way, tap);
          }
          u2_rl_lose(wir_r, tap); 
          u2_rl_lose(wir_r, tip);

          return pro;
        }
      }
      case c3__core: {
        u2_noun pq_sut, qq_sut, rq_sut;
        u2_noun prq_sut, qrq_sut;

        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ||
             (u2_no == u2_as_trel(q_sut, &pq_sut, &qq_sut, &rq_sut)) ||
             (u2_no == u2_as_cell(rq_sut, &prq_sut, &qrq_sut)) )
        {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          u2_atom tip = j2_mbc(Pt3, cap)(wir_r, axe);
          u2_atom tap = j2_mbc(Pt3, mas)(wir_r, axe);

          if ( _2 == tip ) {
            if ( u2_no == j2_mcy(Pt6, ut, park)(wir_r, van, sut, way, tap) ) 
            {
              u2_noun dun = j2_mcy(Pt6, ut, dunq)(wir_r, van, "type", sut);
              u2_noun waz = j2_mcy(Pt6, ut, shep)
                (wir_r, van, "axis", 'd', u2_rx(wir_r, axe));

              u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dun));
              u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, waz));

              return u2_bl_error(wir_r, "peek-park");
            }
            else pro = j2_mcx(Pt6, ut, peek)(wir_r, van, p_sut, way, tap);
          }
          else {
            pro = c3__noun;
          }
          u2_rl_lose(wir_r, tap); 
          u2_rl_lose(wir_r, tip);

          return pro;
        }
      }
      case c3__cube: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          return _peek_in(wir_r, van, q_sut, way, axe, gil);
        }
      }
      case c3__face: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          return _peek_in(wir_r, van, q_sut, way, axe, gil);
        }
      }
      case c3__fine: {
        if ( (u2_no == u2_as_qual(sut, 0, &p_sut, &q_sut, &r_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          return _peek_in(wir_r, van, r_sut, way, axe, gil);
        }
      }
      case c3__fork: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          u2_noun hed = _peek_in(wir_r, van, p_sut, way, axe, gil);
          u2_noun tal = _peek_in(wir_r, van, q_sut, way, axe, gil);

          pro = j2_mby(Pt6, fork)(wir_r, hed, tal);

          u2_rl_lose(wir_r, hed);
          u2_rl_lose(wir_r, tal);

          return pro;
        }
      } 
      case c3__hold: {
        p_sut = u2_t(sut);
        if ( (u2_yes == j2_mcc(Pt4, in, has)(wir_r, gil, sut)) ) {
          return c3__void;
        } 
        else {
          u2_noun zoc = j2_mcc(Pt4, in, put)(wir_r, gil, sut);
          u2_type fop = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
          u2_noun pro = _peek_in(wir_r, van, fop, way, axe, zoc);

          u2_rl_lose(wir_r, fop);
          u2_rl_lose(wir_r, zoc);

          return pro;
        }
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, peek)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,
                        u2_atom axe)                              //  retain
  {
    if ( _1 == axe ) {
      return u2_rx(wir_r, sut);
    }
    else return _peek_in(wir_r, van, sut, way, axe, u2_nul);
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, peek)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, peek)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, way, axe, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, 
                                u2_cw_sam_2, &way, 
                                u2_cw_sam_3, &axe,
                                0)) ||
         (u2_no == u2_stud(axe)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, peek)(wir_r, van, sut, way, axe);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, peek)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun way,                              //  retain
                        u2_noun axe)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "peek");

    if ( u2_none == hoc ) {
      c3_assert(!"register peek");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, 
                                      u2_cw_sam_2, u2_rx(wir_r, way), 
                                      u2_cw_sam_3, u2_rx(wir_r, axe), 
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, peek)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, peek)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, peek)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun axe)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, peek)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, peek)(wir_r, van, sut, way, axe);
      }
      else {
        c3_m    fun_m = c3__peek;
        u2_noun pro   = u2_rl_find_trel(wir_r, fun_m, sut, way, axe);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, peek)(wir_r, van, sut, way, axe);

          return u2_rl_save_trel(wir_r, fun_m, sut, way, axe, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, peek)(wir_r, van, sut, way, axe);
      fol = u2_t(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, peek)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, way, axe, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, 
                                u2_cw_sam_2, &way, 
                                u2_cw_sam_3, &axe,
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rt
        (wir_r, u2_rx(wir_r, sut), u2_rx(wir_r, way), u2_rx(wir_r, axe));
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, peek)[] = {
    { ".3", c3__hevy, 
        j2_mc(Pt6, ut, peek), 
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, peek), c3__peek
    },
    { }
  };
