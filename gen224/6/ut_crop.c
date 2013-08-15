/* j/6/crop.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun
  _crop_dext(u2_wire, u2_noun, u2_noun, u2_noun, u2_noun);
  static u2_noun
  _crop_sint(u2_wire, u2_noun, u2_noun, u2_noun, u2_noun);

  static u2_noun
  _crop_repo(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_noun ref,
             u2_noun bix)
  {
    u2_noun rep = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
    u2_noun ret = _crop_dext(wir_r, van, rep, ref, bix);

    if ( u2_yes == u2_sing(ret, rep) ) {
      if ( c3__void == rep ) {
        return c3__void;
      } else {
        u2_rz(wir_r, rep);
        u2_rz(wir_r, ret);
        return u2_rx(wir_r, sut);
      }
    } else {
      u2_rz(wir_r, rep);
      return ret;
    }
  }

  static u2_noun                                                  //  produce 
  _crop_dext(u2_wire wir_r,
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun ref,                                         //  retain
             u2_noun bix)                                         //  retain
  {
    u2_noun p_sut, q_sut, r_sut, p_ref, q_ref;

    if ( u2_no == u2_dust(ref) ) {
      switch ( ref ) {
        case c3__void: return u2_rx(wir_r, sut);
        case c3__noun: return c3__void;
        default: return u2_bl_bail(wir_r, c3__fail);
      }
    }
    if ( u2_yes == u2_sing(sut, ref) ) {
      return c3__void;
    }
    if ( u2_no == u2_dust(sut) ) switch ( sut ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__noun: return _crop_repo(wir_r, van, sut, ref, bix);
      case c3__void: return c3__void;
    }
    else switch ( u2_h(sut) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__atom:
      {
        if ( c3__atom == u2_h(ref) ) {
          return c3__void;
        }
        else if ( c3__cell == u2_h(ref) ) {
          return u2_rx(wir_r, sut);
        }
        else return _crop_sint(wir_r, van, sut, ref, bix);
      }
      case c3__cell: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        if ( c3__atom == u2_h(ref) ) {
          return u2_rx(wir_r, sut);
        }
        else if ( c3__cell == u2_h(ref) ) {
          u2_bi_cell(wir_r, u2_t(ref), &p_ref, &q_ref);

          if ( u2_yes == j2_mcy(Pt6, ut, nest)
                              (wir_r, van, p_ref, u2_no, p_sut) ) 
          {
            u2_noun foz = _crop_dext(wir_r, van, q_sut, q_ref, bix);
            u2_noun ret = j2_mby(Pt6, cell)(wir_r, p_sut, foz);

            u2_rz(wir_r, foz);
            return ret;
          }
          else return u2_rx(wir_r, sut);
        }
        else return _crop_sint(wir_r, van, sut, ref, bix);
      }
      case c3__core: 
      {
        if ( (c3__atom == u2_h(ref)) || (c3__cell == u2_h(ref)) ) {
          return u2_rx(wir_r, sut);
        }
        else return _crop_sint(wir_r, van, sut, ref, bix);
      }
      case c3__cube: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        if ( (c3__cube == u2_h(ref)) && 
             (u2_yes == u2_sing(p_sut, u2_h(u2_t(ref)))) )
        {
          return c3__void;
        } 
        else if ( (c3__atom == u2_h(ref)) || (c3__cell == u2_h(ref)) ) {
          u2_noun foz = _crop_dext(wir_r, van, q_sut, ref, bix);
          u2_noun ret;

          if ( u2_yes == j2_mcy(Pt6, ut, firm)(wir_r, van, foz, p_sut) ) {
            ret = j2_mby(Pt6, cube)(wir_r, p_sut, foz);
          }
          else ret = c3__void;

          u2_rz(wir_r, foz);
          return ret;
        }
        else return _crop_sint(wir_r, van, sut, ref, bix);
      }
      case c3__face: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        u2_noun foz = _crop_dext(wir_r, van, q_sut, ref, bix);
        u2_noun ret = j2_mby(Pt6, face)(wir_r, p_sut, foz);

        u2_rz(wir_r, foz);
        return ret;
      }
      case c3__fine: u2_bi_trel(wir_r, u2_t(sut), &p_sut, &q_sut, &r_sut);
      {
        u2_noun foz = _crop_dext(wir_r, van, r_sut, ref, bix);
        u2_noun ret = j2_mby(Pt6, fine)(wir_r, p_sut, q_sut, foz);

        u2_rz(wir_r, foz);
        return ret;
      }
      case c3__fork: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        u2_noun dis = _crop_dext(wir_r, van, p_sut, ref, bix);
        u2_noun dat = _crop_dext(wir_r, van, q_sut, ref, bix);
        u2_noun ret = j2_mby(Pt6, fork)(wir_r, dis, dat);

        u2_rz(wir_r, dis);
        u2_rz(wir_r, dat);
        return ret;
      }
      case c3__hold: p_sut = u2_t(sut);
      {
        u2_noun hud = u2_bc(wir_r, u2_rx(wir_r, sut), u2_rx(wir_r, ref));

        if ( u2_yes == j2_mcc(Pt4, in, has)(wir_r, bix, hud) ) {
          u2_noun dun = j2_mcy(Pt6, ut, dunq)(wir_r, van, "type", sut);
          u2_noun niz = j2_mcy(Pt6, ut, dunq)(wir_r, van, "over", ref);

          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dun));
          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, niz));

          return u2_bl_error(wir_r, "crop-loop");
        } else {
          u2_noun bux = j2_mcc(Pt4, in, put)(wir_r, bix, hud);
          u2_noun ret = _crop_repo(wir_r, van, sut, ref, bux);

          u2_rz(wir_r, hud);
          u2_rz(wir_r, bux);
          return ret;
        }
      }
    }
  }

  static u2_noun                                                  //  produce
  _crop_sint(u2_wire wir_r,
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun ref,                                         //  retain
             u2_noun bix)                                         //  retain
  {
    u2_noun p_ref, q_ref;

    switch ( u2_h(ref) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__core:
      case c3__cube: 
      case c3__fine: {
        return u2_rx(wir_r, sut);
      }
      case c3__face: u2_bi_cell(wir_r, u2_t(ref), &p_ref, &q_ref);
      {
        return _crop_dext(wir_r, van, sut, q_ref, bix);
      }
      case c3__fork: u2_bi_cell(wir_r, u2_t(ref), &p_ref, &q_ref);
      {
        u2_noun hin = _crop_dext(wir_r, van, sut, p_ref, bix);
        u2_noun ret = _crop_dext(wir_r, van, hin, q_ref, bix);

        u2_rz(wir_r, hin);
        return ret;
      }
      case c3__hold: p_ref = u2_t(ref);
      {
        u2_noun rep = j2_mcy(Pt6, ut, repo)(wir_r, van, ref);
        u2_noun ret = _crop_dext(wir_r, van, sut, rep, bix);

        u2_rz(wir_r, rep);
        return ret;
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, crop)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun ref)                              //  retain
  {
    return _crop_dext(wir_r, van, sut, ref, u2_nul);
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, crop)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, crop)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, ref, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &ref, 0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, crop)(wir_r, van, sut, ref);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, crop)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun ref)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "crop");

    if ( u2_none == hoc ) {
      c3_assert(!"register crop");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cw_sam, u2_rx(wir_r, ref), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, crop)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, crop)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, crop)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun ref)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, crop)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, crop)(wir_r, van, sut, ref);
      }
      else {
        c3_m    fun_m = c3__crop;
        u2_noun pro   = u2_rl_find_cell(wir_r, fun_m, sut, ref);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, crop)(wir_r, van, sut, ref);

          return u2_rl_save_cell(wir_r, fun_m, sut, ref, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, crop)(wir_r, van, sut, ref);
      fol = u2_t(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, crop)(u2_wire wir_r,
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
  j2_mcj(Pt6, ut, crop)[] = {
    { ".3", c3__hevy, 
        j2_mc(Pt6, ut, crop), 
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, crop), c3__crop
    },
    { }
  };
