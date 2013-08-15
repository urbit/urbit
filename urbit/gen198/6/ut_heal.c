/* j/6/heal.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"


/* internals
*/
  static u2_noun
  _heal_as(u2_wire, u2_noun, u2_noun, u2_noun, u2_atom, u2_noun);

  static u2_noun
  _heal_by(u2_wire wir_r,
           u2_noun van,
           u2_noun sut,
           u2_noun qog, 
           u2_noun ref)
  {
    u2_noun p_sut, q_sut;

    if ( u2_no == u2_dust(sut) ) {
      return u2_bl_error(wir_r, "heal-name");
    }
    else switch ( u2_h(sut) ) {
      default: return u2_bl_error(wir_r, "heal-name");

      case c3__core:
      {
        return u2_rx(wir_r, ref);
      }
      case c3__face: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        if ( u2_yes == u2_sing(p_sut, u2_t(qog)) ) {
          return j2_mby(Pt6, face)(wir_r, p_sut, ref);
        }
        else return u2_bl_error(wir_r, "heal-name");
      }
      case c3__fine: 
      case c3__hold: {
        u2_noun rep = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
        u2_noun ret = _heal_by(wir_r, van, rep, qog, ref);

        u2_rz(wir_r, rep);
        return ret;
      }
      case c3__fork: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        u2_noun dis = _heal_by(wir_r, van, p_sut, qog, ref);
        u2_noun dat = _heal_by(wir_r, van, q_sut, qog, ref);
        u2_noun ret = j2_mby(Pt6, fork)(wir_r, dis, dat);

        u2_rl_lose(wir_r, dis);
        u2_rl_lose(wir_r, dat);
        return ret;
      }
    }
  }

  static u2_noun
  _heal_to(u2_wire wir_r,
           u2_noun van,
           u2_noun sut,
           u2_noun gil,
           u2_noun qog,
           u2_noun ref,
           u2_atom now,
           u2_atom lat)
  {
    u2_noun p_sut, q_sut;

    if ( u2_no == u2_dust(sut) ) {
      goto repo;
    }
    else switch ( u2_h(sut) ) {
      default: goto repo;

      case c3__atom: return c3__void;
      case c3__cell: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        u2_noun ter, ret;

        if ( _2 == now ) {
          ter = _heal_as(wir_r, van, p_sut, qog, lat, ref);
          ret = j2_mby(Pt6, cell)(wir_r, ter, q_sut);
        } else {
          ter = _heal_as(wir_r, van, q_sut, qog, lat, ref);
          ret = j2_mby(Pt6, cell)(wir_r, p_sut, ter);
        }
        u2_rz(wir_r, ter);
        return ret;
      }

      case c3__core: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        if ( _3 != now ) {
          return u2_bl_error(wir_r, "heal-core");
        } else {
          u2_noun ter = _heal_as(wir_r, van, p_sut, qog, lat, ref);
          u2_noun ret = j2_mby(Pt6, core)(wir_r, ter, q_sut);

          u2_rz(wir_r, ter);
          return ret;
        }
      }

      case c3__fork: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        u2_noun dis = _heal_to(wir_r, van, p_sut, gil, qog, ref, now, lat);
        u2_noun dat = _heal_to(wir_r, van, q_sut, gil, qog, ref, now, lat);
        u2_noun ret = j2_mby(Pt6, fork)(wir_r, dis, dat);

        u2_rl_lose(wir_r, dis);
        u2_rl_lose(wir_r, dat);
        return ret;
      }

      case c3__hold: {
        if ( (u2_yes == j2_mcc(Pt4, in, has)(wir_r, gil, sut)) ) {
          return c3__void;
        } 
        else {
          u2_noun zoc = j2_mcc(Pt4, in, put)(wir_r, gil, sut);
          u2_type rep = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
          u2_noun ret = _heal_to(wir_r, van, rep, zoc, qog, ref, now, lat);

          u2_rl_lose(wir_r, rep);
          u2_rl_lose(wir_r, zoc);

          return ret;
        }
      }
    }

    repo: {
      u2_noun rep = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
      u2_noun ret = _heal_to(wir_r, van, gil, rep, qog, ref, now, lat);

      u2_rz(wir_r, rep);
      return ret;
    }
  }

  static u2_noun
  _heal_as(u2_wire wir_r,
           u2_noun van,
           u2_noun sut,
           u2_noun qog,
           u2_atom axe,
           u2_noun ref)
  {
    if ( _1 == axe ) {
      if ( u2_nul == qog ) {
        return u2_rx(wir_r, ref);
      } else return _heal_by(wir_r, van, sut, qog, ref);
    }
    else {
      u2_atom now = j2_mbc(Pt3, cap)(wir_r, axe);
      u2_atom lat = j2_mbc(Pt3, mas)(wir_r, axe);
      u2_noun ret = _heal_to(wir_r, van, sut, u2_nul, qog, ref, now, lat);

      u2_rz(wir_r, lat);
      return ret;
    }
  }
  
/* functions
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, heal)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun qog,                              //  retain
                        u2_noun axe,                              //  retain
                        u2_noun ref)                              //  retain
  {
    return _heal_as(wir_r, van, sut, qog, axe, ref);
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, heal)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, heal)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, qog, axe, ref;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, 
                                u2_cw_sam_2, &qog, 
                                u2_cw_sam_6, &axe,
                                u2_cw_sam_7, &ref,
                                0)) ||
         (u2_no == u2_stud(axe)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, heal)(wir_r, van, sut, qog, axe, ref);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, heal)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun qog,                              //  retain
                        u2_noun axe,                              //  retain
                        u2_noun ref)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "heal");

    if ( u2_none == hoc ) {
      c3_assert(!"register heal");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cw_sam_2, u2_rx(wir_r, qog), 
                                           u2_cw_sam_6, u2_rx(wir_r, axe),
                                           u2_cw_sam_7, u2_rx(wir_r, ref),
                                           0);

      if ( (u2_none == j2_mcj(Pt6, ut, heal)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, heal)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, heal)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun qog,                              //  retain
                        u2_noun axe,                              //  retain
                        u2_noun ref)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, heal)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, heal)(wir_r, van, sut, qog, axe, ref);
      }
      else {
        c3_m    fun_m = c3__heal;
        u2_noun pro   = u2_rl_find_qual(wir_r, fun_m, sut, qog, axe, ref);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, heal)(wir_r, van, sut, qog, axe, ref);

          return u2_rl_save_qual(wir_r, fun_m, sut, qog, axe, ref, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, heal)(wir_r, van, sut, qog, axe, ref);
      fol = u2_t(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, heal)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, qog, axe, ref, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, 
                                u2_cw_sam_2, &qog, 
                                u2_cw_sam_6, &axe,
                                u2_cw_sam_7, &ref,
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rq
        (wir_r, u2_rx(wir_r, qog),
                u2_rx(wir_r, sut), 
                u2_rx(wir_r, axe), 
                u2_rx(wir_r, ref));
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, heal)[] = {
    { ".3", c3__hevy, 
        j2_mc(Pt6, ut, heal), 
        Tier6_b,
        u2_none, u2_none,
        j2_mck(Pt6, ut, heal), c3__heal,
    },
    { }
  };
