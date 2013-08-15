/* j/6/ut_odor.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun                                                  //  produce
  _odor_fork(u2_wire wir_r,
             u2_noun duv,                                         //  retain
             u2_noun wil)                                         //  retain
  {
    if ( (u2_nul == duv) || (u2_nul == wil) ) {
      return u2_nul;
    }
    else {
      u2_noun dyv = j2_mbc(Pt3, end)(wir_r, _3, _1, duv);
      u2_noun wyl = j2_mbc(Pt3, end)(wir_r, _3, _1, wil);

      if ( dyv != wyl ) {
        return u2_nul;
      }
      else {
        u2_noun fuv = j2_mbc(Pt3, rsh)(wir_r, _3, _1, duv);
        u2_noun pil = j2_mbc(Pt3, rsh)(wir_r, _3, _1, wil);
        u2_noun wug = _odor_fork(wir_r, fuv, pil);
        u2_noun yac = j2_mbc(Pt3, lsh)(wir_r, _3, _1, wug);
        u2_noun ret = j2_mbc(Pt1, add)(wir_r, yac, dyv);

        u2_rz(wir_r, yac);
        u2_rz(wir_r, wug);
        u2_rz(wir_r, pil);
        u2_rz(wir_r, fuv);

        return ret;
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, odor)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut)                              //  retain
  {
    u2_noun p_sut, q_sut;

    if ( u2_no == u2_dust(sut) ) {
      return u2_bl_error(wir_r, "odor-less");
    }
    else switch ( u2_h(sut) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__atom: return u2_rx(wir_r, u2_t(sut));

      case c3__fork: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      {
        u2_noun duv = j2_mcx(Pt6, ut, odor)(wir_r, van, p_sut);
        u2_noun wil = j2_mcx(Pt6, ut, odor)(wir_r, van, q_sut);
        u2_noun ret = _odor_fork(wir_r, duv, wil);

        u2_rz(wir_r, wil);
        u2_rz(wir_r, duv);
        return ret;
      }
      case c3__cell: 
      case c3__core: return u2_bl_error(wir_r, "odor-less");

      case c3__cube: 
      case c3__face:
      case c3__fine:
      case c3__hold: {
        u2_noun hoy = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
        u2_noun ret = j2_mcx(Pt6, ut, odor)(wir_r, van, hoy);

        u2_rz(wir_r, hoy);
        return ret;
      }
    }
  }

  extern u2_ho_jet 
  j2_mbj(Pt6, ut)[];

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, odor)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mbj(Pt6, ut)[0];

    if ( !(jet_j->sat_s & u2_jet_memo) ) {
      return j2_mcx(Pt6, ut, odor)(wir_r, van, sut);
    }
    else {
      c3_m    fun_m = u2_jet_fun_m(jet_j);
      u2_noun pro   = u2_rl_find(wir_r, fun_m, sut);

      if ( u2_none != pro ) {
        return pro;
      }
      else {
        pro = j2_mcx(Pt6, ut, odor)(wir_r, van, sut);

        return u2_rl_save(wir_r, fun_m, sut, pro);
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, odor)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut;

    if ( u2_none == (sut = u2_frag(u2_cw_sam, cor)) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcy(Pt6, ut, odor)(wir_r, cor, sut);
    }
  }
