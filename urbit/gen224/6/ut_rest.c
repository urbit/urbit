/* j/6/ut_rest.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun 
  _rest_in_list(u2_wire wir_r, 
                u2_noun van,                                      //  retain
                u2_noun leg)                                      //  retain
  {
    if ( u2_nul == leg ) {
      return u2_nul;
    } else {
      u2_noun i_leg = u2_h(leg);
      u2_noun t_leg = u2_t(leg);

      return u2_bc
        (wir_r, j2_mcy(Pt6, ut, play)(wir_r, van, u2_h(i_leg), u2_t(i_leg)),
                _rest_in_list(wir_r, van, t_leg));
    }
  }

  static u2_noun
  _rest_in_stil(u2_wire wir_r,
                u2_noun van,
                u2_noun gar)
  {
    u2_noun gun = j2_mcc(Pt4, in, gas)(wir_r, u2_nul, gar);
    u2_noun nog = j2_mcc(Pt4, in, tap)(wir_r, gun, u2_nul);

    u2_rl_lose(wir_r, gun);
    return nog;
  }

  static u2_noun                                                  //  produce
  _rest_in_fork(u2_wire wir_r, 
                u2_noun nog,                                      //  retain
                u2_noun fub)                                      //  retain
  {
    if ( u2_no == u2_dust(nog) ) {
      return fub;
    } else {
      u2_noun buf = _rest_in_fork
        (wir_r, 
         u2_t(nog),
         j2_mby(Pt6, fork)(wir_r, u2_h(nog), fub));

        u2_rz(wir_r, fub);
      return buf;
    }
  }

  static u2_noun                                                  //  produce
  _rest_in(u2_wire wir_r,
           u2_noun van,                                           //  retain
           u2_noun leg)                                           //  retain
  {
    u2_noun gar = _rest_in_list(wir_r, van, leg);
    u2_noun nog = _rest_in_stil(wir_r, van, gar);
    u2_noun fub = _rest_in_fork(wir_r, nog, c3__void);

    u2_rl_lose(wir_r, gar);
    u2_rl_lose(wir_r, nog);

    return fub;
  }

  static u2_flag
  _rest_hit_fan(u2_wire wir_r,
                u2_noun fan,                                      //  retain
                u2_noun leg)                                      //  retain
  {
    if ( u2_nul == leg ) {
      return u2_no;
    } else {
      return u2_or(j2_mcc(Pt4, in, has)(wir_r, fan, u2_h(leg)),
                   _rest_hit_fan(wir_r, fan, u2_t(leg)));
    }
  }

  u2_noun                                                         //  produce
  j2_mcx(Pt6, ut, rest)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun leg)                              //  retain
  {
    u2_noun fan = u2_frag(j2_ut_van_fan, van);

    if ( u2_yes == _rest_hit_fan(wir_r, fan, leg) ) {
      return u2_bl_error(wir_r, "rest-loop");
    }
    else {
      u2_noun naf = j2_mcc(Pt4, in, gas)(wir_r, fan, leg);
      u2_noun nav = u2_bn_molt(wir_r, van, j2_ut_van_fan, naf, 0);
      u2_noun mez = _rest_in(wir_r, nav, leg);

      u2_rz(wir_r, naf);
      u2_rz(wir_r, nav);
      return mez;
    }
  } 

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, rest)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, rest)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, leg, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &leg, 0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, rest)(wir_r, van, sut, leg);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, rest)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun leg)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "rest");

    if ( u2_none == hoc ) {
      c3_assert(!"register rest");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cw_sam, u2_rx(wir_r, leg), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, rest)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, rest)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, rest)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun leg)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, rest)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, rest)(wir_r, van, sut, leg);
      }
      else {
        c3_m    fun_m = c3__rest;
        u2_noun pro   = u2_rl_find_cell(wir_r, fun_m, sut, leg);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, rest)(wir_r, van, sut, leg);

          return u2_rl_save_cell(wir_r, fun_m, sut, leg, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, rest)(wir_r, van, sut, leg);
      fol = u2_t(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, rest)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, leg, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &leg, 0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rc(wir_r, u2_rx(wir_r, sut), u2_rx(wir_r, leg));
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, rest)[] = {
    { ".3", c3__hevy, 
        j2_mc(Pt6, ut, rest), 
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, rest), c3__rest
    },
    { }
  };
