/* j/6/ut_rest.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun
  _rest_in_list(
                u2_noun van,                                      //  retain
                u2_noun leg)                                      //  retain
  {
    if ( u2_nul == leg ) {
      return u2_nul;
    } else {
      u2_noun i_leg = u2h(leg);
      u2_noun t_leg = u2t(leg);

      return u2nc
        (j2_mcy(Pt6, ut, play)(van, u2h(i_leg), u2t(i_leg)),
                _rest_in_list(van, t_leg));
    }
  }

  static u2_noun
  _rest_in_stil(
                u2_noun van,
                u2_noun gar)
  {
    u2_noun gun = j2_mcc(Pt4, in, gas)(u2_nul, gar);
    u2_noun nog = j2_mcc(Pt4, in, tap)(gun, u2_nul);

    u2z(gun);
    return nog;
  }

  static u2_noun                                                  //  produce
  _rest_in_fork(
                u2_noun nog,                                      //  retain
                u2_noun fub)                                      //  retain
  {
    if ( u2_no == u2du(nog) ) {
      return fub;
    } else {
      u2_noun buf = _rest_in_fork
        (
         u2t(nog),
         j2_mby(Pt6, fork)(u2h(nog), fub));

        u2z(fub);
      return buf;
    }
  }

  static u2_noun                                                  //  produce
  _rest_in(
           u2_noun van,                                           //  retain
           u2_noun leg)                                           //  retain
  {
    u2_noun gar = _rest_in_list(van, leg);
    u2_noun nog = _rest_in_stil(van, gar);
    u2_noun fub = _rest_in_fork(nog, c3__void);

    u2z(gar);
    u2z(nog);

    return fub;
  }

  static u2_bean
  _rest_hit_fan(
                u2_noun fan,                                      //  retain
                u2_noun leg)                                      //  retain
  {
    if ( u2_nul == leg ) {
      return u2_no;
    } else {
      return u2_or(j2_mcc(Pt4, in, has)(fan, u2h(leg)),
                   _rest_hit_fan(fan, u2t(leg)));
    }
  }

  u2_noun                                                         //  produce
  j2_mcx(Pt6, ut, rest)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun leg)                              //  retain
  {
    u2_noun fan = u2_cr_at(j2_ut_van_fan, van);

    if ( u2_yes == _rest_hit_fan(fan, leg) ) {
      return u2_cm_error("rest-loop");
    }
    else {
      u2_noun naf = j2_mcc(Pt4, in, gas)(fan, leg);
      u2_noun nav = u2_ci_molt(u2k(van), j2_ut_van_fan, naf, 0);
      u2_noun mez = _rest_in(nav, leg);

      u2z(naf);
      u2z(nav);
      return mez;
    }
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, rest)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, rest)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, leg, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &leg, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, rest)(van, sut, leg);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, rest)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun leg)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "rest");

    if ( u2_none == hoc ) {
      c3_assert(!"register rest");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat), u2_cv_sam, u2k(leg), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, rest)[0].xip) ) {
        u2_noun xip = u2_cj_find(u2k(cor));

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, rest)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, rest)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun leg)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, rest)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, rest)(van, sut, leg);
      }
      else {
        c3_m    fun_m = c3__rest;
        u2_noun pro   = u2_ch_find_2(fun_m, sut, leg);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, rest)(van, sut, leg);

          return u2_ch_save_2(fun_m, sut, leg, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, rest)(van, sut, leg);
      fol = u2h(cor);

      pro = u2_ho_use(jet_j, cor, fol);
      if ( u2_none == pro ) return u2_cm_bail(c3__fail);

      u2z(cor);
      u2z(fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, rest)(
                        u2_noun cor)
  {
    u2_noun sut, leg, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &leg, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nc(u2k(sut), u2k(leg));
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, rest)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, rest),
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, rest), c3__rest,
    },
    { }
  };
