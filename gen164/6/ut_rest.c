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
                u2_noun van,
                u2_noun leg)
  {
    if ( u2_nul == leg ) {
      return u2_nul;
    } else {
      u2_noun i_leg = u2h(leg);
      u2_noun t_leg = u2t(leg);

      return u2nc
        (u2_cqfu_play(van, u2h(i_leg), u2t(i_leg)),
                _rest_in_list(van, t_leg));
    }
  }

  static u2_noun
  _rest_in_stil(
                u2_noun van,
                u2_noun gar)
  {
    u2_noun gun = u2_cqdi_gas(u2_nul, gar);
    u2_noun nog = u2_cqdi_tap(gun, u2_nul);

    u2z(gun);
    return nog;
  }

  static u2_noun
  _rest_in_fork(
                u2_noun nog,
                u2_noun fub)
  {
    if ( u2_no == u2du(nog) ) {
      return fub;
    } else {
      u2_noun buf = _rest_in_fork
        (
         u2t(nog),
         u2_cqf_fork(u2h(nog), fub));

        u2z(fub);
      return buf;
    }
  }

  static u2_noun
  _rest_in(
           u2_noun van,
           u2_noun leg)
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
                u2_noun fan,
                u2_noun leg)
  {
    if ( u2_nul == leg ) {
      return u2_no;
    } else {
      return u2_or(u2_cqdi_has(fan, u2h(leg)),
                   _rest_hit_fan(fan, u2t(leg)));
    }
  }

  u2_noun
  _cqfu_rest(
                        u2_noun van,
                        u2_noun sut,
                        u2_noun leg)
  {
    u2_noun fan = u2_cr_at(u2_cqfu_van_fan, van);

    if ( u2_yes == _rest_hit_fan(fan, leg) ) {
      return u2_cm_error("rest-loop");
    }
    else {
      u2_noun naf = u2_cqdi_gas(fan, leg);
      u2_noun nav = u2_ci_molt(u2k(van), u2_cqfu_van_fan, naf, 0);
      u2_noun mez = _rest_in(nav, leg);

      u2z(naf);
      u2z(nav);
      return mez;
    }
  }

/* boilerplate
*/
  u2_noun
  u2_cwfu_rest(
                       u2_noun cor)
  {
    u2_noun sut, leg, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &leg, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_rest(van, sut, leg);
    }
  }

  u2_noun
  u2_cqfu_rest(u2_noun van,
                        u2_noun sut,
                        u2_noun leg)
  {
    c3_m    fun_m = c3__rest;
    u2_noun pro   = u2_cz_find_2(fun_m, sut, leg);

    if ( u2_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_rest(van, sut, leg);

      return u2_cz_save_2(fun_m, sut, leg, pro);
    }
  }
