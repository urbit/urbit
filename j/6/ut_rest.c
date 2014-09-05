/* j/6/ut_rest.c
**
** This file is in the public domain.
*/
#include "all.h"


/* logic
*/
  static u3_noun
  _rest_in_list(
                u3_noun van,
                u3_noun leg)
  {
    if ( u3_nul == leg ) {
      return u3_nul;
    } else {
      u3_noun i_leg = u3h(leg);
      u3_noun t_leg = u3t(leg);

      return u3nc
        (u3_cqfu_play(van, u3h(i_leg), u3t(i_leg)),
                _rest_in_list(van, t_leg));
    }
  }

  static u3_noun
  _rest_in_stil(
                u3_noun van,
                u3_noun gar)
  {
    u3_noun gun = u3_cqdi_gas(u3_nul, gar);
    u3_noun nog = u3_cqdi_tap(gun, u3_nul);

    u3z(gun);
    return nog;
  }

  static u3_noun
  _rest_in_fork(
                u3_noun nog,
                u3_noun fub)
  {
    if ( u3_no == u3du(nog) ) {
      return fub;
    } else {
      u3_noun buf = _rest_in_fork
        (
         u3t(nog),
         u3_cqf_fork(u3h(nog), fub));

        u3z(fub);
      return buf;
    }
  }

  static u3_noun
  _rest_in(
           u3_noun van,
           u3_noun leg)
  {
    u3_noun gar = _rest_in_list(van, leg);
    u3_noun nog = _rest_in_stil(van, gar);
    u3_noun fub = _rest_in_fork(nog, c3__void);

    u3z(gar);
    u3z(nog);

    return fub;
  }

  static u3_bean
  _rest_hit_fan(
                u3_noun fan,
                u3_noun leg)
  {
    if ( u3_nul == leg ) {
      return u3_no;
    } else {
      return u3_or(u3_cqdi_has(fan, u3h(leg)),
                   _rest_hit_fan(fan, u3t(leg)));
    }
  }

  u3_noun
  _cqfu_rest(
                        u3_noun van,
                        u3_noun sut,
                        u3_noun leg)
  {
    u3_noun fan = u3_cr_at(u3_cqfu_van_fan, van);

    if ( u3_yes == _rest_hit_fan(fan, leg) ) {
      return u3_cm_error("rest-loop");
    }
    else {
      u3_noun naf = u3_cqdi_gas(fan, leg);
      u3_noun nav = u3_ci_molt(u3k(van), u3_cqfu_van_fan, naf, 0);
      u3_noun mez = _rest_in(nav, leg);

      u3z(naf);
      u3z(nav);
      return mez;
    }
  }

/* boilerplate
*/
  u3_noun
  u3_cwfu_rest(
                       u3_noun cor)
  {
    u3_noun sut, leg, van;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam, &leg, u3_cv_con, &van, 0)) ||
         (u3_none == (sut = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqfu_rest(van, sut, leg);
    }
  }

  u3_noun
  u3_cqfu_rest(u3_noun van,
                        u3_noun sut,
                        u3_noun leg)
  {
    c3_m    fun_m = c3__rest;
    u3_noun pro   = u3_cz_find_2(fun_m, sut, leg);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_rest(van, sut, leg);

      return u3_cz_save_2(fun_m, sut, leg, pro);
    }
  }
