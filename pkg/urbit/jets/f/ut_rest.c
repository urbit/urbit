/* j/6/ut_rest.c
**
*/
#include "all.h"


/* logic
*/
  static u3_noun
  _rest_in_list(u3_noun van,
                u3_noun leg)
  {
    if ( u3_nul == leg ) {
      return u3_nul;
    } else {
      u3_noun i_leg = u3h(leg);
      u3_noun t_leg = u3t(leg);

      return u3nc(u3qfu_play(van, u3h(i_leg), u3t(i_leg)),
                  _rest_in_list(van, t_leg));
    }
  }

  static u3_noun
  _rest_in_stil(u3_noun van,
                u3_noun gar)
  {
    u3_noun gun = u3qdi_gas(u3_nul, gar);
    u3_noun yed = u3qdi_tap(gun);

    u3z(gun);
    return yed;
  }

  static u3_noun
  _rest_in(u3_noun van,
           u3_noun leg)
  {
    u3_noun gar = _rest_in_list(van, leg);
    u3_noun yed = _rest_in_stil(van, gar);
    u3_noun fub = u3qf_fork(yed);

    u3z(gar);
    u3z(yed);

    return fub;
  }

  static u3_noun
  _rest_hit_fan(u3_noun fan,
                u3_noun leg)
  {
    if ( u3_nul == leg ) {
      return c3n;
    } else {
      return c3o(u3qdi_has(fan, u3h(leg)),
                   _rest_hit_fan(fan, u3t(leg)));
    }
  }

  static u3_noun
  _cqfu_rest(u3_noun van,
             u3_noun sut,
             u3_noun leg)
  {
    u3_noun fan = u3r_at(u3qfu_van_fan, van);

    if ( c3y == _rest_hit_fan(fan, leg) ) {
      return u3m_error("rest-loop");
    }
    else {
      u3_noun naf = u3qdi_gas(fan, leg);
      u3_noun nav = u3i_molt(u3k(van), u3qfu_van_fan, u3k(naf), 0);
      u3_noun mez = _rest_in(nav, leg);

      u3z(naf);
      u3z(nav);
      return mez;
    }
  }

/* boilerplate
*/
  u3_noun
  u3wfu_rest(u3_noun cor)
  {
    u3_noun sut, leg, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &leg, u3x_con, &van, 0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_rest(van, sut, leg);
    }
  }

  u3_noun
  u3qfu_rest(u3_noun van,
             u3_noun sut,
             u3_noun leg)
  {
    c3_m    fun_m = 141 + c3__rest + ((!!u3r_at(u3qfu_van_vet, van)) << 8);
    u3_noun pro   = u3z_find_2(fun_m, sut, leg);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_rest(van, sut, leg);

      return u3z_save_2(fun_m, sut, leg, pro);
    }
  }
