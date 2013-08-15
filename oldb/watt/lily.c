/* watt/lily.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* cons:lily
*/
u4_tool
_lily_cons(u4_plow p,
           u4_tool vur,
           u4_tool sed)
{
  u4_lane lan = p->lan;
  u4_noun p_vur, p_sed;

  if ( u4_b_p(vur, u4_nock_bone, &p_vur) && 
       u4_b_p(sed, u4_nock_bone, &p_sed) ) {
    return u4_kt(lan, u4_nock_bone, p_vur, p_sed);
  }
  else if ( u4_b_p(vur, u4_nock_frag, &p_vur) && 
            u4_b_p(sed, u4_nock_frag, &p_sed) &&
            !u4_n_eq(u4_axis_1, p_vur) &&
            !u4_n_eq(p_vur, p_sed) &&
            u4_n_zero(u4_op_ord(p_vur, p_sed)) )
  {
    u4_atom fub = u4_op_div(lan, u4_math_2, p_vur);
    u4_atom nof = u4_op_div(lan, u4_math_2, p_sed);

    if ( u4_n_eq(fub, nof) ) {
      return u4_kc(lan, u4_nock_frag, fub);
    }
  }
  return u4_kc(lan, vur, sed);
}

/* comb:lily
*/
u4_tool
_lily_comb(u4_plow p,
           u4_tool mal,
           u4_tool buz)
{
  u4_lane lan = p->lan;
  u4_noun p_mal, q_mal, p_buz, q_buz, pp_buz, pq_buz;

  if ( u4_b_p(mal, u4_nock_frag, &p_mal) ) {
    if ( u4_b_p(buz, u4_nock_frag, &p_buz) ) {
      if ( u4_n_zero(p_buz) ) {
        return buz;
      }
      return u4_kc(lan, u4_nock_frag, u4_op_peg(lan, p_mal, p_buz));
    }
    else if ( u4_b_pq(buz, u4_nock_sail, &p_buz, &q_buz) &&
              u4_b_p(p_buz, u4_nock_frag, &pp_buz) &&
              u4_b_p(q_buz, u4_nock_frag, &pq_buz) )
    {
      return u4_kt
        (lan, u4_nock_sail, 
               u4_kc(lan, u4_nock_frag, u4_op_peg(lan, p_mal, pp_buz)),
               u4_kc(lan, u4_nock_frag, u4_op_peg(lan, p_mal, pq_buz)));
    }
  }
  else if ( u4_b_fork(mal, &p_mal, &q_mal) ) {
    if ( !u4_n_atom(q_mal) && 
         u4_n_eq(u4_noun_0, u4_ch(q_mal)) &&
         u4_n_eq(u4_noun_1, u4_ct(q_mal)) )
    {
      return u4_kt(lan, u4_nock_gant, p_mal, buz);
    }
  }
  else if ( u4_b_p(buz, u4_nock_frag, &p_buz) ) {
    if ( u4_n_eq(u4_axis_1, p_buz) ) {
      return mal;
    }
  }
  // return u4_kq(lan, u4_nock_sail, mal, u4_nock_bone, buz);
  return u4_kt(lan, u4_nock_flac, mal, buz);
}

/* flan:lily
*/
u4_tool
_lily_flan(u4_plow p,
           u4_tool zeg,
           u4_tool dac)
{
  u4_lane lan = p->lan;

  if ( u4_n_eq(u4_nock_bone, u4_ch(zeg)) ) {
    if ( u4_n_zero(u4_ct(zeg)) ) {
      return dac;
    }
    else return zeg;
  }
  else {
    if ( u4_n_eq(u4_nock_bone, u4_ch(dac)) ) {
      if ( u4_n_zero(u4_ct(dac)) ) {
        return zeg;
      }
      else return dac;
    }
    else {
      return u4_kq
        (lan, u4_nock_trol, zeg, dac, u4_kc(lan, u4_nock_bone, u4_no));
    }
  }
}

/* flor:lily
*/
u4_tool
_lily_flor(u4_plow p,
           u4_tool bos,
           u4_tool nif)
{
  u4_lane lan = p->lan;

  if ( u4_n_eq(u4_nock_bone, u4_ch(bos)) ) {
    if ( u4_n_zero(u4_ct(bos)) ) {
      return bos;
    }
    else return nif;
  }
  else {
    if ( u4_n_eq(u4_nock_bone, u4_ch(nif)) ) {
      if ( u4_n_zero(u4_ct(nif)) ) {
        return nif;
      }
      else return bos;
    }
    else {
      return u4_kq
        (lan, u4_nock_trol, bos, u4_kc(lan, u4_nock_bone, u4_yes), nif);
    }
  }
}

/* _lily_flop(): invert boolean.
*/
u4_tool
_lily_flop(u4_plow p,
           u4_tool zet)
{
  u4_lane lan = p->lan;

  if ( u4_n_eq(u4_nock_bone, u4_ch(zet)) ) {
    if ( u4_n_eq(u4_yes, u4_ct(zet)) ) {
      return u4_kc(lan, u4_nock_bone, u4_no);
    }
    else {
      u4_assert(u4_n_eq(u4_no, u4_ct(zet)));

      return u4_kc(lan, u4_nock_bone, u4_yes);
    }
  }
  else {
    return u4_kq
      (lan, u4_nock_trol, 
             zet, 
             u4_kc(lan, u4_nock_bone, u4_no), 
             u4_kc(lan, u4_nock_bone, u4_yes));
  }
}

/* hike:lily:plow
*/
  static u4_axis 
  _lily_hike_axis_l(u4_lane lan, 
                    u4_axis axis)
  {
    return u4_op_add(lan, axis, axis);
  }
  u4_axis 
  _lily_hike_axis_r(u4_lane lan, 
                    u4_axis axis)
  {
    return u4_op_inc(lan, _lily_hike_axis_l(lan, axis));
  }

  /* _lily_hike_belt_root(): convert (vix) to a log of root tools.
  */
  static u4_log
  _lily_hike_belt_root(u4_lane lan,
                       u4_list vix)
  {
    if ( u4_n_zero(vix) ) {
      return u4_nul;
    }
    else {
      u4_axis axis     = u4_ch(u4_ch(vix));
      u4_tool tool     = u4_ct(u4_ch(vix));
      u4_list log_tool = _lily_hike_belt_root(lan, u4_ct(vix));

      if ( u4_n_eq(u4_axis_1, axis) ) {
        return u4_kc(lan, tool, log_tool);
      }
      else return log_tool;
    }
  }

  /* _lily_hike_belt_l(): factor (vix) left.
  */
  static u4_log
  _lily_hike_belt_l(u4_lane lan,
              u4_list vix)
  {
    if ( u4_n_zero(vix) ) {
      return u4_nul;
    }
    else {
      u4_axis axis       = u4_ch(u4_ch(vix));
      u4_tool tool       = u4_ct(u4_ch(vix));
      u4_list belt_l = _lily_hike_belt_l(lan, u4_ct(vix));
      {
        if ( u4_n_eq(u4_axis_2, u4_op_tip(axis)) ) {
          u4_axis axis_tap = u4_op_tap(lan, axis);

          return u4_kc(lan, u4_kc(lan, axis_tap, tool), belt_l);
        }
        else return belt_l;
      }
    }
  }

  /* _lily_hike_belt_r(): factor (vix) right.
  */
  static u4_log
  _lily_hike_belt_r(u4_lane lan,
                    u4_list vix)
  {
    if ( u4_n_zero(vix) ) {
      return u4_nul;
    }
    else {
      u4_axis axis       = u4_ch(u4_ch(vix));
      u4_tool tool       = u4_ct(u4_ch(vix));
      u4_list belt_r = _lily_hike_belt_r(lan, u4_ct(vix));
      {
        if ( u4_n_eq(u4_axis_3, u4_op_tip(axis)) ) {
          u4_axis axis_tap = u4_op_tap(lan, axis);

          return u4_kc(lan, u4_kc(lan, axis_tap, tool), belt_r);
        }
        else return belt_r;
      }
    }
  }
u4_tool
_lily_hike(u4_plow p,
           u4_axis axe,
           u4_list vix)  /* (axis tool) */
{
  u4_lane lan = p->lan;

  if ( u4_n_zero(vix) ) {
    return u4_kc(lan, u4_nock_frag, axe);
  }
  else {
    u4_list log_tool    = _lily_hike_belt_root(lan, vix);
    u4_list belt_l  = _lily_hike_belt_l(lan, vix);
    u4_list belt_r  = _lily_hike_belt_r(lan, vix);

    if ( !u4_n_zero(log_tool) ) {
      return u4_ch(log_tool);
    }
    else {
      u4_tool tool_l, tool_r;
      
      tool_l = _lily_hike(p, _lily_hike_axis_l(lan, axe), belt_l);
      tool_r = _lily_hike(p, _lily_hike_axis_r(lan, axe), belt_r);

      return _lily_cons(p, tool_l, tool_r);
    }
  }
}
