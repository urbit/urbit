/* mill/hike.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _axis_l(): (axis + axis).
*/
static u4_axis 
_axis_l(u4_lane lane, 
        u4_axis axis)
{
  return u4_op_add(lane, axis, axis);
}

/* _axis_r(): (axis + axis + 1).
*/
u4_axis 
_axis_r(u4_lane lane, 
        u4_axis axis)
{
  return u4_op_inc(lane, _axis_l(lane, axis));
}

/* _belt_root(): convert (vix) to a log of root nocks.
*/
static u4_log
_belt_root(u4_lane lane,
               u4_belt vix)
{
  if ( u4_n_zero(vix) ) {
    return u4_noun_0;
  }
  else {
    u4_axis axis     = u4_ch(u4_ch(vix));
    u4_nock nock     = u4_ct(u4_ch(vix));
    u4_belt log_nock = _belt_root(lane, u4_ct(vix));

    if ( u4_n_eq(u4_noun_1, axis) ) {
      return u4_k_cell(lane, nock, log_nock);
    }
    else return log_nock;
  }
}

/* _belt_l(): factor (vix) left.
*/
static u4_log
_belt_l(u4_lane lane,
            u4_belt vix)
{
  if ( u4_n_zero(vix) ) {
    return u4_noun_0;
  }
  else {
    u4_axis axis       = u4_ch(u4_ch(vix));
    u4_nock nock       = u4_ct(u4_ch(vix));
    u4_belt belt_l = _belt_l(lane, u4_ct(vix));
    {
      if ( u4_n_eq(u4_noun_2, u4_op_tip(axis)) ) {
        u4_axis axis_tap = u4_op_tap(lane, axis);

        return u4_k_cell(lane, u4_k_cell(lane, axis_tap, nock), belt_l);
      }
      else return belt_l;
    }
  }
}

/* _belt_r(): factor (vix) right.
*/
static u4_log
_belt_r(u4_lane lane,
            u4_belt vix)
{
  if ( u4_n_zero(vix) ) {
    return u4_noun_0;
  }
  else {
    u4_axis axis       = u4_ch(u4_ch(vix));
    u4_nock nock       = u4_ct(u4_ch(vix));
    u4_belt belt_r = _belt_r(lane, u4_ct(vix));
    {
      if ( u4_n_eq(u4_noun_3, u4_op_tip(axis)) ) {
        u4_axis axis_tap = u4_op_tap(lane, axis);

        return u4_k_cell(lane, u4_k_cell(lane, axis_tap, nock), belt_r);
      }
      else return belt_r;
    }
  }
}

/* _mill_hike(): mutate nock.
*/
u4_nock
_mill_hike(u4_milr m,
           u4_axis axe,
           u4_belt vix)  /* (axis nock) */
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(vix) ) {
    return u4_k_cell(lane, u4_noun_0, axe);
  }
  else {
    u4_belt log_nock    = _belt_root(lane, vix);
    u4_belt belt_l  = _belt_l(lane, vix);
    u4_belt belt_r  = _belt_r(lane, vix);

    if ( !u4_n_zero(log_nock) ) {
      return u4_ch(log_nock);
    }
    else {
      u4_nock nock_l, nock_r;
      
      nock_l = _mill_hike(m, _axis_l(lane, axe), belt_l);
      nock_r = _mill_hike(m, _axis_r(lane, axe), belt_r);

      return _mill_cons(m, nock_l, nock_r);
    }
  }
}
