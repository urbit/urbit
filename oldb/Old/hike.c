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

/* _log_pike_root(): convert (log_pike) to a log of root forms.
*/
static u4_log
_log_pike_root(u4_lane lane,
               u4_log  log_pike)
{
  if ( u4_n_zero(log_pike) ) {
    return u4_noun_0;
  }
  else {
    u4_axis axis     = u4_ch(u4_ch(log_pike));
    u4_form form     = u4_ct(u4_ch(log_pike));
    u4_log  log_form = _log_pike_root(lane, u4_ct(log_pike));

    if ( u4_n_eq(u4_noun_1, axis) ) {
      return u4_k_cell(lane, form, log_form);
    }
    else return log_form;
  }
}

/* _log_pike_l(): factor (log_pike) left.
*/
static u4_log
_log_pike_l(u4_lane lane,
            u4_log  log_pike)
{
  if ( u4_n_zero(log_pike) ) {
    return u4_noun_0;
  }
  else {
    u4_axis axis       = u4_ch(u4_ch(log_pike));
    u4_form form       = u4_ct(u4_ch(log_pike));
    u4_log  log_pike_l = _log_pike_l(lane, u4_ct(log_pike));
    {
      if ( u4_n_eq(u4_noun_2, u4_op_tip(axis)) ) {
        u4_axis axis_tap = u4_op_tap(lane, axis);

        return u4_k_cell(lane, u4_k_cell(lane, axis_tap, form), log_pike_l);
      }
      else return log_pike_l;
    }
  }
}

/* _log_pike_r(): factor (log_pike) right.
*/
static u4_log
_log_pike_r(u4_lane lane,
            u4_log  log_pike)
{
  if ( u4_n_zero(log_pike) ) {
    return u4_noun_0;
  }
  else {
    u4_axis axis       = u4_ch(u4_ch(log_pike));
    u4_form form       = u4_ct(u4_ch(log_pike));
    u4_log  log_pike_r = _log_pike_r(lane, u4_ct(log_pike));
    {
      if ( u4_n_eq(u4_noun_3, u4_op_tip(axis)) ) {
        u4_axis axis_tap = u4_op_tap(lane, axis);

        return u4_k_cell(lane, u4_k_cell(lane, axis_tap, form), log_pike_r);
      }
      else return log_pike_r;
    }
  }
}
/* _mill_hike(): mutate form.
*/
u4_form
_mill_hike(u4_milr m,
           u4_axis axis_hike,
           u4_log  log_pike)  /* (axis form) */
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(log_pike) ) {
    return u4_k_cell(lane, u4_noun_0, axis_hike);
  }
  else {
    u4_log  log_form    = _log_pike_root(lane, log_pike);
    u4_log  log_pike_l  = _log_pike_l(lane, log_pike);
    u4_log  log_pike_r  = _log_pike_r(lane, log_pike);

    if ( !u4_n_zero(log_form) ) {
      return u4_ch(log_form);
    }
    else {
      u4_form form_l, form_r;
      
      form_l = _mill_hike(m, _axis_l(lane, axis_hike), log_pike_l);
      form_r = _mill_hike(m, _axis_r(lane, axis_hike), log_pike_r);

      return _mill_cons(m, form_l, form_r);
    }
  }
}
