/* fake/log.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_unit_need():
*/
u4_noun
u4_unit_need(u4_noun n)
{
  if ( u4_n_zero(n) ) {
    return u4_exit;
  }
  else return u4_ct(n);
}

/* u4_log_at():
**
**   Produce the nth cow in (log), or bull.
*/
_(u4_nopt, cow)
u4_log_at(u4_log  log,
          u4_atom n)
{
  /* Applies only for a 32-bit system.
  */
  if ( u4_a_bin(n, 5) > 1 ) {
    return u4_bull;
  }
  else {
    u4_xw xw_n = u4_a_wbail(n, u4_bail_trip);

    while ( xw_n ) {
      if ( u4_n_atom(log) ) {
        return u4_bull;
      }
      else log = u4_ct(log);
      
      xw_n--;
    }
    if ( u4_n_atom(log) ) {
      return u4_bull;
    }
    else return u4_ch(log);
  }
}

/* u4_log_flip(): 
**
**   On (lane), produce the inverse of (log).
*/
_(u4_log, log_pro)
u4_log_flip(u4_lane lane,
            u4_log  log)
{
  u4_log log_pro = u4_noun_0;

  while ( u4_n_cell(log) ) {
    log_pro = u4_k_cell(lane, u4_ch(log), log_pro);
    log = u4_ct(log);
  }
  return log_pro;
}

/* u4_log_len():
**
**   Produce the length of (log).
*/
_(u4_xw, xw_len)
u4_log_len(u4_log log)
{
  u4_xw xw_len = 0;

  while ( !u4_n_zero(log) ) {
    xw_len++;
    log = u4_ct(log);
  }
  return xw_len;
}

/* u4_log_cat():
**
**   Concatenate (log_a) and (log_b).
*/
u4_log
u4_log_cat(u4_lane lane,
           u4_log  log_a,
           u4_log  log_b)
{
  if ( u4_n_zero(log_a) ) {
    return log_b;
  }
  else {
    return u4_k_cell(lane, u4_ch(log_a), u4_log_cat(lane, u4_ct(log_a), log_b));
  }
}

/* u4_log_tupl():
**
**   Convert (log) to a tuple.
*/
_(u4_noun, pro)
u4_log_tupl(u4_lane lane,
            u4_log  log)
{
  u4_assert(!u4_n_zero(log));

  if ( u4_n_zero(u4_ct(log)) ) {
    return u4_ch(log);
  }
  else return u4_k_cell(lane, u4_ch(log), u4_log_tupl(lane, u4_ct(log)));
}
