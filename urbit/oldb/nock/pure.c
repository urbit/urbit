/* nock/pure.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _nock_pure_leak(): as u4_nock_pure(), leaking on (lane).
*/
static u4_noun
_nock_pure_leak(u4_lane lane,
                u4_noun sub,
                u4_noun form)
{
  u4_noun form_2 = u4_chx(form);
  u4_noun form_3 = u4_ctx(form);

  if ( u4_n_cell(form_2) ) {
    u4_noun pro_a = _nock_pure_leak(lane, sub, form_2);
    u4_noun pro_b = _nock_pure_leak(lane, sub, form_3);

    return u4_k_cell(lane, pro_a, pro_b);
  }
  else {
    switch ( u4_a_wbail(form_2, u4_bail_exit) ) {
      default: return u4_exit;

      case 0: {
        if ( u4_n_atom(form_3) ) {
          u4_noun pro = u4_n_snip_(form_3, sub);

          return (u4_bull == pro) ? u4_exit : u4_k_safe(lane, pro);
        }
        else return u4_bull;
      }
      case 1: {
        return u4_k_safe(lane, form_3);
      }
      case 2: {
        u4_noun form_6 = u4_chx(form_3);
        u4_noun form_7 = u4_ctx(form_3);
        {
          u4_noun pro_a = _nock_pure_leak(lane, sub, form_6);
          u4_noun pro_b = u4_n_eq(u4_noun_0, pro_a)
                            ? _nock_pure_leak(lane, sub, u4_chx(form_7))
                            : u4_n_eq(u4_noun_1, pro_a)
                                ? _nock_pure_leak(lane, sub, u4_ctx(form_7))
                                : u4_exit;

          return pro_b;
        }
      }
      case 3: {
        u4_noun pro_a = _nock_pure_leak(lane, sub, form_3);
        u4_noun pro_b = _nock_pure_leak(lane, u4_chx(pro_a), u4_ctx(pro_a));

        return pro_b;
      }
      case 4: {
        u4_noun pro_a = _nock_pure_leak(lane, sub, form_3);
        u4_noun pro_b = u4_n_atom(pro_a) ? u4_noun_1 : u4_noun_0;

        return pro_b;
      }
      case 5: {
        u4_noun pro_a = _nock_pure_leak(lane, sub, form_3);
        u4_noun pro_b = u4_n_cell(pro_a)
                          ? u4_exit
                          : u4_op_inc(lane, pro_a);
        return pro_b;
      } 
      case 6: {
        u4_noun pro_a = _nock_pure_leak(lane, sub, form_3);
        u4_noun pro_b = u4_n_eq(u4_chx(pro_a), u4_ctx(pro_a))
                          ? u4_noun_0
                          : u4_noun_1;
        return pro_b;
      }
    }
  }
}

/* u4_nock_pure():
**
**   As simply as possible, produce nock(subject formula) on (lane).
*/
u4_noun
u4_nock_pure(u4_lane lane,
             u4_noun subject,
             u4_noun formula)
{
  u4_road_lane_in(lane, road, pro)
  {
    /* Compose (pro) on u4_hat(road).
    */
    u4_noun pro_cap = _nock_pure_leak(u4_cap(road), subject, formula);

    pro = u4_k_safe(u4_hat(road), pro_cap);
  }
  u4_road_lane_out(lane, road, pro)
}
