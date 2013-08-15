/* fake/op/tap.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _tap_hat(): as u4_op_tap(), on the hat.
*/
static u4_nopt
_tap_hat(u4_road road,
         u4_twig twig)
{
  if ( u4_n_eq(twig, u4_noun_1) ) {
    return u4_bull;
  }
  else {
    u4_bar  bar_cap = u4_road_bar_cap(road);
    u4_atom pro;

    /* Ugly.
    */
    {
      u4_atom a = u4_op_log(u4_cap(road), twig);
      u4_atom b = u4_op_dec(u4_cap(road), a);
      u4_atom c = u4_op_lsh(u4_cap(road), b, u4_noun_1);
      u4_atom d = u4_op_sub(u4_cap(road), c, twig);
      u4_atom e = u4_op_rsh(u4_cap(road), u4_noun_1, c);
      
      pro = u4_op_con(u4_hat(road), e, d);
    }
    u4_road_bar_cap(road) = bar_cap;
    return pro;
  }
}

/* u4_op_tap():
**
**   Produce (twig) with the root bit removed, or bull if (twig) is 1.
*/
u4_nopt
u4_op_tap(u4_lane lane,
          u4_twig twig)
{
  u4_road road = u4_lane_road(lane);
  u4_noun pro;

  if ( u4_lane_side(lane) == u4_side_hat ) {
    pro = _tap_hat(road, twig);
  }
  else {
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    pro = _tap_hat(road_nest, twig);
    u4_r_nest_out(road, road_nest);
  }
  return pro;
}
