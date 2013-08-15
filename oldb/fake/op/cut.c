/* fake/op/cut.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _cut_hat(): as u4_op_cut(), on the hat.
*/
static u4_atom
_cut_hat(u4_road road,
         u4_atom a,
         u4_atom b,
         u4_atom c)
{
  u4_bar  bar_cap = u4_road_bar_cap(road);
  u4_atom pro;
  {
    u4_atom rsh;

    rsh = u4_op_rsh(u4_cap(road), b, c);
    pro = u4_op_end(u4_hat(road), a, rsh);
  }
  u4_road_bar_cap(road) = bar_cap;
  return pro;
}

/* u4_op_cut():
**
**   Produce (c >> b) &~ (1 << a).
*/
u4_atom
u4_op_cut(u4_lane lane,
          u4_atom a,
          u4_atom b,
          u4_atom c)
{
  u4_road road = u4_lane_road(lane);
  u4_noun pro;

  if ( u4_lane_side(lane) == u4_side_hat ) {
    pro = _cut_hat(road, a, b, c);
  }
  else {
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    pro = _cut_hat(road_nest, a, b, c);
    u4_r_nest_out(road, road_nest);
  }
  return pro;
}
