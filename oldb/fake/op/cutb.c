/* fake/op/cutb.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _cutb_hat(): as u4_op_cutb(), on the hat.
*/
static u4_atom
_cutb_hat(u4_road road,
          u4_atom a,
          u4_atom b,
          u4_atom c,
          u4_atom d)
{
  u4_bar  bar_cap = u4_road_bar_cap(road);
  u4_atom pro;
  {
    u4_atom rsh;
    u4_atom ba, ca;

    ba = u4_op_lsh(u4_cap(road), a, b); 
    ca = u4_op_lsh(u4_cap(road), a, c); 

    rsh = u4_op_rsh(u4_cap(road), ca, d);
    pro = u4_op_end(u4_hat(road), ba, rsh);
  }
  u4_road_bar_cap(road) = bar_cap;
  return pro;
}

/* u4_op_cutb():
**
**   Produce (d >> (c << a) &~ (1 << (b << a))).
*/
u4_atom
u4_op_cutb(u4_lane lane,
           u4_atom a,
           u4_atom b,
           u4_atom c,
           u4_atom d)
{
  u4_road road = u4_lane_road(lane);
  u4_noun pro;

  if ( u4_lane_side(lane) == u4_side_hat ) {
    pro = _cutb_hat(road, a, b, c, d);
  }
  else {
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    pro = _cutb_hat(road_nest, a, b, c, d);
    u4_r_nest_out(road, road_nest);
  }
  return pro;
}
