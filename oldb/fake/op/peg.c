/* fake/op/peg.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _peg_hat(): as u4_op_peg(), on the hat.
*/
static u4_nopt
_peg_hat(u4_road road,
         u4_twig twig_a,
         u4_twig twig_b)
{
  u4_bar  bar_cap = u4_road_bar_cap(road);
  u4_atom pro;

  /* Efficiency: terrible.  Should be done directly.
  */
  {
    u4_atom c, d, e, f, g;

    c = u4_op_log(u4_cap(road), twig_b);
    d = u4_op_dec(u4_cap(road), c);
    e = u4_op_lsh(u4_cap(road), d, u4_noun_1);
    f = u4_op_sub(u4_cap(road), e, twig_b);
    g = u4_op_lsh(u4_cap(road), d, twig_a);

    pro = u4_op_add(u4_hat(road), f, g);
  }

  u4_road_bar_cap(road) = bar_cap;
  return pro;
}

/* u4_op_peg():
**
**   Concatenate (twig_a) above (twig_b).
*/
u4_twig
u4_op_peg(u4_lane lane,
          u4_twig twig_a,
          u4_twig twig_b)
{
  u4_road road = u4_lane_road(lane);
  u4_noun pro;

  if ( u4_lane_side(lane) == u4_side_hat ) {
    pro = _peg_hat(road, twig_a, twig_b);
  }
  else {
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    pro = _peg_hat(road_nest, twig_a, twig_b);
    u4_r_nest_out(road, road_nest);
  }
  return pro;
}
