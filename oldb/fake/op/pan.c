/* fake/op/pan.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _pan_hat(): as u4_op_pan(), on the hat.
*/
static u4_nopt
_pan_hat(u4_road road,
         u4_twig twig_a,
         u4_twig twig_b)
{
  if ( u4_n_zero(twig_a) || u4_n_zero(twig_b) ) {
    return u4_bull;
  }
  else {
    u4_bar  bar_cap = u4_road_bar_cap(road); 
    u4_nopt pro;
    {
      while ( !u4_n_eq(twig_a, u4_noun_1) && 
              !u4_n_eq(twig_b, u4_noun_1) ) 
      {
        u4_bit bit_a_tip, bit_b_tip;
        u4_t   t_match;

        bit_a_tip = u4_op_tip(twig_a);
        bit_b_tip = u4_op_tip(twig_b);
        
        t_match = u4_n_eq(bit_a_tip, bit_b_tip);

        if ( !t_match ) {
          u4_road_bar_cap(road) = bar_cap;
          return u4_bull;
        }
        else {
          twig_a = u4_op_tap(u4_cap(road), twig_a);
          twig_b = u4_op_tap(u4_cap(road), twig_b);
        }
      }
      pro = u4_k_cell(u4_hat(road), twig_a, twig_b);
    }
    u4_road_bar_cap(road) = bar_cap;
    return pro;
  }
}

/* u4_op_pan():
**
**   Factor out the shared root of (twig_a twig_b).  The result is 
**   either (1 twig) or (twig 1), or bull if they diverge.
*/
u4_nopt
u4_op_pan(u4_lane lane,
          u4_twig twig_a,
          u4_twig twig_b)
{
  u4_road road = u4_lane_road(lane);
  u4_noun pro;

  if ( u4_lane_side(lane) == u4_side_hat ) {
    pro = _pan_hat(road, twig_a, twig_b);
  }
  else {
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    pro = _pan_hat(road_nest, twig_a, twig_b);
    u4_r_nest_out(road, road_nest);
  }
  return pro;
}
