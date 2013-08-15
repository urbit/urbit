/* cake/road.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include "u4/all.h"

/* u4_r_init_bar():
**
**   Initialize a road between (bar_hat) and (bar_cap).
*/
u4_road
u4_r_init_bar(u4_bar bar_hat,
              u4_bar bar_cap)
{
  u4_road road = (bar_cap + u4_wasp_sw(road));

  u4_road_bar_hat(road) = bar_hat;
  u4_road_bar_cap(road) = road;
  u4_road_bar_mat(road) = road;

  return road;
}

/* u4_r_init_foo():
**
**   Initialize a road of length (sw), at (foo).
**
**   The cap will be west, the hat east.
*/
u4_road
u4_r_init_foo(u4_foo foo,
              u4_sw  sw)
{
  u4_assert((foo + sw) <= u4_cake_sw);
  {
    /* Bars before the first words in (foo, sw).
    **
    ** For example, if foo is 0 and sw is u4_cake_sw, both bars
    ** will be at spot 0.
    */
    u4_bar bar_west = u4_bar_make(u4_pole_west, foo);
    u4_bar bar_east = u4_bar_make(u4_pole_east, (u4_cake_sw - (foo + sw)));

    return u4_r_init_bar(bar_east, bar_west);
  }
}

/* u4_r_left(): 
**
**   Return the number of words remaining in (lane).
*/
u4_sw
u4_r_left(u4_lane lane)
{
  u4_road road    = u4_lane_road(lane);
  u4_bar  bar_hat = u4_road_bar_hat(road);
  u4_bar  bar_cap = u4_road_bar_cap(road);
  u4_sw   foo_hat = u4_bar_foo(bar_hat);
  u4_sw   foo_cap = u4_bar_foo(bar_cap);

  return (foo_hat > foo_cap) ? (foo_hat - foo_cap)
                             : (foo_cap - foo_hat);
}

/* u4_r_init_on():
**
**   Produce a new road on (lane, sw).
*/
u4_road
u4_r_init_on(u4_lane lane,
             u4_sw   sw)
{
  u4_bar bar_hi = u4_r_take(lane, sw);
  u4_bar bar_lo = (bar_hi - sw);
  u4_foo foo_a  = u4_bar_foo(bar_hi);
  u4_foo foo_b  = u4_bar_foo(bar_lo);
  u4_foo foo_lo = u4_min(foo_a, foo_b);

  return u4_r_init_foo(foo_lo, sw);
}

/* u4_r_init_part():
**
**   Initialize a subroad partition on (lane), using (xb_a/xb_b)
**   of the remaining storage.
*/
u4_road
u4_r_init_part(u4_lane lane,
               u4_xb   xb_a,
               u4_xb   xb_b)
{
  u4_assert(xb_b && (xb_b > xb_a));
  {
    u4_sw   sw_left = u4_r_left(lane);
    u4_sw   sw_init  = (sw_left / xb_b) * xb_a;

    return u4_r_init_on(lane, sw_init);
  }
}

/* u4_r_take():
**
**   Allocate (sw) uninitialized words on (lane).
*/
u4_bar
u4_r_take(u4_lane lane,
          u4_sw   sw)
{
  u4_road road = u4_lane_road(lane);
  u4_bar  bar;

  if ( u4_lane_side(lane) == u4_side_hat ) {
    u4_road_bar_hat(road) += sw;
    bar = u4_road_bar_hat(road);
  } 
  else {
    u4_road_bar_cap(road) += sw;
    bar = u4_road_bar_cap(road);
  }

  if ( u4_bar_cross(u4_road_bar_hat(road), u4_road_bar_cap(road)) ) {
    return u4_tank;
  }
  else return bar;
}

/* u4_r_clip():
**
**   Remove all storage above (bar) in (lane).
*/
void
u4_r_clip(u4_lane lane,
          u4_bar  bar)
{
  u4_road road = u4_lane_road(lane);

  if ( u4_lane_side(lane) == u4_side_hat ) {
    if ( u4_road_bar_hat(road) > bar ) {
      u4_road_bar_hat(road) = bar;
    }
  }
  else {
    if ( u4_road_bar_cap(road) > bar ) {
      u4_road_bar_cap(road) = bar;
    }
  }
}

/* u4_r_give():
**
**   Return (sw) words to (lane).  Rarely used.
*/
void
u4_r_give(u4_lane lane,
          u4_sw   sw)
{
  u4_road road = u4_lane_road(lane);

  if ( u4_lane_side(lane) == u4_side_hat ) {
    u4_assert(u4_road_bar_hat(road) >= sw);
    u4_road_bar_hat(road) -= sw;
  } else {
    u4_assert(u4_road_bar_cap(road) >= sw);
    u4_road_bar_cap(road) -= sw;
  }
}

/* u4_r_safe():
**
**   Return 1 iff (bar) cap be written safely as a pointer
**   to (lane).
*/
u4_t
u4_r_safe(u4_lane lane,
          u4_bar  bar)
{
  u4_road road = u4_lane_road(lane);

  if ( u4_lane_side(lane) == u4_side_cap ) {
    return 1;
  }
  else {
    u4_bar bar_hat = u4_road_bar_hat(road);

    if ( u4_bar_pole(bar) == u4_bar_pole(bar_hat) ) {
      return 1;
    }
    else {
      u4_bar bar_mat = u4_road_bar_mat(road);

      u4_assert(u4_bar_pole(bar) == u4_bar_pole(bar_mat));
      u4_assert(bar_mat <= u4_road_bar_cap(road));

      return (bar <= bar_mat);
    }
  }
}

/* u4_r_nest_in():
**
**   Spawn a nested road, (road_in), within (road_out).
*/
_(u4_road, road_in)
u4_r_nest_in(u4_road road_out)
{
  u4_road road_in;
  u4_bar  bar_hat_out = u4_road_bar_hat(road_out);
  u4_bar  bar_cap_out = u4_road_bar_cap(road_out);

  road_in = bar_hat_out + u4_wasp_sw(road);
  if ( u4_bar_cross(road_in, bar_cap_out) ) {
    return u4_tank;
  }

  u4_road_bar_hat(road_in) = bar_cap_out;
  u4_road_bar_cap(road_in) = road_in;
  u4_road_bar_mat(road_in) = road_in;

  return road_in;
}

/* u4_r_nest_out():
**
**   Complete a nested road, (road_in), returning its parent
**   (road_out).  Acquire the hat of (road_in) as the cap of 
**   (road_out).
*/
void
u4_r_nest_out(u4_road road_out,
              u4_road road_in)
{
  u4_road_bar_cap(road_out) = u4_road_bar_hat(road_in);
}

/* u4_r_flip_on():
**
**   Flip the road, saving the old mat.
*/
_(u4_bar, bar_flip)
u4_r_flip_on(u4_road road)
{
  u4_bar bar_fol = u4_road_bar_hat(road);
  u4_bar bar_gam = u4_road_bar_cap(road);
  u4_bar bar_zeg = u4_road_bar_mat(road);

  u4_road_bar_hat(road) = bar_gam; 
  u4_road_bar_cap(road) = bar_fol; 
  u4_road_bar_mat(road) = bar_fol;

  return bar_zeg;
}

/* u4_r_flip_off():
**
**   Flip back, restoring the mat, acquiring the hat, and
**   tossing the cap. 
*/
void
u4_r_flip_off(u4_road road,
              u4_bar  bar_zeg)
{
  u4_bar bar_hig = u4_road_bar_hat(road);
  u4_bar bar_suz = u4_road_bar_mat(road);

  u4_road_bar_hat(road) = bar_suz;
  u4_road_bar_cap(road) = bar_hig;
  u4_road_bar_mat(road) = bar_zeg;
}
