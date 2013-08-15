/* include/cake/road.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /* The wasp for road.
    **
    ** All variables in a wasp must be word-aligned.
    */
      struct u4_wasp_road {
        u4_bar bar_hat;
        u4_bar bar_cap;
        u4_bar bar_mat;
      };

    /* A road is a bar which points to a struct u4_wasp_road.
    */
      typedef u4_bar u4_road;
#       define u4_road_bar_hat(r)  u4_bar_at(r, u4_wasp_to(road, bar_hat))
#       define u4_road_bar_cap(r)  u4_bar_at(r, u4_wasp_to(road, bar_cap))
#       define u4_road_bar_mat(r)  u4_bar_at(r, u4_wasp_to(road, bar_mat))

    /* A lane is a road plus an end/cap bit.
    **
    ** This takes advantage of the fact that we have a free bit in road,
    ** because it is a bar.  It is just a convenience.
    */
      enum u4_side {
        u4_side_hat = 0,
        u4_side_cap = 1,
      };
      typedef u4_xw u4_lane;

    /** Lane macros.
    **/
      /* Lane to road and side.
      */
#       define u4_lane_side(lane) \
          ((lane) & 1)
#       define u4_lane_road(lane) \
          ((lane) >> 1)

      /* Lane test.
      */
#       define u4_lane_is_hat(lane)   ( u4_lane_side(lane) == u4_side_hat ) 
#       define u4_lane_is_cap(lane)   ( u4_lane_side(lane) == u4_side_cap ) 

      /* Road to lane.
      */
#       define u4_road_lane_hat(road)  ( ((road) << 1) | u4_side_hat )
#       define u4_road_lane_cap(road)  ( ((road) << 1) | u4_side_cap )

#       define u4_hat(road)            u4_road_lane_hat(road)
#       define u4_cap(road)            u4_road_lane_cap(road)


  /** Macros.
  **/
    /* Road nesting.
    **
    ** To nest, use this pattern at the root of a C function:
    **
    **  u4_road_lane_in(lane, road, pro)
    **  {
    **    // compose (pro) on u4_hat(road) 
    **  }
    **  u4_road_lane_out(lane, road, pro)
    */
#     define u4_road_lane_in(lane, road, pro) \
        u4_noun pro; \
        u4_road road_lane = u4_lane_road(lane); \
        u4_road road      = u4_lane_is_cap(lane) \
                              ? u4_r_nest_in(road_lane) \
                              : road_lane;

#     define u4_road_lane_out(lane, road, pro) \
        if ( u4_lane_is_cap(lane) ) { \
          u4_r_nest_out(road_lane, road); \
        } \
        return pro;


  /** Functions.
  **/
    /* u4_r_init_bar():
    **
    **   Initialize a road between (bar_hat) and (bar_cap).
    */
      u4_road
      u4_r_init_bar(u4_bar bar_hat,
                    u4_bar bar_cap);

    /* u4_r_init_foo():
    **
    **   Initialize a road of length (sw), at (foo).
    **
    **   The cap will be west, the hat east.
    */
      u4_road
      u4_r_init_foo(u4_foo foo,
                    u4_sw  sw);

    /* u4_r_init_on():
    **
    **   Produce a new road on (lane, sw).
    */
      u4_road
      u4_r_init_on(u4_lane lane,
                   u4_sw   sw);

    /* u4_r_init_part():
    **
    **   Initialize a subroad partition on (lane), using (xb_a/xb_b)
    **   of the remaining storage.
    */
      u4_road
      u4_r_init_part(u4_lane lane,
                     u4_xb   xb_a,
                     u4_xb   xb_b);

    /* u4_r_left(): 
    **
    **   Return the number of words remaining in (lane).
    */
      u4_sw
      u4_r_left(u4_lane lane);

    /* u4_r_take():
    **
    **   Allocate (sw) uninitialized words on (lane).
    */
      u4_bar
      u4_r_take(u4_lane lane,
                u4_sw   sw);

    /* u4_r_give():
    **
    **   Return (sw) words to (lane).  Rarely used.
    */
      void
      u4_r_give(u4_lane lane,
                u4_sw   sw);
   
    /* u4_r_clip():
    **
    **   Remove all storage above (bar) in (lane).
    */
      void
      u4_r_clip(u4_lane lane,
                u4_bar  bar);

    /* u4_r_safe():
    **
    **   Return 1 iff (bar) can be written safely as a pointer
    **   to (lane).
    */
      u4_t
      u4_r_safe(u4_lane lane,
                u4_bar  bar);

    /* u4_r_nest_in():
    **
    **   Spawn a nested road, (road_in), within (road_out).
    */
      _(u4_road, road_in)
      u4_r_nest_in(u4_road road_out);

    /* u4_r_nest_out():
    **
    **   Complete a nested road, (road_in), returning its parent
    **   (road_out).  Acquire the hat of (road_in) as the cap of 
    **   (road_out).
    */
      void
      u4_r_nest_out(u4_road road_out,
                    u4_road road_in);

    /* u4_r_flip_on():
    **
    **   Flip the road, saving the old mat.
    */
      _(u4_bar, bar_zeg)
      u4_r_flip_on(u4_road road);

    /* u4_r_flip_off():
    **
    **   Flip back, dumping the can, restoring the mat and 
    **   acquiring the hat.
    */
      void
      u4_r_flip_off(u4_road road,
                    u4_bar  bar_zeg);
