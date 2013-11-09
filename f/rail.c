/* f/rail.c
** 
** This file is in the public domain.
*/
#include "all.h"

#if 0
int LEAK=0;
u2_ray LEAKY=0;
#endif

/* _rl_feed():
**
**   Initialize allocator in `ral`.
*/
static void
_rl_feed(u2_ray ral_r)
{
  if ( c3__rock == u2_rail_hip_m(ral_r) ) {
    u2_ray sop_r = u2_rail_rut_r(ral_r);
    u2_ray lot_r = u2_soup_lot_r(sop_r);

    u2_rail_hat_r(ral_r) += c3_wiseof(u2_loom_soup);
    {
      c3_w i_w;

      for ( i_w = 0; i_w < u2_soup_free_no; i_w++ ) {
        u2_soup_fre_r(sop_r, i_w) = 0;
      }
    }
#ifdef U2_PROFILE_MEMORY
    u2_soup_liv_w(sop_r) = 0;
#endif
    u2_cs_init(lot_r);
  }
}

/* u2_rl_init():
**
**   Install an empty rail within `hat_r` and `mat_r` in the loom,
**   with memory model `hip`.
**
**   Returns ray to rail, which always equalls the passed `hat_r`.
*/
u2_ray
u2_rl_init(c3_m   hip_m,
           u2_ray hat_r,
           u2_ray mat_r)
{
  c3_assert(u2_ray_a(hat_r) != u2_ray_a(mat_r));
  c3_assert(u2_ray_open(hat_r, mat_r, c3_wiseof(u2_loom_rail)));

  {
    u2_ray ral_r = hat_r;

    /* Initial empty configuration.  Don't fall, as there is no pork.
    */
    hat_r += c3_wiseof(u2_loom_rail);

    u2_rail_cap_r(ral_r) = mat_r;
    u2_rail_hat_r(ral_r) = hat_r;
    u2_rail_mat_r(ral_r) = mat_r;
    u2_rail_rut_r(ral_r) = hat_r;
    u2_rail_hip_m(ral_r) = hip_m;

    _rl_feed(ral_r);

    return ral_r;
  }
}

/* u2_rl_boot():
**
**  Create an empty rail in an empty loom, with memory model `hip`.
**  See u2_rl_leap() for storage policies.
*/
u2_ray
u2_rl_boot(c3_m hip_m)
{
  return u2_rl_init(hip_m, u2_ray_of(0, 0), u2_ray_of(1, 0));
}

/* u2_rl_leap():
**
**   Reverse the beams forward.
*/
u2_bean
u2_rl_leap(u2_ray ral_r,
           c3_m   hop_m)
{
  if ( hop_m == c3__rock ) {
    if ( u2_no == u2_rl_open(ral_r, c3_wiseof(u2_loom_soup)) ) {
      return u2_no;
    }
  } else {
    if ( u2_no == u2_rl_open(ral_r, c3_wiseof(u2_loom_floe)) ) {
      return u2_no;
    }
  }

  /*    Before: 
  * 0           rut   hat                                  1GB
  * |            |     |                                     | 
  * |-------------------########-----------------------------|
  * |                           |      |                     |
  *                            cap    mat
  *//*  After:
  * 0                mat+cap                              1GB
  * |                   |
  * |--------------------#######-----------------------------|
  * |                         / |                            |
  *                        hat rut 
  */
  {
    u2_ray cap_r = u2_rail_cap_r(ral_r);
    u2_ray hat_r = u2_rail_hat_r(ral_r);
    u2_ray mat_r = u2_rail_mat_r(ral_r);
    u2_ray rut_r = u2_rail_rut_r(ral_r);
    c3_m   hip_m = u2_rail_hip_m(ral_r);

    /* Classical beam reversal.
    */
    {
      u2_rail_cap_r(ral_r) = hat_r;
      u2_rail_hat_r(ral_r) = cap_r;
      u2_rail_mat_r(ral_r) = hat_r; 
      u2_rail_rut_r(ral_r) = cap_r;
      u2_rail_hip_m(ral_r) = hop_m;
    }

    /* Store restoration data (pork) on mat under cap.
    */
    {
      u2_ray pik_r = u2_rail_cap_r(ral_r);

      u2_rail_cap_r(ral_r) += c3_wiseof(u2_loom_pork);

      u2_pork_mut_r(pik_r) = mat_r;
      u2_pork_rit_r(pik_r) = rut_r;
      u2_pork_hap_m(pik_r) = hip_m;
    }

    /* Activate allocation, if applicable.
    */
    _rl_feed(ral_r);
  }
  return u2_yes;
}

/* u2_rl_fall():
**
**   Reverse the beams backward, restoring the old mat and rut.
*/
void
u2_rl_fall(u2_ray ral_r)
{
  /*    Before: 
  * 0           rut   hat                                  1GB
  * |            |     |                                     | 
  * |-------------------########-----------------------------|
  * |                           |      |                     |
  *                            cap    mat
  *//*  After:
  * 0      mat        cap                                  1GB
  * |       |          |                                     | 
  * |-------------------###############----------------------|
  * |                                  |      |              |
  *                                   hat    rut
  */
  {
    u2_ray mat_r = u2_rail_mat_r(ral_r);
    u2_ray hat_r = u2_rail_hat_r(ral_r);
    u2_ray pik_r = mat_r;
   
    u2_rail_cap_r(ral_r) = hat_r;
    u2_rail_hat_r(ral_r) = mat_r;

    u2_rail_mat_r(ral_r) = u2_pork_mut_r(pik_r);
    u2_rail_rut_r(ral_r) = u2_pork_rit_r(pik_r);
    u2_rail_hip_m(ral_r) = u2_pork_hap_m(pik_r);
  }
}

/* u2_rl_leap_part():
**
**   Reverse and split rail, inserting partition of size `num/dem`
**   plus `tip`. 
**
**   Returns partition rail, `aux_r`.
*/
u2_ray
u2_rl_leap_part(u2_ray ral_r,
                c3_m   hop_m,
                c3_w   num_w,
                c3_w   dem_w,
                c3_w   tip_w)
{
  u2_ray cap_r = u2_rail_cap_r(ral_r);
  u2_ray hat_r = u2_rail_hat_r(ral_r);
  u2_ray mat_r = u2_rail_mat_r(ral_r);
  u2_ray rut_r = u2_rail_rut_r(ral_r);
  c3_m   hip_m = u2_rail_hip_m(ral_r);
  c3_w   siz_w;
  c3_w   pad_w;

  /* Compute and control partition size.
  */
  {
    c3_assert(num_w > 0); c3_assert(dem_w > 0); c3_assert((num_w + 1) < dem_w);
    {
      c3_w   gap_w = u2_ray_gap(cap_r, hat_r);

      pad_w = (gap_w / dem_w);
      siz_w = (num_w * pad_w) + tip_w;

      if ( (siz_w < 64) || (gap_w < (siz_w + 64)) ) {
        /* Entirely arbitrary, excessive and unfair.
        */
        u2_ho_warn_here();
        return 0;
      }
    }
  }

  /*    Before: 
  * 0           rut   hat
  * |            |     |            
  * |-------------^-----##########################------$----|
  * |                                             |      |
  *                                              cap    mat
  *     Auxiliary rail:
  *
  * 0                 rut hat
  * |                  | /
  * |-------------------^###########-------------------------|
  * |                               |             |
  *                                cap           mat
  *     Main rail:
  *
  * 0                              rut hat
  * |                               | /
  * |--------------------------------^###########$-----------|
  * |                                           / |
  *                                          cap mat
  *   # == unallocated data
  *   - == frozen data
  *   ^ == allocation control block
  *   $ == frame control block
  */
  {
    u2_ray aux_r = u2_ray_over(hat_r, siz_w);

    /* Auxiliary rail.
    */
    {
      u2_rail_rut_r(aux_r) = hat_r;
      u2_rail_hat_r(aux_r) = hat_r;
      u2_rail_mat_r(aux_r) = mat_r;
      u2_rail_cap_r(aux_r) = (aux_r + c3_wiseof(u2_loom_rail));
      u2_rail_hip_m(aux_r) = c3__rock;

      _rl_feed(aux_r);
    }

    /* Main rail.
    */
    {
      u2_rail_rut_r(ral_r) = (hat_r + siz_w);
      u2_rail_hat_r(ral_r) = (hat_r + siz_w);
      u2_rail_mat_r(ral_r) = cap_r;
      u2_rail_cap_r(ral_r) = cap_r;
      u2_rail_hip_m(ral_r) = hip_m;

      /* Pork - partition recovery record.
      */
      {
        u2_ray pik_r = u2_rail_mat_r(ral_r);

        u2_rail_cap_r(ral_r) += c3_wiseof(u2_loom_pork);

        u2_pork_mut_r(pik_r) = mat_r;
        u2_pork_rit_r(pik_r) = rut_r;
        u2_pork_hap_m(pik_r) = hip_m;
      }
      _rl_feed(ral_r);
    }

    return aux_r;
  }
}


/* u2_rl_fall_part():
**
**   Fall on `ral`, also releasing the partition `aux` - assimilation style.
*/
void
u2_rl_fall_part(u2_ray ral_r,
                u2_ray aux_r)
{
  /*
  * Initially:
  * 0           rut   hat
  * |            |     |            
  * |-------------^-----##########################------$----|
  * |                                             |      |
  *                                              cap    mat
  * Main rail, initially:
  *
  * 0                              rut hat
  * |                               | /
  * |--------------------------------^###########$-----------|
  * |                                           / |
  *                                          cap mat
  * Main rail, later:
  *
  * 0                              rut hat
  * |                               |   | 
  * |--------------------------------^---####----$-----------|
  * |                                        |    |
  *                                         cap  mat
  * Auxiliary rail, initially:
  *
  * 0                 rut hat
  * |                  | /
  * |-------------------^###########-------------------------|
  * |                               |             |
  *                                cap           mat
  *
  * Auxiliary rail, later:
  *
  * 0                 rut  hat
  * |                  |    | 
  * |-------------------^----###-----------------------------|
  * |                           |                 |
  *                            cap               mat
  *
  * On return, assimilate style A (right, not supported):
  *
  * 0           rut          hat
  * |            |            |            
  * |-------------^------------###################------$----|
  * |                                             |      |
  *                                              cap    mat
  * On return, compose style B (wrong, not supported):
  *
  * 0           rut   hat
  * |            |     |            
  * |-------------^-------------------------------------$----|
  * |                   |                                |
  *                    cap                              mat
  */

  c3_assert(0);

  /* To correctly assimilate a returning partition:
  **
  **   (1) gain all aux storage referenced from main hat
  **   (2) release all aux storage
  **   (3) copy main hat noun to aux store
  **   (4) merge aux store with deep rut, if rock; compact
  */
}

/* _rl_bloq_make():
**
**  Install a liquid block at `box_r`, with size `siz_w` and 
**  use count `use_w`.
*/
static void
_rl_bloq_make(u2_ray ral_r,
              u2_ray box_r,
              c3_w   siz_w,
              c3_w   use_w)
{
  c3_assert(siz_w >= 5);
  {
    *u2_at_ray(box_r) = siz_w;
    *u2_at_ray(box_r + 1) = use_w;
    *u2_at_ray(box_r + siz_w - 1) = siz_w;
  }
}

/* _rl_free_select()
**
**  Select the correct free list for an object.
*/
c3_w
_rl_free_select(c3_w siz_w)
{
#if 0
  if ( siz_w == 6 ) {
    return 0;
  }
  else return 1;
#else
  if ( siz_w == 6 ) {
    return 0;
  }
  else {
    c3_w i_w = 1;

    while ( 1 ) {
      if ( i_w == u2_soup_free_no ) {
        return (i_w - 1);
      }
      if ( siz_w < 16 ) {
        return i_w;
      }
      siz_w = (siz_w + 1) >> 1;
      i_w += 1;
    }
  }
#endif
}

/* _rl_bloq_attach():
**
**  Attach the bloq at `box_r` to the appropriate free list.
*/
static void
_rl_bloq_attach(u2_ray ral_r, 
                u2_ray box_r)
{
  c3_w   siz_w = u2_rail_box_siz(box_r);
  c3_w   sel_w = _rl_free_select(siz_w); 
  u2_ray sop_r = u2_rail_rut_r(ral_r);
  u2_ray fre_r = u2_soup_fre_r(sop_r, sel_w);

  u2_rail_hut_pre(box_r) = 0;
  u2_rail_hut_nex(box_r) = u2_soup_fre_r(sop_r, sel_w);

  if ( 0 != fre_r ) {
    c3_assert(u2_rail_hut_pre(fre_r) == 0);
    u2_rail_hut_pre(fre_r) = box_r;
  }
  u2_soup_fre_r(sop_r, sel_w) = box_r;
}

/* _rl_bloq_detach():
**
**  Unlist the bloq at `box_r` from its free list.
*/
static void
_rl_bloq_detach(u2_ray ral_r,
                u2_ray box_r)
{
  c3_w   siz_w = u2_rail_box_siz(box_r);
  c3_w   sel_w = _rl_free_select(siz_w); 
  u2_ray sop_r = u2_rail_rut_r(ral_r);
  u2_ray pre_r = u2_rail_hut_pre(box_r);
  u2_ray nex_r = u2_rail_hut_nex(box_r);

  if ( 0 != pre_r ) {
    u2_rail_hut_nex(pre_r) = nex_r;
  } else {
    u2_soup_fre_r(sop_r, sel_w) = u2_rail_hut_nex(box_r);
  }

  if ( 0 != nex_r ) {
    u2_rail_hut_pre(nex_r) = pre_r;
  }
}

/* _rl_live_grab(): 
*/
#ifdef U2_PROFILE_MEMORY
  static void
  _rl_live_grab(u2_ray ral_r,
                c3_ws  wad_ws)
  {
    u2_ray sop_r = u2_rail_rut_r(ral_r);

    u2_soup_liv_w(sop_r) += wad_ws;
  }
#else
#  define _rl_live_grab(ral_r, wad_ws)
#endif

/* u2_rl_dump():
**
**  Print memory structure for benefit of archeologists.
*/
void
u2_rl_dump(u2_ray ral_r)
{
  if ( c3__rock == u2_rail_hip_m(ral_r) ) {
    u2_ray sop_r = u2_rail_rut_r(ral_r);
    c3_w i_w;
    c3_w tot_w;
    c3_w rag_w;
   
    printf("soup dump:\n");
    tot_w = 0;

    for ( i_w=0; i_w < u2_soup_free_no; i_w++ ) {
      u2_ray box_r = u2_soup_fre_r(sop_r, i_w);
      c3_w num_w = 0;
      c3_w min_w = 0xffffffff;
      c3_w max_w = 0;
      c3_w all_w = 0;

      while ( box_r ) {
        c3_w   siz_w = u2_rail_box_siz(box_r);

        if ( siz_w < min_w ) {
          min_w = siz_w;
        } 
        if ( siz_w > max_w ) {
          max_w = siz_w;
        }
        all_w += siz_w;
        num_w++;
        box_r = u2_rail_hut_nex(box_r);
      }
      tot_w += all_w;

      if ( num_w != 0 ) {
        printf("  list %d, num %d, min %d, max %d, kb %d.%d.%d\n",
                  i_w, num_w, min_w, max_w, 
                  ((all_w * 4) >> 20),
                  ((all_w * 4) >> 10) % 1024,
                  ((all_w * 4) % 1024));
      }
    }
    rag_w = HalfSize -
           ( u2_ray_b(u2_rail_hat_r(ral_r)) + 
             u2_ray_b(u2_rail_cap_r(ral_r)));
    tot_w += rag_w;

    printf("  tail free kb %d.%d.%d\n",
            ((rag_w * 4) >> 20),
            ((rag_w * 4) >> 10) % 1024,
            ((rag_w * 4) % 1024));

    printf("soup: cap %x, hat %x, free %d.%d.%d\n", 
            u2_ray_b(u2_rail_hat_r(ral_r)),
            u2_ray_b(u2_rail_cap_r(ral_r)),
            ((tot_w * 4) >> 20),
            ((tot_w * 4) >> 10) % 1024,
            ((tot_w * 4) % 1024));
  }
}

extern void u2_loop_signal_memory(void);

/* _rl_bloq_cheq(): check box against leak hunt.
*/
static void
_rl_bloq_cheq(u2_ray box_r)
{
#if 0
  u2_ray bad_r = 0x85461f;
  int z = 37;
  int y = 99;
  static int x;

  if ( box_r == bad_r ) {
    fprintf(stderr, "BOX %x/%d: size %d\r\n", bad_r, x, u2_rail_box_siz(box_r));
    if ( x == y ) { c3_assert(0); } else { x++; }
  }
#endif

#if 0
  if ( u2_rail_box_siz(box_r) == z ) {
    fprintf(stderr, "BOX %x/%d: size %d\r\n", bad_r, x, u2_rail_box_siz(box_r));
    if ( x == y ) { c3_assert(0); } else { x++; }
  }
#endif
}

/* _rl_bloq_grab():
**
**  Allocate `len_w` words of memory on `ral_r`, or return 0.
*/
static u2_ray
_rl_bloq_grab(u2_ray ral_r,
              c3_w   len_w)
{
  if ( c3__sand == u2_rail_hip_m(ral_r) ) {
    /* Sand allocation - no box, no overhead.
    */
    if ( u2_no == u2_rl_open(ral_r, len_w) ) {
      u2_loop_signal_memory();
      return 0;
    }
    else { 
      u2_ray box_r;

      box_r = u2_rail_hat_r(ral_r);
      u2_rail_hat_r(ral_r) += len_w;
      return box_r;
    }
  }
  else c3_assert(c3__rock == u2_rail_hip_m(ral_r));
  {
    u2_ray sop_r = u2_rail_rut_r(ral_r);
    c3_w   siz_w = (len_w + c3_wiseof(u2_loom_rail_box) + 1);
    c3_w   sel_w = _rl_free_select(siz_w);
    u2_ray pfr_r;

    if ( (sel_w != 0) && (sel_w != u2_soup_free_no - 1) ) {
      sel_w += 1;
    }
    while ( 1 ) {
      pfr_r = u2_aftr(sop_r, u2_loom_soup, fre_r) + sel_w;

      while ( 1 ) {
        u2_ray fre_r = *u2_at_ray(pfr_r);
        u2_ray box_r;

        if ( 0 == fre_r ) {
          if ( sel_w < (u2_soup_free_no - 1) ) {
            sel_w += 1; 
            break;
          } 
          else {
            /* Nothing in top free list.  Chip away at the hat.
            */
            if ( u2_no == u2_rl_open(ral_r, siz_w) ) {
#if 1
              u2_loop_signal_memory();
#else
              /* Yo, our rail is totally full.
              */
              printf("lose: siz_w: %d sel_w: %d\n", siz_w, sel_w);
              u2_rl_dump(ral_r);

              u2_ho_warn_here();
              //  XX: integrate with integral wire.
              //
              //  A bunch of testing is needed to make this actually work.
              // return 0;    
              c3_assert(0);
#endif
            }
            else { 
              box_r = u2_rail_hat_r(ral_r);
              u2_rail_hat_r(ral_r) += siz_w;

              _rl_bloq_make(ral_r, box_r, siz_w, 1);
              _rl_live_grab(ral_r, siz_w);

              _rl_bloq_cheq(box_r);
              return (box_r + c3_wiseof(u2_loom_rail_box));
            }
          }
        } else {
          if ( siz_w > u2_rail_hut_siz(fre_r) ) {
            /* This free block is too small.  Continue searching.
            */
            pfr_r = u2_aftr(fre_r, u2_loom_rail_hut, nex_r);
          } 
          else {
            /* We have found a free block of adequate size.  Remove it
            ** from the free list.
            */
            box_r = fre_r;

            {
              u2_ray pre_r = u2_rail_hut_pre(box_r);
              u2_ray nex_r = u2_rail_hut_nex(box_r);

              c3_assert((0 == pre_r) || 
                        (u2_at_ray(pfr_r) == &u2_rail_hut_nex(pre_r)));
              *u2_at_ray(pfr_r) = nex_r;
              
              if ( 0 != nex_r ) {
                u2_rail_hut_pre(nex_r) = pre_r;
              }
            }

            if ( (siz_w + 6) < u2_rail_hut_siz(box_r) ) {
              /* Split the block.
              */
              u2_ray end_r = (box_r + siz_w);

              _rl_bloq_make(ral_r, end_r, u2_rail_hut_siz(fre_r) - siz_w, 0);
              _rl_bloq_attach(ral_r, end_r);
              _rl_bloq_make(ral_r, box_r, siz_w, 1);

              _rl_live_grab(ral_r, siz_w);
            }
            else {
              c3_assert(u2_rail_box_use(box_r) == 0);
              u2_rail_box_use(box_r) = 1;

              _rl_live_grab(ral_r, u2_rail_hut_siz(box_r));
            }
            _rl_bloq_cheq(box_r);
            return (box_r + c3_wiseof(u2_loom_rail_box));
          }
        }
      }
    }
  }
}

#if 0
static int xzx=0;

/* _rl_bloq_grap()::
*/
static u2_ray
_rl_bloq_grap(u2_ray ral_r,
              c3_w   len_w)
{
  u2_ray nov_r;

  nov_r = _rl_bloq_grab(ral_r, len_w);

#if 0
  if ( (nov_r - c3_wiseof(u2_loom_rail_box)) == 0x8acb5a ) {
    printf("alloc leak %d - nov_r %x\n", xzx, nov_r);
    if ( xzx == 45 ) { xzx++; c3_assert(0); }
    xzx++;
  }
#endif
  return nov_r;
}
#endif

/* _rl_bloq_free():
**
**    Release and coalesce a block.
*/
static void
_rl_bloq_free(u2_ray ral_r,
              u2_ray box_r)
{
  u2_ray rut_r = u2_rail_rut_r(ral_r);
  u2_ray beg_r = (rut_r + c3_wiseof(u2_loom_soup));
  u2_ray hat_r = u2_rail_hat_r(ral_r);

  c3_assert(u2_rail_hip_m(ral_r) == c3__rock);
  c3_assert(u2_rail_hut_use(box_r) == 0);
  c3_assert(u2_ray_a(box_r) == u2_ray_a(rut_r));
  c3_assert(box_r >= rut_r);

  /* Clear the contents of the block, for debugging.
  */
  {
    c3_w   siz_w = u2_rail_box_siz(box_r);
    u2_ray bod_r;
    
    for ( bod_r = (box_r + c3_wiseof(u2_loom_rail_box));
          (bod_r + 1) < (box_r + siz_w);
          bod_r++ )
    {
      *u2_at_ray(bod_r) = 0xdeadbeef;
    }
  }       

  _rl_live_grab(ral_r, (-1 * u2_rail_hut_siz(box_r)));

  /* Try to coalesce with the previous block.
  */
  if ( box_r != beg_r ) {
    c3_w   las_w = *u2_at_ray(box_r - 1);
    u2_ray tod_r = (box_r - las_w);

    if ( 0 == u2_rail_hut_use(tod_r) ) {
      _rl_bloq_detach(ral_r, tod_r);
      _rl_bloq_make(ral_r, tod_r, (las_w + u2_rail_hut_siz(box_r)), 0);
      box_r = tod_r;
    }
  }

  /* Try to coalesce with the next block, or with the wilderness.
  */
  {
    c3_w siz_w = u2_rail_hut_siz(box_r);

    if ( (box_r + siz_w == hat_r) ) {
      u2_rail_hat_r(ral_r) = box_r;
    }
    else {
      u2_ray hob_r = (box_r + siz_w);

      if ( 0 == u2_rail_hut_use(hob_r) ) {
        _rl_bloq_detach(ral_r, hob_r);
        _rl_bloq_make(ral_r, box_r, (siz_w + u2_rail_hut_siz(hob_r)), 0);
      }

      /* Add to the appropriate free list.
      */
      _rl_bloq_attach(ral_r, box_r);
    }
  }
}

#if 0
/* _rl_sloq_free(): wrapper for _rl_bloq_free(), disabling signals.
*/
static void
_rl_sloq_free(u2_ray ral_r,
              u2_ray box_r)
{
  return _rl_bloq_free(ral_r, box_r);
}

/* _rl_sloq_grab(): wrapper for _rl_bloq_grab(), disabling signals.
*/
static u2_ray
_rl_sloq_grab(u2_ray ral_r,
              c3_w   len_w)
{
  return _rl_bloq_grab(ral_r, len_w);
}
#endif

/* u2_rl_ralloc():
**
**   Allocate `siz_w` words of raw ray storage.
*/
u2_ray
u2_rl_ralloc(u2_ray ral_r,
             c3_w   siz_w)
{
  return _rl_bloq_grab(ral_r, siz_w);
}

/* u2_rl_rfree():
**
**   Free raw ray storage allocated by `u2_rl_ralloc()`.
*/
void
u2_rl_rfree(u2_ray ral_r,
            u2_ray nov_r)
{
  if ( c3__rock == u2_rail_hip_m(ral_r) ) {
    u2_ray box_r = (nov_r - c3_wiseof(u2_loom_rail_box));

    c3_assert(u2_rail_box_use(box_r) == 1);
    u2_rail_box_use(box_r) = 0;

    _rl_bloq_free(ral_r, box_r);
  }
}

/* u2_rl_malloc():
**
**   Allocate `sib_w` *bytes* of raw C storage.
*/
void*
u2_rl_malloc(u2_ray ral_r,
             c3_w   sib_w)
{
  c3_w siz_w = (sib_w + 3) >> 2;
  u2_ray nov_r = u2_rl_ralloc(ral_r, c3_max(5, siz_w));

  return u2_at_ray(nov_r);
}

/* u2_rl_free():
**
**   Free storage allocated by u2_rl_malloc().
*/
void
u2_rl_free(u2_ray ral_r,
           void*  lag_v)
{
  u2_ray nov_r = u2_nit_at(lag_v);

  c3_assert(lag_v == u2_at_ray(nov_r));
  u2_rl_rfree(ral_r, nov_r);
}

/* u2_rl_gain():
**
**   Gain a reference to [som] in [ral_r].
*/
u2_noun
u2_rl_gain(u2_ray  ral_r,
           u2_noun som)
{
  if ( u2_none == som ) {
    return u2_none;
  }

  if ( u2_fly_is_dog(som) ) {
    u2_ray som_r = u2_dog_a(som);
    u2_ray hat_r = u2_rail_hat_r(ral_r);

    if ( u2_ray_a(som_r) == u2_ray_a(hat_r) ) {
      c3_m   hip_m = u2_rail_hip_m(ral_r);

      if ( c3__rock == hip_m ) {
        u2_ray rut_r = u2_rail_rut_r(ral_r);
        
        if ( som_r > rut_r ) {
          u2_ray box_r = (som_r - c3_wiseof(u2_loom_rail_box));
          c3_w   use_w = u2_rail_box_use(box_r);

#if 0
          if ( LEAK && (som_r == LEAKY) ) {
            printf("LEAK: gain %x, use %d\n", som, use_w); 
            // if ( LEAK == XXXX ) c3_assert(0);
            LEAK++;
          }
#endif
          c3_assert(use_w != 0);
          if ( use_w != 0x7fffffff ) {
            u2_rail_box_use(box_r) = (use_w + 1);
          }
        }
      }
    }
    else {
      /* In the can (above the mat), counting propagates down.
      */
      if ( u2_dog_is_pom(som) ) {
        u2_ray mat_r = u2_rail_mat_r(ral_r);

        if ( som_r >= mat_r ) {
          u2_rl_gain(ral_r, *u2_at(som_r, u2_loom_cell, hed_r));
          u2_rl_gain(ral_r, *u2_at(som_r, u2_loom_cell, tel_r));
        }
      }
    }
  }
  return som;
}

/* u2_rl_ok():
**
**   Ensure that all reference counts are valid in `som`.
*/
void
u2_rl_ok(u2_ray  ral_r,
         u2_noun som)
{
  if ( (u2_none == som) || u2_fly_is_cat(som) ) {
    return;
  }
  else {
    u2_ray som_r = u2_dog_a(som);
    u2_ray hat_r = u2_rail_hat_r(ral_r);

    if ( u2_ray_a(som_r) == u2_ray_a(hat_r) ) {
      c3_m hip_m = u2_rail_hip_m(ral_r);

      if ( c3__rock == hip_m ) {
        u2_ray rut_r = u2_rail_rut_r(ral_r);
        
        if ( som_r >= rut_r ) {
          u2_ray box_r = (som_r - c3_wiseof(u2_loom_rail_box));
          c3_w   use_w = u2_rail_box_use(box_r);

          if ( use_w == 0 ) {
            fprintf(stderr, "free noun: %u\n", som);
            c3_assert(0);
          }
        }
      }
    }
    if ( u2_yes == u2_dust(som) ) {
      u2_rl_ok(ral_r, u2_h(som));
      u2_rl_ok(ral_r, u2_t(som));
    }
  }
}

/* u2_rl_refs():
**
**   Return the reference count of (som).  For debugging.
*/
c3_w
u2_rl_refs(u2_ray  ral_r,
           u2_noun som)
{
  if ( u2_fly_is_cat(som) ) {
    return 1;
  }
  else {
    u2_ray som_r = u2_dog_a(som);
    u2_ray hat_r = u2_rail_hat_r(ral_r);

    if ( u2_ray_a(som_r) == u2_ray_a(hat_r) ) {
      c3_m hip_m = u2_rail_hip_m(ral_r);

      if ( c3__rock == hip_m ) {
        u2_ray rut_r = u2_rail_rut_r(ral_r);
        
        if ( som_r >= rut_r ) {
          u2_ray box_r = (som_r - c3_wiseof(u2_loom_rail_box));
          c3_w   use_w = u2_rail_box_use(box_r);

          return use_w;
        }
      }
    }
  }
  return 1;
}

/* u2_rl_lose():
**
**   Lose a reference to (som).  Free it if refcount == 0.
*/
void
u2_rl_lose(u2_ray  ral_r, 
           u2_noun som)
{
  if ( u2_none == som ) {
    return;
  }

top: 
  if ( u2_fly_is_dog(som) ) {
    u2_ray som_r = u2_dog_a(som);
    u2_ray hat_r = u2_rail_hat_r(ral_r);

    if ( u2_ray_a(som_r) == u2_ray_a(hat_r) ) {
      c3_m hip_m = u2_rail_hip_m(ral_r);

      if ( c3__rock == hip_m ) {
        u2_ray rut_r = u2_rail_rut_r(ral_r);
        
        if ( som_r >= rut_r ) {
          u2_ray box_r = (som_r - c3_wiseof(u2_loom_rail_box));
          c3_w   use_w = u2_rail_box_use(box_r);

#if 0
          if ( LEAK && (som_r == LEAKY) ) {
            printf("LEAK: lose %x, use %d\n", som, use_w); 
            // if ( 2 == LEAK ) c3_assert(0);
            // LEAK++;
          }
#endif
          if ( 1 == use_w ) {
            if ( u2_dog_is_pom(som) ) {
              u2_noun h_som = u2_h(som);
              u2_noun t_som = u2_t(som);

              u2_rl_lose(ral_r, h_som);

              u2_rail_box_use(box_r) = 0;
              _rl_bloq_free(ral_r, box_r);
              som = t_som;
              goto top;
            } 
            else {
              u2_rail_box_use(box_r) = 0;
              _rl_bloq_free(ral_r, box_r);
            }
          }
          else {
            // if ( use_w == 0 ) { u2_err(ral_r, "useless", som); }

            c3_assert(use_w != 0);
            if ( use_w != 0x7fffffff ) {
              u2_rail_box_use(box_r) = (use_w - 1);
            }
          }
        }
      }
    }
    else {
      /* In the can (above the mat), counting propagates down.
      */
      if ( u2_dog_is_pom(som) ) {
        u2_ray mat_r = u2_rail_mat_r(ral_r);

        if ( som_r >= mat_r ) {
          u2_noun h_som = u2_h(som);
          u2_noun t_som = u2_t(som);

          u2_rl_lose(ral_r, h_som);
          som = t_som;
          goto top;
        }
      }
    }
  }
}

/* u2_rl_senior():
**
**   Yes iff `som` is senior in `ral` - ie, does not
**   require reference counting.
*/
u2_bean
u2_rl_senior(u2_ray  ral_r,
             u2_noun som)
{
  if ( u2_fly_is_dog(som) ) {
    u2_ray som_r = u2_dog_a(som);
    u2_ray hat_r = u2_rail_hat_r(ral_r);

    if ( u2_ray_a(som_r) == u2_ray_a(hat_r) ) {
      u2_ray rut_r = u2_rail_rut_r(ral_r);
      c3_m   hip_m = u2_rail_hip_m(ral_r);

      if ( (c3__rock == hip_m) && (som_r > rut_r) ) {
        return u2_no;
      }
    }
    else {
      u2_ray mat_r = u2_rail_mat_r(ral_r);

      if ( som_r >= mat_r ) {
        return u2_no;
      }
    }
  }
  return u2_yes;
}

/* u2_rl_junior():
**
**   Yes iff `som` is junior in `ral` - ie, must be copied
**   to be referenced on the hat.
*/
u2_bean
u2_rl_junior(u2_ray  ral_r,
             u2_noun som)
{
  if ( u2_fly_is_cat(som) ) {
    return u2_no;
  }
  else {
    u2_ray som_r = u2_dog_a(som);
    u2_ray hat_r = u2_rail_hat_r(ral_r);
    u2_ray mat_r = u2_rail_mat_r(ral_r);
    u2_nit som_n = u2_ray_fnit(som_r);
    u2_nit hat_n = u2_ray_fnit(hat_r);
    u2_nit mat_n = u2_ray_fnit(mat_r);

    if ( u2_ray_a(hat_r) == 0 ) {
      if ( (som_n >= hat_n) && (som_n <= mat_n) )
        return u2_yes;
      else return u2_no;
    } else {
      if ( (som_n >= mat_n) && (som_n <= hat_n) )
        return u2_yes;
      else return u2_no;
    }
  }
}

/* u2_rl_flog():
**
**   Release the can, setting cap to top of pot.
*/
void
u2_rl_flog(u2_ray ral_r)
{
  u2_rail_cap_r(ral_r) = u2_rail_mat_r(ral_r);
}

/* u2_rl_open():
**
**   Yes iff [a] more words remain in the pad.
*/
u2_bean
u2_rl_open(u2_ray ral_r,
           c3_w   a_w)
{
  return 
    ((a_w + u2_ray_b(u2_rail_hat_r(ral_r)) + u2_ray_b(u2_rail_cap_r(ral_r)))
     >= HalfSize)
    ? u2_no
    : u2_yes;
}

/* u2_rl_clear():
**
**   Yes iff [lef] does not point to any word >= [net]
**   and < [bat].
*/
u2_bean
u2_rl_clear(u2_noun lef,
            u2_ray  net_r,
            u2_ray  bat_r)
{
  c3_assert(u2_ray_a(net_r) == u2_ray_a(bat_r));

  if ( u2_fly_is_cat(lef) ) {
    return u2_yes;
  } else {
    u2_ray ray_lef = u2_dog_a(lef);

    if ( u2_ray_a(ray_lef) != u2_ray_a(net_r) ) {
      return u2_yes;
    }
    else if ( ray_lef < net_r ) {
      return u2_yes;
    }
    else if ( ray_lef >= bat_r ) {
      if ( u2_dog_is_pom(lef) ) {
        u2_noun hed = *u2_at_pom_hed(lef);
        u2_noun tel = *u2_at_pom_tel(lef);

        if ( (u2_yes == u2_rl_clear(hed, net_r, bat_r)) &&
             (u2_yes == u2_rl_clear(tel, net_r, bat_r)) ) {
          return u2_yes;
        }
        else return u2_no;
      }
      else return u2_yes;
    }
    else return u2_no;
  }
}

/* u2_rl_tamp():
**
**   Tamp, eneting the segment from [net_r] up to [bat_r],
**   preserving the root [lef].
**
**   Assumes u2_rl_clear() with the same arguments.
*/
  /* _tamp_swizzle()::
  **
  **   Shift the root [lef], above [bat], down by [pif].
  */
  static void
  _tamp_swizzle(u2_noun lef,
                c3_w    pif_w,
                u2_ray  bat_r,
                c3_b    nax_b[])
  {
    /* Totally unnecessary assertions.
    */
    {
      c3_assert(u2_fly_is_dog(lef));
      c3_assert(u2_dog_a(lef) >= bat_r);
    }

    /* Only poms are fixed.
    */
    if ( u2_dog_is_pom(lef) ) {
      u2_ray ray_lef = u2_dog_a(lef);
      
      /* Is this pom fixed?
      */
      if ( nax_b[ray_lef - bat_r] ) {
        return;
      }
      else {
        /* Fix the pom!
        */
        nax_b[ray_lef - bat_r] = 1;
        {
          u2_noun fes = *u2_at_pom_hed(lef);
          u2_noun hoz = *u2_at_pom_tel(lef);

          if ( u2_fly_is_dog(fes) &&
               (u2_dog_beam(fes) == u2_ray_beam(bat_r)) &&
               (u2_dog_ray(fes) >= bat_r) )
          {
            /* Rewrite the pointer.
            */
            *u2_at_pom_hed(lef) = (fes - (pif_w << 2));
            
            /* Swizzle into it.
            */
            _tamp_swizzle(fes, pif_w, bat_r, nax_b);
          }

          if ( u2_fly_is_dog(hoz) &&
               (u2_dog_beam(hoz) == u2_ray_beam(bat_r)) &&
               (u2_dog_ray(hoz) >= bat_r) )
          {
            /* Rewrite the pointer.
            */
            *u2_at_pom_tel(lef) = (hoz - (pif_w << 2));
            
            /* Swizzle into it.
            */
            _tamp_swizzle(hoz, pif_w, bat_r, nax_b);
          }
        }
      }
    }
  }
u2_noun
u2_rl_tamp(u2_ray  ral_r,
           u2_noun lef,
           u2_ray  net_r,
           u2_ray  bat_r)
{
  /* pif: length of the segment to elide.
  ** lam: length of the segment to shift down over it.
  */
  c3_w pif_w = (bat_r - net_r);
  c3_w lam_w = (u2_rail_cap_r(ral_r) - bat_r);
  c3_w i_w;

  /* Stupid, unnecessary assertions.
  */
  c3_assert(u2_ray_a(bat_r) == u2_ray_a(net_r));
  c3_assert(bat_r >= net_r);
  c3_assert(u2_rail_cap_r(ral_r) >= bat_r);

  /* Check that there's actually a root.  If not, tamp is trivial.
  */
  if ( !u2_fly_is_dog(lef) ||
       (u2_ray_a(u2_dog_a(lef)) != u2_ray_a(net_r)) || 
       (u2_dog_a(lef) < bat_r) )
  {
    u2_rail_cap_r(ral_r) = net_r;
    return lef;
  }

  /* Swizzle the good segment down to its new location.
  **
  **   nax[i_w]: 1 iff a pom at bat[i_w] has been fixed.
  */
  {
    c3_b nax_b[lam_w];
    c3_w i_w;

    for ( i_w = 0; i_w < lam_w; i_w++ ) {
      nax_b[i_w] = 0;
    }
    _tamp_swizzle(lef, pif_w, bat_r, nax_b);
  }

  /* Move the good segment down.
  */
  for ( i_w = 0; i_w < lam_w; i_w++ ) {
    *u2_at_ray((net_r + i_w)) = *u2_at_ray((bat_r + i_w));
  }
  u2_rail_cap_r(ral_r) -= pif_w;

  /* Move and return [lef].
  */
  return (lef - (pif_w << 2));
}

/* u2_rl_water():
**
**   Return east and west watermarks, respectively.
*/
void
u2_rl_water(u2_ray ral_r,
            c3_w*  maz_w,
            c3_w*  boc_w)
{
  if ( !u2_ray_beam(u2_rail_hat_r(ral_r)) ) {
    *maz_w = u2_ray_point(u2_rail_hat_r(ral_r));
    *boc_w = u2_ray_point(u2_rail_cap_r(ral_r));
  } else {
    *maz_w = u2_ray_point(u2_rail_cap_r(ral_r));
    *boc_w = u2_ray_point(u2_rail_hat_r(ral_r));
  }
}

#if 1
/* u2_rl_copy():
**
**   Copy indirect noun `fiz` into main storage, preserving dags.
**   Must be followed by `rl_wash(fiz)` if `fiz` is to be preserved.
*/
u2_weak                                                           //  transfer
u2_rl_copy(u2_ray ral_r,
           u2_dog fiz)                                            //  retain
{
  if ( u2_no == u2_rl_junior(ral_r, fiz) ) {
    u2_rl_gain(ral_r, fiz);
    return fiz;
  }
  else {
    c3_w mug_w = *u2_at_dog_mug(fiz);

    /* Borrow mug slot to record new destination, if it doesn't already.
    */
    if ( mug_w >> 31 ) {      //  mug is 31 bits
      u2_noun nov = mug_w;

      // printf("mug: dag!\n");
      u2_rl_gain(ral_r, nov);
      return nov;
    } else {
      u2_noun nov;

      if ( u2_dog_is_pom(fiz) ) {
        if ( u2_no == u2_rl_open(ral_r, c3_wiseof(u2_loom_cell)) ) {
          u2_loop_signal_memory();

          return u2_none;
        }
        else {
          u2_weak hed, tel;
          u2_ray nov_r;

          if ( u2_none == (hed = u2_rl_copy(ral_r, *u2_at_pom_hed(fiz))) ) {
            u2_loop_signal_memory();

            return u2_none;
          }
          if ( u2_none == (tel = u2_rl_copy(ral_r, *u2_at_pom_tel(fiz))) ) {
            u2_loop_signal_memory();

            u2_rl_lose(ral_r, hed);
            return u2_none;
          }

          nov_r = _rl_bloq_grab(ral_r, c3_wiseof(u2_loom_cell));
          if ( 0 == nov_r ) {
            u2_loop_signal_memory();

            u2_rl_lose(ral_r, hed);
            u2_rl_lose(ral_r, tel);
            return u2_none;
          }
          nov = u2_pom_of(nov_r, 0);

          *u2_at_dog_mug(nov) = mug_w;
          *u2_at_pom_hed(nov) = hed;
          *u2_at_pom_tel(nov) = tel;
   
          c3_assert(u2_no == u2_rl_junior(ral_r, nov));
        }
      }
      else {
        c3_w len_w = *u2_at_pug_len(fiz);
        u2_ray nov_r;

        nov_r = _rl_bloq_grab(ral_r, (len_w + c3_wiseof(u2_loom_atom)));
        if ( 0 == nov_r ) {
          u2_loop_signal_memory();

          return u2_none;
        }
        nov = u2_pug_of(nov_r, 0);

        *u2_at_dog_mug(nov) = mug_w;
        *u2_at_pug_len(nov) = len_w;
        {
          c3_w i_w;

          for ( i_w=0; i_w < len_w; i_w++ ) {
            *u2_at_pug_buf(nov, i_w) = *u2_at_pug_buf(fiz, i_w);
          }
        }
        c3_assert(u2_no == u2_rl_junior(ral_r, nov));
      }
      // printf("  wiped fiz %x; was %x; now %x\n", fiz, mug_w, nov);

      *u2_at_dog_mug(fiz) = nov;
      return nov;
    }
  }
}

void
u2_rl_wash(u2_rail ral_r,
           u2_dog  fiz)                                            //  retain
{
  if ( u2_yes == u2_rl_junior(ral_r, fiz) ) {
    c3_w mug_w = *u2_at_dog_mug(fiz);

    if ( mug_w >> 31 ) {
      u2_noun nov = mug_w;

      *u2_at_dog_mug(fiz) = *u2_at_dog_mug(nov);
      // printf("  fixed fiz %x; was %x; now %x\n", 
      //                         fiz, mug_w, *u2_at_dog_mug(fiz));
    
      if ( u2_yes == u2_dust(fiz) ) {
        u2_rl_wash(ral_r, u2_h(fiz));
        u2_rl_wash(ral_r, u2_t(fiz));
      }
    }
  }
}

/* u2_rl_take():
**
**   Produce `a`, not referencing the can.  Copy or gain reference.
*/
u2_weak
u2_rl_take(u2_ray  ral_r,
           u2_noun fiz)
{
  if ( u2_no == u2_rl_junior(ral_r, fiz) ) {
    u2_rl_gain(ral_r, fiz);

    return fiz;
  } else {
    u2_noun nov;

    // printf("copy in %x\n", fiz);
    {
      nov = u2_rl_copy(ral_r, fiz);

      u2_rl_wash(ral_r, fiz);
    }
    // printf("copy to %x\n", fiz);
    return nov;
  }
}
#endif

/* u2_rl_gc_mark_noun():
**
**   Mark a noun for gc.
*/
c3_w
u2_rl_gc_mark_noun(u2_ray  ral_r,
                   u2_noun som)
{
  c3_w siz_w = 0;
  c3_assert(som != u2_none);

top:
  if ( u2_fly_is_dog(som) ) {
    u2_ray som_r = u2_dog_a(som);
    u2_ray hat_r = u2_rail_hat_r(ral_r);

    if ( (u2_ray_a(som_r) == u2_ray_a(hat_r)) &&
         (som_r >= u2_rail_rut_r(ral_r)) )
    {
      u2_ray box_r  = (som_r - c3_wiseof(u2_loom_rail_box));
      c3_w   use_w  = u2_rail_box_use(box_r);
      c3_ws  use_ws = (c3_ws) use_w;
 
      c3_assert(use_ws != 0);

      if ( use_ws < 0 ) {
        use_ws -= 1;
        use_w = (c3_w) use_ws;
        u2_rail_box_use(box_r) = use_w;
      }
      else {
        use_ws = -1;
        use_w = (c3_w) use_ws;
        u2_rail_box_use(box_r) = use_w;
        siz_w += u2_rail_box_siz(box_r);

        if ( u2_dog_is_pom(som) ) {
          siz_w += u2_rl_gc_mark_noun(ral_r, u2_h(som));

          som = u2_t(som);
          goto top;
        }
      }
    }
  }
  return siz_w;
}

/* u2_rl_gc_mark_ptr():
**
**   Mark a pointer allocated with ralloc.  Return allocated words.
*/
c3_w
u2_rl_gc_mark_ptr(u2_ray ral_r,
                  u2_ray ptr_r)
{
  u2_ray hat_r = u2_rail_hat_r(ral_r);

  if ( (u2_ray_a(ptr_r) == u2_ray_a(hat_r)) &&
       (ptr_r >= u2_rail_rut_r(ral_r)) )
  {
    u2_ray box_r  = (ptr_r - c3_wiseof(u2_loom_rail_box));
    c3_w   use_w  = u2_rail_box_use(box_r);
    c3_ws  use_ws = (c3_ws) use_w;
    c3_w   siz_w  = u2_rail_box_siz(box_r);

    c3_assert(use_ws != 0);

    if ( use_ws < 0 ) {
      use_ws -= 1;
      siz_w = 0;
    } else {
      use_ws = -1;
    }
    use_w = (c3_w) use_ws;
    u2_rail_box_use(box_r) = use_w;

    return siz_w;
  }
  else return 0;
}

/* u2_rl_gc_mark():
**
**   Mark a rail.  Return allocated words.
*/
c3_w
u2_rl_gc_mark(u2_ray ral_r)
{
  u2_ray sop_r = u2_rail_rut_r(ral_r);
  
  return u2_cs_mark(ral_r, u2_soup_lot_r(sop_r));
}

/* u2_rl_drain():
**
**   Clear the memo cache (soup).
*/
void
u2_rl_drain(u2_ray ral_r)
{
  if ( c3__rock == u2_rail_hip_m(ral_r) ) {
    u2_ray sop_r = u2_rail_rut_r(ral_r);
    
    u2_cs_lose(ral_r, u2_soup_lot_r(sop_r));
  }
}

/* u2_rl_gc_sweep(): 
**
**   Sweep memory, freeing unused blocks.  Match live, save leaked.
*/
c3_w
u2_rl_gc_sweep(u2_ray ral_r, c3_w sav_w)
{
  u2_ray rut_r = u2_rail_rut_r(ral_r);
  u2_ray hat_r = u2_rail_hat_r(ral_r);
  u2_ray bot_r = (rut_r + c3_wiseof(u2_loom_soup));
  u2_ray box_r = bot_r;
  c3_w   liv_w = 0;
  c3_w   lek_w = 0;

#if 0
  while ( box_r < hat_r ) {
    c3_w siz_w = u2_rail_box_siz(box_r);
    c3_w use_w = u2_rail_box_use(box_r);

    if ( use_w > 3 ) {
      printf("box %x, siz %d, use %d\n", box_r, siz_w, use_w);
    }
    box_r += siz_w;
  }
#endif

  while ( box_r < hat_r ) {
    c3_w  siz_w  = u2_rail_box_siz(box_r);
    c3_w  use_w  = u2_rail_box_use(box_r);
    c3_ws use_ws = (c3_ws) use_w;

    if ( use_ws > 0 ) {
#if 0
      fprintf(stderr, "leak: box %x, siz %d, use %d\r\n", box_r, siz_w, use_w);
#endif
      lek_w += siz_w;
      u2_rail_box_use(box_r) = 0;
      _rl_bloq_free(ral_r, box_r);
    } 
    else if ( use_ws < 0 ) {
      // printf("live: box %x, siz %d, use %d\n", box_r, siz_w, use_w);
      use_ws = (0 - use_ws);
      use_w = (c3_w) use_ws;
      u2_rail_box_use(box_r) = use_w;
      liv_w += siz_w;
    }
    box_r += siz_w;
  }
  c3_assert(liv_w == sav_w);
  return lek_w;
}

#if 0
/* u2_rl_take():
**
**   Produce `a`, not referencing the can.  Copy or gain reference.
*/
u2_weak
u2_rl_take(u2_ray  ral_r,
          u2_noun fiz)
{
  if ( u2_no == u2_rl_junior(ral_r, fiz) ) {
    u2_rl_gain(ral_r, fiz);

    return fiz;
  } else {
    if ( u2_dog_is_pom(fiz) ) {
      if ( u2_no == u2_rl_open(ral_r, c3_wiseof(u2_loom_cell)) ) {
        u2_loop_signal_memory();

        return u2_none;
      }
      else {
        u2_weak hed, tel;
        u2_ray nov_r;
        u2_noun nov;

        if ( u2_none == (hed = u2_rl_take(ral_r, *u2_at_pom_hed(fiz))) ) {
          u2_loop_signal_memory();

          return u2_none;
        }
        if ( u2_none == (tel = u2_rl_take(ral_r, *u2_at_pom_tel(fiz))) ) {
          u2_loop_signal_memory();

          u2_rl_lose(ral_r, hed);
          return u2_none;
        }

        nov_r = _rl_bloq_grab(ral_r, c3_wiseof(u2_loom_cell));
        if ( 0 == nov_r ) {
          u2_loop_signal_memory();

          u2_rl_lose(ral_r, hed);
          u2_rl_lose(ral_r, tel);
          return u2_none;
        }
        nov = u2_pom_of(nov_r, 0);

        *u2_at_dog_mug(nov) = *u2_at_dog_mug(fiz);
        *u2_at_pom_hed(nov) = hed;
        *u2_at_pom_tel(nov) = tel;
 
        c3_assert(u2_no == u2_rl_junior(ral_r, nov));
        return nov;
      }
    }
    else {
      c3_w len_w = *u2_at_pug_len(fiz);
      u2_ray nov_r;
      u2_noun nov;

      nov_r = _rl_bloq_grab(ral_r, (len_w + c3_wiseof(u2_loom_atom)));
      if ( 0 == nov_r ) {
        u2_loop_signal_memory();

        return u2_none;
      }

      nov = u2_pug_of(nov_r, 0);

      *u2_at_dog_mug(nov) = 0;
      *u2_at_pug_len(nov) = len_w;
      {
        c3_w i_w;

        for ( i_w=0; i_w < len_w; i_w++ ) {
          *u2_at_pug_buf(nov, i_w) = *u2_at_pug_buf(fiz, i_w);
        }
      }
      c3_assert(u2_no == u2_rl_junior(ral_r, nov));
      return nov;
    }
  }
}
#endif

/* u2_rl_slab():
**
**   Create a blank atomic slab of `len` words.
*/
u2_ray
u2_rl_slab(u2_rail ral_r,
           c3_w    len_w)
{
  u2_ray  nov_r = u2_rl_ralloc(ral_r, (len_w + c3_wiseof(u2_loom_atom)));
  u2_atom nov   = u2_pug_of(nov_r, 0);

  *u2_at_dog_mug(nov) = 0;
  *u2_at_pug_len(nov) = len_w;

  /* Clear teh slab.
  */
  {
    c3_w i_w;

    for ( i_w=0; i_w < len_w; i_w++ ) {
      *u2_at_pug_buf(nov, i_w) = 0;
    }
  }
  return (nov_r + c3_wiseof(u2_loom_atom));
}

/* u2_rl_slaq():
**
**   Create a blank atomic slab of `len` bloqs of size `met`.
*/
u2_ray
u2_rl_slaq(u2_wire ral_r,
           c3_g    met_g,
           c3_w    len_w)
{
  return u2_rl_slab(ral_r, ((len_w << met_g) + 31) >> 5);
}

/* u2_rl_mint():
**
**   Initialize slab `sal` as an atom, externally measured.
*/
u2_atom
u2_rl_mint(u2_rail ral_r,
           u2_ray  sal_r,
           c3_w    len_w)
{
  u2_ray  nov_r = (sal_r - c3_wiseof(u2_loom_atom));
  u2_atom nov = u2_pug_of(nov_r, 0);

  /* See if we can free the slab entirely.
  */
  if ( len_w == 0 ) {
    u2_rl_lose(ral_r, nov);

    return _0;
  } 
  else if ( len_w == 1 ) {
    c3_w low_w = *u2_at_pug_buf(nov, 0);

    if ( u2_fly_is_cat(low_w) ) {
      u2_rl_lose(ral_r, nov);

      return low_w;
    }
  }

  /* See if we can strip off a block on the end.
  */
  {
    c3_w old_w = *u2_at_pug_len(nov);
    c3_w dif_w = (old_w - len_w);

    if ( dif_w >= 6 ) {
      u2_ray box_r = nov_r - c3_wiseof(u2_loom_rail_box);
      u2_ray end_r = (nov_r + c3_wiseof(u2_loom_atom) + len_w + 1);
      c3_w   asz_w = (end_r - box_r);
      c3_w   bsz_w = *u2_at_ray(box_r) - asz_w;

      _rl_bloq_make(ral_r, end_r, bsz_w, 0);
      _rl_bloq_attach(ral_r, end_r);

      *u2_at_ray(box_r) = asz_w;
      *u2_at_ray(box_r + asz_w - 1) = asz_w;
    }
    *u2_at_pug_len(nov) = len_w;
  }
  return nov;
}

/* u2_rl_moot():
**
**   Initialize slab `sal` as an atom, originally measured.
*/
u2_atom                                                           //  transfer
u2_rl_moot(u2_rail ral_r,
           u2_ray  sal_r)
{
  u2_ray  nov_r = (sal_r - c3_wiseof(u2_loom_atom));
  u2_atom nov = u2_pug_of(nov_r, 0);
  c3_w    len_w = *u2_at_pug_len(nov);
  c3_w    las_w = *u2_at_pug_buf(nov, (len_w - 1));

  c3_assert(0 != las_w);

  if ( 1 == len_w ) {
    if ( u2_fly_is_cat(las_w) ) {
      u2_rl_lose(ral_r, nov);

      return las_w;
    }
  }
  return nov;
}

/* u2_rl_malt():
**
**   Initialize slab `sal` as an atom, internally measured.
*/
u2_atom
u2_rl_malt(u2_rail ral_r,
           u2_ray  sal_r)
{
  u2_ray  nov_r = (sal_r - c3_wiseof(u2_loom_atom));
  u2_atom nov = u2_pug_of(nov_r, 0);
  c3_w    len_w;

  for ( len_w = *u2_at_pug_len(nov); len_w; len_w-- ) {
    if ( 0 != *u2_at_pug_buf(nov, (len_w - 1)) ) {
      break;
    }
  }
  return u2_rl_mint(ral_r, sal_r, len_w);
}

/* u2_rl_bytes():
**
**   Copy `a` bytes from `b` to an LSB first atom.
*/
u2_weak                           // transfer
u2_rl_bytes(u2_ray      ral_r,
            c3_w        a_w,
            const c3_y* b_y)
{
  /* Strip trailing zeroes.
  */
  while ( a_w && !b_y[a_w - 1] ) {
    a_w--;
  }

  /* Check for cat.
  */
  if ( a_w <= 4 ) {
    if ( !a_w ) {
      return 0;
    }
    else if ( a_w == 1 ) {
      return b_y[0];
    }
    else if ( a_w == 2 ) {
      return (b_y[0] | (b_y[1] << 8));
    }
    else if ( a_w == 3 ) {
      return (b_y[0] | (b_y[1] << 8) | (b_y[2] << 16));
    }
    else if ( (b_y[3] <= 0x7f) ) {
      return (b_y[0] | (b_y[1] << 8) | (b_y[2] << 16) | (b_y[3] << 24));
    }
  }

  /* Allocate, fill, return.
  */
  {
    c3_w len_w = (a_w + 3) >> 2;

    if ( len_w >= (1 << 27) ) {
      u2_loop_signal_memory();
      return u2_none;
    }
    if ( u2_no == u2_rl_open(ral_r, (len_w + c3_wiseof(u2_loom_atom))) ) {
      u2_loop_signal_memory();
      return u2_none;
    }
    else { 
      u2_ray nov_r;
      u2_noun nov;

      nov_r = _rl_bloq_grab(ral_r, (len_w + c3_wiseof(u2_loom_atom)));
      nov = u2_pug_of(nov_r, 0);

      *u2_at_dog_mug(nov) = 0;
      *u2_at_pug_len(nov) = len_w;

      /* Clear the words.
      */
      {
        c3_w i_w;

        for ( i_w=0; i_w < len_w; i_w++ ) {
          *u2_at_pug_buf(nov, i_w) = 0;
        }
      }

      /* Fill the bytes.
      */
      {
        c3_w i_w;

        for ( i_w=0; i_w < a_w; i_w++ ) {
          *u2_at_pug_buf(nov, (i_w >> 2))
            |=
              (b_y[i_w] << ((i_w & 3) * 8));
        }
      }
      return nov;
    }
  }
}

/* u2_rl_cell(): 
**
**   Produce the cell `[a b]`.
*/
u2_weak                       //  transfer
u2_rl_cell(u2_ray  ral_r,
           u2_weak a,         //  transfer
           u2_weak b)         //  transfer
{
  if ( (u2_none == a) || (u2_none == b) ) {
    u2_rl_lose(ral_r, a);
    u2_rl_lose(ral_r, b);
    return u2_none;
  }

  /* Seniority restrictions.  Ice if these cannot be met.
  */
  {
    if ( u2_yes == u2_rl_junior(ral_r, a) ) {
      u2_noun som = a;

      u2_ray som_r = u2_dog_a(som);
      u2_ray hat_r = u2_rail_hat_r(ral_r);
      u2_ray mat_r = u2_rail_mat_r(ral_r);
      u2_nit som_n = u2_ray_fnit(som_r);
      u2_nit hat_n = u2_ray_fnit(hat_r);
      u2_nit mat_n = u2_ray_fnit(mat_r);

      if ( u2_ray_a(hat_r) == 0 ) {
        if ( (som_n >= hat_n) && (som_n <= mat_n) )
          printf("junior x\n");
      } else {
        if ( (som_n >= mat_n) && (som_n <= hat_n) ) {
          printf("junior y\n");
          printf("hat %d.%d == %d\n", 
              u2_ray_a(hat_r), u2_ray_b(hat_r), u2_ray_fnit(hat_r));
          printf("mat %d.%d == %d\n", 
              u2_ray_a(mat_r), u2_ray_b(mat_r), u2_ray_fnit(mat_r));
          printf("som %d.%d == %d\n", 
              u2_ray_a(som_r), u2_ray_b(som_r), u2_ray_fnit(som_r));
        }
      }
    }

    c3_assert(u2_no == u2_rl_junior(ral_r, a));
    c3_assert(u2_no == u2_rl_junior(ral_r, b));
  }

  {
    u2_ray nov_r;
    u2_noun nov;

    nov_r = _rl_bloq_grab(ral_r, c3_wiseof(u2_loom_cell));
    nov = u2_pom_of(nov_r, 0);

    *u2_at_dog_mug(nov) = 0;
    *u2_at_pom_hed(nov) = a;
    *u2_at_pom_tel(nov) = b;

#if 0
    if ( LEAK && ((u2_fly_is_dog(a) && (u2_dog_a(a) == LEAKY)) ||
                 (u2_fly_is_dog(b) && (u2_dog_a(b) == LEAKY))) ) {
      printf("LEAKY %x => %x\n", (LEAKY - 2), (nov_r - 2));
      LEAKY = nov_r;
    }
#endif
    return nov;
  }
}

/* u2_rl_list():
**
**   Produce a null-terminated list, terminating `...` with `u2_none`.
*/
u2_weak                       //  transfer
u2_rl_list(u2_rail ral_r,
           ...)               //  transfer
{
  c3_w    len_w = 0;
  va_list vap;

  /* Count.
  */
  {
    va_start(vap, ral_r);
    while ( u2_none != va_arg(vap, u2_noun) ) {
      len_w++;
    }
    va_end(vap);
  }

  /* Allocate.
  */
  {
    c3_w    i_w;
    u2_noun yit[len_w];

    va_start(vap, ral_r);
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      yit[i_w] = va_arg(vap, u2_weak);
    }
    va_end(vap);

    /* Construct.
    */
    {
      u2_weak woq = u2_nul;

      for ( i_w = 0; i_w < len_w; i_w++ ) {
        woq = u2_rc(ral_r, yit[len_w - (i_w + 1)], woq);
      }
      return woq;
    }
  }
}

/* u2_rl_molt():
**
**   Mutate `som` with a 0-terminated list of axis, noun pairs.
**   Axes must be cats (31 bit).
*/
  struct _molt_pair {
    c3_w    axe_w;
    u2_noun som;
  };

  static c3_w
  _molt_cut(c3_w               len_w,
            struct _molt_pair* pms_m)
  {
    c3_w i_w, cut_t, cut_w;

    cut_t = c3_false;
    cut_w = 0;
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      c3_w axe_w = pms_m[i_w].axe_w;

      if ( (cut_t == c3_false) && (3 == u2_ax_cap(axe_w)) ) {
        cut_t = c3_true;
        cut_w = i_w;
      }
      pms_m[i_w].axe_w = u2_ax_mas(axe_w);
    }
    return cut_t ? cut_w : i_w;
  }

  static u2_weak                            //  transfer
  _molt_apply(u2_rail            ral_r,
              u2_weak            som,       //  retain
              c3_w               len_w,
              struct _molt_pair* pms_m)     //  transfer
  {
    if ( len_w == 0 ) {
      return u2_rl_take(ral_r, som);
    }
    else if ( (len_w == 1) && (1 == pms_m[0].axe_w) ) {
      return pms_m[0].som;
    }
    else {
      c3_w cut_w = _molt_cut(len_w, pms_m);

      if ( u2_no == u2_dust(som) ) {
        return u2_rc
          (ral_r,
           _molt_apply(ral_r, u2_nul, cut_w, pms_m),
           _molt_apply(ral_r, u2_nul, (len_w - cut_w), (pms_m + cut_w)));
      } else {
        return u2_rc
          (ral_r,
           _molt_apply(ral_r, u2_h(som), cut_w, pms_m),
           _molt_apply(ral_r, u2_t(som), (len_w - cut_w), (pms_m + cut_w)));
      }
    }
  }
u2_weak                       //  transfer
u2_rl_molt(u2_rail ral_r,
           u2_weak som,       //  retain
           ...)               //  transfer
{
  va_list            ap;
  c3_w               len_w;
  struct _molt_pair* pms_m;

  /* Count.
  */
  len_w = 0;
  {
    va_start(ap, som);
    while ( 1 ) {
      if ( 0 == va_arg(ap, c3_w) ) {
        break;
      }
      va_arg(ap, u2_weak*);
      len_w++;
    }
    va_end(ap);
  }
  pms_m = alloca(len_w * sizeof(struct _molt_pair));

  /* Install.
  */
  {
    c3_w i_w;

    va_start(ap, som);
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      pms_m[i_w].axe_w = va_arg(ap, c3_w);
      pms_m[i_w].som = va_arg(ap, u2_noun);
    }
    va_end(ap);
  }

  /* Apply.
  */
  return _molt_apply(ral_r, som, len_w, pms_m);
}
 
/* u2_rl_molv():
**
**   As u2_rl_molt(), by argument pointer.
*/
u2_weak                       //  transfer
u2_rl_molv(u2_rail ral_r,
           u2_weak som,       //  retain
           va_list vap)       //  transfer
{
  va_list            vaq;
  c3_w               len_w;
  struct _molt_pair* pms_m;

  /* Count.
  */
  len_w = 0;
  {
    va_copy(vaq, vap);
    while ( 1 ) {
      if ( 0 == va_arg(vaq, c3_w) ) {
        break;
      }
      va_arg(vaq, u2_weak*);
      len_w++;
    }
    va_end(vaq);
  }
  pms_m = alloca(len_w * sizeof(struct _molt_pair));

  /* Install.
  */
  {
    c3_w i_w;

    va_copy(vaq, vap);
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      pms_m[i_w].axe_w = va_arg(vaq, c3_w);
      pms_m[i_w].som = va_arg(vaq, u2_noun);
    }
    va_end(vaq);
  }

  /* Apply.
  */
  return _molt_apply(ral_r, som, len_w, pms_m);
}

/* u2_rl_mp():
**
**   Copy the GMP integer [a] into an atom.
*/
u2_weak                       //  transfer
u2_rl_mp(u2_ray ral_r,
         mpz_t  a_mp)         //  transfer (GMP)
{
  /* Efficiency: unnecessary copy.
  */
  {
    c3_w pyg_w  = mpz_size(a_mp) * ((sizeof(mp_limb_t)) / 4);
    c3_w *buz_w = alloca(pyg_w * 4);
    c3_w i_w;

    for ( i_w = 0; i_w < pyg_w; i_w++ ) {
      buz_w[i_w] = 0;
    }
    mpz_export(buz_w, 0, -1, 4, 0, 0, a_mp);
    mpz_clear(a_mp);

    return u2_rl_words(ral_r, pyg_w, buz_w);
  }
}

/* u2_rl_rack():
**
**   Produce an n-tuple, terminating `...` with `u2_none`.
*/
u2_weak                       //  transfer
u2_rl_rack(u2_rail ral_r,
           ...)               //  transfer
{
  c3_w    len_w = 0;
  va_list vap;

  /* Count.
  */
  {
    va_start(vap, ral_r);
    while ( u2_none != va_arg(vap, u2_noun) ) {
      len_w++;
    }
    va_end(vap);

    if ( 0 == len_w ) {
      return u2_none;
    }
  }

  /* Allocate.
  */
  {
    c3_w    i_w;
    u2_noun yit[len_w];

    va_start(vap, ral_r);
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      yit[i_w] = va_arg(vap, u2_weak);
    }
    va_end(vap);

    /* Construct.
    */
    {
      u2_weak woq = yit[len_w - 1];

      for ( i_w = 1; i_w < len_w; i_w++ ) {
        woq = u2_rc(ral_r, yit[len_w - (i_w + 1)], woq);
      }
      return woq;
    }
  }
}

/* u2_rl_string():
**
**   Produce an LSB-first atom from the C string `a`.
*/
u2_weak                           //  transfer
u2_rl_string(u2_ray      ral_r,
             const c3_c* a_c)
{
  return u2_rl_bytes(ral_r, strlen(a_c), (c3_y *)a_c);
}

/* u2_rl_vint():
**
**   Create `a + 1`.
*/
u2_weak                                                           //  transfer
u2_rl_vint(u2_rail ral_r,
           u2_weak a)                                             //  transfer
{
  if ( u2_none == a ) {
    return a;
  } else {
    if ( u2_fly_is_cat(a) ) {
      c3_w vin_w = (a + 1);

      if ( a == 0x7fffffff ) {
        return u2_rl_words(ral_r, 1, &vin_w);
      }
      else return vin_w;
    }
    else if ( u2_yes == u2du(a) ) {
      return u2_none;
    } 
    else {
      mpz_t a_mp;

      u2_mp(a_mp, a);
      mpz_add_ui(a_mp, a_mp, 1);

      return u2_rl_mp(ral_r, a_mp);
    }
  }
}

/* u2_rl_words():
**
**   Copy [a] words from [b] into an atom.
*/
u2_weak                         //  transfer
u2_rl_words(u2_ray      ral_r,
            c3_w        a_w,
            const c3_w* b_w)
{
  /* Strip trailing zeroes.
  */
  while ( a_w && !b_w[a_w - 1] ) {
    a_w--;
  }

  /* Check for cat.
  */
  if ( !a_w ) {
    return 0;
  }
  else if ( (a_w == 1) && !(b_w[0] >> 31) ) {
    return b_w[0];
  }

  /* Allocate, fill, return.
  */
  {
    if ( a_w >= (1 << 27) ) {
      u2_loop_signal_memory();
      return u2_none;
    }
    if ( u2_no == u2_rl_open(ral_r, (a_w + c3_wiseof(u2_loom_atom))) ) {
      return u2_none;
    }
    else { 
      u2_ray  nov_r;
      u2_noun nov;

      nov_r = _rl_bloq_grab(ral_r, (a_w + c3_wiseof(u2_loom_atom)));
      nov = u2_pug_of(nov_r, 0);

      *u2_at_dog_mug(nov) = 0;
      *u2_at_pug_len(nov) = a_w;

      /* Fill the words.
      */
      {
        c3_w i_w;

        for ( i_w=0; i_w < a_w; i_w++ ) {
          *u2_at_pug_buf(nov, i_w) = b_w[i_w];
        }
      }
      return nov;
    }
  }
}

/* u2_rl_find():
**
**   Cache search for function (0 means nock) and sample.
*/
u2_weak                                                           //  transfer
u2_rl_find(u2_ray  ral_r,
           u2_mote fun_m,
           u2_noun sam)                                           //  retain
{ 
  if ( c3__rock != u2_rail_hip_m(ral_r) ) {
    return u2_none;
  } else {
    u2_ray  sop_r = u2_rail_rut_r(ral_r);
    u2_ray  lot_r = u2_soup_lot_r(sop_r);
    u2_noun pro   = u2_cs_find(ral_r, lot_r, fun_m, sam);

    return u2_rx(ral_r, pro);
  }
}

/* u2_rl_find_cell():
**
**   As `u2_rl_find()`, for `[a b]`.
*/
u2_weak                                                           //  transfer
u2_rl_find_cell(u2_ray  ral_r,
                u2_mote fun_m,
                u2_noun a,                                        //  retain
                u2_noun b)                                        //  retain
{
  if ( c3__rock != u2_rail_hip_m(ral_r) ) {
    return u2_none;
  } else {
    u2_ray  sop_r = u2_rail_rut_r(ral_r);
    u2_ray  lot_r = u2_soup_lot_r(sop_r);
    u2_noun pro   = u2_cs_find_cell(ral_r, lot_r, fun_m, a, b);

    return u2_rx(ral_r, pro);
  }
}

/* u2_rl_find_trel():
**
**   As `u2_rl_find()`, for `[a b c]`.
*/
u2_weak                                                           //  transfer
u2_rl_find_trel(u2_ray  ral_r,
                u2_mote fun_m,
                u2_noun a,                                        //  retain
                u2_noun b,                                        //  retain
                u2_noun c)                                        //  retain
{
  if ( c3__rock != u2_rail_hip_m(ral_r) ) {
    return u2_none;
  } else {
    u2_ray  sop_r = u2_rail_rut_r(ral_r);
    u2_ray  lot_r = u2_soup_lot_r(sop_r);
    u2_noun pro   = u2_cs_find_trel(ral_r, lot_r, fun_m, a, b, c);

    return u2_rx(ral_r, pro);
  }
}

/* u2_rl_find_qual():
**
**   As `u2_rl_find()`, for `[a b c d]`.
*/
u2_weak                                                           //  transfer
u2_rl_find_qual(u2_ray  ral_r,
                u2_mote fun_m,
                u2_noun a,                                        //  retain
                u2_noun b,                                        //  retain
                u2_noun c,                                        //  retain
                u2_noun d)                                        //  retain
{
  if ( c3__rock != u2_rail_hip_m(ral_r) ) {
    return u2_none;
  } else {
    u2_ray  sop_r = u2_rail_rut_r(ral_r);
    u2_ray  lot_r = u2_soup_lot_r(sop_r);
    u2_noun pro   = u2_cs_find_qual(ral_r, lot_r, fun_m, a, b, c, d);

    return u2_rx(ral_r, pro);
  }
}

/* u2_rl_save():
**
**   Cache store for function (0 means nock), sample and product.
*/
u2_noun                                                           //  transfer
u2_rl_save(u2_ray  ral_r,
           u2_mote fun_m,
           u2_noun sam,                                           //  retain
           u2_noun pro)                                           //  transfer
{
  if ( c3__rock != u2_rail_hip_m(ral_r) ) {
    return pro;
  } else {
    u2_ray sop_r = u2_rail_rut_r(ral_r);
    u2_ray lot_r = u2_soup_lot_r(sop_r);

    return u2_cs_save(ral_r, lot_r, fun_m, sam, pro);
  }
}

/* u2_rl_uniq():
**
**   Use cache to render object unique.
*/
u2_noun                                                            //  produce
u2_rl_uniq(u2_ray  ral_r,
           u2_noun som)                                            //  submit
{
  if ( c3__rock != u2_rail_hip_m(ral_r) ) {
    return som;
  } else {
    u2_ray sop_r = u2_rail_rut_r(ral_r);
    u2_ray lot_r = u2_soup_lot_r(sop_r);

    return u2_cs_save(ral_r, lot_r, 1, som, som);
  }
}

/* u2_rl_save_cell():
**
**   As `u2_rl_save()`, for `[a b]`.
*/
u2_weak                                                           //  transfer
u2_rl_save_cell(u2_ray  ral_r,
                u2_mote fun_m,
                u2_noun a,                                        //  retain
                u2_noun b,                                        //  retain
                u2_noun pro)                                      //  transfer
{
  if ( c3__rock != u2_rail_hip_m(ral_r) ) {
    return pro;
  } else {
    u2_ray sop_r = u2_rail_rut_r(ral_r);
    u2_ray lot_r = u2_soup_lot_r(sop_r);

    return u2_cs_save_cell(ral_r, lot_r, fun_m, a, b, pro);
  }
}

/* u2_rl_save_trel():
**
**   As `u2_rl_save()`, for `[a b c]`.
*/
u2_weak                                                           //  transfer
u2_rl_save_trel(u2_ray  ral_r,
                u2_mote fun_m,
                u2_noun a,                                        //  retain
                u2_noun b,                                        //  retain
                u2_noun c,                                        //  retain
                u2_noun pro)                                      //  transfer
{
  if ( c3__rock != u2_rail_hip_m(ral_r) ) {
    return pro;
  } else {
    u2_ray sop_r = u2_rail_rut_r(ral_r);
    u2_ray lot_r = u2_soup_lot_r(sop_r);

    return u2_cs_save_trel(ral_r, lot_r, fun_m, a, b, c, pro);
  }
}

/* u2_rl_save_qual():
**
**   As `u2_rl_save()`, for `[a b c d]`.
*/
u2_weak                                                           //  transfer
u2_rl_save_qual(u2_ray  ral_r,
                u2_mote fun_m,
                u2_noun a,                                        //  retain
                u2_noun b,                                        //  retain
                u2_noun c,                                        //  retain
                u2_noun d,                                        //  retain
                u2_noun pro)                                      //  transfer
{
  if ( c3__rock != u2_rail_hip_m(ral_r) ) {
    return pro;
  } else {
    u2_ray sop_r = u2_rail_rut_r(ral_r);
    u2_ray lot_r = u2_soup_lot_r(sop_r);

    return u2_cs_save_qual(ral_r, lot_r, fun_m, a, b, c, d, pro);
  }
}
