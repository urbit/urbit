/* f/meme.c
**
** This file is in the public domain.
*/
#include "all.h"
#include <sys/uio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sigsegv.h>
#include <termios.h>
#include <uv.h>
#include "v/vere.h"

/* _me_boot_north(): install a north road.
*/
static u2_road*
_me_boot_north(c3_w* mem_w, c3_w len_w)
{
  c3_w*    rut_w = mem_w;
  c3_w*    hat_w = rut_w;
  c3_w*    mat_w = ((mem_w + len_w) - c3_wiseof(u2_road));
  c3_w*    cap_w = mat_w;
  u2_road* rod_u = (void*) mat_w;

  memset(rod_u, 0, sizeof(u2_road));

  rod_u->rut_w = rut_w;
  rod_u->hat_w = hat_w;
 
  rod_u->mat_w = mat_w;
  rod_u->cap_w = cap_w;
  
  return rod_u;
}

/* _me_boot_south(): install a south road.
*/
static u2_road*
_me_boot_south(c3_w* mem_w, c3_w len_w)
{
  c3_w*    rut_w = mem_w;
  c3_w*    hat_w = rut_w;
  c3_w*    mat_w = ((mem_w + len_w) - c3_wiseof(u2_road));
  c3_w*    cap_w = mat_w;
  u2_road* rod_u = (void*) mat_w;

  memset(rod_u, 0, sizeof(u2_road));

  rod_u->rut_w = rut_w;
  rod_u->hat_w = hat_w;
 
  rod_u->mat_w = mat_w;
  rod_u->cap_w = cap_w;
  
  return rod_u;
}

/* u2_me_boot(): make u2R and u2H from nothing.
*/
void
u2_me_boot(void* mem_v, c3_w len_w)
{
  u2H = u2R = _me_boot_north(mem_v, len_w);
}

/* _me_road_all_hat(): in u2R, allocate directly on the hat.
*/
static c3_w*
_me_road_all_hat(c3_w len_w)
{
  if ( len_w > u2_me_open ) {
    return u2_me_bail(c3__meme);
  }

  if ( u2_yes == u2_me_is_north ) {
    c3_w* all_w;
     
    all_w = u2R->hat;
    u2R->hat += len_w;
    return all_w;
  }  
  else {
    u2R->hat -= len_w;
    return u2R->hat;
  }
}

/* _me_road_all_cap(): in u2R, allocate directly on the cap.
*/
static c3_w*
_me_road_all_hat(c3_w len_w)
{
  if ( len_w > u2_me_open ) {
    return u2_me_bail(c3__meme);
  }

  if ( u2_yes == u2_me_is_north ) {
    u2R->cap -= len_w;
    return u2R->cap;
  }  
  else {
    c3_w* all_w;
     
    all_w = u2R->cap;
    u2R->cap += len_w;
    return all_w;
  }
}

/* u2_me_leap(): in u2R, create a new road within the existing one.
*/
void
u2_me_leap()
{
  u2_road* rod_u;

  if ( u2_yes == u2_me_is_north ) {
    rod_u = _me_boot_south(u2R->hat_w, (u2R->cap_w - u2R->hat_w));
  } 
  else {
    rod_u = _me_boot_south(u2R->cap_w, (u2R->hat_w - u2R->cap_w));
  }

  c3_assert(0 == u2R->kid_u);
  rod_u->par_u = u2R;
  u2R->kid_u = rod_u;
  u2R = rod_u;
}

/* u2_me_fall(): in u2R, return an inner road to its parent.
*/
void
u2_me_fall()
{
  c3_assert(0 != u2R->par_u);

  u2R->par_u->cap_w = u2R->hat_u;
  u2R = u2R->par_u;
}


/* u2_me_golf(): record cap length for u2_flog().
*/
c3_w
u2_me_golf(void)
{
  if ( u2_yes == u2_me_is_north ) {
    return u2R->mat - u2R->cap;
  } 
  else {
    return u2R->cap - u2R->mat;
  }
}

/* u2_me_flog(): reset cap.
*/
void
u2_me_flog(c3_w gof_w)
{
  if ( u2_yes == u2_me_is_north ) {
    u2R->cap = u2R->mat - gof_w;
  } else {
    u2R->cap = u2R->mat + gof_w;
  }
}

/* u2_me_water(): produce watermarks.
*/
void
u2_me_water(c3_w* low_w, c3_w* hig_w);
{
  c3_assert(u2R == u2H);

  *low_w = (u2H->hat_w - u2H->rut_w);
  *hig_w = (u2H->mat_w - u2H->cap_w) + c3_wiseof(u2_road);
}

/* _me_box_slot(): select the right free list to search for a block.
*/
c3_w
_me_box_slot(c3_w siz_w)
{
  if ( siz_w < 8 ) {
    return 0;
  }
  else {
    c3_w i_w = 1;

    while ( 1 ) {
      if ( i_w == u2_me_free_no ) {
        return (i_w - 1);
      }
      if ( siz_w < 16 ) {
        return i_w;
      }
      siz_w = (siz_w + 1) >> 1;
      i_w += 1;
    }
  }
}

/* _me_box_make(): construct a box.
*/
u2_me_box*
_me_box_make(c3_v* box_v, c3_w siz_w, c3_w use_w)
{
  u2_me_box* box_u = box_v;
  c3_w*      box_w = box_v;

  box_w[0] = siz_w;
  box_w[siz_w - 1] = siz_w;

#ifdef  U2_MEMORY_DEBUG
    box_u->cod_w = COD_w;
#endif
  box_u->use_w = use_w;

  return box_u;
}

/* _me_box_attach(): attach a box to the free list.
*/
void*
_me_box_attach(u2_me_box* box_u)
{
  c3_assert(box_u->siz_w >= (1 + c3_wiseof(u2_me_free)));
  {
    c3_w sel_w         = _me_box_slot(box_u->siz_w);
    u2_me_free* fre_u  = (void *)box_u;
    u2_me_free** pfr_u = &u2R->all.fre_u[sel_w];

    fre_u->pre_u = 0;
    fre_u->nex_u = (*pfr_u);
    (*pfr_u) = fre_u;
  }
}

/* u2_me_walloc(): allocate storage words on hat.
*/
void*
u2_me_walloc(c3_w len_w)
{
  c3_w siz_w = c3_max(u2_me_minimum, u2_me_boxed(len_w));
  c3_w sel_w = _me_box_slot(siz_w);

  //  XX: this logic is totally bizarre, but preserve it.
  //
  if ( (sel_w != 0) && (sel_w != u2_me_free_no - 1) ) {
    sel_w += 1;
  }

  while ( 1 ) {
    u2_me_free** pfr_u = &u2R->all.fre_u[sel_w];

    while ( 1 ) {
      if ( 0 == *pfr_u ) {
        if ( sel_w < (u2_me_free_no - 1) ) {
          sel_w += 1;
          break;
        }
        else {
          /* Nothing in top free list.  Chip away at the hat.
          */
          return u2_me_boxto(_me_box_make(_me_road_all_hat(siz_w), siz_w, 1));
        }
      }
      else {
        if ( siz_w > (*pfr_u)->box_u->siz_w ) {
          /* This free block is too small.  Continue searching.
          */
          pfr_u = &((*pfr_u)->nex_u);
          continue;
        } 
        else {
          u2_me_box* box_u = &((*pfr_u)->box_u);

          /* We have found a free block of adequate size.  Remove it
          ** from the free list.
          */
          {
            {
              c3_assert((0 == (*pfr_u)->pre_u) || 
                        (*pfr_u)->pre_u->nex_u == (*pfr_u));
              c3_assert((0 == (*pfr_u)->nex_u) || 
                        (*pfr_u)->nex_u->pre_u == (*pfr_u));
            }
            if ( 0 != (*pfr_u)->nex_u ) {
              (*pfr_u)->nex_u->pre_u = (*pfr_u)->pre_u;
            }
            *pfr_u = (*pfr_u)->nex_u;
          }

          /* If we can chop off another block, do it.
          */
          if ( (siz_w + c3_wiseof(u2_me_free) + 1) <= box_u->siz_w ) {
            /* Split the block.
            */ 
            c3_w* box_w = ((c3_w *)(void *)box_uy);
            c3_w* end_w = box_w + siz_w;
            c3_w  lef_w = (box_u->siz_w - siz_w);

            _me_box_attach(_me_box_make(end_w, lef_w, 0));
            return u2_me_boxto(_me_box_make(box_w, siz_w, 1));
          }
          else {
            c3_assert(0 == box_u->use_w);
#ifdef      U2_MEMORY_DEBUG
              box_u->cod_w = COD_w;
#endif
            return u2_me_boxto(box_u);
        }
      }
    }
  }
}


/* u2_me_malloc(): allocate storage measured in bytes.
*/
void*
u2_me_malloc(c3_w len_w);

/* u2_me_free(): free storage.
*/
void
u2_me_free(void* lag_v);
{
  c3_w* lag_w = lag_v;

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

//////// Atoms from proto-atoms.

      /* Basic allocation.
      */
        /* u2_me_walloc(): allocate storage measured in words.
        */
          void*
          u2_me_walloc(c3_w len_w);

        /* u2_me_malloc(): allocate storage measured in bytes.
        */
          void*
          u2_me_malloc(c3_w len_w);

        /* u2_me_free(): free storage.
        */
          void
          u2_me_free(void* lag_v);



/* u2_me_slab(): create a length-bounded proto-atom.
*/
c3_w*
u2_me_slab(c3_w len_w)
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

/* u2_me_slaq(): u2_me_slaq() with a defined blocksize.
*/
c3_w*
u2_me_slaq(c3_g met_g, c3_w len_w);

/* u2_me_malt(): measure and finish a proto-atom.
*/
u2_noun
u2_me_malt(c3_w* sal_w);

/* u2_me_moot(): finish a pre-measured proto-atom; dangerous.
*/
u2_noun
u2_me_moot(c3_w* sal_w);

/* u2_me_mint(): finish a measured proto-atom.
*/
u2_noun
u2_me_mint(c3_w* sal_w, c3_w len_w);
