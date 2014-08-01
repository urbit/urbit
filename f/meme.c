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
static void
_me_boot_north(void* mem_v, c3_w len_w)
{
  for 
}

/* _me_boot_south(): install a south road.
/* u2_me_boot(): make u2R and u2H from nothing.
*/
void
u2_me_boot(void* mem_v, c3_w len_w)
{
  c3_w* mem_w = mem_v;

  u2H = (u2_road*) mem_v;
  memset(u2H, 0, sizeof(u2_road));

  u2H->rut_w = (mem_w + c3_sizeof(u2_road));
  u2H->hat_w = u2H->rut_w;

  u2H->mat_w = (mem_w + len_w);
  u2H->cap_w = u2H->mat_w;

  u2R = u2H;
}

/* u2_me_leap(): advance to inner road.
*/
void
u2_me_leap(void)
{















/* _me_free_slot(): select the right free list to search for a block.
*/
c3_w
_me_free_slot(c3_w siz_w)
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

/* u2_me_walloc(): allocate storage measured in words.
*/
void*
u2_me_walloc(c3_w len_w)
{
  c3_w siz_w = c3_max(u2_me_minimum, u2_me_boxed(len_w));
  c3_w sel_w = _me_free_slot(siz_w);

  if ( (sel_w != 0) && (sel_w != u2_me_free_no - 1) ) {
    sel_w += 1;
  }

  while ( 1 ) {
    u2_me_free* pfr_u = u2R->all.fre_u[sel_w];
    c3_w*       box_w;

    if ( 0 == pfr_u ) {
      if ( sel_w < (u2_me_free_no - 1) ) {
        sel_w += 1;
        break;
      }
      else {
        /* Nothing in top free list.  Chip away at the hat.
        */
        if ( u2_no == u2_me_open(siz_w) ) {
          u2_loop_signal_memory();
        }
        else {
          box_w = u2R->lay.hat_w;
          u2R->lay.hat_w += siz_w;

          _me_box_make(

              box_r = u2_rail_hat_r(ral_r);
              u2_rail_hat_r(ral_r) += siz_w;

              _rl_bloq_make(ral_r, box_r, siz_w, 1);
              _rl_live_grab(ral_r, siz_w);

              _rl_bloq_cheq(box_r);
              return (box_r + c3_wiseof(u2_loom_rail_box));
        }
      }
    }
  }
      pfr_r = u2_aftr(sop_r, u2_loom_soup, fre_r) + sel_w;

      while ( 1 ) {
        u2_ray fre_r = *u2_at_ray(pfr_r);
        u2_ray box_r;

        if ( 0 == fre_r ) {
          if ( sel_w < (u2_me_free_no - 1) ) {
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
#             ifdef  U2_LEAK_DEBUG
                *u2_at_ray(box_r + 2) = COD_w;
#             endif

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
  return;
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
