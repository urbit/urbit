/* f/meme.c
**
** This file is in the public domain.
*/
#define C3_GLOBAL

  // delete me
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <dirent.h>
#include <stdint.h>
#include <uv.h>
#include <curses.h>
#include <termios.h>
#include <term.h>
#include <errno.h>

#include "f/meme.h"
#include "f/pork.h"
#include "../gen164/pit.h"

  /**  Jet dependencies.  Minimize these.
  **/
#   define Pt5Y   k_164__mood__hoon

  /**  Jet dependencies.  Minimize these.
  **/
#   define Pt3Y   k_164__mood__hoon
#   define Pt4Y   k_164__mood__hoon
#   define Pt5Y   k_164__mood__hoon

    u2_noun j2_mbc(Pt3Y, gor)(u2_noun a, u2_noun b);
    u2_noun j2_mcc(Pt4Y, by, get)(u2_noun a, u2_noun b);
    u2_noun j2_mcc(Pt4Y, by, put)(u2_noun a, u2_noun b, u2_noun c);
    u2_noun j2_mby(Pt5Y, cue)(u2_noun a);
    u2_noun j2_mby(Pt5Y, jam)(u2_noun a);
    u2_noun j2_mby(Pt5Y, trip)(u2_noun a);

#   define _coal_cue  j2_mby(Pt5Y, cue)
#   define _coal_jam  j2_mby(Pt5Y, jam)
#   define _coal_trip j2_mby(Pt5Y, trip)

#   define _coal_gor  j2_mbc(Pt3Y, gor)
#   define _coal_by_gas  j2_mcc(Pt4Y, by, gas)
#   define _coal_by_get  j2_mcc(Pt4Y, by, get)
#   define _coal_by_has  j2_mcc(Pt4Y, in, has)
#   define _coal_by_put  j2_mcc(Pt4Y, by, put)
#   define _coal_in_gas  j2_mcc(Pt4Y, in, gas)
#   define _coal_in_has  j2_mcc(Pt4Y, in, has)
#   define _coal_in_tap  j2_mcc(Pt4Y, in, tap)



/* _boot_north(): install a north road.
*/
static u2_road*
_boot_north(c3_w* mem_w, c3_w len_w)
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

/* _boot_south(): install a south road.
*/
static u2_road*
_boot_south(c3_w* mem_w, c3_w len_w)
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

/* u2_cm_boot(): make u2R and u2H from nothing.
*/
void
u2_cm_boot(c3_p adr_p, c3_w len_w)
{
  void* map_v;

  map_v = mmap((void *)(c3_p)adr_p,
               (len_w << 2),
               PROT_READ | PROT_WRITE,
               (MAP_ANON | MAP_FIXED | MAP_PRIVATE),
               -1, 0);

  if ( -1 == (c3_ps)map_v ) {
    map_v = mmap((void *)0,
                 (len_w << 2),
                  PROT_READ | PROT_WRITE,
                  MAP_ANON | MAP_PRIVATE,
                  -1, 0);

    if ( -1 == (c3_ps)map_v ) {
      fprintf(stderr, "map failed twice\n");
    } else {
      fprintf(stderr, "map failed - try U2_OS_LoomBase %p\n", map_v);
    }
    exit(1);
  }
  printf("loom: mapped %dMB\n", (len_w >> 18));
  u2L = map_v;
  u2H = u2R = _boot_north(map_v, len_w);
}

/* _me_road_all_hat(): in u2R, allocate directly on the hat_w.
*/
static c3_w*
_me_road_all_hat(c3_w len_w)
{
  if ( len_w > u2_co_open ) {
    u2_cm_bail(c3__meme); return 0;
  }

  if ( u2_yes == u2_co_is_north ) {
    c3_w* all_w;
     
    all_w = u2R->hat_w;
    u2R->hat_w += len_w;
    return all_w;
  }  
  else {
    u2R->hat_w -= len_w;
    return u2R->hat_w;
  }
}

#if 0  // not yet used
/* _me_road_all_cap(): in u2R, allocate directly on the cap.
*/
static c3_w*
_me_road_all_cap(c3_w len_w)
{
  if ( len_w > u2_co_open ) {
    u2_cm_bail(c3__meme); return 0;
  }

  if ( u2_yes == u2_co_is_north ) {
    u2R->cap_w -= len_w;
    return u2R->cap_w;
  }  
  else {
    c3_w* all_w;
     
    all_w = u2R->cap_w;
    u2R->cap_w += len_w;
    return all_w;
  }
}
#endif

static void
_road_dump(void)
{
  c3_w hat_w;
  c3_w fre_w = 0;
  c3_w i_w;

  hat_w = u2_so(u2_co_is_north) ? u2R->hat_w - u2R->rut_w 
                                : u2R->hat_w - u2R->rut_w;

  for ( i_w = 0; i_w < u2_cc_fbox_no; i_w++ ) {
    u2_cs_fbox* fre_u = u2R->all.fre_u[i_w];
    
    while ( fre_u ) {
      fre_w += fre_u->box_u.siz_w;
      fre_u = fre_u->nex_u;
    }
  }
  printf("dump: hat_w %x, fre_w %x, allocated %x\n",
          hat_w, fre_w, (hat_w - fre_w));

  if ( 0 != (hat_w - fre_w) ) {
    c3_w* box_w = u2R->rut_w;
    c3_w  mem_w = 0;

    while ( box_w < u2R->hat_w ) {
      u2_cs_box* box_u = (void *)box_w;

      if ( 0 != box_u->use_w ) {
        mem_w += box_u->siz_w;
      }
      box_w += box_u->siz_w;
    }

    printf("second count: %x\n", mem_w);
  }
}

#if 1
static void
_road_sane(void)
{
  c3_w i_w;
   
  for ( i_w = 0; i_w < u2_cc_fbox_no; i_w++ ) {
    u2_cs_fbox* fre_u = u2R->all.fre_u[i_w];
    
    while ( fre_u ) {
      if ( fre_u == u2R->all.fre_u[i_w] ) {
        c3_assert(fre_u->pre_u == 0);
      }
      else {
        c3_assert(fre_u->pre_u != 0);
        c3_assert(fre_u->pre_u->nex_u == fre_u);
        if ( fre_u->nex_u != 0 ) {
          c3_assert(fre_u->nex_u->pre_u == fre_u);
        }
      }
      fre_u = fre_u->nex_u;
    }
  }
}
#endif

/* u2_cm_bail(): bail out.  Does not return.
**
**  Bail motes:
**
**    %evil               ::  erroneous cryptography
**    %exit               ::  semantic failure
**    %oops               ::  assertion failure
**    %intr               ::  interrupt
**    %fail               ::  computability failure
**    %need               ::  namespace block
**    %meme               ::  out of memory
*/ 
c3_i
u2_cm_bail(c3_m how_m)
{
  c3_c str_c[5];

  str_c[0] = ((how_m >> 0) & 0xff);
  str_c[1] = ((how_m >> 8) & 0xff);
  str_c[2] = ((how_m >> 16) & 0xff);
  str_c[3] = ((how_m >> 24) & 0xff);
  str_c[4] = 0;
  printf("bail: %s\n", str_c);

  assert(0);
  if ( c3__meme == how_m ) {
    _road_dump();
  }

  _longjmp(u2R->esc.buf, how_m);
  return how_m;
}

/* u2_cm_error(): bail out with %exit, ct_pushing error.
*/
c3_i
u2_cm_error(c3_c* str_c)
{
  printf("error: %s\n", str_c);   // rong
  return u2_cm_bail(c3__exit);
}

/* u2_cm_leap(): in u2R, create a new road within the existing one.
*/
void
u2_cm_leap()
{
  u2_road* rod_u;

  if ( u2_yes == u2_co_is_north ) {
    rod_u = _boot_south(u2R->hat_w, (u2R->cap_w - u2R->hat_w));
  } 
  else {
    rod_u = _boot_north(u2R->cap_w, (u2R->hat_w - u2R->cap_w));
  }

  c3_assert(0 == u2R->kid_u);
  rod_u->par_u = u2R;
  u2R->kid_u = rod_u;
  u2R = rod_u;
}

/* u2_cm_fall(): in u2R, return an inner road to its parent.
*/
void
u2_cm_fall()
{
  c3_assert(0 != u2R->par_u);

  u2R->par_u->cap_w = u2R->hat_w;
  u2R = u2R->par_u;
}

/* u2_cm_golf(): record cap_w length for u2_flog().
*/
c3_w
u2_cm_golf(void)
{
  if ( u2_yes == u2_co_is_north ) {
    return u2R->mat_w - u2R->cap_w;
  } 
  else {
    return u2R->cap_w - u2R->mat_w;
  }
}

/* u2_cm_flog(): reset cap_w.
*/
void
u2_cm_flog(c3_w gof_w)
{
  if ( u2_yes == u2_co_is_north ) {
    u2R->cap_w = u2R->mat_w - gof_w;
  } else {
    u2R->cap_w = u2R->mat_w + gof_w;
  }
}

/* u2_cm_water(): produce watermarks.
*/
void
u2_cm_water(c3_w* low_w, c3_w* hig_w)
{
  c3_assert(u2R == u2H);

  *low_w = (u2H->hat_w - u2H->rut_w);
  *hig_w = (u2H->mat_w - u2H->cap_w) + c3_wiseof(u2_road);
}

/* _box_slot(): select the right free list to search for a block.
*/
c3_w
_box_slot(c3_w siz_w)
{
  if ( siz_w < 8 ) {
    return 0;
  }
  else {
    c3_w i_w = 1;

    while ( 1 ) {
      if ( i_w == u2_cc_fbox_no ) {
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

/* _box_make(): construct a box.
*/
u2_cs_box*
_box_make(void* box_v, c3_w siz_w, c3_w use_w)
{
  u2_cs_box* box_u = box_v;
  c3_w*      box_w = box_v;

  box_w[0] = siz_w;
  box_w[siz_w - 1] = siz_w;
  box_u->use_w = use_w;

# ifdef  U2_MEMORY_DEBUG
    box_u->cod_w = COD_w;
# endif

  return box_u;
}

/* _box_attach(): attach a box to the free list.
*/
void
_box_attach(u2_cs_box* box_u)
{
  c3_assert(box_u->siz_w >= (1 + c3_wiseof(u2_cs_fbox)));

  {
    c3_w sel_w         = _box_slot(box_u->siz_w);
    u2_cs_fbox* fre_u  = (void *)box_u;
    u2_cs_fbox** pfr_u = &u2R->all.fre_u[sel_w];
    u2_cs_fbox* nex_u  = *pfr_u;

    fre_u->pre_u = 0;
    fre_u->nex_u = nex_u;
    if ( fre_u->nex_u ) {
      fre_u->nex_u->pre_u = fre_u;
    }
    (*pfr_u) = fre_u;
  }
}

/* _box_detach(): detach a box from the free list.
*/
void
_box_detach(u2_cs_box* box_u)
{
  u2_cs_fbox* fre_u = (void*) box_u;
  u2_cs_fbox* pre_u = fre_u->pre_u;
  u2_cs_fbox* nex_u = fre_u->nex_u;

  if ( nex_u ) {
    c3_assert(nex_u->pre_u == fre_u);
    nex_u->pre_u = pre_u;
  }
  if ( pre_u ) {
    c3_assert(pre_u->nex_u == fre_u);
    pre_u->nex_u = nex_u;
  }
  else {
    c3_w sel_w = _box_slot(box_u->siz_w);

    c3_assert(fre_u == u2R->all.fre_u[sel_w]);
    u2R->all.fre_u[sel_w] = nex_u;
  }
}

/* u2_ca_walloc(): allocate storage words on hat_w.
*/
void*
u2_ca_walloc(c3_w len_w)
{
  c3_w siz_w = c3_max(u2_cc_minimum, u2_co_boxed(len_w));
  c3_w sel_w = _box_slot(siz_w);

  //  XX: this logic is totally bizarre, but preserve it.
  //
  if ( (sel_w != 0) && (sel_w != u2_cc_fbox_no - 1) ) {
    sel_w += 1;
  }

  while ( 1 ) {
    u2_cs_fbox** pfr_u = &u2R->all.fre_u[sel_w];

    while ( 1 ) {
      if ( 0 == *pfr_u ) {
        if ( sel_w < (u2_cc_fbox_no - 1) ) {
          sel_w += 1;
          break;
        }
        else {
          /* Nothing in top free list.  Chip away at the hat_w.
          */
          return u2_co_boxto(_box_make(_me_road_all_hat(siz_w), siz_w, 1));
        }
      }
      else {
        if ( siz_w > (*pfr_u)->box_u.siz_w ) {
          /* This free block is too small.  Continue searching.
          */
          pfr_u = &((*pfr_u)->nex_u);
          continue;
        } 
        else {
          u2_cs_box* box_u = &((*pfr_u)->box_u);

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
          if ( (siz_w + c3_wiseof(u2_cs_fbox) + 1) <= box_u->siz_w ) {
            /* Split the block.
            */ 
            c3_w* box_w = ((c3_w *)(void *)box_u);
            c3_w* end_w = box_w + siz_w;
            c3_w  lef_w = (box_u->siz_w - siz_w);

            _box_attach(_box_make(end_w, lef_w, 0));
            return u2_co_boxto(_box_make(box_w, siz_w, 1));
          }
          else {
            c3_assert(0 == box_u->use_w);
            box_u->use_w = 1;

#ifdef      U2_MEMORY_DEBUG
              box_u->cod_w = COD_w;
#endif
            return u2_co_boxto(box_u);
          }
        }
      }
    }
  }
}

/* u2_ca_malloc(): allocate storage measured in bytes.
*/
void*
u2_ca_malloc(c3_w len_w)
{
  return u2_ca_walloc((len_w + 3) >> 2);
}

/* u2_ca_wealloc(): realloc in words.
*/
void*
u2_ca_wealloc(void* lag_v, c3_w len_w)
{
  if ( !lag_v ) {
    return u2_ca_malloc(len_w);
  } 
  else {
    u2_cs_box* box_u = u2_co_botox(lag_v);
    c3_w*      old_w = lag_v;
    c3_w       tiz_w = c3_min(box_u->siz_w, len_w);
    {
      c3_w* new_w = u2_ca_walloc(len_w);
      c3_w  i_w;

      for ( i_w = 0; i_w < tiz_w; i_w++ ) {
        new_w[i_w] = old_w[i_w];
      }
      u2_ca_free(lag_v);
      return new_w;
    }
  }
}

/* u2_ca_realloc(): realloc in bytes.
*/
void*
u2_ca_realloc(void* lag_v, c3_w len_w)
{
  return u2_ca_wealloc(lag_v, (len_w + 3) >> 2);
}

/* u2_ca_free(): free storage.
*/
void
u2_ca_free(void* tox_v)
{
  u2_cs_box* box_u = u2_co_botox(tox_v);
  c3_w*      box_w = (c3_w *)(void *)box_u;

  c3_assert(box_u->use_w != 0);
  box_u->use_w -= 1;
  if ( 0 != box_u->use_w ) return;

  c3_assert(u2_yes == u2_co_is_north);
#if 0
  /* Clear the contents of the block, for debugging.
  */
  {
    c3_w i_w;

    for ( i_w = c3_wiseof(u2_cs_box); (i_w + 1) < box_u->siz_w; i_w++ ) {
      box_w[i_w] = 0xdeadbeef;
    }
  }
#endif

  if ( u2_yes == u2_co_is_north ) {
    /* Try to coalesce with the block below.
    */
    if ( box_w != u2R->rut_w ) {
      c3_w       laz_w = *(box_w - 1);
      u2_cs_box* pox_u = (u2_cs_box*)(void *)(box_w - laz_w);

      if ( 0 == pox_u->use_w ) {
        _box_detach(pox_u);
        _box_make(pox_u, (laz_w + box_u->siz_w), 0);

        box_u = pox_u;
        box_w = (c3_w*)(void *)pox_u;
      }
    }

    /* Try to coalesce with the block above, or the wilderness.
    */
    if ( (box_w + box_u->siz_w) == u2R->hat_w ) {
      u2R->hat_w = box_w;
    }
    else {
      u2_cs_box* nox_u = (u2_cs_box*)(void *)(box_w + box_u->siz_w);

      if ( 0 == nox_u->use_w ) {
        _box_detach(nox_u);
        _box_make(box_u, (box_u->siz_w + nox_u->siz_w), 0);
      }
      _box_attach(box_u);
    }
  }
  else {
    /* Try to coalesce with the block above.
    */
    if ( (box_w + box_u->siz_w) != u2R->rut_w ) {
      u2_cs_box* nox_u = (u2_cs_box*)(void *)(box_w + box_u->siz_w);

      if ( 0 == nox_u->use_w ) {
        _box_detach(nox_u);
        _box_make(box_u, (box_u->siz_w + nox_u->siz_w), 0);

        box_u = nox_u;
        box_w = (c3_w*)(void *)nox_u;
      }
    }

    /* Try to coalesce with the block below, or with the wilderness.
    */
    if ( box_w == u2R->hat_w ) {
      u2R->hat_w = (box_w + box_u->siz_w);
    }
    else {
      c3_w laz_w = *(box_w - 1);
      u2_cs_box* pox_u = (u2_cs_box*)(void *)(box_w - laz_w);

      if ( 0 == pox_u->use_w ) {
        _box_detach(pox_u);
        _box_make(pox_u, (laz_w + box_u->siz_w), 0);
      }
      _box_attach(box_u);
    }
  }
}

/* _me_north_is_senior(): yes iff only in the senior region.
*/
static u2_bean
_me_north_is_senior(u2_noun dog)
{
  c3_w* dog_w = u2_co_to_ptr(dog);

  return u2_say((dog_w < u2R->rut_w) || (dog_w >= u2R->mat_w));
}

/* _me_north_is_junior(): yes iff only in the junior section.
*/
static u2_bean
_me_north_is_junior(u2_noun dog)
{
  c3_w* dog_w = u2_co_to_ptr(dog);

  return u2_say((dog_w >= u2R->cap_w) && (dog_w < u2R->mat_w));
}
/* _me_north_is_normal(): yes iff only in the normal heap.
*/
static u2_bean
_me_north_is_normal(u2_noun dog)
{
  return u2_and(u2_not(_me_north_is_senior(dog)), 
                u2_not(_me_north_is_junior(dog)));
}

/* _me_south_is_senior(): yes iff only in the senior region.
*/
static u2_bean
_me_south_is_senior(u2_noun dog)
{
  c3_w* dog_w = u2_co_to_ptr(dog);

  return u2_say((dog_w >= u2R->mat_w) || (dog_w < u2R->cap_w));
}
/* _me_south_is_junior(): yes iff only in the junior section.
*/
static u2_bean
_me_south_is_junior(u2_noun dog)
{
  c3_w* dog_w = u2_co_to_ptr(dog);

  return u2_say((dog_w >= u2R->cap_w) && (dog_w < u2R->mat_w));
}
/* _me_south_is_normal(): yes iff only in the normal heap.
*/
static u2_bean
_me_south_is_normal(u2_noun dog)
{
  return u2_and(u2_not(_me_south_is_senior(dog)), 
                u2_not(_me_south_is_junior(dog)));
}

/* u2_co_is_junior(): yes iff (som) is junior.
*/
u2_bean
u2_co_is_junior(u2_noun som)
{
  if ( u2_so(u2_co_is_cat(som)) ) {
    return u2_no;
  }
  else { 
    if ( u2_so(u2_co_is_north) ) {
      return _me_north_is_junior(som);
    } else {
      return _me_south_is_junior(som);
    }
  }
}

/* _me_wash_north(): clean up mug slots after copy.
*/
static void _me_wash_north(u2_noun dog);
static void
_me_wash_north_in(u2_noun som)
{
  if ( u2_so(u2_co_is_cat(som)) ) return;
  if ( u2_ne(_me_north_is_junior(som)) ) return;

  _me_wash_north(som);
}
static void
_me_wash_north(u2_noun dog)
{
  c3_assert(u2_co_is_dog(dog));
  c3_assert(u2_yes == _me_north_is_junior(dog));
  {
    u2_cs_noun* dog_u = u2_co_to_ptr(dog);

    if ( dog_u->mug_w >> 31 ) { dog_u->mug_w = 0; }

    if ( u2_so(u2_co_is_pom(dog)) ) {
      u2_cs_cell* god_u = (u2_cs_cell *)(void *)dog_u;
    
      _me_wash_north_in(god_u->hed);
      _me_wash_north_in(god_u->tel);
    }
  } 
}

/* _me_wash_south(): clean up mug slots after copy.
*/
static void _me_wash_south(u2_noun dog);
static void
_me_wash_south_in(u2_noun som)
{
  if ( u2_so(u2_co_is_cat(som)) ) return;
  if ( u2_ne(_me_south_is_junior(som)) ) return;

  _me_wash_south(som);
}
static void
_me_wash_south(u2_noun dog)
{
  c3_assert(u2_co_is_dog(dog));
  c3_assert(u2_yes == _me_south_is_junior(dog));
  {
    u2_cs_noun* dog_u = u2_co_to_ptr(dog);

    if ( dog_u->mug_w >> 31 ) { dog_u->mug_w = 0; }

    if ( u2_so(u2_co_is_pom(dog)) ) {
      u2_cs_cell* god_u = (u2_cs_cell *)(void *)dog_u;
    
      _me_wash_south_in(god_u->hed);
      _me_wash_south_in(god_u->tel);
    }
  } 
}

/* _me_gain_use(): increment use count.
*/
static void
_me_gain_use(u2_noun dog)
{
  c3_w* dog_w      = u2_co_to_ptr(dog);
  u2_cs_box* box_u = u2_co_botox(dog_w);

  if ( 0xffffffff == box_u->use_w ) {
    u2_cm_bail(c3__fail);
  }
  else {
    box_u->use_w += 1;
  }
}

/* _me_copy_north_in(): copy subjuniors on a north road.
*/
static u2_noun _me_copy_north(u2_noun);
static u2_noun
_me_copy_north_in(u2_noun som)
{
  c3_assert(u2_none != som);
  if ( u2_so(u2_co_is_cat(som)) ) {
    return som;
  }
  else { 
    u2_noun dog = som;

    if ( u2_so(_me_north_is_senior(dog)) ) {
      return dog;
    }
    else if ( u2_so(_me_north_is_junior(dog)) ) {
      return _me_copy_north(dog);
    }
    else {
      _me_gain_use(dog);
      return dog;
    }
  }
}
/* _me_copy_north(): copy juniors on a north road.
*/
static u2_noun
_me_copy_north(u2_noun dog)
{
  c3_assert(u2_yes == _me_north_is_junior(dog));

  if ( u2_ne(_me_north_is_junior(dog)) ) {
    if ( u2_ne(_me_north_is_senior(dog)) ) {
      _me_gain_use(dog);
    }
    return dog;
  } 
  else {
    u2_cs_noun* dog_u = u2_co_to_ptr(dog);

    /* Borrow mug slot to record new destination.
    */
    if ( dog_u->mug_w >> 31 ) {
      u2_noun nov = (u2_noun) dog_u->mug_w;

      c3_assert(u2_so(_me_north_is_normal(nov)));
      _me_gain_use(nov);

      return nov;
    }
    else {
      if ( u2_yes == u2_co_is_pom(dog) ) {
        u2_cs_cell* old_u = u2_co_to_ptr(dog);
        c3_w*       new_w = u2_ca_walloc(c3_wiseof(u2_cs_cell));
        u2_noun     new   = u2_co_de_twin(dog, new_w);
        u2_cs_cell* new_u = (u2_cs_cell*)(void *)new_w;

        new_u->mug_w = old_u->mug_w;
        new_u->hed = _me_copy_north_in(old_u->hed);
        new_u->tel = _me_copy_north_in(old_u->tel);

        /* Borrow mug slot to record new destination.
        */
        old_u->mug_w = new;
        return new;
      } 
      else {
        u2_cs_atom* old_u = u2_co_to_ptr(dog);
        c3_w*       new_w = u2_ca_walloc(old_u->len_w + c3_wiseof(u2_cs_atom));
        u2_noun     new   = u2_co_de_twin(dog, new_w);
        u2_cs_atom* new_u = (u2_cs_atom*)(void *)new_w;

        new_u->mug_w = old_u->mug_w;
        new_u->len_w = old_u->len_w;
        {
          c3_w i_w;

          for ( i_w=0; i_w < old_u->len_w; i_w++ ) {
            new_u->buf_w[i_w] = old_u->buf_w[i_w];
          }
        }

        /* Borrow mug slot to record new destination.
        */
        old_u->mug_w = new;
        return new;
      }
    }
  }
}

/* _me_copy_south_in(): copy subjuniors on a south road.
*/
static u2_noun _me_copy_south(u2_noun);
static u2_noun
_me_copy_south_in(u2_noun som)
{
  c3_assert(u2_none != som);
  if ( u2_so(u2_co_is_cat(som)) ) {
    return som;
  }
  else { 
    u2_noun dog = som;

    if ( u2_so(_me_south_is_senior(dog)) ) {
      return dog;
    }
    else if ( u2_so(_me_south_is_junior(dog)) ) {
      return _me_copy_south(dog);
    }
    else {
      _me_gain_use(dog);
      return dog;
    }
  }
}
/* _me_copy_south(): copy juniors on a south road.
*/
static u2_noun
_me_copy_south(u2_noun dog)
{
  c3_assert(u2_yes == _me_south_is_junior(dog));

  if ( u2_ne(_me_south_is_junior(dog)) ) {
    if ( u2_ne(_me_south_is_senior(dog)) ) {
      _me_gain_use(dog);
    }
    return dog;
  } 
  else {
    u2_cs_noun* dog_u = u2_co_to_ptr(dog);

    /* Borrow mug slot to record new destination.
    */
    if ( dog_u->mug_w >> 31 ) {
      u2_noun nov = (u2_noun) dog_u->mug_w;

      c3_assert(u2_so(_me_south_is_normal(nov)));
      _me_gain_use(nov);

      return nov;
    }
    else {
      if ( u2_yes == u2_co_is_pom(dog) ) {
        u2_cs_cell* old_u = u2_co_to_ptr(dog);
        c3_w*       new_w = u2_ca_walloc(c3_wiseof(u2_cs_cell));
        u2_noun     new   = u2_co_de_twin(dog, new_w);
        u2_cs_cell* new_u = (u2_cs_cell*)(void *)new_w;

        new_u->mug_w = old_u->mug_w;
        new_u->hed = _me_copy_south_in(old_u->hed);
        new_u->tel = _me_copy_south_in(old_u->tel);

        /* Borrow mug slot to record new destination.
        */
        old_u->mug_w = new;
        return new;
      } 
      else {
        u2_cs_atom* old_u = u2_co_to_ptr(dog);
        c3_w*       new_w = u2_ca_walloc(old_u->len_w + c3_wiseof(u2_cs_atom));
        u2_noun     new   = u2_co_de_twin(dog, new_w);
        u2_cs_atom* new_u = (u2_cs_atom*)(void *)new_w;

        new_u->mug_w = old_u->mug_w;
        new_u->len_w = old_u->len_w;
        {
          c3_w i_w;

          for ( i_w=0; i_w < old_u->len_w; i_w++ ) {
            new_u->buf_w[i_w] = old_u->buf_w[i_w];
          }
        }

        /* Borrow mug slot to record new destination.
        */
        old_u->mug_w = new;
        return new;
      }
    }
  }
}

/* _me_gain_north(): gain on a north road.
*/
static u2_noun
_me_gain_north(u2_noun dog)
{
  if ( u2_yes == _me_north_is_senior(dog) ) {
    /*  senior pointers are not refcounted
    */
    return dog;
  }
  else if ( u2_yes == _me_north_is_junior(dog) ) {
    /* junior pointers are copied
    */
    u2_noun mos = _me_copy_north(dog);

    _me_wash_north(dog);
    return mos;
  }
  else {
    /* normal pointers are refcounted
    */
    _me_gain_use(dog);
    return dog;
  }
}

/* _me_gain_south(): gain on a south road.
*/
static u2_noun
_me_gain_south(u2_noun dog)
{
  if ( u2_yes == _me_south_is_senior(dog) ) {
    /*  senior pointers are not refcounted
    */
    return dog;
  }
  else if ( u2_yes == _me_south_is_junior(dog) ) {
    /* junior pointers are copied
    */
    u2_noun mos = _me_copy_south(dog);

    _me_wash_south(dog);
    return mos;
  }
  else {
    /* normal pointers are refcounted
    */
    _me_gain_use(dog);
    return dog;
  }
}

/* _me_lose_north(): lose on a north road.
*/
static void
_me_lose_north(u2_noun dog)
{
top:
  if ( u2_yes == _me_north_is_normal(dog) ) {
    c3_w* dog_w      = u2_co_to_ptr(dog);
    u2_cs_box* box_u = u2_co_botox(dog_w);

    if ( box_u->use_w > 1 ) {
      box_u->use_w -= 1;
    }
    else {
      if ( 0 == box_u->use_w ) {
        u2_cm_bail(c3__foul);
      }
      else {
        if ( u2_so(u2_co_is_pom(dog)) ) {
          u2_cs_cell* dog_u = (void *)dog_w;
          u2_noun     h_dog = dog_u->hed;
          u2_noun     t_dog = dog_u->tel;

          if ( u2_ne(u2_co_is_cat(h_dog)) ) {
            _me_lose_north(h_dog);
          }
          u2_ca_free(dog_w);
          if ( u2_ne(u2_co_is_cat(t_dog)) ) {
            dog = t_dog;
            goto top;
          }
        }
        else {
          u2_ca_free(dog_w);
        }
      }
    }
  }
}

/* _me_lose_south(): lose on a south road.
*/
static void
_me_lose_south(u2_noun dog)
{
top:
  if ( u2_yes == _me_south_is_normal(dog) ) {
    c3_w* dog_w      = u2_co_to_ptr(dog);
    u2_cs_box* box_u = u2_co_botox(dog_w);

    if ( box_u->use_w > 1 ) {
      box_u->use_w -= 1;
    }
    else {
      if ( 0 == box_u->use_w ) {
        u2_cm_bail(c3__foul);
      }
      else {
        if ( u2_so(u2_co_is_pom(dog)) ) {
          u2_cs_cell* dog_u = (void *)dog_w;
          u2_noun     h_dog = dog_u->hed;
          u2_noun     t_dog = dog_u->tel;

          if ( u2_ne(u2_co_is_cat(h_dog)) ) {
            _me_lose_south(h_dog);
          }
          u2_ca_free(dog_w);
          if ( u2_ne(u2_co_is_cat(t_dog)) ) {
            dog = t_dog;
            goto top;
          }
        }
        else {
          u2_ca_free(dog_w);
        }
      }
    }
  }
}

/* u2_ca_gain(): gain a reference count, and/or copy juniors.
*/
u2_noun
u2_ca_gain(u2_noun som)
{
  c3_assert(u2_none != som);

  if ( u2_so(u2_co_is_cat(som)) ) {
    return som;
  }
  else {
    return u2_so(u2_co_is_north)
              ? _me_gain_north(som)
              : _me_gain_south(som);
  }
}

/* u2_ca_lose(): lose a reference count.
*/
void
u2_ca_lose(u2_noun som)
{
  if ( u2_ne(u2_co_is_cat(som)) ) {
    if ( u2_so(u2_co_is_north) ) {
      _me_lose_north(som);
    } else {
      _me_lose_south(som);
    }
  }
}

/* u2_ca_use(): reference count.
*/
c3_w
u2_ca_use(u2_noun som)
{
  if ( u2_so(u2_co_is_cat(som)) ) {
    return 1;
  } 
  else {
    c3_w* dog_w      = u2_co_to_ptr(som);
    u2_cs_box* box_u = u2_co_botox(dog_w);

    return box_u->use_w;
  }
}

/* u2_ca_slab(): create a length-bounded proto-atom.
*/
c3_w*
u2_ca_slab(c3_w len_w)
{
  c3_w*       nov_w = u2_ca_walloc(len_w + c3_wiseof(u2_cs_atom));
  u2_cs_atom* pug_u = (void *)nov_w;

  pug_u->mug_w = 0;
  pug_u->len_w = len_w;

  /* Clear teh slab.
  */
  {
    c3_w i_w;

    for ( i_w=0; i_w < len_w; i_w++ ) {
      pug_u->buf_w[i_w] = 0;
    }
  }
  return pug_u->buf_w;
}

/* u2_ca_slaq(): u2_ca_slaq() with a defined blocksize.
*/
c3_w*
u2_ca_slaq(c3_g met_g, c3_w len_w)
{
  return u2_ca_slab(((len_w << met_g) + 31) >> 5);
}

/* u2_ca_malt(): measure and finish a proto-atom.
*/
u2_noun
u2_ca_malt(c3_w* sal_w)
{
  c3_w*       nov_w = (sal_w - c3_wiseof(u2_cs_atom));
  u2_cs_atom* nov_u = (void *)nov_w;
  c3_w        len_w;

  for ( len_w = nov_u->len_w; len_w; len_w-- ) {
    if ( 0 != nov_u->buf_w[len_w - 1] ) {
      break;
    }
  }
  return u2_ca_mint(sal_w, len_w);
}

/* u2_ca_moot(): finish a pre-measured proto-atom; dangerous.
*/
u2_noun
u2_ca_moot(c3_w* sal_w)
{
  c3_w*       nov_w = (sal_w - c3_wiseof(u2_cs_atom));
  u2_cs_atom* nov_u = (void*)nov_w;
  c3_w        len_w = nov_u->len_w;
  c3_w        las_w = nov_u->buf_w[len_w - 1];

  c3_assert(0 != len_w);
  c3_assert(0 != las_w);

  if ( 1 == len_w ) {
    if ( u2_so(u2_co_is_cat(las_w)) ) {
      u2_ca_free(nov_w);

      return las_w;
    }
  }
  return u2_co_to_pug(u2_co_outa(nov_w));
}

/* u2_ca_mint(): finish a measured proto-atom.
*/
u2_noun
u2_ca_mint(c3_w* sal_w, c3_w len_w)
{
  c3_w*       nov_w = (sal_w - c3_wiseof(u2_cs_atom));
  u2_cs_atom* nov_u = (void*)nov_w;

  /* See if we can free the slab entirely.
  */
  if ( len_w == 0 ) {
    u2_ca_free(nov_w);

    return 0;
  }
  else if ( len_w == 1 ) {
    c3_w low_w = nov_u->buf_w[0];

    if ( u2_so(u2_co_is_cat(low_w)) ) {
      u2_ca_free(nov_w);

      return low_w;
    }
  }

  /* See if we can strip off a block on the end.
  */
  {
    c3_w old_w = nov_u->len_w;
    c3_w dif_w = (old_w - len_w);

    if ( dif_w >= u2_cc_minimum ) {
      c3_w* box_w = (void *)u2_co_botox(nov_w);
      c3_w* end_w = (nov_w + c3_wiseof(u2_cs_atom) + len_w + 1);
      c3_w  asz_w = (end_w - box_w);
      c3_w  bsz_w = box_w[0] - asz_w;

      _box_attach(_box_make(end_w, bsz_w, 0));

      box_w[0] = asz_w;
      box_w[asz_w - 1] = asz_w;
    }
    nov_u->len_w = len_w;
  }
  return u2_co_to_pug(u2_co_outa(nov_w));
}

/* u2_ci_words():
**
**   Copy [a] words from [b] into an atom.
*/
u2_noun
u2_ci_words(c3_w        a_w,
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
    c3_w*       nov_w = u2_ca_walloc(a_w + c3_wiseof(u2_cs_atom));
    u2_cs_atom* nov_u = (void*)nov_w;

    nov_u->mug_w = 0;
    nov_u->len_w = a_w;

    /* Fill the words.
    */
    {
      c3_w i_w;

      for ( i_w=0; i_w < a_w; i_w++ ) {
        nov_u->buf_w[i_w] = b_w[i_w];
      }
    }
    return u2_co_to_pug(u2_co_outa(nov_w));
  }
}

/* u2_ci_chubs():
**
**   Construct `a` double-words from `b`, LSD first, as an atom.
*/
u2_atom
u2_ci_chubs(c3_w        a_w,
            const c3_d* b_d)
{
  c3_w *b_w = c3_malloc(a_w * 8);
  c3_w i_w;
  u2_atom p;

  for ( i_w = 0; i_w < a_w; i_w++ ) {
    b_w[(2 * i_w)] = b_d[i_w] & 0xffffffffULL;
    b_w[(2 * i_w) + 1] = b_d[i_w] >> 32ULL;
  }
  p = u2_ci_words((a_w * 2), b_w);
  free(b_w);
  return p;
}

/* u2_ci_bytes():
**
**   Copy `a` bytes from `b` to an LSB first atom.
*/
u2_noun
u2_ci_bytes(c3_w        a_w,
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
    c3_w        len_w = (a_w + 3) >> 2;
    c3_w*       nov_w = u2_ca_walloc((len_w + c3_wiseof(u2_cs_atom)));
    u2_cs_atom* nov_u = (void*)nov_w;

    nov_u->mug_w = 0;
    nov_u->len_w = len_w;

    /* Clear the words.
    */
    {
      c3_w i_w;

      for ( i_w=0; i_w < len_w; i_w++ ) {
        nov_u->buf_w[i_w] = 0;
      }
    }

    /* Fill the bytes.
    */
    {
      c3_w i_w;

      for ( i_w=0; i_w < a_w; i_w++ ) {
        nov_u->buf_w[i_w >> 2] |= (b_y[i_w] << ((i_w & 3) * 8));
      }
    }
    return u2_co_to_pug(u2_co_outa(nov_w));
  }
}

/* u2_ci_mp():
**
**   Copy the GMP integer `a` into an atom, and clear it.
*/
u2_noun
u2_ci_mp(mpz_t a_mp)
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

    return u2_ci_words(pyg_w, buz_w);
  }
}

/* u2_ci_vint():
**
**   Create `a + 1`.
*/
u2_noun
u2_ci_vint(u2_noun a)
{
  c3_assert(u2_none != a);

  if ( u2_so(u2_co_is_cat(a)) ) {
    c3_w vin_w = (a + 1);

    if ( a == 0x7fffffff ) {
      return u2_ci_words(1, &vin_w);
    }
    else return vin_w;
  }
  else if ( u2_so(u2_co_is_cell(a)) ) {
    return u2_cm_bail(c3__exit);
  }
  else {
    mpz_t a_mp;

    u2_cr_mp(a_mp, a);
    u2_ca_lose(a);

    mpz_add_ui(a_mp, a_mp, 1);
    return u2_ci_mp(a_mp);
  }
}

/* u2_ci_cell():
**
**   Produce the cell `[a b]`.
*/
u2_noun
u2_ci_cell(u2_noun a, u2_noun b)
{
  c3_assert(u2_none != a);
  c3_assert(u2_none != b);

  c3_assert(u2_ne(u2_co_is_junior(a)));
  c3_assert(u2_ne(u2_co_is_junior(b)));

  {
    c3_w*       nov_w = u2_ca_walloc(c3_wiseof(u2_cs_cell));
    u2_cs_cell* nov_u = (void *)nov_w;

    nov_u->mug_w = 0;
    nov_u->hed = a;
    nov_u->tel = b;

    return u2_co_to_pom(u2_co_outa(nov_w));
  }
}

/* u2_ci_trel():
**
**   Produce the triple `[a b c]`.
*/
u2_noun
u2_ci_trel(u2_noun a, u2_noun b, u2_noun c)
{
  return u2_ci_cell(a, u2_ci_cell(b, c));
}

/* u2_ci_qual():
**
**   Produce the cell `[a b c d]`.
*/
u2_noun
u2_ci_qual(u2_noun a, u2_noun b, u2_noun c, u2_noun d)
{
  return u2_ci_cell(a, u2_ci_trel(b, c, d));
}

/* u2_ci_string():
**
**   Produce an LSB-first atom from the C string `a`.
*/
u2_noun
u2_ci_string(const c3_c* a_c)
{
  return u2_ci_bytes(strlen(a_c), (c3_y *)a_c);
}

/* u2_ci_tape(): from a C string, to a list of bytes.
*/
u2_atom
u2_ci_tape(const c3_c* txt_c)
{
  if ( !*txt_c ) {
    return u2_nul;
  } else return u2_ci_cell(*txt_c, u2_ci_tape(txt_c + 1));
}

/* u2_ci_decimal():
**
**   Parse `a` as a list of decimal digits.
*/
u2_atom
u2_ci_decimal(u2_noun a);

/* u2_ci_heximal():
**
**   Parse `a` as a list of hex digits.
*/
u2_noun
u2_ci_heximal(u2_noun a);

/* u2_ci_list():
**
**   Generate a null-terminated list, with `u2_none` as terminator.
*/
u2_noun
u2_ci_list(u2_weak one, ...);


/* u2_ci_molt():
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

  static u2_noun                            //  transfer
  _molt_apply(u2_noun            som,       //  retain
              c3_w               len_w,
              struct _molt_pair* pms_m)     //  transfer
  {
    if ( len_w == 0 ) {
      return u2_ca_gain(som);
    }
    else if ( (len_w == 1) && (1 == pms_m[0].axe_w) ) {
      return pms_m[0].som;
    }
    else {
      c3_w cut_w = _molt_cut(len_w, pms_m);

      if ( u2_no == u2_co_is_cell(som) ) {
        return u2_cm_bail(c3__exit);
      } 
      else {
        return u2_ci_cell
           (_molt_apply(u2_co_h(som), cut_w, pms_m),
            _molt_apply(u2_co_t(som), (len_w - cut_w), (pms_m + cut_w)));
      }
    }
  }
u2_noun 
u2_ci_molt(u2_noun som, ...)
{
  va_list            ap;
  c3_w               len_w;
  struct _molt_pair* pms_m;
  u2_noun            pro;

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
  pro = _molt_apply(som, len_w, pms_m);
  u2_ca_lose(som);
  return pro;
}

/* u2_cx_at (u2at): fragment.
*/
u2_noun
u2_cx_at(u2_noun axe, u2_noun som)
{
  u2_weak pro = u2_cr_at(axe, som);

  if ( u2_none == pro ) {
    return u2_cm_bail(c3__exit);
  } else return pro;
}

/* u2_cx_cell():
**
**   Divide `a` as a cell `[b c]`.
*/
void
u2_cx_cell(u2_noun  a,
           u2_noun* b,
           u2_noun* c)
{
  if ( u2_no == u2_cr_cell(a, b, c) ) {
    u2_cm_bail(c3__exit);
  }
}

/* u2_cx_trel():
**
**   Divide `a` as a trel `[b c d]`, or bail.
*/
void
u2_cx_trel(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d)
{
  if ( u2_no == u2_cr_trel(a, b, c, d) ) {
    u2_cm_bail(c3__exit);
  }
}

/* u2_cx_qual():
**
**   Divide `a` as a quadruple `[b c d e]`.
*/
void
u2_cx_qual(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e)
{
  if ( u2_no == u2_cr_qual(a, b, c, d, e) ) {
    u2_cm_bail(c3__exit);
  }
}

/* _frag_word(): fast fragment/branch prediction for top word.
*/
static u2_weak
_frag_word(c3_w a_w, u2_noun b)
{
  c3_assert(0 != a_w);

  {
    c3_w dep_w = u2_ax_dep(a_w);

    while ( dep_w ) {
      if ( u2_no == u2_co_is_cell(b) ) {
        return u2_none;
      }
      else {
        u2_cs_cell* b_u = u2_co_to_ptr(b);

        b = *(((u2_noun*)&(b_u->hed)) + (1 & (a_w >> (dep_w - 1))));
        dep_w--;
      }
    }
    return b;
  }
}

/* _frag_deep(): fast fragment/branch for deep words.
*/
static u2_weak
_frag_deep(c3_w a_w, u2_noun b)
{
  c3_w dep_w = 32;

  while ( dep_w ) {
    if ( u2_no == u2_co_is_cell(b) ) {
      return u2_none;
    }
    else {
      u2_cs_cell* b_u = u2_co_to_ptr(b);

      b = *(((u2_noun*)&(b_u->hed)) + (1 & (a_w >> (dep_w - 1))));
      dep_w--;
    }
  }
  return b;
}

/* u2_cr_at():
**
**   Return fragment (a) of (b), or u2_none if not applicable.
*/
u2_weak
u2_cr_at(u2_atom a,
         u2_noun b)
{
  c3_assert(u2_none != a);
  c3_assert(u2_none != b);

  if ( 0 == a ) {
    return u2_none;
  }

  if ( u2_so(u2_co_is_cat(a)) ) {
    return _frag_word(a, b);
  }
  else {
    if ( u2_ne(u2_co_is_pug(a)) ) {
      return u2_none;
    }
    else {
      u2_cs_atom* a_u = u2_co_to_ptr(a);
      c3_w len_w      = a_u->len_w;

      b = _frag_word(a_u->buf_w[len_w - 1], b);
      len_w -= 1;

      while ( len_w ) {
        b = _frag_deep(a_u->buf_w[len_w], b);

        if ( u2_none == b ) {
          return b;
        } else {
          len_w--;
        }
      }
      return b;
    }
  }
}

/* u2_cr_mean():
**
**   Attempt to deconstruct `a` by axis, noun pairs; 0 terminates.
**   Axes must be sorted in tree order.
*/
  struct _mean_pair {
    c3_w    axe_w;
    u2_noun* som;
  };

  static c3_w
  _mean_cut(c3_w               len_w,
            struct _mean_pair* prs_m)
  {
    c3_w i_w, cut_t, cut_w;

    cut_t = c3_false;
    cut_w = 0;
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      c3_w axe_w = prs_m[i_w].axe_w;

      if ( (cut_t == c3_false) && (3 == u2_ax_cap(axe_w)) ) {
        cut_t = c3_true;
        cut_w = i_w;
      }
      prs_m[i_w].axe_w = u2_ax_mas(axe_w);
    }
    return cut_t ? cut_w : i_w;
  }

  static u2_bean
  _mean_extract(u2_noun            som,
                c3_w               len_w,
                struct _mean_pair* prs_m)
  {
    if ( len_w == 0 ) {
      return u2_yes;
    }
    else if ( (len_w == 1) && (1 == prs_m[0].axe_w) ) {
      *prs_m->som = som;
      return u2_yes;
    }
    else {
      if ( u2_no == u2_co_is_cell(som) ) {
        return u2_no;
      } else {
        c3_w cut_w = _mean_cut(len_w, prs_m);

        return u2_and
          (_mean_extract(u2_co_h(som), cut_w, prs_m),
           _mean_extract(u2_co_t(som), (len_w - cut_w), (prs_m + cut_w)));
      }
    }
  }

u2_bean
u2_cr_mean(u2_noun som,
        ...)
{
  va_list            ap;
  c3_w               len_w;
  struct _mean_pair* prs_m;

  c3_assert(u2_none != som);

  /* Count.
  */
  len_w = 0;
  {
    va_start(ap, som);
    while ( 1 ) {
      if ( 0 == va_arg(ap, c3_w) ) {
        break;
      }
      va_arg(ap, u2_noun*);
      len_w++;
    }
    va_end(ap);
  }
  prs_m = alloca(len_w * sizeof(struct _mean_pair));

  /* Install.
  */
  {
    c3_w i_w;

    va_start(ap, som);
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      prs_m[i_w].axe_w = va_arg(ap, c3_w);
      prs_m[i_w].som = va_arg(ap, u2_noun*);
    }
    va_end(ap);
  }

  /* Extract.
  */
  return _mean_extract(som, len_w, prs_m);
}

static __inline__ c3_w
_mug_fnv(c3_w has_w)
{
  return (has_w * ((c3_w)16777619));
}

static __inline__ c3_w
_mug_out(c3_w has_w)
{
  return (has_w >> 31) ^ (has_w & 0x7fffffff);
}

static __inline__ c3_w
_mug_both(c3_w lef_w, c3_w rit_w)
{
  c3_w bot_w = _mug_fnv(lef_w ^ _mug_fnv(rit_w));
  c3_w out_w = _mug_out(bot_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_both(lef_w, ++rit_w);
  }
}

/* u2_cr_mug_both():
**
**   Join two mugs.
*/
c3_w
u2_cr_mug_both(c3_w lef_w, c3_w rit_w)
{
  return _mug_both(lef_w, rit_w);
}

static __inline__ c3_w
_mug_bytes_in(c3_w off_w, c3_w nby_w, c3_y* byt_y)
{
  c3_w i_w;

  for ( i_w = 0; i_w < nby_w; i_w++ ) {
    off_w = _mug_fnv(off_w ^ byt_y[i_w]);
  }
  return off_w;
}

static c3_w
_mug_bytes(c3_w off_w, c3_w nby_w, c3_y* byt_y)
{
  c3_w has_w = _mug_bytes_in(off_w, nby_w, byt_y);
  c3_w out_w = _mug_out(has_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_bytes(++off_w, nby_w, byt_y);
  }
}

static __inline__ c3_w
_mug_words_in_buf(c3_w off_w, c3_w nwd_w, u2_noun veb)
{
  u2_cs_atom* veb_u = u2_co_to_ptr(veb);

  if ( 0 == nwd_w ) {
    return off_w;
  } else {
    c3_w i_w, x_w;

    for ( i_w = 0; i_w < (nwd_w - 1); i_w++ ) {
      x_w = veb_u->buf_w[i_w];
      {
        c3_y a_y = (x_w & 0xff);
        c3_y b_y = ((x_w >> 8) & 0xff);
        c3_y c_y = ((x_w >> 16) & 0xff);
        c3_y d_y = ((x_w >> 24) & 0xff);

        off_w = _mug_fnv(off_w ^ a_y);
        off_w = _mug_fnv(off_w ^ b_y);
        off_w = _mug_fnv(off_w ^ c_y);
        off_w = _mug_fnv(off_w ^ d_y);
      }
    }
    x_w = veb_u->buf_w[nwd_w - 1];

    if ( x_w ) {
      off_w = _mug_fnv(off_w ^ (x_w & 0xff));
      x_w >>= 8;

      if ( x_w ) {
        off_w = _mug_fnv(off_w ^ (x_w & 0xff));
        x_w >>= 8;

        if ( x_w ) {
          off_w = _mug_fnv(off_w ^ (x_w & 0xff));
          x_w >>= 8;

          if ( x_w ) {
            off_w = _mug_fnv(off_w ^ (x_w & 0xff));
          }
        }
      }
    }
  }
  return off_w;
}

static __inline__ c3_w
_mug_words_in(c3_w off_w, c3_w nwd_w, const c3_w* wod_w)
{
  if ( 0 == nwd_w ) {
    return off_w;
  } else {
    c3_w i_w, x_w;

    for ( i_w = 0; i_w < (nwd_w - 1); i_w++ ) {
      x_w = wod_w[i_w];
      {
        c3_y a_y = (x_w & 0xff);
        c3_y b_y = ((x_w >> 8) & 0xff);
        c3_y c_y = ((x_w >> 16) & 0xff);
        c3_y d_y = ((x_w >> 24) & 0xff);

        off_w = _mug_fnv(off_w ^ a_y);
        off_w = _mug_fnv(off_w ^ b_y);
        off_w = _mug_fnv(off_w ^ c_y);
        off_w = _mug_fnv(off_w ^ d_y);
      }
    }
    x_w = wod_w[nwd_w - 1];

    if ( x_w ) {
      off_w = _mug_fnv(off_w ^ (x_w & 0xff));
      x_w >>= 8;

      if ( x_w ) {
        off_w = _mug_fnv(off_w ^ (x_w & 0xff));
        x_w >>= 8;

        if ( x_w ) {
          off_w = _mug_fnv(off_w ^ (x_w & 0xff));
          x_w >>= 8;

          if ( x_w ) {
            off_w = _mug_fnv(off_w ^ (x_w & 0xff));
          }
        }
      }
    }
  }
  return off_w;
}

static c3_w
_mug_words(c3_w off_w, c3_w nwd_w, const c3_w* wod_w)
{
  c3_w has_w = _mug_words_in(off_w, nwd_w, wod_w);
  c3_w out_w = _mug_out(has_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_words(++off_w, nwd_w, wod_w);
  }
}

static c3_w
_mug_words_buf(c3_w off_w, c3_w nwd_w, u2_noun veb)
{
  c3_w has_w = _mug_words_in_buf(off_w, nwd_w, veb);
  c3_w out_w = _mug_out(has_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_words_buf(++off_w, nwd_w, veb);
  }
}

/* u2_cr_mug():
**
**   Compute and/or recall the mug (31-bit FNV1a hash) of (a).
*/
c3_w
u2_cr_mug(u2_noun veb)
{
  c3_assert(u2_none != veb);

  if ( u2_so(u2_co_is_cat(veb)) ) {
    c3_w x_w = veb;

    return _mug_words(2166136261, (veb ? 1 : 0), &x_w);
  } else {
    u2_cs_noun* veb_u = u2_co_to_ptr(veb);

    if ( veb_u->mug_w ) {
      return veb_u->mug_w;
    }
    else {
      if ( u2_so(u2_co_is_cell(veb)) ) {
        u2_cs_cell* veb_u = u2_co_to_ptr(veb);
        u2_noun     hed   = veb_u->hed;
        u2_noun     tel   = veb_u->tel;

        veb_u->mug_w = u2_cr_mug_cell(hed, tel);
        return veb_u->mug_w;
      }
      else {
        u2_cs_atom* veb_u = u2_co_to_ptr(veb);
        c3_w        len_w = veb_u->len_w;

        veb_u->mug_w = _mug_words_buf(2166136261, len_w, veb);
        return veb_u->mug_w;
      }
    }
  }
}

/* u2_cr_mug_words():
**
**   Compute the mug of `buf`, `len`, LSW first.
*/
c3_w
u2_cr_mug_words(const c3_w *buf_w,
                c3_w        len_w)
{
  return _mug_words(2166136261, len_w, buf_w);
}

/* u2_cr_mug_string():
**
**   Compute the mug of `a`, LSB first.
*/
c3_w
u2_cr_mug_string(const c3_c *a_c)
{
  return _mug_bytes(2166136261, strlen(a_c), (c3_y *)a_c);
}

/* u2_cr_mug_cell():
**
**   Compute the mug of the cell `[hed tel]`.
*/
c3_w
u2_cr_mug_cell(u2_noun hed,
               u2_noun tel)
{
  c3_w   lus_w = u2_cr_mug(hed);
  c3_w   biq_w = u2_cr_mug(tel);

  return u2_cr_mug_both(lus_w, biq_w);
}

/* u2_cr_mug_trel():
**
**   Compute the mug of `[a b c]`.
*/
c3_w
u2_cr_mug_trel(u2_noun a,
               u2_noun b,
               u2_noun c)
{
  return u2_cr_mug_both(u2_cr_mug(a), u2_cr_mug_both(u2_cr_mug(b), u2_cr_mug(c)));
}

/* u2_cr_mug_qual():
**
**   Compute the mug of `[a b c d]`.
*/
c3_w
u2_cr_mug_qual(u2_noun a,
               u2_noun b,
               u2_noun c,
               u2_noun d)
{
  return u2_cr_mug_both
          (u2_cr_mug(a),
           u2_cr_mug_both(u2_cr_mug(b),
                          u2_cr_mug_both(u2_cr_mug(c), u2_cr_mug(d))));
}

/* _sing_x():
**
**   Yes iff (a) and (b) are the same noun.
*/
static u2_bean
_sing_x(u2_noun a,
        u2_noun b)
{
  c3_assert(u2_none != a);
  c3_assert(u2_none != b);

  if ( a == b ) {
    return u2_yes;
  }
  else {
    if ( u2_so(u2_co_is_atom(a)) ) {
      u2_cs_atom* a_u = u2_co_to_ptr(a);

      if ( !u2_so(u2_co_is_atom(b)) ||
           u2_so(u2_co_is_cat(a)) ||
           u2_so(u2_co_is_cat(b)) )
      {
        return u2_no;
      }
      else {
        u2_cs_atom* b_u = u2_co_to_ptr(b);

        if ( a_u->mug_w &&
             b_u->mug_w &&
             (a_u->mug_w != b_u->mug_w) )
        {
          return u2_no;
        }
        else {
          c3_w w_rez = a_u->len_w;
          c3_w w_mox = b_u->len_w;

          if ( w_rez != w_mox ) {
            return u2_no;
          }
          else {
            c3_w i_w;

            for ( i_w = 0; i_w < w_rez; i_w++ ) {
              if ( a_u->buf_w[i_w] != b_u->buf_w[i_w] ) {
                return u2_no;
              }
            }
            return u2_yes;
          }
        }
      }
    }
    else {
      if ( u2_so(u2_co_is_atom(b)) ) {
        return u2_no;
      }
      else {
        u2_cs_cell* a_u = u2_co_to_ptr(a);
        u2_cs_cell* b_u = u2_co_to_ptr(b);

        if ( a_u->mug_w &&
             b_u->mug_w &&
             (a_u->mug_w != b_u->mug_w) )
        {
          return u2_no;
        }
        else {
          if ( u2_no == _sing_x(u2_co_h(a), u2_co_h(b)) ) {
            return u2_no;
          }
          else if ( u2_no == _sing_x(u2_co_t(a), u2_co_t(b)) ) {
            return u2_no;
          }
          return u2_yes;
        }
      }
    }
  }
}

/* u2_cr_sing():
**
**   Yes iff (a) and (b) are the same noun.
*/
u2_bean
u2_cr_sing(u2_noun a,
           u2_noun b)
{
  return _sing_x(a, b);
}

u2_bean
u2_cr_fing(u2_noun a,
           u2_noun b)
{
  return (a == b) ? u2_yes : u2_no;
}

/* u2_cr_sing_cell():
**
**   Yes iff `[p q]` and `b` are the same noun.
*/
u2_bean
u2_cr_sing_cell(u2_noun p,
                u2_noun q,
                u2_noun b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_sing(p, u2_co_h(b)),
                       u2_cr_sing(q, u2_co_t(b))));
}
u2_bean
u2_cr_fing_cell(u2_noun p,
                u2_noun q,
                u2_noun b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_fing(p, u2_co_h(b)),
                       u2_cr_fing(q, u2_co_t(b))));
}

/* u2_cr_sing_mixt():
**
**   Yes iff `[p q]` and `b` are the same noun.
*/
u2_bean
u2_cr_sing_mixt(const c3_c* p_c,
                u2_noun     q,
                u2_noun     b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_sing_c(p_c, u2_co_h(b)),
                       u2_cr_sing(q, u2_co_t(b))));
}
u2_bean
u2_cr_fing_mixt(const c3_c* p_c,
                u2_noun     q,
                u2_noun     b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_sing_c(p_c, u2_co_h(b)),
                       u2_cr_fing(q, u2_co_t(b))));
}

/* u2_cr_sing_trel():
**
**   Yes iff `[p q r]` and `b` are the same noun.
*/
u2_bean
u2_cr_sing_trel(u2_noun p,
                u2_noun q,
                u2_noun r,
                u2_noun b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_sing(p, u2_co_h(b)),
                       u2_cr_sing_cell(q, r, u2_co_t(b))));
}
u2_bean
u2_cr_fing_trel(u2_noun p,
                u2_noun q,
                u2_noun r,
                u2_noun b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_fing(p, u2_co_h(b)),
                       u2_cr_fing_cell(q, r, u2_co_t(b))));
}

/* u2_cr_sing_qual():
**
**   Yes iff `[p q r]` and `b` are the same noun.
*/
u2_bean
u2_cr_sing_qual(u2_noun p,
                u2_noun q,
                u2_noun r,
                u2_noun s,
                u2_noun b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_sing(p, u2_co_h(b)),
                       u2_cr_sing_trel(q, r, s, u2_co_t(b))));
}
u2_bean
u2_cr_fing_qual(u2_noun p,
                u2_noun q,
                u2_noun r,
                u2_noun s,
                u2_noun b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_fing(p, u2_co_h(b)),
                       u2_cr_fing_trel(q, r, s, u2_co_t(b))));
}

/* u2_cr_nord():
**
**   Return 0, 1 or 2 if `a` is below, equal to, or above `b`.
*/
u2_atom
u2_cr_nord(u2_noun a,
        u2_noun b)
{
  c3_assert(u2_none != a);
  c3_assert(u2_none != b);

  if ( a == b ) {
    return 1;
  }
  else {
    if ( u2_so(u2_co_is_atom(a)) ) {
      if ( !u2_so(u2_co_is_atom(b)) ) {
        return 0;
      } else {
        if ( u2_so(u2_co_is_cat(a)) ) {
          if ( u2_so(u2_co_is_cat(b)) ) {
            return (a < b) ? 0 : 2;
          }
          else return 0;
        }
        else if ( u2_so(u2_co_is_cat(b)) ) {
          return 2;
        }
        else {
          u2_cs_atom* a_u = u2_co_to_ptr(a);
          u2_cs_atom* b_u = u2_co_to_ptr(b);

          c3_w w_rez = a_u->len_w;
          c3_w w_mox = b_u->len_w;

          if ( w_rez != w_mox ) {
            return (w_rez < w_mox) ? 0 : 2;
          }
          else {
            c3_w i_w;

            for ( i_w = 0; i_w < w_rez; i_w++ ) {
              c3_w ai_w = a_u->buf_w[i_w];
              c3_w bi_w = b_u->buf_w[i_w];

              if ( ai_w != bi_w ) {
                return (ai_w < bi_w) ? 0 : 2;
              }
            }
            return 1;
          }
        }
      }
    } else {
      if ( u2_so(u2_co_is_atom(b)) ) {
        return 2;
      } else {
        u2_atom c = u2_cr_nord(u2_co_h(a), u2_co_h(b));

        if ( 1 == c ) {
          return u2_cr_nord(u2_co_t(a), u2_co_t(b));
        } else {
          return c;
        }
      }
    }
  }
}

/* u2_cr_sing_c():
**
**   Yes iff (b) is the same noun as the C string a_c.
*/
u2_bean
u2_cr_sing_c(const c3_c* a_c,
          u2_noun     b)
{
  c3_assert(u2_none != b);

  if ( !u2_so(u2_co_is_atom(b)) ) {
    return u2_no;
  }
  else {
    c3_w w_sof = strlen(a_c);
    c3_w i_w;

    for ( i_w = 0; i_w < w_sof; i_w++ ) {
      if ( u2_cr_byte(i_w, b) != a_c[i_w] ) {
        return u2_no;
      }
    }
    return u2_yes;
  }
}

/* u2_cr_bush():
**
**   Factor [a] as a bush [b.[p q] c].
*/
u2_bean
u2_cr_bush(u2_noun  a,
           u2_noun* b,
           u2_noun* c)
{
  c3_assert(u2_none != a);

  if ( u2_so(u2_co_is_atom(a)) ) {
    return u2_no;
  }
  else {
    *b = u2_co_h(a);

    if ( u2_so(u2_co_is_atom(*b)) ) {
      return u2_no;
    } else {
      *c = u2_co_t(a);
      return u2_yes;
    }
  }
}

/* u2_cr_cell():
**
**   Factor (a) as a cell (b c).
*/
u2_bean
u2_cr_cell(u2_noun  a,
           u2_noun* b,
           u2_noun* c)
{
  c3_assert(u2_none != a);

  if ( u2_so(u2_co_is_atom(a)) ) {
    return u2_no;
  }
  else {
    if ( b ) *b = u2_co_h(a);
    if ( c ) *c = u2_co_t(a);
    return u2_yes;
  }
}

/* u2_cr_p():
**
**   & [0] if [a] is of the form [b *c].
*/
u2_bean
u2_cr_p(u2_noun  a,
        u2_noun  b,
        u2_noun* c)
{
  u2_noun feg, nux;

  if ( (u2_yes == u2_cr_cell(a, &feg, &nux)) &&
       (u2_yes == u2_cr_sing(feg, b)) )
  {
    *c = nux;
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_pq():
**
**   & [0] if [a] is of the form [b *c d].
*/
u2_bean
u2_cr_pq(u2_noun  a,
         u2_noun  b,
         u2_noun* c,
         u2_noun* d)
{
  u2_noun nux;

  if ( (u2_yes == u2_cr_p(a, b, &nux)) &&
       (u2_yes == u2_cr_cell(nux, c, d)) )
  {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_pqr():
**
**   & [0] if [a] is of the form [b *c *d *e].
*/
u2_bean
u2_cr_pqr(u2_noun  a,
          u2_noun  b,
          u2_noun* c,
          u2_noun* d,
          u2_noun* e)
{
  u2_noun nux;

  if ( (u2_yes == u2_cr_p(a, b, &nux)) &&
       (u2_yes == u2_cr_trel(nux, c, d, e)) )
  {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_pqrs():
**
**   & [0] if [a] is of the form [b *c *d *e *f].
*/
u2_bean
u2_cr_pqrs(u2_noun  a,
           u2_noun  b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e,
           u2_noun* f)
{
  u2_noun nux;

  if ( (u2_yes == u2_cr_p(a, b, &nux)) &&
       (u2_yes == u2_cr_qual(nux, c, d, e, f)) )
  {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_trel():
**
**   Factor (a) as a trel (b c d).
*/
u2_bean
u2_cr_trel(u2_noun a,
           u2_noun *b,
           u2_noun *c,
           u2_noun *d)
{
  u2_noun guf;

  if ( (u2_yes == u2_cr_cell(a, b, &guf)) &&
       (u2_yes == u2_cr_cell(guf, c, d)) ) {
    return u2_yes;
  }
  else {
    return u2_no;
  }
}

/* u2_cr_qual():
**
**   Factor (a) as a qual (b c d e).
*/
u2_bean
u2_cr_qual(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e)
{
  u2_noun guf;

  if ( (u2_yes == u2_cr_cell(a, b, &guf)) &&
       (u2_yes == u2_cr_trel(guf, c, d, e)) ) {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_quil():
**
**   Factor (a) as a quil (b c d e f).
*/
u2_bean
u2_cr_quil(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e,
           u2_noun* f)
{
  u2_noun guf;

  if ( (u2_yes == u2_cr_cell(a, b, &guf)) &&
       (u2_yes == u2_cr_qual(guf, c, d, e, f)) ) {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_hext():
**
**   Factor (a) as a hext (b c d e f g)
*/
u2_bean
u2_cr_hext(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e,
           u2_noun* f,
           u2_noun* g)
{
  u2_noun guf;

  if ( (u2_yes == u2_cr_cell(a, b, &guf)) &&
       (u2_yes == u2_cr_quil(guf, c, d, e, f, g)) ) {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_met():
**
**   Return the size of (b) in bits, rounded up to
**   (1 << a_y).
**
**   For example, (a_y == 3) returns the size in bytes.
*/
c3_w
u2_cr_met(c3_y    a_y,
          u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_so(u2_co_is_atom(b)));

  if ( b == 0 ) {
    return 0;
  }
  else {
    /* gal_w: number of words besides (daz_w) in (b).
    ** daz_w: top word in (b).
    */
    c3_w gal_w;
    c3_w daz_w;

    if ( u2_so(u2_co_is_cat(b)) ) {
      gal_w = 0;
      daz_w = b;
    }
    else {
      u2_cs_atom* b_u = u2_co_to_ptr(b);

      gal_w = (b_u->len_w) - 1;
      daz_w = b_u->buf_w[gal_w];
    }

    switch ( a_y ) {
      case 0:
      case 1:
      case 2: {
        /* col_w: number of bits in (daz_w)
        ** bif_w: number of bits in (b)
        */
        c3_w bif_w, col_w;

        col_w = c3_bits_word(daz_w);
        bif_w = col_w + (gal_w << 5);

        return (bif_w + ((1 << a_y) - 1)) >> a_y;
      }
      case 3: {
        return  (gal_w << 2)
              + ((daz_w >> 24) ? 4 : (daz_w >> 16) ? 3 : (daz_w >> 8) ? 2 : 1);
      }
      case 4: {
        return  (gal_w << 1)
              + ((daz_w >> 16) ? 2 : 1);
      }
      default: {
        c3_y gow_y = (a_y - 5);

        return ((gal_w + 1) + ((1 << gow_y) - 1)) >> gow_y;
      }
    }
  }
}

/* u2_cr_bit():
**
**   Return bit (a_w) of (b).
*/
c3_b
u2_cr_bit(c3_w    a_w,
          u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_so(u2_co_is_atom(b)));

  if ( u2_so(u2_co_is_cat(b)) ) {
    if ( a_w >= 31 ) {
      return 0;
    }
    else return (1 & (b >> a_w));
  }
  else {
    u2_cs_atom* b_u   = u2_co_to_ptr(b);
    c3_y        vut_y = (a_w & 31);
    c3_w        pix_w = (a_w >> 5);

    if ( pix_w >= b_u->len_w ) {
      return 0;
    }
    else {
      c3_w nys_w = b_u->buf_w[pix_w];

      return (1 & (nys_w >> vut_y));
    }
  }
}

/* u2_cr_byte():
**
**   Return byte (a_w) of (b).
*/
c3_y
u2_cr_byte(c3_w    a_w,
           u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_so(u2_co_is_atom(b)));

  if ( u2_so(u2_co_is_cat(b)) ) {
    if ( a_w > 3 ) {
      return 0;
    }
    else return (255 & (b >> (a_w << 3)));
  }
  else {
    u2_cs_atom* b_u   = u2_co_to_ptr(b);
    c3_y        vut_y = (a_w & 3);
    c3_w        pix_w = (a_w >> 2);

    if ( pix_w >= b_u->len_w ) {
      return 0;
    }
    else {
      c3_w nys_w = b_u->buf_w[pix_w];

      return (255 & (nys_w >> (vut_y << 3)));
    }
  }
}

/* u2_cr_bytes():
**
**  Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
*/
void
u2_cr_bytes(c3_w    a_w,
            c3_w    b_w,
            c3_y*   c_y,
            u2_atom d)
{
  c3_w i_w;

  c3_assert(u2_none != d);

  /* Efficiency: don't call u2_cr_byte().
  */
  for ( i_w = 0; i_w < b_w; i_w++ ) {
    c_y[i_w] = u2_cr_byte((a_w + i_w), d);
  }
}

/* u2_cr_mp():
**
**   Copy (b) into (a_mp).
*/
void
u2_cr_mp(mpz_t   a_mp,
         u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_so(u2_co_is_atom(b)));

  if ( u2_so(u2_co_is_cat(b)) ) {
    mpz_init_set_ui(a_mp, b);
  }
  else {
    u2_cs_atom* b_u   = u2_co_to_ptr(b);
    c3_w        len_w = b_u->len_w;

    /* Slight deficiency in the GMP API.
    */
    c3_assert(!(len_w >> 27));
    mpz_init2(a_mp, len_w << 5);

    /* Efficiency: horrible.
    */
    {
      c3_w *buf_w = alloca(len_w << 2);
      c3_w i_w;

      for ( i_w=0; i_w < len_w; i_w++ ) {
        buf_w[i_w] = b_u->buf_w[i_w];
      }
      mpz_import(a_mp, len_w, -1, 4, 0, 0, buf_w);
    }
  }
}

/* u2_cr_word():
**
**   Return word (a_w) of (b).
*/
c3_w
u2_cr_word(c3_w    a_w,
           u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_so(u2_co_is_atom(b)));

  if ( u2_so(u2_co_is_cat(b)) ) {
    if ( a_w > 0 ) {
      return 0;
    }
    else return b;
  }
  else {
    u2_cs_atom* b_u = u2_co_to_ptr(b);

    if ( a_w >= b_u->len_w ) {
      return 0;
    }
    else return b_u->buf_w[a_w];
  }
}

/* u2_cr_chub():
**
**   Return double-word (a_w) of (b).
*/
c3_d
u2_cr_chub(c3_w  a_w,
           u2_atom b)
{
  c3_w wlo_w = u2_cr_word(a_w * 2, b);
  c3_w whi_w = u2_cr_word(1 + (a_w * 2), b);

  return (((uint64_t)whi_w) << 32ULL) | ((uint64_t)wlo_w);
}

/* u2_cr_words():
**
**  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
*/
void
u2_cr_words(c3_w    a_w,
            c3_w    b_w,
            c3_w*   c_w,
            u2_atom d)
{
  c3_w i_w;

  c3_assert(u2_none != d);

  /* Efficiency: don't call u2_cr_word().
  */
  for ( i_w = 0; i_w < b_w; i_w++ ) {
    c_w[i_w] = u2_cr_word((a_w + i_w), d);
  }
}

/* u2_cr_chop():
**
**   Into the bloq space of `met`, from position `fum` for a
**   span of `wid`, to position `tou`, XOR from atom `src`
**   into `dst_w`.
*/
void
u2_cr_chop(c3_g    met_g,
           c3_w    fum_w,
           c3_w    wid_w,
           c3_w    tou_w,
           c3_w*   dst_w,
           u2_atom src)
{
  c3_w  i_w;
  c3_w  len_w;
  c3_w* buf_w;
  
  c3_assert(u2_none != src);
  c3_assert(u2_so(u2_co_is_atom(src)));

  if ( u2_so(u2_co_is_cat(src)) ) {
    len_w = src ? 1 : 0;
    buf_w = &src;
  }
  else {
    u2_cs_atom* src_u = u2_co_to_ptr(src);
   
    len_w = src_u->len_w;
    buf_w = src_u->buf_w;
  }

  if ( met_g < 5 ) {
    c3_w san_w = (1 << met_g);
    c3_w mek_w = ((1 << san_w) - 1);
    c3_w baf_w = (fum_w << met_g);
    c3_w bat_w = (tou_w << met_g);

    // XX: efficiency: poor.  Iterate by words.
    //
    for ( i_w = 0; i_w < wid_w; i_w++ ) {
      c3_w waf_w = (baf_w >> 5);
      c3_g raf_g = (baf_w & 31);
      c3_w wat_w = (bat_w >> 5);
      c3_g rat_g = (bat_w & 31);
      c3_w hop_w;

      hop_w = (waf_w >= len_w) ? 0 : buf_w[waf_w];
      hop_w = (hop_w >> raf_g) & mek_w;

      dst_w[wat_w] ^= (hop_w << rat_g);

      baf_w += san_w;
      bat_w += san_w;
    }
  }
  else {
    c3_g hut_g = (met_g - 5);
    c3_w san_w = (1 << hut_g);
    c3_w j_w;

    for ( i_w = 0; i_w < wid_w; i_w++ ) {
      c3_w wuf_w = (fum_w + i_w) << hut_g;
      c3_w wut_w = (tou_w + i_w) << hut_g;

      for ( j_w = 0; j_w < san_w; j_w++ ) {
        dst_w[wut_w + j_w] ^=
            ((wuf_w + j_w) >= len_w)
              ? 0
              : buf_w[wuf_w + j_w];
      }
    }
  }
}

/* u2_cr_string(): `a` as malloced C string.
*/
c3_c*
u2_cr_string(u2_atom a)
{
  c3_w  met_w = u2_cr_met(3, a);
  c3_c* str_c = c3_malloc(met_w + 1);

  u2_cr_bytes(0, met_w, (c3_y*)str_c, a);
  str_c[met_w] = 0;
  return str_c;
}

/* u2_cr_tape(): `a`, a list of bytes, as malloced C string.
*/
c3_y*
u2_cr_tape(u2_noun a)
{
  u2_noun b;
  c3_w    i_w;
  c3_y    *a_y;

  for ( i_w = 0, b=a; u2_yes == u2_co_is_cell(b); i_w++, b=u2_co_t(b) )
    ;
  a_y = c3_malloc(i_w + 1);

  for ( i_w = 0, b=a; u2_yes == u2_co_is_cell(b); i_w++, b=u2_co_t(b) ) {
    a_y[i_w] = u2_co_h(b);
  }
  a_y[i_w] = 0;

  return a_y;
}

#if 0
static c3_w*
_test_walloc(c3_w siz_w)
{
  c3_w *ptr_w = u2_ca_walloc(siz_w);
  c3_w i_w;

  c3_assert(siz_w >= 1);
  *ptr_w = siz_w;

  for ( i_w = 1; i_w < siz_w; i_w++ ) {
    ptr_w[i_w] = u2_cr_mug((0xffff & (c3_p)(ptr_w)) + i_w);
  }
  return ptr_w;
}

static void
_test_free(c3_w* ptr_w)
{
  c3_w i_w, siz_w = *ptr_w;

  for ( i_w = 1; i_w < siz_w; i_w++ ) {
    c3_assert(ptr_w[i_w] == u2_cr_mug((0xffff & (c3_p)(ptr_w)) + i_w));
  }
  u2_ca_free(ptr_w);
}

#define NUM 16384

// Simple allocation test.
//
void
test(void)
{
  c3_w* one_w[NUM];
  c3_w* two_w[NUM];
  c3_w  i_w;

  for ( i_w = 0; i_w < NUM; i_w++ ) {
    c3_w siz_w = c3_max(1, u2_cr_mug(i_w) & 0xff);

    one_w[i_w] = _test_walloc(siz_w);
    two_w[i_w] = _test_walloc(siz_w);
  }
  _road_sane();

  for ( i_w = 0; i_w < NUM; i_w++ ) {
    _test_free(two_w[NUM - (i_w + 1)]);
    _road_sane();
  }
  for ( i_w = 0; i_w < NUM; i_w++ ) {
    c3_w siz_w = c3_max(1, u2_cr_mug(i_w + 1) & 0xff);

    two_w[i_w] = _test_walloc(siz_w);
    _road_sane();
  }

  for ( i_w = 0; i_w < NUM; i_w++ ) {
    _test_free(one_w[NUM - (i_w + 1)]);
    _road_sane();
  }
  for ( i_w = 0; i_w < NUM; i_w++ ) {
    c3_w siz_w = c3_max(1, u2_cr_mug(i_w + 2) & 0xff);

    one_w[i_w] = _test_walloc(siz_w);
    _road_sane();
  }

  for ( i_w = 0; i_w < NUM; i_w++ ) {
    _test_free(one_w[NUM - (i_w + 1)]);
    _road_sane();
  }
  for ( i_w = 0; i_w < NUM; i_w++ ) {
    _test_free(two_w[NUM - (i_w + 1)]);
    _road_sane();
  }

  printf("allocations %d, iterations %d\n", ALL_w, ITE_w);
}
#endif

#if 1
/* u2_walk_load(): load file or bail.
*/
static u2_noun
u2_walk_load(c3_c* pas_c)
{
  struct stat buf_b;
  c3_i        fid_i = open(pas_c, O_RDONLY, 0644);
  c3_w        fln_w, red_w;
  c3_y*       pad_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_b) < 0) ) {
    fprintf(stderr, "%s: %s\r\n", pas_c, strerror(errno));
    return u2_cm_bail(c3__fail);
  }
  fln_w = buf_b.st_size;
  pad_y = c3_malloc(buf_b.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    return u2_cm_bail(c3__fail);
  }
  else {
    u2_noun pad = u2_ci_bytes(fln_w, (c3_y *)pad_y);
    free(pad_y);

    return pad;
  }
}
#endif

/* u2_cka_add(): a + b.
*/
u2_noun
u2_cka_add(u2_noun a, u2_noun b)
{
  u2_noun c = j2_mbc(Pt1, add)(a, b);

  u2z(a); u2z(b);
  return c;
}

#if 0
/* u2_cka_sub(): a + b.
*/
u2_noun
u2_cka_sub(u2_noun a, u2_noun b)
{
  u2_noun c = j2_mbc(Pt1, sub)(a, b);

  u2z(a); u2z(b);
  return c;
}

/* u2_cka_gth(): a + b.
*/
u2_noun
u2_cka_gth(u2_noun a, u2_noun b)
{
  u2_noun c = j2_mbc(Pt1, gth)(a, b);

  u2z(a); u2z(b);
  return c;
}

/* u2_cka_mul(): a * b.
*/
u2_noun
u2_cka_mul(u2_noun a, u2_noun b)
{
  u2_noun c = j2_mbc(Pt1, mul)(a, b);

  u2z(a); u2z(b);
  return c;
}

/* u2_cka_lte(): a * b.
*/
u2_noun
u2_cka_lte(u2_noun a, u2_noun b)
{
  u2_noun c = j2_mbc(Pt1, lte)(a, b);

  u2z(a); u2z(b);
  return c;
}

/* u2_ckb_lent(): length of list `a`.
*/
u2_noun
u2_ckb_lent(u2_noun a)
{
  u2_noun b = j2_mbc(Pt2, lent)(a);

  u2z(a);
  return b;
}

/* u2_ckb_flop(): reverse list `a`.
*/
u2_noun
u2_ckb_flop(u2_noun a)
{
  u2_noun b = j2_mbc(Pt2, flop)(a);

  u2z(a);
  return b;
}

/* u2_ckb_weld(): concatenate lists `a` before `b`.
*/
u2_noun
u2_ckb_weld(u2_noun a, u2_noun b)
{
  u2_noun c = j2_mbc(Pt2, weld)(a, b);

  u2z(a); u2z(b);
  return c;
}

/* u2_ckc_lsh(): left shift.
*/
u2_noun
u2_ckc_lsh(u2_noun a, u2_noun b, u2_noun c)
{
  u2_noun d = j2_mbc(Pt3, lsh)(a, b, c);

  u2z(a); u2z(b); u2z(c);
  return d;
}

/* u2_ckc_rsh(): right shift.
*/
u2_noun
u2_ckc_rsh(u2_noun a, u2_noun b, u2_noun c)
{
  u2_noun d = j2_mbc(Pt3, rsh)(a, b, c);

  u2z(a); u2z(b); u2z(c);
  return d;
}

/* u2_ckd_by_get(): map get for key `b` in map `a` with u2_none.
*/
u2_weak
u2_ckd_by_get(u2_noun a, u2_noun b)
{
  u2_noun c = _coal_by_get(a, b);

  u2z(a); u2z(b);
  if ( u2_no == u2_cr_du(c) ) {
    u2z(c);
    return u2_none;
  } else {
    u2_noun pro = u2_ct(u2t(c));

    u2z(c);
    return pro;
  }
}

/* u2_ckd_by_got(): map get for key `b` in map `a` with fail.
*/
u2_noun
u2_ckd_by_got(u2_noun a, u2_noun b)
{
  u2_weak c = u2_ckd_by_get(a, b);

  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  }
  else return c;
}

/* u2_ckd_by_put(): map put for key `b`, value `c` in map `a`.
*/
u2_weak
u2_ckd_by_put(u2_noun a, u2_noun b, u2_noun c)
{
  // Bizarre asymmetry in old jets.
  //
  // (Mysterious comment in old glue code.)
  //
  u2_noun pro = _coal_by_put(a, b, c);

  u2z(a); u2z(b); u2z(c);
  return pro;
}

/* u2_ckd_by_gas(): list to map.
*/
u2_noun
u2_ckd_by_gas(u2_noun a, u2_noun b)
{
  u2_weak c = _coal_by_gas(a, b);

  u2z(a); u2z(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  }
  else return c;
}

/* u2_ckd_in_gas(): list to map.
*/
u2_noun
u2_ckd_in_gas(u2_noun a, u2_noun b)
{
  u2_weak c = _coal_in_gas(a, b);

  u2z(a); u2z(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  }
  else return c;
}

/* u2_ckd_by_has(): test for presence.
*/
u2_bean
u2_ckd_by_has(u2_noun a, u2_noun b)
{
  u2_weak c = _coal_by_has(a, b);

  u2z(a); u2z(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  }
  else return c;
}

/* u2_ckd_in_has(): test for presence.
*/
u2_bean
u2_ckd_in_has(u2_noun a, u2_noun b)
{
  u2_weak c = _coal_in_has(a, b);

  u2z(a); u2z(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  }
  else return c;
}

/* u2_ckd_in_tap(): map/set convert to list.  (solves by_tap also.)
*/
u2_noun
u2_ckd_in_tap(u2_noun a, u2_noun b)
{
  u2_weak c = _coal_in_tap(a, b);

  u2z(a); u2z(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  }
  else return c;
}
#endif

/* u2_cke_cue(): expand saved pill.
*/

u2_noun
u2_cke_cue(u2_atom a)
{
  u2_noun b = _coal_cue(a);

  u2z(a);
  return b;
}

/* u2_cke_jam(): pack noun as atom.
*/
u2_atom
u2_cke_jam(u2_noun a)
{
  u2_atom b = _coal_jam(a);

  u2z(a);
  return b;
}

#if 0
/* u2_cke_trip(): atom to tape.
*/
u2_atom
u2_cke_trip(u2_noun a)
{
  u2_atom b = _coal_trip(a);

  u2z(a);
  return b;
}
#endif

#if 0
static c3_w 
_depth(u2_noun som)
{
  if ( u2_so(u2_co_is_atom(som)) ) {
    return 1;
  }
  else {
    return c3_max(1 + _depth(u2_co_h(som)), 1 + _depth(u2_co_t(som)));
  }
}
#endif 

// Wordy.

#if 0
static int
_bits_word(c3_w w)
{
  return w ? (32 - __builtin_clz(w)) : 0;
}

static void
_test_words(void)
{
  c3_w i_w = 0;

  while ( 1 ) {
    if ( _bits_word(i_w) != c3_bits_word(i_w) ) {
      printf("i_w %x, bw %d, cbw %d\n", 
              i_w, _bits_word(i_w), c3_bits_word(i_w));
      c3_assert(0);
    }
    if ( 0xffffffff == i_w ) {
      break;
    }
    i_w++;
  }
}
#endif

static void
_test_jam(void)
{
  _road_dump();
  {
    u2_noun pil = u2_walk_load("urb/urbit.pill");
    u2_noun cue, jam;

    printf("cueing pill - %d bytes\n", u2_cr_met(3, pil));
    cue = u2_cke_cue(pil);
    printf("cued - mug %x\n", u2_cr_mug(cue));

#if 1
    jam = u2_cke_jam(cue);
    printf("jammed - %d bytes\n", u2_cr_met(3, jam));
    cue = u2_cke_cue(jam);
    printf("cued - mug %x\n", u2_cr_mug(cue));
#endif

    u2z(cue);
  }
  _road_dump();
}

static void
_test_hash_bad(void)
{
  _road_dump();
  {
    u2_ha_root* har_u = u2_ha_new();
    c3_w        i_w;
    c3_w        max_w = (1 << 20);

    for ( i_w = 0; i_w < max_w; i_w++ ) {
      u2_ha_put(har_u, u2nc(0, i_w), u2nc(0, (i_w + 1)));
    }
    for ( i_w = 0; i_w < max_w; i_w++ ) {
      u2_noun val = u2_ha_get(har_u, u2nc(0, i_w));

      if ( u2_none == val ) {
        printf("at %d, nothing\n", i_w);
        c3_assert(0);
      }
      if ( (u2h(val) != 0) || (u2t(val) != (i_w + 1)) ) {
        printf("at %d, oddly, is %d\n", i_w, val);
        c3_assert(0);
      }
      u2_ca_lose(val);
    }
    u2_ha_free(har_u);
  }
  _road_dump();
}

static void
_test_hash(void)
{
  _road_dump();
  {
    u2_ha_root* har_u = u2_ha_new();
    c3_w        i_w;
    c3_w        max_w = (1 << 20);

    for ( i_w = 0; i_w < max_w; i_w++ ) {
      u2_ha_put(har_u, u2nc(0, i_w), (i_w + 1));
    }
    for ( i_w = 0; i_w < max_w; i_w++ ) {
      u2_noun val = u2_ha_get(har_u, u2nc(0, i_w));

      if ( val != (i_w + 1) ) {
        if ( u2_none == val ) {
          printf("at %d, nothing\n", i_w);
        }
        else printf("at %d, oddly, is %d\n", i_w, val);
        c3_assert(0);
      }
    }
    u2_ha_free(har_u);
  }
  _road_dump();
}

// A simple memory tester.
//
int c3_cooked() { u2_cm_bail(c3__oops); return 0; }
int
main(int argc, char *argv[])
{
  printf("hello, world: len %dMB\n", (1 << U2_OS_LoomBits) >> 18);
  // _test_words();

  u2_cm_boot(U2_OS_LoomBase, (1 << U2_OS_LoomBits));
  printf("booted.\n");

  _test_jam();
}
