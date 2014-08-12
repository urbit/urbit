/* f/meme.c
**
** This file is in the public domain.
*/
#include "f/meme.h"

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

/* _me_road_all_hat(): in u2R, allocate directly on the hat_w.
*/
static c3_w*
_me_road_all_hat(c3_w len_w)
{
  if ( len_w > u2_me_open ) {
    u2_me_bail(c3__meme); return 0;
  }

  if ( u2_yes == u2_me_is_north ) {
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
  if ( len_w > u2_me_open ) {
    u2_me_bail(c3__meme); return 0;
  }

  if ( u2_yes == u2_me_is_north ) {
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

/* u2_me_bail(): bail out.  Does not return.
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
u2_me_bail(c3_m how_m)
{
  _longjmp(u2R->esc.buf, how_m);
  return how_m;
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

  u2R->par_u->cap_w = u2R->hat_w;
  u2R = u2R->par_u;
}


/* u2_me_golf(): record cap_w length for u2_flog().
*/
c3_w
u2_me_golf(void)
{
  if ( u2_yes == u2_me_is_north ) {
    return u2R->mat_w - u2R->cap_w;
  } 
  else {
    return u2R->cap_w - u2R->mat_w;
  }
}

/* u2_me_flog(): reset cap_w.
*/
void
u2_me_flog(c3_w gof_w)
{
  if ( u2_yes == u2_me_is_north ) {
    u2R->cap_w = u2R->mat_w - gof_w;
  } else {
    u2R->cap_w = u2R->mat_w + gof_w;
  }
}

/* u2_me_water(): produce watermarks.
*/
void
u2_me_water(c3_w* low_w, c3_w* hig_w)
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
      if ( i_w == u2_me_fbox_no ) {
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
_me_box_make(void* box_v, c3_w siz_w, c3_w use_w)
{
  u2_me_box* box_u = box_v;
  c3_w*      box_w = box_v;

  box_w[0] = siz_w;
  box_w[siz_w - 1] = siz_w;
  box_u->use_w = use_w;

# ifdef  U2_MEMORY_DEBUG
    box_u->cod_w = COD_w;
# endif

  return box_u;
}

/* _me_box_attach(): attach a box to the free list.
*/
void
_me_box_attach(u2_me_box* box_u)
{
  c3_assert(box_u->siz_w >= (1 + c3_wiseof(u2_me_fbox)));
  {
    c3_w sel_w         = _me_box_slot(box_u->siz_w);
    u2_me_fbox* fre_u  = (void *)box_u;
    u2_me_fbox** pfr_u = &u2R->all.fre_u[sel_w];

    fre_u->pre_u = 0;
    fre_u->nex_u = (*pfr_u);
    (*pfr_u) = fre_u;
  }
}

/* _me_box_detach(): detach a box from the free list.
*/
void
_me_box_detach(u2_me_box* box_u)
{
  u2_me_fbox* fre_u = (void*) box_u;

  if ( fre_u->pre_u ) {
    fre_u->pre_u->nex_u = fre_u->nex_u;
  } 
  else {
    c3_w sel_w = _me_box_slot(box_u->siz_w);

    u2R->all.fre_u[sel_w] = fre_u->nex_u;
  }
}

/* u2_me_walloc(): allocate storage words on hat_w.
*/
void*
u2_me_walloc(c3_w len_w)
{
  c3_w siz_w = c3_max(u2_me_minimum, u2_me_boxed(len_w));
  c3_w sel_w = _me_box_slot(siz_w);

  //  XX: this logic is totally bizarre, but preserve it.
  //
  if ( (sel_w != 0) && (sel_w != u2_me_fbox_no - 1) ) {
    sel_w += 1;
  }

  while ( 1 ) {
    u2_me_fbox** pfr_u = &u2R->all.fre_u[sel_w];

    while ( 1 ) {
      if ( 0 == *pfr_u ) {
        if ( sel_w < (u2_me_fbox_no - 1) ) {
          sel_w += 1;
          break;
        }
        else {
          /* Nothing in top free list.  Chip away at the hat_w.
          */
          return u2_me_boxto(_me_box_make(_me_road_all_hat(siz_w), siz_w, 1));
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
          if ( (siz_w + c3_wiseof(u2_me_fbox) + 1) <= box_u->siz_w ) {
            /* Split the block.
            */ 
            c3_w* box_w = ((c3_w *)(void *)box_u);
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
}

/* u2_me_malloc(): allocate storage measured in bytes.
*/
void*
u2_me_malloc(c3_w len_w)
{
  return u2_me_walloc((len_w + 3) >> 2);
}

/* u2_me_free(): free storage.
*/
void
u2_me_free(void* tox_v)
{
  u2_me_box* box_u = u2_me_botox(tox_v);
  c3_w*      box_w = (c3_w *)(void *)box_u;

  c3_assert(box_u->use_w != 0);
  box_u->use_w -= 1;
  if ( 0 != box_u->use_w ) return;

  /* Clear the contents of the block, for debugging.
  */
  {
    c3_w i_w;

    for ( i_w = c3_wiseof(u2_me_box); (i_w + 1) < box_u->siz_w; i_w++ ) {
      ((c3_w*)tox_v)[i_w] = 0xdeadbeef;
    }
  }

  if ( u2_yes == u2_me_is_north ) {
    /* Try to coalesce with the block below.
    */
    if ( box_w != u2R->rut_w ) {
      c3_w       laz_w = *(box_w - 1);
      u2_me_box* pox_u = (u2_me_box*)(void *)(box_w - laz_w);

      if ( 0 == pox_u->use_w ) {
        _me_box_detach(pox_u);
        _me_box_make(pox_u, (laz_w + box_u->siz_w), 0);

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
      u2_me_box* nox_u = (u2_me_box*)(void *)(box_w + box_u->siz_w);

      if ( 0 == nox_u->use_w ) {
        _me_box_detach(nox_u);
        _me_box_make(box_u, (box_u->siz_w + nox_u->siz_w), 0);
      }
      _me_box_attach(box_u);
    }
  }
  else {
    /* Try to coalesce with the block above.
    */
    if ( (box_w + box_u->siz_w) != u2R->rut_w ) {
      u2_me_box* nox_u = (u2_me_box*)(void *)(box_w + box_u->siz_w);

      if ( 0 == nox_u->use_w ) {
        _me_box_detach(nox_u);
        _me_box_make(box_u, (box_u->siz_w + nox_u->siz_w), 0);

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
      u2_me_box* pox_u = (u2_me_box*)(void *)(box_w - laz_w);

      if ( 0 == pox_u->use_w ) {
        _me_box_detach(pox_u);
        _me_box_make(pox_u, (laz_w + box_u->siz_w), 0);
      }
      _me_box_attach(box_u);
    }
  }
}

/* _me_north_senior(): yes iff only in the senior region.
*/
static u2_bean
_me_north_senior(u2_noun dog)
{
  c3_w* dog_w = u2_me_to_ptr(dog);

  return u2_say((dog_w < u2R->rut_w) || (dog_w >= u2R->mat_w));
}

/* _me_north_junior(): yes iff only in the junior section.
*/
static u2_bean
_me_north_junior(u2_noun dog)
{
  c3_w* dog_w = u2_me_to_ptr(dog);

  return u2_say((dog_w >= u2R->cap_w) && (dog_w < u2R->mat_w));
}
/* _me_north_normal(): yes iff only in the normal heap.
*/
static u2_bean
_me_north_normal(u2_noun dog)
{
  return u2_and(u2_not(_me_north_senior(dog)), 
                u2_not(_me_north_junior(dog)));
}

/* _me_south_senior(): yes iff only in the senior region.
*/
static u2_bean
_me_south_senior(u2_noun dog)
{
  c3_w* dog_w = u2_me_to_ptr(dog);

  return u2_say((dog_w >= u2R->mat_w) || (dog_w < u2R->cap_w));
}
/* _me_south_junior(): yes iff only in the junior section.
*/
static u2_bean
_me_south_junior(u2_noun dog)
{
  c3_w* dog_w = u2_me_to_ptr(dog);

  return u2_say((dog_w >= u2R->cap_w) && (dog_w < u2R->mat_w));
}
/* _me_south_normal(): yes iff only in the normal heap.
*/
static u2_bean
_me_south_normal(u2_noun dog)
{
  return u2_and(u2_not(_me_south_senior(dog)), 
                u2_not(_me_south_junior(dog)));
}

/* _me_wash_north(): clean up mug slots after copy.
*/
static void _me_wash_north(u2_noun dog);
static void
_me_wash_north_in(u2_noun som)
{
  if ( u2_so(u2_me_is_cat(som)) ) return;
  if ( u2_ne(_me_north_junior(som)) ) return;

  _me_wash_north(som);
}
static void
_me_wash_north(u2_noun dog)
{
  c3_assert(u2_me_is_dog(dog));
  c3_assert(u2_yes == _me_north_junior(dog));
  {
    u2_me_noun* dog_u = u2_me_to_ptr(dog);

    if ( dog_u->mug_w >> 31 ) { dog_u->mug_w = 0; }

    if ( u2_so(u2_me_is_pom(dog)) ) {
      u2_me_cell* god_u = (u2_me_cell *)(void *)dog_u;
    
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
  if ( u2_so(u2_me_is_cat(som)) ) return;
  if ( u2_ne(_me_south_junior(som)) ) return;

  _me_wash_south(som);
}
static void
_me_wash_south(u2_noun dog)
{
  c3_assert(u2_me_is_dog(dog));
  c3_assert(u2_yes == _me_south_junior(dog));
  {
    u2_me_noun* dog_u = u2_me_to_ptr(dog);

    if ( dog_u->mug_w >> 31 ) { dog_u->mug_w = 0; }

    if ( u2_so(u2_me_is_pom(dog)) ) {
      u2_me_cell* god_u = (u2_me_cell *)(void *)dog_u;
    
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
  c3_w* dog_w      = u2_me_to_ptr(dog);
  u2_me_box* box_u = u2_me_botox(dog_w);

  box_u = u2_me_botox(dog_w);
  if ( 0xffffffff == box_u->use_w ) {
    u2_me_bail(c3__fail);
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
  if ( u2_so(u2_me_is_cat(som)) ) {
    return som;
  }
  else { 
    u2_noun dog = som;

    if ( u2_so(_me_north_senior(dog)) ) {
      return dog;
    }
    else if ( u2_so(_me_north_junior(dog)) ) {
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
  c3_assert(u2_yes == _me_north_junior(dog));

  if ( u2_ne(_me_north_junior(dog)) ) {
    if ( u2_ne(_me_north_senior(dog)) ) {
      _me_gain_use(dog);
    }
    return dog;
  } 
  else {
    u2_me_noun* dog_u = u2_me_to_ptr(dog);

    /* Borrow mug slot to record new destination.
    */
    if ( dog_u->mug_w >> 31 ) {
      u2_noun nov = (u2_noun) dog_u->mug_w;

      c3_assert(u2_so(_me_north_normal(nov)));
      _me_gain_use(nov);

      return nov;
    }
    else {
      if ( u2_yes == u2_me_is_pom(dog) ) {
        u2_me_cell* old_u = u2_me_to_ptr(dog);
        c3_w*       new_w = u2_me_walloc(c3_wiseof(u2_me_cell));
        u2_noun     new   = u2_me_de_twin(dog, new_w);
        u2_me_cell* new_u = (u2_me_cell*)(void *)new_w;

        new_u->mug_w = old_u->mug_w;
        new_u->hed = _me_copy_north_in(old_u->hed);
        new_u->tel = _me_copy_north_in(old_u->tel);

        /* Borrow mug slot to record new destination.
        */
        old_u->mug_w = new;
        return new;
      } 
      else {
        u2_me_atom* old_u = u2_me_to_ptr(dog);
        c3_w*       new_w = u2_me_walloc(old_u->len_w + c3_wiseof(u2_me_atom));
        u2_noun     new   = u2_me_de_twin(dog, new_w);
        u2_me_atom* new_u = (u2_me_atom*)(void *)new_w;

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
  if ( u2_so(u2_me_is_cat(som)) ) {
    return som;
  }
  else { 
    u2_noun dog = som;

    if ( u2_so(_me_south_senior(dog)) ) {
      return dog;
    }
    else if ( u2_so(_me_south_junior(dog)) ) {
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
  c3_assert(u2_yes == _me_south_junior(dog));

  if ( u2_ne(_me_south_junior(dog)) ) {
    if ( u2_ne(_me_south_senior(dog)) ) {
      _me_gain_use(dog);
    }
    return dog;
  } 
  else {
    u2_me_noun* dog_u = u2_me_to_ptr(dog);

    /* Borrow mug slot to record new destination.
    */
    if ( dog_u->mug_w >> 31 ) {
      u2_noun nov = (u2_noun) dog_u->mug_w;

      c3_assert(u2_so(_me_south_normal(nov)));
      _me_gain_use(nov);

      return nov;
    }
    else {
      if ( u2_yes == u2_me_is_pom(dog) ) {
        u2_me_cell* old_u = u2_me_to_ptr(dog);
        c3_w*       new_w = u2_me_walloc(c3_wiseof(u2_me_cell));
        u2_noun     new   = u2_me_de_twin(dog, new_w);
        u2_me_cell* new_u = (u2_me_cell*)(void *)new_w;

        new_u->mug_w = old_u->mug_w;
        new_u->hed = _me_copy_south_in(old_u->hed);
        new_u->tel = _me_copy_south_in(old_u->tel);

        /* Borrow mug slot to record new destination.
        */
        old_u->mug_w = new;
        return new;
      } 
      else {
        u2_me_atom* old_u = u2_me_to_ptr(dog);
        c3_w*       new_w = u2_me_walloc(old_u->len_w + c3_wiseof(u2_me_atom));
        u2_noun     new   = u2_me_de_twin(dog, new_w);
        u2_me_atom* new_u = (u2_me_atom*)(void *)new_w;

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
  c3_assert(u2_none != dog);
  if ( u2_yes == _me_north_senior(dog) ) {
    /*  senior pointers are not refcounted
    */
    return dog;
  }
  else if ( u2_yes == _me_north_junior(dog) ) {
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
  if ( u2_yes == _me_south_senior(dog) ) {
    /*  senior pointers are not refcounted
    */
    return dog;
  }
  else if ( u2_yes == _me_south_junior(dog) ) {
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

/* u2_me_gain(): gain and/or copy juniors.
*/
u2_noun
u2_me_gain(u2_noun som)
{
  c3_assert(u2_none != som);

  if ( u2_so(u2_me_is_cat(som)) ) {
    return som;
  }
  else {
    return u2_so(u2_me_is_north)
              ? _me_gain_north(som)
              : _me_gain_south(som);
  }
}
