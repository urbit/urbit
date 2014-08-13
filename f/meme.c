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
    rod_u = _me_boot_north(u2R->cap_w, (u2R->hat_w - u2R->cap_w));
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

/* _me_north_is_senior(): yes iff only in the senior region.
*/
static u2_bean
_me_north_is_senior(u2_noun dog)
{
  c3_w* dog_w = u2_me_to_ptr(dog);

  return u2_say((dog_w < u2R->rut_w) || (dog_w >= u2R->mat_w));
}

/* _me_north_is_junior(): yes iff only in the junior section.
*/
static u2_bean
_me_north_is_junior(u2_noun dog)
{
  c3_w* dog_w = u2_me_to_ptr(dog);

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
  c3_w* dog_w = u2_me_to_ptr(dog);

  return u2_say((dog_w >= u2R->mat_w) || (dog_w < u2R->cap_w));
}
/* _me_south_is_junior(): yes iff only in the junior section.
*/
static u2_bean
_me_south_is_junior(u2_noun dog)
{
  c3_w* dog_w = u2_me_to_ptr(dog);

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

/* u2_me_is_junior(): yes iff (som) is junior.
*/
u2_bean
u2_me_is_junior(u2_noun som)
{
  if ( u2_so(u2_me_is_cat(som)) ) {
    return u2_no;
  }
  else { 
    if ( u2_so(u2_me_is_north) ) {
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
  if ( u2_so(u2_me_is_cat(som)) ) return;
  if ( u2_ne(_me_north_is_junior(som)) ) return;

  _me_wash_north(som);
}
static void
_me_wash_north(u2_noun dog)
{
  c3_assert(u2_me_is_dog(dog));
  c3_assert(u2_yes == _me_north_is_junior(dog));
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
  if ( u2_ne(_me_south_is_junior(som)) ) return;

  _me_wash_south(som);
}
static void
_me_wash_south(u2_noun dog)
{
  c3_assert(u2_me_is_dog(dog));
  c3_assert(u2_yes == _me_south_is_junior(dog));
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

  if ( 0xffffffff == box_u->use_w ) {
    u2_me_bail(c3__fail);
  }
  else {
    box_u->use_w += 1;
  }
}

/* _me_lose_use(): increment use count.
*/
static void
_me_lose_use(u2_noun dog)
{
  c3_w* dog_w      = u2_me_to_ptr(dog);
  u2_me_box* box_u = u2_me_botox(dog_w);

  if ( box_u->use_w > 1 ) {
    box_u->use_w -= 1;
  }
  else {
    if ( 0 == box_u->use_w ) {
      u2_me_bail(c3__foul);
    }
    else u2_me_free(dog_w);
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
    u2_me_noun* dog_u = u2_me_to_ptr(dog);

    /* Borrow mug slot to record new destination.
    */
    if ( dog_u->mug_w >> 31 ) {
      u2_noun nov = (u2_noun) dog_u->mug_w;

      c3_assert(u2_so(_me_north_is_normal(nov)));
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
    u2_me_noun* dog_u = u2_me_to_ptr(dog);

    /* Borrow mug slot to record new destination.
    */
    if ( dog_u->mug_w >> 31 ) {
      u2_noun nov = (u2_noun) dog_u->mug_w;

      c3_assert(u2_so(_me_south_is_normal(nov)));
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
static u2_noun
_me_lose_north(u2_noun dog)
{
  if ( u2_yes == _me_north_is_normal(dog) ) {
    _me_lose_use(dog);
  }
  return dog;
}

/* _me_lose_south(): lose on a south road.
*/
static u2_noun
_me_lose_south(u2_noun dog)
{
  if ( u2_yes == _me_north_is_normal(dog) ) {
    _me_lose_use(dog);
  }
  return dog;
}

/* u2_me_gain(): gain a reference count, and/or copy juniors.
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

/* u2_me_lose(): lose a reference count.
*/
void
u2_me_lose(u2_noun som)
{
  if ( u2_ne(u2_me_is_cat(som)) ) {
    if ( u2_so(u2_me_is_north) ) {
      _me_lose_north(som);
      _me_lose_south(som);
    }
  }
}

/* u2_me_use(): reference count.
*/
c3_w
u2_me_use(u2_noun som)
{
  if ( u2_so(u2_me_is_cat(som)) ) {
    return 1;
  } 
  else {
    c3_w* dog_w      = u2_me_to_ptr(som);
    u2_me_box* box_u = u2_me_botox(dog_w);

    return box_u->use_w;
  }
}

/* u2_me_slab(): create a length-bounded proto-atom.
*/
c3_w*
u2_me_slab(c3_w len_w)
{
  c3_w*       nov_w = u2_me_walloc(len_w + c3_wiseof(u2_me_atom));
  u2_me_atom* pug_u = (void *)nov_w;

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

/* u2_me_slaq(): u2_me_slaq() with a defined blocksize.
*/
c3_w*
u2_me_slaq(c3_g met_g, c3_w len_w)
{
  return u2_me_slab(((len_w << met_g) + 31) >> 5);
}

/* u2_me_malt(): measure and finish a proto-atom.
*/
u2_noun
u2_me_malt(c3_w* sal_w)
{
  c3_w*       nov_w = (sal_w - c3_wiseof(u2_me_atom));
  u2_me_atom* nov_u = (void *)nov_w;
  c3_w        len_w;

  for ( len_w = nov_u->len_w; len_w; len_w-- ) {
    if ( 0 != nov_u->buf_w[len_w - 1] ) {
      break;
    }
  }
  return u2_me_mint(sal_w, len_w);
}

/* u2_me_moot(): finish a pre-measured proto-atom; dangerous.
*/
u2_noun
u2_me_moot(c3_w* sal_w)
{
  c3_w*       nov_w = (sal_w - c3_wiseof(u2_me_atom));
  u2_me_atom* nov_u = (void*)nov_w;
  c3_w        len_w = nov_u->len_w;
  c3_w        las_w = nov_u->buf_w[len_w - 1];

  c3_assert(0 != len_w);
  c3_assert(0 != las_w);

  if ( 1 == len_w ) {
    if ( u2_so(u2_me_is_cat(las_w)) ) {
      u2_me_free(nov_w);

      return las_w;
    }
  }
  return u2_me_to_pug(u2_me_outa(nov_w));
}

/* u2_me_mint(): finish a measured proto-atom.
*/
u2_noun
u2_me_mint(c3_w* sal_w, c3_w len_w)
{
  c3_w*       nov_w = (sal_w - c3_wiseof(u2_me_atom));
  u2_me_atom* nov_u = (void*)nov_w;

  /* See if we can free the slab entirely.
  */
  if ( len_w == 0 ) {
    u2_me_free(nov_w);

    return 0;
  }
  else if ( len_w == 1 ) {
    c3_w low_w = nov_u->buf_w[0];

    if ( u2_so(u2_me_is_cat(low_w)) ) {
      u2_me_free(nov_w);

      return low_w;
    }
  }

  /* See if we can strip off a block on the end.
  */
  {
    c3_w old_w = nov_u->len_w;
    c3_w dif_w = (old_w - len_w);

    if ( dif_w >= u2_me_minimum ) {
      c3_w* box_w = (void *)u2_me_botox(nov_w);
      c3_w* end_w = (nov_w + c3_wiseof(u2_me_atom) + len_w + 1);
      c3_w  asz_w = (end_w - box_w);
      c3_w  bsz_w = box_w[0] - asz_w;

      _me_box_attach(_me_box_make(end_w, bsz_w, 0));

      box_w[0] = asz_w;
      box_w[asz_w - 1] = asz_w;
    }
    nov_u->len_w = len_w;
  }
  return u2_me_to_pug(u2_me_outa(nov_w));
}

/* u2_me_words():
**
**   Copy [a] words from [b] into an atom.
*/
u2_noun
u2_me_words(c3_w        a_w,
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
    c3_w*       nov_w = u2_me_walloc(a_w + c3_wiseof(u2_me_atom));
    u2_me_atom* nov_u = (void*)nov_w;

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
    return u2_me_to_pug(u2_me_outa(nov_w));
  }
}

/* u2_me_bytes():
**
**   Copy `a` bytes from `b` to an LSB first atom.
*/
u2_noun
u2_me_bytes(c3_w        a_w,
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
    c3_w*       nov_w = u2_me_walloc((len_w + c3_wiseof(u2_me_atom)));
    u2_me_atom* nov_u = (void*)nov_w;

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
    return u2_me_to_pug(u2_me_outa(nov_w));
  }
}

/* u2_me_mp():
**
**   Copy the GMP integer `a` into an atom, and clear it.
*/
u2_noun
u2_me_mp(mpz_t a_mp)
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

    return u2_me_words(pyg_w, buz_w);
  }
}

/* u2_mr_mp():
**
**   Copy (b) into (a_mp).
*/
void
u2_mr_mp(mpz_t   a_mp,
         u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_so(u2_me_is_atom(b)));

  if ( u2_so(u2_me_is_cat(b)) ) {
    mpz_init_set_ui(a_mp, b);
  }
  else {
    u2_me_atom* b_u   = u2_me_to_ptr(b);
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

/* u2_me_vint():
**
**   Create `a + 1`.
*/
u2_noun
u2_me_vint(u2_noun a)
{
  c3_assert(u2_none != a);

  if ( u2_so(u2_me_is_cat(a)) ) {
    c3_w vin_w = (a + 1);

    if ( a == 0x7fffffff ) {
      return u2_me_words(1, &vin_w);
    }
    else return vin_w;
  }
  else if ( u2_so(u2_me_is_cell(a)) ) {
    return u2_me_bail(c3__exit);
  }
  else {
    mpz_t a_mp;

    u2_mr_mp(a_mp, a);
    u2_me_lose(a);

    mpz_add_ui(a_mp, a_mp, 1);
    return u2_me_mp(a_mp);
  }
}

/* u2_me_cons():
**
**   Produce the cell `[a b]`.
*/
u2_noun
u2_me_cons(u2_noun a, u2_noun b)
{
  c3_assert(u2_none != a);
  c3_assert(u2_none != b);

  c3_assert(u2_ne(u2_me_is_junior(a)));
  c3_assert(u2_ne(u2_me_is_junior(b)));

  {
    c3_w*       nov_w = u2_me_walloc(c3_wiseof(u2_me_cell));
    u2_me_cell* nov_u = (void *)nov_w;

    nov_u->mug_w = 0;
    nov_u->hed = a;
    nov_u->tel = b;

    return u2_me_to_pom(u2_me_outa(nov_w));
  }
}

/* u2_me_molt():
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
      return u2_me_gain(som);
    }
    else if ( (len_w == 1) && (1 == pms_m[0].axe_w) ) {
      return pms_m[0].som;
    }
    else {
      c3_w cut_w = _molt_cut(len_w, pms_m);

      if ( u2_no == u2_me_is_cell(som) ) {
        return u2_me_bail(c3__exit);
      } 
      else {
        return u2_me_cons
           (_molt_apply(u2_h(som), cut_w, pms_m),
            _molt_apply(u2_t(som), (len_w - cut_w), (pms_m + cut_w)));
      }
    }
  }
u2_noun 
u2_me_molt(u2_noun som, ...)
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
  u2_me_lose(som);
  return pro;
}

