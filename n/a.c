/* g/a.c
**
** This file is in the public domain.
*/
#include "all.h"

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
      if ( i_w == u3a_fbox_no ) {
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
u3a_box*
_box_make(void* box_v, c3_w siz_w, c3_w use_w)
{
  u3a_box* box_u = box_v;
  c3_w*      box_w = box_v;

  c3_assert(siz_w >= u3a_minimum);

  box_w[0] = siz_w;
  box_w[siz_w - 1] = siz_w;
  box_u->use_w = use_w;
  c3_assert(2 != use_w);

# ifdef  U3_MEMORY_DEBUG
    box_u->cod_w = u3_Code;
    box_u->eus_w = 0;
# endif

  return box_u;
}

/* _box_attach(): attach a box to the free list.
*/
void
_box_attach(u3a_box* box_u)
{
  c3_assert(box_u->siz_w >= (1 + c3_wiseof(u3a_fbox)));
  c3_assert(0 != u3of(u3a_fbox, box_u));

#if 0
  //  For debugging, fill the box with beef.
  {
    c3_w* box_w = (void *)box_u;
    c3_w  i_w;

    for ( i_w = c3_wiseof(u3a_box); (i_w + 1) < box_u->siz_w; i_w++ ) {
      box_w[i_w] = 0xdeadbeef;
    }
  }
#endif

  u3R->all.fre_w += box_u->siz_w;
  {
    c3_w             sel_w = _box_slot(box_u->siz_w);
    u3p(u3a_fbox)  fre_p = u3of(u3a_fbox, box_u);
    u3p(u3a_fbox)* pfr_p = &u3R->all.fre_p[sel_w];
    u3p(u3a_fbox)  nex_p = *pfr_p;

    u3to(u3a_fbox, fre_p)->pre_p = 0;
    u3to(u3a_fbox, fre_p)->nex_p = nex_p;
    if ( u3to(u3a_fbox, fre_p)->nex_p ) {
      u3to(u3a_fbox, u3to(u3a_fbox, fre_p)->nex_p)->pre_p = fre_p;
    }
    (*pfr_p) = fre_p;
  }
}

/* _box_detach(): detach a box from the free list.
*/
void
_box_detach(u3a_box* box_u)
{
  u3p(u3a_fbox) fre_p = u3of(u3a_fbox, box_u);
  u3p(u3a_fbox) pre_p = u3to(u3a_fbox, fre_p)->pre_p;
  u3p(u3a_fbox) nex_p = u3to(u3a_fbox, fre_p)->nex_p;

  u3R->all.fre_w -= box_u->siz_w;

  if ( nex_p ) {
    c3_assert(u3to(u3a_fbox, nex_p)->pre_p == fre_p);
    u3to(u3a_fbox, nex_p)->pre_p = pre_p;
  }
  if ( pre_p ) {
    c3_assert(u3to(u3a_fbox, pre_p)->nex_p == fre_p);
    u3to(u3a_fbox, pre_p)->nex_p = nex_p;
  }
  else {
    c3_w sel_w = _box_slot(box_u->siz_w);

    c3_assert(fre_p == u3R->all.fre_p[sel_w]);
    u3R->all.fre_p[sel_w] = nex_p;
  }
}

/* _me_align_pad(): pad to first point after pos_p aligned at (ald_w, alp_w).
*/
static __inline__ c3_w
_me_align_pad(u3_post pos_p, c3_w ald_w, c3_w alp_w)
{
  c3_w adj_w = (ald_w - (alp_w + 1));
  c3_p off_p = (pos_p + adj_w);
  c3_p orp_p = off_p &~ (ald_w - 1);
  c3_p fin_p = orp_p + alp_w;
  c3_w pad_w = (fin_p - pos_p);

  return pad_w;
}

/* _me_align_dap(): pad to last point before pos_p aligned at (ald_w, alp_w).
*/
static __inline__ c3_w
_me_align_dap(u3_post pos_p, c3_w ald_w, c3_w alp_w)
{
  c3_w adj_w = alp_w;
  c3_p off_p = (pos_p - adj_w);
  c3_p orp_p = (off_p &~ (ald_w - 1));
  c3_p fin_p = orp_p + alp_w;
  c3_w pad_w = (pos_p - fin_p);

  return pad_w;
}

/* _ca_box_make_hat(): in u3R, allocate directly on the hat.
*/
static u3a_box*
_ca_box_make_hat(c3_w len_w, c3_w ald_w, c3_w alp_w, c3_w use_w)
{
  c3_w    pad_w;
  u3_post all_p;
     
  if ( c3y == u3a_is_north(u3R) ) {
    all_p = u3R->hat_p;
    pad_w = _me_align_pad(all_p, ald_w, alp_w);

    u3R->hat_p += (len_w + pad_w);

    if ( u3R->hat_p >= u3R->cap_p ) {
      u3m_bail(c3__meme); return 0;
    }
  }  
  else {
    all_p = (u3R->hat_p - len_w);
    pad_w = _me_align_dap(all_p, ald_w, alp_w);
    all_p -= pad_w;
    u3R->hat_p = all_p;

    if ( u3R->hat_p <= u3R->cap_p ) {
      u3m_bail(c3__meme); return 0;
    }
  }
  return _box_make(u3a_into(all_p), (len_w + pad_w), use_w);
}

#if 0
/* _me_road_all_hat(): in u3R, allocate directly on the hat.
*/
static u3a_box*
_ca_box_make_hat(c3_w len_w, c3_w alm_w, c3_w use_w)
{
  return _box_make(_me_road_all_hat(len_w), len_w, use_w);
}
#endif

#if 0  // not yet used
/* _me_road_all_cap(): in u3R, allocate directly on the cap.
*/
static c3_w*
_me_road_all_cap(c3_w len_w)
{
  if ( len_w > u3a_open(u3R) ) {
    u3m_bail(c3__meme); return 0;
  }

  if ( c3y == u3a_is_north(u3R) ) {
    u3R->cap_p -= len_w;
    return u3a_into(u3R->cap_p);
  }  
  else {
    u3_post all_p;
     
    all_p = u3R->cap_p;
    u3R->cap_p += len_w;
    return u3a_into(all_p);
  }
}
#endif

#if 0
/* u3a_sane(): check allocator sanity.
*/
void
u3a_sane(void)
{
  c3_w i_w;
   
  for ( i_w = 0; i_w < u3a_fbox_no; i_w++ ) {
    u3a_fbox* fre_u = u3R->all.fre_u[i_w];
 
    while ( fre_u ) {
      if ( fre_u == u3R->all.fre_u[i_w] ) {
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

/* _ca_willoc(): u3a_walloc() internals.
*/
static void*
_ca_willoc(c3_w len_w, c3_w ald_w, c3_w alp_w)
{
  c3_w siz_w = c3_max(u3a_minimum, u3a_boxed(len_w));
  c3_w sel_w = _box_slot(siz_w);

  alp_w = (alp_w + c3_wiseof(u3a_box)) % ald_w;

  //  XX: this logic is totally bizarre, but preserve it.
  //
  if ( (sel_w != 0) && (sel_w != u3a_fbox_no - 1) ) {
    sel_w += 1;
  }

  // fprintf(stderr, "walloc %d: *pfr_p %x\n", len_w, u3R->all.fre_p[sel_w]);
  while ( 1 ) {
    u3p(u3a_fbox) *pfr_p = &u3R->all.fre_p[sel_w];

    while ( 1 ) {
      if ( 0 == *pfr_p ) {
        if ( sel_w < (u3a_fbox_no - 1) ) {
          sel_w += 1;
          break;
        }
        else {
          /* Nothing in top free list.  Chip away at the hat.
          */
          return u3a_boxto(_ca_box_make_hat(siz_w, ald_w, alp_w, 1));
        }
      }
      else {
        c3_w pad_w = _me_align_pad(*pfr_p, ald_w, alp_w);

        if ( 1 == ald_w ) c3_assert(0 == pad_w);

        if ( (siz_w + pad_w) > u3to(u3a_fbox, *pfr_p)->box_u.siz_w ) {
          /* This free block is too small.  Continue searching.
          */
          pfr_p = &(u3to(u3a_fbox, *pfr_p)->nex_p);
          continue;
        } 
        else {
          u3a_box* box_u = &(u3to(u3a_fbox, *pfr_p)->box_u);

          /* We have found a free block of adequate size.  Remove it
          ** from the free list.
          */
          siz_w += pad_w;
          {
            {
              c3_assert((0 == u3to(u3a_fbox, *pfr_p)->pre_p) || 
                  (u3to(u3a_fbox, u3to(u3a_fbox, *pfr_p)->pre_p)->nex_p 
                        == (*pfr_p)));

              c3_assert((0 == u3to(u3a_fbox, *pfr_p)->nex_p) || 
                  (u3to(u3a_fbox, u3to(u3a_fbox, *pfr_p)->nex_p)->pre_p 
                        == (*pfr_p)));
            }

            if ( 0 != u3to(u3a_fbox, *pfr_p)->nex_p ) {
              u3to(u3a_fbox, u3to(u3a_fbox, *pfr_p)->nex_p)->pre_p = 
                u3to(u3a_fbox, *pfr_p)->pre_p;
            }
            *pfr_p = u3to(u3a_fbox, *pfr_p)->nex_p;
          }

          /* If we can chop off another block, do it.
          */
          if ( (siz_w + u3a_minimum) <= box_u->siz_w ) {
            /* Split the block.
            */ 
            c3_w* box_w = ((c3_w *)(void *)box_u);
            c3_w* end_w = box_w + siz_w;
            c3_w  lef_w = (box_u->siz_w - siz_w);

            _box_attach(_box_make(end_w, lef_w, 0));
            return u3a_boxto(_box_make(box_w, siz_w, 1));
          }
          else {
            c3_assert(0 == box_u->use_w);
            box_u->use_w = 1;

#ifdef      U3_MEMORY_DEBUG
              box_u->cod_w = u3_Code; 
#endif
            return u3a_boxto(box_u);
          }
        }
      }
    }
  }
}

/* _ca_walloc(): u3a_walloc() internals.
*/
static void*
_ca_walloc(c3_w len_w, c3_w ald_w, c3_w alp_w)
{
  void* ptr_v;
  
  u3t_on(mal_o);
  ptr_v = _ca_willoc(len_w, ald_w, alp_w);
  u3t_off(mal_o);

#if 0
  if ( SUB ) {
    fprintf(stderr, "sub: at %p; kid %p\r\n", 
            ptr_v, 
            u3R->kid_u);

    fprintf(stderr, "this: hat %p, cap %p, rut %p, mat %p\r\n",
                    u3a_into(u3R->hat_p),
                    u3a_into(u3R->cap_p),
                    u3a_into(u3R->rut_p),
                    u3a_into(u3R->mat_p));
    
    if ( u3R->kid_u ) {
      fprintf(stderr, "kids: hat %p, cap %p, rut %p, mat %p\r\n\n",
                      u3a_into(u3R->kid_u->hat_p),
                      u3a_into(u3R->kid_u->cap_p),
                      u3a_into(u3R->kid_u->rut_p),
                      u3a_into(u3R->kid_u->mat_p));
    }
  }
#endif
#if 0
  if ( u3a_botox(ptr_v) == (u3a_box*)(void *)0x27f50a02c ) {
    static int xuc_i;

    printf("xuc_i %d\r\n", xuc_i);
    // if ( 2 == xuc_i ) { abort(); }
    xuc_i++;
  }
#endif
  return ptr_v;
}

/* u3a_walloc(): allocate storage words on hat.
*/
void*
u3a_walloc(c3_w len_w)
{
  void* ptr_v;
 
  ptr_v = _ca_walloc(len_w, 1, 0);

#if 0
  if ( (703 == u3_Code) &&
      u3a_botox(ptr_v) == (u3a_box*)(void *)0x200dfe3e4 ) {
    static int xuc_i;

    printf("xuc_i %d\r\n", xuc_i);
    if ( 1 == xuc_i ) { 
      u3a_box* box_u = u3a_botox(ptr_v);

      box_u->cod_w = 999;
    }
    xuc_i++;
  }
#endif
  return ptr_v;
}

/* u3a_wealloc(): realloc in words.
*/
void*
u3a_wealloc(void* lag_v, c3_w len_w)
{
  if ( !lag_v ) {
    return u3a_malloc(len_w);
  } 
  else {
    u3a_box* box_u = u3a_botox(lag_v);
    c3_w*      old_w = lag_v;
    c3_w       tiz_w = c3_min(box_u->siz_w, len_w);
    {
      c3_w* new_w = u3a_walloc(len_w);
      c3_w  i_w;

      for ( i_w = 0; i_w < tiz_w; i_w++ ) {
        new_w[i_w] = old_w[i_w];
      }
      u3a_wfree(lag_v);
      return new_w;
    }
  }
}

/* u3a_wfree(): free storage.
*/
void
u3a_wfree(void* tox_v)
{
  u3a_box* box_u = u3a_botox(tox_v);
  c3_w*      box_w = (c3_w *)(void *)box_u;

  u3t_on(mal_o);

  c3_assert(box_u->use_w != 0);
  box_u->use_w -= 1;
  if ( 0 != box_u->use_w ) {
    u3t_off(mal_o);
    return;
  }

#if 0
  /* Clear the contents of the block, for debugging.
  */
  {
    c3_w i_w;

    for ( i_w = c3_wiseof(u3a_box); (i_w + 1) < box_u->siz_w; i_w++ ) {
      box_w[i_w] = 0xdeadbeef;
    }
  }
#endif

  if ( c3y == u3a_is_north(u3R) ) {
    /* Try to coalesce with the block below.
    */
    if ( box_w != u3a_into(u3R->rut_p) ) {
      c3_w       laz_w = *(box_w - 1);
      u3a_box* pox_u = (u3a_box*)(void *)(box_w - laz_w);

      if ( 0 == pox_u->use_w ) {
        _box_detach(pox_u);
        _box_make(pox_u, (laz_w + box_u->siz_w), 0);

        box_u = pox_u;
        box_w = (c3_w*)(void *)pox_u;
      }
    }

    /* Try to coalesce with the block above, or the wilderness.
    */
    if ( (box_w + box_u->siz_w) == u3a_into(u3R->hat_p) ) {
      u3R->hat_p = u3a_outa(box_w);
    }
    else {
      u3a_box* nox_u = (u3a_box*)(void *)(box_w + box_u->siz_w);

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
    if ( (box_w + box_u->siz_w) != u3a_into(u3R->rut_p) ) {
      u3a_box* nox_u = (u3a_box*)(void *)(box_w + box_u->siz_w);

      if ( 0 == nox_u->use_w ) {
        _box_detach(nox_u);
        _box_make(box_u, (box_u->siz_w + nox_u->siz_w), 0);
      }
    }

    /* Try to coalesce with the block below, or with the wilderness.
    */
    if ( box_w == u3a_into(u3R->hat_p) ) {
      u3R->hat_p = u3a_outa(box_w + box_u->siz_w);
    }
    else {
      c3_w laz_w = *(box_w - 1);
      u3a_box* pox_u = (u3a_box*)(void *)(box_w - laz_w);

      if ( 0 == pox_u->use_w ) {
        _box_detach(pox_u);
        _box_make(pox_u, (laz_w + box_u->siz_w), 0);
        box_u = pox_u;
      }
      _box_attach(box_u);
    }
  }
  u3t_off(mal_o);
}

#if 0
/* u3a_malloc(): allocate storage measured in bytes.
*/
void*
u3a_malloc(size_t len_i)
{
  c3_w len_w = (c3_w)len_i;

  return u3a_walloc((len_w + 3) >> 2);
}

/* u3a_realloc(): realloc in bytes.
*/
void*
u3a_realloc(void* lag_v, size_t len_i)
{
  c3_w len_w = (c3_w)len_i;

  return u3a_wealloc(lag_v, (len_w + 3) >> 2);
}

#else
/* u3a_malloc(): aligned storage measured in bytes.
*/
void*
u3a_malloc(size_t len_i)
{
  c3_w    len_w = (c3_w)((len_i + 3) >> 2);
  c3_w*   ptr_w = _ca_walloc(len_w + 1, 4, 3);
  u3_post ptr_p = u3a_outa(ptr_w);
  c3_w    pad_w = _me_align_pad(ptr_p, 4, 3);
  c3_w*   out_w = u3a_into(ptr_p + pad_w + 1);

#if 0
  if ( u3a_botox(out_w) == (u3a_box*)(void *)0x202320b88) {
    static int xuc_i;

    printf("xuc_i %d\r\n", xuc_i);
    // if ( 1 == xuc_i ) { abort(); }
    xuc_i++;
  }
#endif
  out_w[-1] = pad_w;

  return out_w;
}

/* u3a_realloc(): aligned realloc in bytes.
*/
void*
u3a_realloc(void* lag_v, size_t len_i)
{
  if ( !lag_v ) {
    return u3a_malloc(len_i);
  } 
  else {
    c3_w     len_w = (c3_w)((len_i + 3) >> 2);
    c3_w*    lag_w = lag_v;
    c3_w     pad_w = lag_w[-1];
    c3_w*    org_w = lag_w - (pad_w + 1);
    u3a_box* box_u = u3a_botox((void *)org_w);
    c3_w*    old_w = lag_v;
    c3_w     tiz_w = c3_min(box_u->siz_w, len_w);
    {
      c3_w* new_w = u3a_malloc(len_i);
      c3_w  i_w;

      for ( i_w = 0; i_w < tiz_w; i_w++ ) {
        new_w[i_w] = old_w[i_w];
      }
      u3a_wfree(org_w);
      return new_w;
    }
  }
  c3_w len_w = (c3_w)len_i;

  return u3a_wealloc(lag_v, (len_w + 3) >> 2);
}

/* u3a_realloc2(): gmp-shaped realloc.
*/
void*
u3a_realloc2(void* lag_v, size_t old_i, size_t new_i)
{
  return u3a_realloc(lag_v, new_i);
}

/* u3a_free(): free for aligned malloc.
*/
void
u3a_free(void* tox_v)
{
  c3_w* tox_w = tox_v;
  c3_w  pad_w = tox_w[-1];
  c3_w* org_w = tox_w - (pad_w + 1);

  // printf("free %p %p\r\n", org_w, tox_w);
  u3a_wfree(org_w);
}

/* u3a_free2(): gmp-shaped free.
*/
void
u3a_free2(void* tox_v, size_t siz_i)
{
  return u3a_free(tox_v);
}
#endif

#if 1
/* _me_wash_north(): clean up mug slots after copy.
*/
static void _me_wash_north(u3_noun dog);
static void
_me_wash_north_in(u3_noun som)
{
  if ( _(u3a_is_cat(som)) ) return;
  if ( !_(u3a_north_is_junior(u3R, som)) ) return;

  _me_wash_north(som);
}
static void
_me_wash_north(u3_noun dog)
{
  c3_assert(c3y == u3a_is_dog(dog));
  // c3_assert(c3y == u3a_north_is_junior(u3R, dog));
  {
    u3a_noun* dog_u = u3a_to_ptr(dog);

    if ( dog_u->mug_w == 0 ) return;  dog_u->mug_w = 0;    //  power wash
    // if ( dog_u->mug_w >> 31 ) { dog_u->mug_w = 0; }

    if ( _(u3a_is_pom(dog)) ) {
      u3a_cell* god_u = (u3a_cell *)(void *)dog_u;
    
      _me_wash_north_in(god_u->hed);
      _me_wash_north_in(god_u->tel);
    }
  } 
}

/* _me_wash_south(): clean up mug slots after copy.
*/
static void _me_wash_south(u3_noun dog);
static void
_me_wash_south_in(u3_noun som)
{
  if ( _(u3a_is_cat(som)) ) return;
  if ( !_(u3a_south_is_junior(u3R, som)) ) return;

  _me_wash_south(som);
}
static void
_me_wash_south(u3_noun dog)
{
  c3_assert(c3y == u3a_is_dog(dog));
  // c3_assert(c3y == u3a_south_is_junior(u3R, dog));
  {
    u3a_noun* dog_u = u3a_to_ptr(dog);

    if ( dog_u->mug_w == 0 ) return;  dog_u->mug_w = 0;    //  power wash
    //  if ( dog_u->mug_w >> 31 ) { dog_u->mug_w = 0; }

    if ( _(u3a_is_pom(dog)) ) {
      u3a_cell* god_u = (u3a_cell *)(void *)dog_u;
    
      _me_wash_south_in(god_u->hed);
      _me_wash_south_in(god_u->tel);
    }
  } 
}

/* u3a_wash(): wash all lazy mugs.  RETAIN.
*/
void
u3a_wash(u3_noun som)
{
  if ( _(u3a_is_cat(som)) ) {
    return;
  }
  if ( _(u3a_is_north(u3R)) ) {
    if ( _(u3a_north_is_junior(u3R, som)) ) {
      _me_wash_north(som);
    }
  } 
  else {
    if ( _(u3a_south_is_junior(u3R, som)) ) {
      _me_wash_south(som);
    }
  }
}
#endif

extern u3_noun BDA, BDB;

/* _me_gain_use(): increment use count.
*/
static void
_me_gain_use(u3_noun dog)
{
  c3_w* dog_w      = u3a_to_ptr(dog);
  u3a_box* box_u = u3a_botox(dog_w);

  if ( 0x7fffffff == box_u->use_w ) {
    u3m_bail(c3__fail);
  }
  else {
    if ( box_u->use_w == 0 ) {
      u3m_bail(c3__foul);
    }
    box_u->use_w += 1;

#ifdef U3_MEMORY_DEBUG
    // if ( u3_Code && !box_u->cod_w ) { box_u->cod_w = u3_Code; }

#if 0
    if ( u3r_mug(dog) == 0x15d47649 ) {
      static c3_w bug_w = 0;

      printf("bad %x %d %d\r\n", dog, bug_w, box_u->use_w);
      if ( bug_w == 0 ) { abort(); }
      bug_w++;
    }
#endif
#if 0
    {
      static c3_w bug_w = 0;

      if ( BDA == dog ) {
        printf("BDA %d %d\r\n", bug_w, box_u->use_w);
        // if ( bug_w == 0 ) { abort(); }
        bug_w++;
      }
    }
#endif

#if 0
    {
      static c3_w bug_w = 0;

      if ( FOO && u3a_botox(u3a_to_ptr(dog)) == (void *)0x200dfe3e4 ) {
        u3a_box* box_u = u3a_botox(u3a_to_ptr(dog));

        printf("GAIN %d %d\r\n", bug_w, box_u->use_w);
        if ( bug_w == 8 ) { abort(); }
        bug_w++;
      }
    }
#endif
#endif

  }
}

/* _me_copy_north_in(): copy subjuniors on a north road.
*/
static u3_noun _me_copy_north(u3_noun);
static u3_noun
_me_copy_north_in(u3_noun som)
{
  c3_assert(u3_none != som);
  if ( _(u3a_is_cat(som)) ) {
    return som;
  }
  else { 
    u3_noun dog = som;

    if ( _(u3a_north_is_senior(u3R, dog)) ) {
      return dog;
    }
    else if ( _(u3a_north_is_junior(u3R, dog)) ) {
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
static u3_noun
_me_copy_north(u3_noun dog)
{
  c3_assert(c3y == u3a_north_is_junior(u3R, dog));

  if ( !_(u3a_north_is_junior(u3R, dog)) ) {
    if ( !_(u3a_north_is_senior(u3R, dog)) ) {
      _me_gain_use(dog);
    }
    return dog;
  } 
  else {
    u3a_noun* dog_u = u3a_to_ptr(dog);

    /* Borrow mug slot to record new destination.
    */
    if ( dog_u->mug_w >> 31 ) {
      u3_noun nov = (u3_noun) dog_u->mug_w;

      c3_assert(_(u3a_north_is_normal(u3R, nov)));
      _me_gain_use(nov);

      return nov;
    }
    else {
      if ( c3y == u3a_is_pom(dog) ) {
        u3a_cell* old_u = u3a_to_ptr(dog);
        c3_w*       new_w = u3a_walloc(c3_wiseof(u3a_cell));
        u3_noun     new   = u3a_de_twin(dog, new_w);
        u3a_cell* new_u = (u3a_cell*)(void *)new_w;

        new_u->mug_w = old_u->mug_w;
        new_u->hed = _me_copy_north_in(old_u->hed);
        new_u->tel = _me_copy_north_in(old_u->tel);

        /* Borrow mug slot to record new destination.
        */
        old_u->mug_w = new;
        return new;
      } 
      else {
        u3a_atom* old_u = u3a_to_ptr(dog);
        c3_w*       new_w = u3a_walloc(old_u->len_w + c3_wiseof(u3a_atom));
        u3_noun     new   = u3a_de_twin(dog, new_w);
        u3a_atom* new_u = (u3a_atom*)(void *)new_w;

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
static u3_noun _me_copy_south(u3_noun);
static u3_noun
_me_copy_south_in(u3_noun som)
{
  c3_assert(u3_none != som);
  if ( _(u3a_is_cat(som)) ) {
    return som;
  }
  else { 
    u3_noun dog = som;

    if ( _(u3a_south_is_senior(u3R, dog)) ) {
      return dog;
    }
    else if ( _(u3a_south_is_junior(u3R, dog)) ) {
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
static u3_noun
_me_copy_south(u3_noun dog)
{
  c3_assert(c3y == u3a_south_is_junior(u3R, dog));

  if ( !_(u3a_south_is_junior(u3R, dog)) ) {
    if ( !_(u3a_south_is_senior(u3R, dog)) ) {
      _me_gain_use(dog);
    }
    return dog;
  } 
  else {
    u3a_noun* dog_u = u3a_to_ptr(dog);

    /* Borrow mug slot to record new destination.
    */
    if ( dog_u->mug_w >> 31 ) {
      u3_noun nov = (u3_noun) dog_u->mug_w;

      // printf("south: %p is already %p\r\n", dog_u, u3a_to_ptr(nov));

      c3_assert(_(u3a_south_is_normal(u3R, nov)));
      _me_gain_use(nov);

      return nov;
    }
    else {
      if ( c3y == u3a_is_pom(dog) ) {
        u3a_cell* old_u = u3a_to_ptr(dog);
        c3_w*       new_w = u3a_walloc(c3_wiseof(u3a_cell));
        u3_noun     new   = u3a_de_twin(dog, new_w);
        u3a_cell* new_u = (u3a_cell*)(void *)new_w;

        // printf("south: cell %p to %p\r\n", old_u, new_u);

        if ( old_u->mug_w == 0x730e66cc ) {
          fprintf(stderr, "BAD: take %p\r\n", new_u);
        }

        new_u->mug_w = old_u->mug_w;
        // new_u->mug_w = 0;
        new_u->hed = _me_copy_south_in(old_u->hed);
        new_u->tel = _me_copy_south_in(old_u->tel);

        /* Borrow mug slot to record new destination.
        */
        old_u->mug_w = new;
        return new;
      } 
      else {
        u3a_atom* old_u = u3a_to_ptr(dog);
        c3_w*       new_w = u3a_walloc(old_u->len_w + c3_wiseof(u3a_atom));
        u3_noun     new   = u3a_de_twin(dog, new_w);
        u3a_atom* new_u = (u3a_atom*)(void *)new_w;

        // printf("south: atom %p to %p\r\n", old_u, new_u);

        new_u->mug_w = old_u->mug_w;
        // new_u->mug_w = 0;
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

/* _me_take_north(): take on a north road.
*/
static u3_noun
_me_take_north(u3_noun dog)
{
  if ( c3y == u3a_north_is_senior(u3R, dog) ) {
    /*  senior pointers are not refcounted
    */
    return dog;
  }
  else if ( c3y == u3a_north_is_junior(u3R, dog) ) {
    /* junior pointers are copied
    */
    u3_noun mos = _me_copy_north(dog);

    // printf("north: %p to %p\r\n", u3a_to_ptr(dog), u3a_to_ptr(mos));
    return mos;
  }
  else {
    /* normal pointers are refcounted
    */
    _me_gain_use(dog);
    return dog;
  }
}

/* _me_take_south(): take on a south road.
*/
static u3_noun
_me_take_south(u3_noun dog)
{
  if ( c3y == u3a_south_is_senior(u3R, dog) ) {
    /*  senior pointers are not refcounted
    */
    return dog;
  }
  else if ( c3y == u3a_south_is_junior(u3R, dog) ) {
    /* junior pointers are copied
    */
    u3_noun mos = _me_copy_south(dog);

    // printf("south: %p to %p\r\n", u3a_to_ptr(dog), u3a_to_ptr(mos));
    return mos;
  }
  else {
    /* normal pointers are refcounted
    */
    _me_gain_use(dog);
    return dog;
  }
}

/* u3a_take(): gain, copying juniors.
*/
u3_noun
u3a_take(u3_noun som)
{
  c3_assert(u3_none != som);

  if ( _(u3a_is_cat(som)) ) {
    return som;
  }
  else {
    return _(u3a_is_north(u3R))
              ? _me_take_north(som)
              : _me_take_south(som);
  }
}

/* u3a_left(): true of junior if preserved.
*/
c3_o
u3a_left(u3_noun som)
{
  if ( _(u3a_is_cat(som)) ||
       !_(u3a_is_junior(u3R, som)) )
  {
    return c3y;
  }
  else {
    u3a_noun* dog_u = u3a_to_ptr(som);

    return __(0 != (dog_u->mug_w >> 31));
  }
}

/* _me_gain_north(): gain on a north road.
*/
static u3_noun
_me_gain_north(u3_noun dog)
{
  if ( c3y == u3a_north_is_senior(u3R, dog) ) {
    /*  senior pointers are not refcounted
    */
    return dog;
  }
  else {
    /* junior nouns are disallowed
    */
    c3_assert(!_(u3a_north_is_junior(u3R, dog)));

    /* normal pointers are refcounted
    */
    _me_gain_use(dog);
    return dog;
  }
}

/* _me_gain_south(): gain on a south road.
*/
static u3_noun
_me_gain_south(u3_noun dog)
{
  if ( c3y == u3a_south_is_senior(u3R, dog) ) {
    /*  senior pointers are not refcounted
    */
    return dog;
  }
  else {
    /* junior nouns are disallowed
    */
    c3_assert(!_(u3a_south_is_junior(u3R, dog)));

    /* normal nouns are refcounted
    */
    _me_gain_use(dog);
    return dog;
  }
}

/* _me_lose_north(): lose on a north road.
*/
static void
_me_lose_north(u3_noun dog)
{
top:
  if ( c3y == u3a_north_is_normal(u3R, dog) ) {
    c3_w* dog_w      = u3a_to_ptr(dog);
    u3a_box* box_u = u3a_botox(dog_w);

    if ( box_u->use_w > 1 ) {
      box_u->use_w -= 1;
    }
    else {
      if ( 0 == box_u->use_w ) {
        u3m_bail(c3__foul);
      }
      else {
        if ( _(u3a_is_pom(dog)) ) {
          u3a_cell* dog_u = (void *)dog_w;
          u3_noun     h_dog = dog_u->hed;
          u3_noun     t_dog = dog_u->tel;

          if ( !_(u3a_is_cat(h_dog)) ) {
            _me_lose_north(h_dog);
          }
          u3a_wfree(dog_w);
          if ( !_(u3a_is_cat(t_dog)) ) {
            dog = t_dog;
            goto top;
          }
        }
        else {
          u3a_wfree(dog_w);
        }
      }
    }
  }
}

/* _me_lose_south(): lose on a south road.
*/
static void
_me_lose_south(u3_noun dog)
{
top:
  if ( c3y == u3a_south_is_normal(u3R, dog) ) {
    c3_w* dog_w      = u3a_to_ptr(dog);
    u3a_box* box_u = u3a_botox(dog_w);

    if ( box_u->use_w > 1 ) {
      box_u->use_w -= 1;
    }
    else {
      if ( 0 == box_u->use_w ) {
        u3m_bail(c3__foul);
      }
      else {
        if ( _(u3a_is_pom(dog)) ) {
          u3a_cell* dog_u = (void *)dog_w;
          u3_noun     h_dog = dog_u->hed;
          u3_noun     t_dog = dog_u->tel;

          if ( !_(u3a_is_cat(h_dog)) ) {
            _me_lose_south(h_dog);
          }
          u3a_wfree(dog_w);
          if ( !_(u3a_is_cat(t_dog)) ) {
            dog = t_dog;
            goto top;
          }
        }
        else {
          u3a_wfree(dog_w);
        }
      }
    }
  }
}

/* u3a_gain(): gain a reference count in normal space.
*/
u3_noun
u3a_gain(u3_noun som)
{
  // u3t_on(mal_o);
  c3_assert(u3_none != som);

  if ( !_(u3a_is_cat(som)) ) {
    som = _(u3a_is_north(u3R))
              ? _me_gain_north(som)
              : _me_gain_south(som);
  }
  // u3t_off(mal_o);

  return som;
}

/* u3a_lose(): lose a reference count.
*/
void
u3a_lose(u3_noun som)
{
  // u3t_on(mal_o);
  if ( !_(u3a_is_cat(som)) ) {
    if ( _(u3a_is_north(u3R)) ) {
      _me_lose_north(som);
    } else {
      _me_lose_south(som);
    }
  }
  // u3t_off(mal_o);
}

/* u3a_use(): reference count.
*/
c3_w
u3a_use(u3_noun som)
{
  if ( _(u3a_is_cat(som)) ) {
    return 1;
  } 
  else {
    c3_w* dog_w      = u3a_to_ptr(som);
    u3a_box* box_u = u3a_botox(dog_w);

    return box_u->use_w;
  }
}

/* u3a_luse(): check refcount sanity.
*/
void
u3a_luse(u3_noun som)
{
  if ( 0 == u3a_use(som) ) {
    fprintf(stderr, "luse: insane %d 0x%x\r\n", som, som);
    abort();
  }
  if ( _(u3du(som)) ) {
    u3a_luse(u3h(som));
    u3a_luse(u3t(som));
  }
}

/* u3a_mark_ptr(): mark a pointer for gc.  Produce size if first mark.
*/
c3_w
u3a_mark_ptr(void* ptr_v)
{
  if ( _(u3a_is_north(u3R)) ) {
    if ( !((ptr_v >= u3a_into(u3R->rut_p)) && 
           (ptr_v < u3a_into(u3R->hat_p))) )
    {
      return 0;
    }
  }
  else {
    if ( !((ptr_v >= u3a_into(u3R->hat_p)) && 
           (ptr_v < u3a_into(u3R->rut_p))) ) 
    {
      return 0;
    }
  }
  {
    u3a_box* box_u  = u3a_botox(ptr_v);
    c3_w       siz_w;

#ifdef U3_MEMORY_DEBUG
    if ( box_u->eus_w == 0 ) {
      siz_w = box_u->siz_w;
    } 
    else {
      siz_w = 0;
    }
    box_u->eus_w += 1;
#else
    c3_ws use_ws = (c3_ws)box_u->use_w;

    if ( use_ws == 0 ) {
      fprintf(stderr, "%p is bogus\r\n", ptr_v);
      siz_w = 0;
    }
    else {
      c3_assert(use_ws != 0);

      if ( use_ws < 0 ) {
        use_ws -= 1;
        siz_w = 0;
      } 
      else {
        use_ws = -1;
        siz_w = box_u->siz_w;
      }
      box_u->use_w = (c3_w)use_ws;
    }
#endif
    return siz_w;
  }
}

/* u3a_mark_mptr(): mark a malloc-allocated ptr for gc.
*/
c3_w
u3a_mark_mptr(void* ptr_v)
{
  c3_w* ptr_w = ptr_v;
  c3_w  pad_w = ptr_w[-1];
  c3_w* org_w = ptr_w - (pad_w + 1);

  // printf("free %p %p\r\n", org_w, ptr_w);
  return u3a_mark_ptr(org_w);
}

/* u3a_mark_noun(): mark a noun for gc.  Produce size.
*/
c3_w
u3a_mark_noun(u3_noun som)
{
  c3_w siz_w = 0;

  while ( 1 ) {
    if ( _(u3a_is_senior(u3R, som)) ) {
      return siz_w;
    }
    else {
      c3_w* dog_w = u3a_to_ptr(som);
      c3_w  new_w = u3a_mark_ptr(dog_w);

      if ( 0 == new_w ) {
        return siz_w;
      }
      else {
        siz_w += new_w;
        if ( _(u3du(som)) ) {
          siz_w += u3a_mark_noun(u3h(som));
          som = u3t(som);
        }
        else return siz_w;
      }
    }
  }
}

/* u3a_print_memory: print memory amount.
*/
void
u3a_print_memory(c3_c* cap_c, c3_w wor_w)
{
  c3_w byt_w = (wor_w * 4);
  c3_w gib_w = (byt_w / 1000000000);
  c3_w mib_w = (byt_w % 1000000000) / 1000000;
  c3_w kib_w = (byt_w % 1000000) / 1000;
  c3_w bib_w = (byt_w % 1000);

  if ( byt_w ) {
    if ( gib_w ) {
      fprintf(stderr, "%s: GB/%d.%03d.%03d.%03d\r\n", 
          cap_c, gib_w, mib_w, kib_w, bib_w);
    }
    else if ( mib_w ) {
      fprintf(stderr, "%s: MB/%d.%03d.%03d\r\n", cap_c, mib_w, kib_w, bib_w);
    }
    else if ( kib_w ) {
      fprintf(stderr, "%s: KB/%d.%03d\r\n", cap_c, kib_w, bib_w);
    }
    else if ( bib_w ) {
      fprintf(stderr, "%s: B/%d\r\n", cap_c, bib_w);
    }
  }
}

/* u3a_sweep(): sweep a fully marked road.
*/
void
u3a_sweep(void)
{
  c3_w neg_w, pos_w, leq_w, weq_w, tot_w, caf_w;

  /* Measure allocated memory by counting the free list.
  */
  {
    c3_w end_w;
    c3_w fre_w = 0;
    c3_w i_w;

    end_w = _(u3a_is_north(u3R)) 
                ? (u3R->hat_p - u3R->rut_p)
                : (u3R->rut_p - u3R->hat_p);

    for ( i_w = 0; i_w < u3a_fbox_no; i_w++ ) {
      u3p(u3a_fbox) fre_p = u3R->all.fre_p[i_w];
   
      while ( fre_p ) {
        u3a_fbox* fre_u = u3to(u3a_fbox, fre_p);

        fre_w += fre_u->box_u.siz_w;
        fre_p = fre_u->nex_p;
      }
    }
    neg_w = (end_w - fre_w);
  }

  /* Sweep through the arena, repairing and counting leaks.
  */
  pos_w = leq_w = weq_w = 0;
  {
    u3_post box_p = _(u3a_is_north(u3R)) ? u3R->rut_p : u3R->hat_p;
    u3_post end_p = _(u3a_is_north(u3R)) ? u3R->hat_p : u3R->rut_p;
    c3_w*   box_w = u3a_into(box_p);
    c3_w*   end_w = u3a_into(end_p);

    while ( box_w < end_w ) {
      u3a_box* box_u = (void *)box_w;

#ifdef U3_MEMORY_DEBUG
      if ( box_u->use_w != box_u->eus_w ) {
        if ( box_u->eus_w != 0 ) {
          if ( box_u->use_w == 0 ) {
            printf("dank %p (%d, %d)\r\n", box_u, box_u->use_w, box_u->eus_w);
          }
          else {
            printf("weak %p %x (cell) %x (%d, %d)\r\n", 
                    box_u, 
                    (u3_noun)u3a_to_pom(u3a_outa(u3a_boxto(box_w))),
                    ((u3a_noun *)(u3a_boxto(box_w)))->mug_w,
                    box_u->use_w, box_u->eus_w);
            u3m_p("weak", u3a_to_pom(u3a_outa(u3a_boxto(box_w))));
          }
          weq_w += box_u->siz_w;
        }
        else {
          printf("leak %p %x (cell)/%x (%d)\r\n", 
                  box_u, 
                  (u3_noun)u3a_to_pom(u3a_outa(u3a_boxto(box_w))),
                  ((u3a_noun *)(u3a_boxto(box_w)))->mug_w
                    ? ((u3a_noun *)(u3a_boxto(box_w)))->mug_w
                    : u3r_mug(u3a_to_pom(u3a_outa(u3a_boxto(box_w)))),
                  box_u->use_w);
          u3m_p("leak", u3a_to_pom(u3a_outa(u3a_boxto(box_w))));
          leq_w += box_u->siz_w;
        }
        if ( box_u->cod_w ) {
          u3m_p("  code", box_u->cod_w);
        }
        box_u->use_w = box_u->eus_w;
      }
      else {
        if ( box_u->use_w ) {
          pos_w += box_u->siz_w;
        }
      }
      box_u->eus_w = 0;
#else
      c3_ws use_ws = (c3_ws)box_u->use_w;

      if ( use_ws > 0 ) {
        printf("leak %p\r\n", box_u);
        leq_w += box_u->siz_w;
        box_u->use_w = 0;

        _box_attach(box_u);
      }
      else if ( use_ws < 0 ) {
        pos_w += box_u->siz_w;
        box_u->use_w = (c3_w)(0 - use_ws);
      }
#endif
      box_w += box_u->siz_w;
    }
  }

  tot_w = _(u3a_is_north(u3R)) 
                ? u3R->mat_p - u3R->rut_p
                : u3R->rut_p - u3R->mat_p;
  caf_w = _(u3a_is_north(u3R)) 
                ? u3R->mat_p - u3R->cap_p
                : u3R->cap_p - u3R->mat_p;

  // u3a_print_memory("available", (tot_w - pos_w));
  // u3a_print_memory("allocated", pos_w);
  // u3a_print_memory("volatile", caf_w);
  u3a_print_memory("leaked", leq_w);
  u3a_print_memory("weaked", weq_w);

  c3_assert((pos_w + leq_w + weq_w) == neg_w);

  if ( 0 != leq_w || (0 != weq_w) ) { c3_assert(0); }
}

/* u3a_slab(): create a length-bounded proto-atom.
*/
c3_w*
u3a_slab(c3_w len_w)
{
  c3_w*       nov_w = u3a_walloc(len_w + c3_wiseof(u3a_atom));
  u3a_atom* pug_u = (void *)nov_w;

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

/* u3a_slaq(): u3a_slaq() with a defined blocksize.
*/
c3_w*
u3a_slaq(c3_g met_g, c3_w len_w)
{
  return u3a_slab(((len_w << met_g) + 31) >> 5);
}

/* u3a_malt(): measure and finish a proto-atom.
*/
u3_noun
u3a_malt(c3_w* sal_w)
{
  c3_w*       nov_w = (sal_w - c3_wiseof(u3a_atom));
  u3a_atom* nov_u = (void *)nov_w;
  c3_w        len_w;

  for ( len_w = nov_u->len_w; len_w; len_w-- ) {
    if ( 0 != nov_u->buf_w[len_w - 1] ) {
      break;
    }
  }
  return u3a_mint(sal_w, len_w);
}

/* u3a_moot(): finish a pre-measured proto-atom; dangerous.
*/
u3_noun
u3a_moot(c3_w* sal_w)
{
  c3_w*     nov_w = (sal_w - c3_wiseof(u3a_atom));
  u3a_atom* nov_u = (void*)nov_w;
  c3_w      len_w = nov_u->len_w;
  c3_w      las_w = nov_u->buf_w[len_w - 1];

  c3_assert(0 != len_w);
  c3_assert(0 != las_w);

  if ( 1 == len_w ) {
    if ( _(u3a_is_cat(las_w)) ) {
      u3a_wfree(nov_w);

      return las_w;
    }
  }
  return u3a_to_pug(u3a_outa(nov_w));
}

#if 0
/* _ca_detect(): in u3a_detect().
*/
static c3_d
_ca_detect(u3p(u3h_root) har_p, u3_noun fum, u3_noun som, c3_d axe_d)
{
  while ( 1 ) {
    if ( som == fum ) {
      return axe_d;
    }
    else if ( !_(u3du(fum)) || (u3_none != u3h_get(har_p, fum)) ) {
      return 0;
    }
    else {
      c3_d eax_d;

      u3h_put(har_p, fum, 0);

      if ( 0 != (eax_d = _ca_detect(har_p, u3h(fum), som, 2ULL * axe_d)) ) {
        return c3y;
      }
      else {
        fum = u3t(fum);
        axe_d = (2ULL * axe_d) + 1;
      }
    }
  }
}

/* u3a_detect(): for debugging, check if (som) is referenced from (fum). 
**
** (som) and (fum) are both RETAINED.
*/
c3_d
u3a_detect(u3_noun fum, u3_noun som)
{
  u3p(u3h_root) har_p = u3h_new();
  c3_o            ret_o;

  ret_o = _ca_detect(har_p, fum, som, 1);
  u3h_free(har_p);

  return ret_o;
}
#endif

/* u3a_mint(): finish a measured proto-atom.
*/
u3_noun
u3a_mint(c3_w* sal_w, c3_w len_w)
{
  c3_w*       nov_w = (sal_w - c3_wiseof(u3a_atom));
  u3a_atom* nov_u = (void*)nov_w;

  /* See if we can free the slab entirely.
  */
  if ( len_w == 0 ) {
    u3a_wfree(nov_w);

    return 0;
  }
  else if ( len_w == 1 ) {
    c3_w low_w = nov_u->buf_w[0];

    if ( _(u3a_is_cat(low_w)) ) {
      u3a_wfree(nov_w);

      return low_w;
    }
  }

  /* See if we can strip off a block on the end.
  */
  {
    c3_w old_w = nov_u->len_w;
    c3_w dif_w = (old_w - len_w);

    if ( dif_w >= u3a_minimum ) {
      c3_w* box_w = (void *)u3a_botox(nov_w);
      c3_w* end_w = (nov_w + c3_wiseof(u3a_atom) + len_w + 1);
      c3_w  asz_w = (end_w - box_w);
      c3_w  bsz_w = box_w[0] - asz_w;

      _box_attach(_box_make(end_w, bsz_w, 0));

      box_w[0] = asz_w;
      box_w[asz_w - 1] = asz_w;
    }
    nov_u->len_w = len_w;
  }
  return u3a_to_pug(u3a_outa(nov_w));
}

#ifdef U3_MEMORY_DEBUG
/* u3a_lush(): leak push.
*/
c3_w 
u3a_lush(c3_w lab_w)
{
  c3_w cod_w = u3_Code;

  u3_Code = lab_w;
  return cod_w;
}

/* u3a_lop(): leak pop.
*/
void
u3a_lop(c3_w lab_w)
{
  u3_Code = lab_w;
}
#else
/* u3a_lush(): leak push.
*/
c3_w 
u3a_lush(c3_w lab_w)
{
  return 0;
}

/* u3a_lop(): leak pop.
*/
void
u3a_lop(c3_w lab_w)
{
}
#endif
