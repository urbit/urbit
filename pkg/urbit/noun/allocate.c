/* g/a.c
**
*/
#include "all.h"

/* _box_count(): adjust memory count.
*/
#ifdef  U3_CPU_DEBUG
static void
_box_count(c3_ws siz_ws)
{
  u3R->all.fre_w += siz_ws;
  {
    c3_w end_w = _(u3a_is_north(u3R))
                  ? (u3R->hat_p - u3R->rut_p)
                  : (u3R->rut_p - u3R->hat_p);
    c3_w all_w = (end_w - u3R->all.fre_w);

    if ( all_w > u3R->all.max_w ) {
      u3R->all.max_w = all_w;
    }
  }
}
#else
static void
_box_count(c3_ws siz_ws) { }
#endif

/* _box_slot(): select the right free list to search for a block.
*/
static c3_w
_box_slot(c3_w siz_w)
{
  if ( siz_w < u3a_minimum ) {
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
static u3a_box*
_box_make(void* box_v, c3_w siz_w, c3_w use_w)
{
  u3a_box* box_u = box_v;
  c3_w*      box_w = box_v;

  c3_assert(siz_w >= u3a_minimum);

  box_w[0] = siz_w;
  box_w[siz_w - 1] = siz_w;
  box_u->use_w = use_w;

# ifdef  U3_MEMORY_DEBUG
    box_u->cod_w = u3_Code;
    box_u->eus_w = 0;
# endif

  return box_u;
}

/* _box_attach(): attach a box to the free list.
*/
static void
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

  _box_count(box_u->siz_w);
  {
    c3_w           sel_w = _box_slot(box_u->siz_w);
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
static void
_box_detach(u3a_box* box_u)
{
  u3p(u3a_fbox) fre_p = u3of(u3a_fbox, box_u);
  u3p(u3a_fbox) pre_p = u3to(u3a_fbox, fre_p)->pre_p;
  u3p(u3a_fbox) nex_p = u3to(u3a_fbox, fre_p)->nex_p;

  _box_count(-(box_u->siz_w));

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

/* _box_free(): free and coalesce.
*/
static void
_box_free(u3a_box* box_u)
{
  c3_w* box_w = (c3_w *)(void *)box_u;

  c3_assert(box_u->use_w != 0);
  box_u->use_w -= 1;
  if ( 0 != box_u->use_w ) {
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
  c3_w    pad_w, siz_w;
  u3_post all_p;

  if ( c3y == u3a_is_north(u3R) ) {
    all_p = u3R->hat_p;
    pad_w = _me_align_pad(all_p, ald_w, alp_w);
    siz_w = (len_w + pad_w);

    if ( (siz_w >= (u3R->cap_p - u3R->hat_p)) ) {
      return 0;
    }
    u3R->hat_p = (all_p + siz_w);
  }
  else {
    all_p = (u3R->hat_p - len_w);
    pad_w = _me_align_dap(all_p, ald_w, alp_w);
    siz_w = (len_w + pad_w);
    all_p -= pad_w;

    if ( siz_w >= (u3R->hat_p - u3R->cap_p) ) {
      return 0;
    }
    u3R->hat_p = all_p;
  }
  return _box_make(u3a_into(all_p), siz_w, use_w);
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

/* u3a_reflux(): dump 1K cells from the cell list into regular memory.
*/
void
u3a_reflux(void)
{
  c3_w i_w;

  for ( i_w = 0; u3R->all.cel_p && (i_w < 1024); i_w++ ) {
    u3_post  cel_p = u3R->all.cel_p;
    u3a_box* box_u = &(u3to(u3a_fbox, cel_p)->box_u);

    u3R->all.cel_p = u3to(u3a_fbox, cel_p)->nex_p;

    // otherwise _box_free() will double-count it
    //
    _box_count(-(u3a_minimum));
    _box_free(box_u);

  }
}

/* u3a_reclaim(): reclaim from memoization cache.
*/
void
u3a_reclaim(void)
{
  //  XX u3l_log avoid here, as it can
  //  cause problems when handling errors

  if ( (0 == u3R->cax.har_p) ||
       (0 == u3to(u3h_root, u3R->cax.har_p)->use_w) )
  {
    fprintf(stderr, "allocate: reclaim: memo cache: empty\r\n");
    u3m_bail(c3__meme);
  }

#if 1
  fprintf(stderr, "allocate: reclaim: half of %d entries\r\n",
          u3to(u3h_root, u3R->cax.har_p)->use_w);

  u3h_trim_to(u3R->cax.har_p, u3to(u3h_root, u3R->cax.har_p)->use_w / 2);
#else
  /*  brutal and guaranteed effective
  */
  u3h_free(u3R->cax.har_p);
  u3R->cax.har_p = u3h_new();
#endif
}

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

  // u3l_log("walloc %d: *pfr_p %x\n", len_w, u3R->all.fre_p[sel_w]);
  while ( 1 ) {
    u3p(u3a_fbox) *pfr_p = &u3R->all.fre_p[sel_w];

    while ( 1 ) {
      if ( 0 == *pfr_p ) {
        if ( sel_w < (u3a_fbox_no - 1) ) {
          sel_w += 1;
          break;
        }
        else {
          //  nothing in top free list; chip away at the hat
          //
          u3a_box* box_u;

          //  memory nearly empty; reclaim; should not be needed
          //
          // if ( (u3a_open(u3R) + u3R->all.fre_w) < 65536 ) { u3a_reclaim(); }
          box_u = _ca_box_make_hat(siz_w, ald_w, alp_w, 1);

          /* Flush a bunch of cell cache, then try again.
          */
          if ( 0 == box_u ) {
            if ( u3R->all.cel_p ) {
              u3a_reflux();

              return _ca_willoc(len_w, ald_w, alp_w);
            }
            else {
              u3a_reclaim();
              return _ca_willoc(len_w, ald_w, alp_w);
            }
          }
          else return u3a_boxto(box_u);
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
          _box_count(-(box_u->siz_w));
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

  while ( 1 ) {
    ptr_v = _ca_willoc(len_w, ald_w, alp_w);
    if ( 0 != ptr_v ) {
      break;
    }
    u3a_reclaim();
  }
  return ptr_v;
}

/* u3a_walloc(): allocate storage words on hat heap.
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

    u3l_log("xuc_i %d\r\n", xuc_i);
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
    c3_w*    old_w = lag_v;
    c3_w     tiz_w = c3_min(box_u->siz_w, len_w);
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
/* u3a_push(): allocate space on the road stack
*/
void*
u3a_push(c3_w len_w)
{
  void *cur, *top = u3to(void, u3R->cap_p);
  if ( c3y == u3a_is_north(u3R) ) {
    top -= len_w;
    cur = top;
    u3p(void) cap_p = u3R->cap_p = u3of(void, top);

    if( !( cap_p < u3R->mat_p &&
           cap_p > u3R->hat_p ) )
    {
      u3m_bail(c3__meme);
    }

    return cur;
  }
  else {
    cur = top;
    top += len_w;
    u3R->cap_p = u3of(void, top);
    u3p(void) cap_p = u3R->cap_p = u3of(void, top);

    if( !( cap_p > u3R->mat_p &&
           cap_p < u3R->hat_p ) )
    {
      u3m_bail(c3__meme);
    }

    return cur;
  }
}

/* u3a_pop(): deallocate space on the road stack
*/
void
u3a_pop(c3_w len_w)
{
  void* top = u3to(void, u3R->cap_p);
  if ( c3y == u3a_is_north(u3R) ) {
    top += len_w;
    u3p(void) cap_p = u3R->cap_p = u3of(void, top);
    c3_assert(cap_p <= u3R->mat_p);
    c3_assert(cap_p > u3R->hat_p);
  }
  else {
    top -= len_w;
    u3p(void) cap_p = u3R->cap_p = u3of(void, top);
    c3_assert(cap_p >= u3R->mat_p);
    c3_assert(cap_p < u3R->hat_p);
  }
}

/* u3a_peek(): examine the top of the road stack
*/
void*
u3a_peek(c3_w len_w)
{
  return u3to(void, u3R->cap_p) - (c3y == u3a_is_north(u3R) ? 0 : len_w);
}

/* u3a_wfree(): free storage.
*/
void
u3a_wfree(void* tox_v)
{
  _box_free(u3a_botox(tox_v));
}

/* u3a_calloc(): allocate and zero-initialize array
*/
void*
u3a_calloc(size_t num_i, size_t len_i)
{
  size_t byt_i = num_i * len_i;
  c3_w* out_w;

  c3_assert(byt_i / len_i == num_i);
  out_w = u3a_malloc(byt_i);
  memset(out_w, 0, byt_i);

  return out_w;
}

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
  if ( u3a_botox(out_w) == (u3a_box*)(void *)0x3bdd1c80) {
    static int xuc_i = 0;

    u3l_log("xuc_i %d\r\n", xuc_i);
    // if ( 1 == xuc_i ) { abort(); }
    xuc_i++;
  }
#endif
  out_w[-1] = pad_w;

  return out_w;
}

/* u3a_cellblock(): allocate a block of cells on the hat.
*/
static c3_o
u3a_cellblock(c3_w num_w)
{
  u3p(u3a_fbox) fre_p;
  c3_w          i_w;

  if ( c3y == u3a_is_north(u3R) ) {
    if ( u3R->cap_p <= (u3R->hat_p + (num_w * u3a_minimum)) ) {
      return c3n;
    }
    else {
      u3_post hat_p = u3R->hat_p;
      u3_post cel_p = u3R->all.cel_p;

      for ( i_w = 0; i_w < num_w; i_w++) {
        u3_post  all_p = hat_p;
        void*    box_v = u3a_into(all_p);
        u3a_box* box_u = box_v;
        c3_w*    box_w = box_v;

        //  hand inline of _box_make(u3a_into(all_p), u3a_minimum, 1)
        {
          box_w[0] = u3a_minimum;
          box_w[u3a_minimum - 1] = u3a_minimum;
          box_u->use_w = 1;
#ifdef U3_MEMORY_DEBUG
            box_u->cod_w = 0;
            box_u->eus_w = 0;
#endif
        }
        hat_p += u3a_minimum;

        fre_p = u3of(u3a_fbox, box_u);
        u3to(u3a_fbox, fre_p)->nex_p = cel_p;
        cel_p = fre_p;
      }
      u3R->hat_p = hat_p;
      u3R->all.cel_p = cel_p;
    }
  }
  else {
    if ( (u3R->cap_p + (num_w * u3a_minimum)) >= u3R->hat_p ) {
      return c3n;
    }
    else {
      u3_post hat_p = u3R->hat_p;
      u3_post cel_p = u3R->all.cel_p;

      for ( i_w = 0; i_w < num_w; i_w++ ) {
        u3_post  all_p = (hat_p -= u3a_minimum);
        void*    box_v = u3a_into(all_p);
        u3a_box* box_u = box_v;
        c3_w*    box_w = box_v;

        //  hand inline of _box_make(u3a_into(all_p), u3a_minimum, 1);
        {
          box_w[0] = u3a_minimum;
          box_w[u3a_minimum - 1] = u3a_minimum;
          box_u->use_w = 1;
# ifdef U3_MEMORY_DEBUG
            box_u->cod_w = 0;
            box_u->eus_w = 0;
# endif
        }
        fre_p = u3of(u3a_fbox, box_u);
        u3to(u3a_fbox, fre_p)->nex_p = cel_p;
        cel_p = fre_p;
      }
      u3R->hat_p = hat_p;
      u3R->all.cel_p = cel_p;
    }
  }
  _box_count(num_w * u3a_minimum);
  return c3y;
}

/* u3a_celloc(): allocate a cell.
*/
c3_w*
u3a_celloc(void)
{
#ifdef U3_MEMORY_DEBUG
  if ( u3C.wag_w & u3o_debug_ram ) {
    return u3a_walloc(c3_wiseof(u3a_cell));
  }
#endif

  u3p(u3a_fbox) cel_p;

  if ( !(cel_p = u3R->all.cel_p) ) {
    if ( u3R == &(u3H->rod_u) ) {
      // no cell allocator on home road
      //
      return u3a_walloc(c3_wiseof(u3a_cell));
    }
    else {
      if ( c3n == u3a_cellblock(4096) ) {
        return u3a_walloc(c3_wiseof(u3a_cell));
      }
      cel_p = u3R->all.cel_p;
    }
  }

  {
    u3a_box* box_u = &(u3to(u3a_fbox, cel_p)->box_u);


    box_u->use_w = 1;
    u3R->all.cel_p = u3to(u3a_fbox, cel_p)->nex_p;

    _box_count(-(u3a_minimum));

    return u3a_boxto(box_u);
  }
}

/* u3a_cfree(): free a cell.
*/
void
u3a_cfree(c3_w* cel_w)
{
#ifdef U3_MEMORY_DEBUG
  if ( u3C.wag_w & u3o_debug_ram ) {
    return u3a_wfree(cel_w);
  }
#endif

  if ( u3R == &(u3H->rod_u) ) {
    return u3a_wfree(cel_w);
  }
  else {
    u3a_box*      box_u = u3a_botox(cel_w);
    u3p(u3a_fbox) fre_p = u3of(u3a_fbox, box_u);

    _box_count(u3a_minimum);

    u3to(u3a_fbox, fre_p)->nex_p = u3R->all.cel_p;
    u3R->all.cel_p = fre_p;
  }
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
  if (NULL == tox_v)
    return;

  c3_w* tox_w = tox_v;
  c3_w  pad_w = tox_w[-1];
  c3_w* org_w = tox_w - (pad_w + 1);

  // u3l_log("free %p %p\r\n", org_w, tox_w);
  u3a_wfree(org_w);
}

/* u3a_free2(): gmp-shaped free.
*/
void
u3a_free2(void* tox_v, size_t siz_i)
{
  return u3a_free(tox_v);
}

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

    if ( dog_u->mug_w == 0 ) return;

    dog_u->mug_w = 0;    //  power wash
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

    if ( dog_u->mug_w == 0 ) return;

    dog_u->mug_w = 0;    //  power wash
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

      u3l_log("bad %x %d %d\r\n", dog, bug_w, box_u->use_w);
      if ( bug_w == 0 ) { abort(); }
      bug_w++;
    }
#endif
#if 0
    {
      static c3_w bug_w = 0;

      if ( BDA == dog ) {
        u3l_log("BDA %d %d\r\n", bug_w, box_u->use_w);
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

        u3l_log("GAIN %d %d\r\n", bug_w, box_u->use_w);
        if ( bug_w == 8 ) { abort(); }
        bug_w++;
      }
    }
#endif
#endif

  }
}

#define TAKE_ROOT 0
#define TAKE_HEAD 1
#define TAKE_TAIL 2

#undef VERBOSE_TAKE

//  stack frame for recording head vs tail iteration
//
//    In Hoon, this structure would be as follows:
//
//    $%  [%root ~]
//        [%head old=* new=*]
//        [%tail old=* new=*]
//    ==
//
typedef struct takeframe
{
  c3_y      tag_y;
  u3a_cell* old_u;
  u3a_cell* new_u;
} takeframe;

static inline void
_ca_take_push(c3_ys mov,
              c3_ys off,
              c3_y tag_y,
              u3a_cell* old_u,
              u3a_cell* new_u)
{
  u3R->cap_p += mov;

  //  ensure we haven't overflowed the stack
  //  (off==0 means we're on a north road)
  //
  if ( 0 == off ) {
    if( !(u3R->cap_p > u3R->hat_p) ) {
      u3m_bail(c3__meme);
    }
  }
  else {
    if( !(u3R->cap_p < u3R->hat_p) ) {
      u3m_bail(c3__meme);
    }
  }

  takeframe* fam_u = u3to(takeframe, u3R->cap_p + off);
  fam_u->tag_y = tag_y;
  fam_u->old_u = old_u;
  fam_u->new_u = new_u;
}

static inline takeframe
_ca_take_pop(c3_ys mov, c3_ys off)
{
  takeframe* fam_u = u3to(takeframe, u3R->cap_p + off);
  u3R->cap_p -= mov;

  return *fam_u;
}

/* u3a_take(): gain, copying juniors.
*/
u3_noun
u3a_take(u3_noun veb)
{
  c3_assert(u3_none != veb);

  u3t_on(coy_o);

  //  initialize signed stack offsets (relative to north/south road)
  //
  c3_o  nor_o = u3a_is_north(u3R);
  c3_ys mov, off;
  {
    c3_y wis_y = c3_wiseof(takeframe);
    mov = ( c3y == nor_o ? -wis_y : wis_y );
    off = ( c3y == nor_o ? 0 : -wis_y );
  }

  //  stash the current stack post
  //
  u3p(takeframe) cap_p = u3R->cap_p;

  //  push the (only) ROOT stack frame (our termination condition)
  //
  _ca_take_push(mov, off, TAKE_ROOT, 0, 0);

  //  the finished copy  of our current noun .veb
  //
  u3_noun pro;

  //  read from the current noun .veb
  //
  advance: {
    if ( c3y == u3a_is_cat(veb) ) {
      pro = veb;
      goto retreat;
    }
    //  senior pointers are not refcounted
    //
    else if ( c3y == (( c3y == nor_o )
                      ? u3a_north_is_senior(u3R, veb)
                      : u3a_south_is_senior(u3R, veb)) )
    {
      pro = veb;
      goto retreat;
    }
    //  not junior; a normal pointer in our road -- refcounted
    //
    else if ( c3n == (( c3y == nor_o )
                      ? u3a_north_is_junior(u3R, veb)
                      : u3a_south_is_junior(u3R, veb)) )
    {
      //  bypass normal road checks in u3k
      //
      _me_gain_use(veb);
      pro = veb;
      goto retreat;


    }
    //  junior pointers are copied
    //
    else {
      u3a_noun* veb_u = u3a_to_ptr(veb);

      //  32-bit mug_w: already copied .veb and .mug_w is the pointer
      //
      if ( veb_u->mug_w >> 31 ) {
        u3_noun nov = (u3_noun)veb_u->mug_w;

        c3_assert( c3y == (( c3y == nor_o)
                           ? u3a_north_is_normal(u3R, nov)
                           : u3a_south_is_normal(u3R, nov)) );

#ifdef VERBOSE_TAKE
          u3l_log("%s: %p is already %p\r\n", ( c3y == nor_o )
                                           ? "north"
                                           : "south",
                                           veb_u,
                                           u3a_to_ptr(nov));
#endif

        //  bypass normal road checks in u3k
        //
        _me_gain_use(nov);
        pro = nov;
        goto retreat;
      }
      else {
        if ( c3y == u3a_is_atom(veb) ) {
          u3a_atom* old_u = u3a_to_ptr(veb);
          c3_w*     new_w = u3a_walloc(old_u->len_w + c3_wiseof(u3a_atom));
          u3a_atom* new_u = (u3a_atom*)(void *)new_w;
          u3_noun     new = u3a_to_pug(u3a_outa(new_u));

#ifdef VERBOSE_TAKE
          u3l_log("%s: atom %p to %p\r\n", ( c3y == nor_o )
                                           ? "north"
                                           : "south",
                                           old_u,
                                           new_u);
#endif

          new_u->mug_w = old_u->mug_w;
          new_u->len_w = old_u->len_w;
          {
            c3_w i_w;

            for ( i_w=0; i_w < old_u->len_w; i_w++ ) {
              new_u->buf_w[i_w] = old_u->buf_w[i_w];
            }
          }

          //  Borrow mug slot to record new destination in .old_u.
          //
          old_u->mug_w = new;

          pro = new;
          goto retreat;
        }
        else {
          u3a_cell* old_u = u3a_to_ptr(veb);
          //  XX use u3a_celloc?
          //
          c3_w*     new_w = u3a_walloc(c3_wiseof(u3a_cell));
          u3a_cell* new_u = (u3a_cell*)(void *)new_w;

#ifdef VERBOSE_TAKE
          u3l_log("%s: cell %p to %p\r\n", ( c3y == nor_o )
                                           ? "north"
                                           : "south",
                                           old_u,
                                           new_u);
#endif

          new_u->mug_w = old_u->mug_w;

          veb = old_u->hed;
          _ca_take_push(mov, off, TAKE_HEAD, old_u, new_u);
          goto advance;
        }
      }
    }
  }

  //  consume: popped stack frame, and .pro from above
  //
  retreat: {
    takeframe fam_u = _ca_take_pop(mov, off);

    switch ( fam_u.tag_y ) {
      default: {
        c3_assert(0);
      }

      //  .fam_u is our stack root, we're done.
      //
      case TAKE_ROOT: {
        break;
      }

      //  .pro is the copied head of a cell; save a pointer to it in .new_u
      //  and advance to copy the tail
      //
      case TAKE_HEAD: {
        fam_u.new_u->hed = pro;

        veb = fam_u.old_u->tel;
        _ca_take_push(mov, off, TAKE_TAIL, fam_u.old_u, fam_u.new_u);
        goto advance;
      }

      //  .pro is the copied tail of a cell; save a pointer to it in .new_u,
      //  and produce the whole copied cell (as if it were a read from above).
      //
      case TAKE_TAIL: {
        fam_u.new_u->tel = pro;
        pro = u3a_to_pom(u3a_outa(fam_u.new_u));

        //  Borrow mug slot to record new destination in old_u.
        //
        fam_u.old_u->mug_w = pro;

        goto retreat;
      }
    }
  }

  //  sanity check
  //
  c3_assert( u3R->cap_p == cap_p );

  u3t_off(coy_o);

  return pro;
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
          u3a_cfree(dog_w);
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
          u3a_cfree(dog_w);
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
  u3t_on(mal_o);
  c3_assert(u3_none != som);

  if ( !_(u3a_is_cat(som)) ) {
    som = _(u3a_is_north(u3R))
              ? _me_gain_north(som)
              : _me_gain_south(som);
  }
  u3t_off(mal_o);

  return som;
}

/* u3a_lose(): lose a reference count.
*/
void
u3a_lose(u3_noun som)
{
  u3t_on(mal_o);
  if ( !_(u3a_is_cat(som)) ) {
    if ( _(u3a_is_north(u3R)) ) {
      _me_lose_north(som);
    } else {
      _me_lose_south(som);
    }
  }
  u3t_off(mal_o);
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
    u3l_log("luse: insane %d 0x%x\r\n", som, som);
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
    if ( 0 == box_u->eus_w ) {
      siz_w = box_u->siz_w;
    }
    else if ( 0xffffffff == box_u->eus_w ) {      // see _raft_prof()
      siz_w = 0xffffffff;
      box_u->eus_w = 0;
    }
    else {
      siz_w = 0;
    }
    box_u->eus_w += 1;
#else
    c3_ws use_ws = (c3_ws)box_u->use_w;

    if ( use_ws == 0 ) {
      u3l_log("%p is bogus\r\n", ptr_v);
      siz_w = 0;
    }
    else {
      c3_assert(use_ws != 0);

      if ( 0x80000000 == (c3_w)use_ws ) {    // see _raft_prof()
        use_ws = -1;
        siz_w = 0xffffffff;
      }
      else if ( use_ws < 0 ) {
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

  // u3l_log("free %p %p\r\n", org_w, ptr_w);
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

      if ( 0 == new_w || 0xffffffff == new_w ) {      //  see u3a_mark_ptr()
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
u3a_print_memory(FILE* fil_u, c3_c* cap_c, c3_w wor_w)
{
  c3_assert( 0 != fil_u );

  c3_w byt_w = (wor_w * 4);
  c3_w gib_w = (byt_w / 1000000000);
  c3_w mib_w = (byt_w % 1000000000) / 1000000;
  c3_w kib_w = (byt_w % 1000000) / 1000;
  c3_w bib_w = (byt_w % 1000);

  if ( byt_w ) {
    if ( gib_w ) {
      fprintf(fil_u, "%s: GB/%d.%03d.%03d.%03d\r\n",
          cap_c, gib_w, mib_w, kib_w, bib_w);
    }
    else if ( mib_w ) {
      fprintf(fil_u, "%s: MB/%d.%03d.%03d\r\n", cap_c, mib_w, kib_w, bib_w);
    }
    else if ( kib_w ) {
      fprintf(fil_u, "%s: KB/%d.%03d\r\n", cap_c, kib_w, bib_w);
    }
    else if ( bib_w ) {
      fprintf(fil_u, "%s: B/%d\r\n", cap_c, bib_w);
    }
  }
}

/* u3a_maid(): maybe print memory.
*/
c3_w
u3a_maid(FILE* fil_u, c3_c* cap_c, c3_w wor_w)
{
  if ( 0 != fil_u ) {
    u3a_print_memory(fil_u, cap_c, wor_w);
  }
  return wor_w;
}

/* u3a_mark_road(): mark ad-hoc persistent road structures.
*/
c3_w
u3a_mark_road(FILE* fil_u)
{
  c3_w tot_w = 0;
  tot_w += u3a_maid(fil_u, "  namespace", u3a_mark_noun(u3R->ski.gul));
  tot_w += u3a_maid(fil_u, "  trace stack", u3a_mark_noun(u3R->bug.tax));
  tot_w += u3a_maid(fil_u, "  trace buffer", u3a_mark_noun(u3R->bug.mer));
  tot_w += u3a_maid(fil_u, "  profile batteries", u3a_mark_noun(u3R->pro.don));
  tot_w += u3a_maid(fil_u, "  profile doss", u3a_mark_noun(u3R->pro.day));
  tot_w += u3a_maid(fil_u, "  new profile trace", u3a_mark_noun(u3R->pro.trace));
  tot_w += u3a_maid(fil_u, "  memoization cache", u3h_mark(u3R->cax.har_p));
  return   u3a_maid(fil_u, "total road stuff", tot_w);
}

/* _ca_print_box(): heuristically print the contents of an allocation box.
*/
static c3_c*
_ca_print_box(u3a_box* box_u)
{
  //  the loom offset pointing to the contents of box_u
  //
  c3_w box_w = u3a_outa(u3a_boxto(box_u));
  //  box_u might not be a cell, we use the struct to inspect further
  //
  u3a_cell* cel_u = (u3a_cell*)box_u;

  if (  //  a cell will never be bigger than the minimum allocation size
        //
        (u3a_minimum < box_u->siz_w) ||
        //  this condition being true potentially corresponds to
        //  box_u containing an indirect atom of only one word.
        //  if the condition is false, we know box_u contains a cell.
        //
        ( (1 == (c3_w)cel_u->hed) &&
          (0x80000000 & (c3_w)cel_u->tel) ) )
  {
    //  box_u might not be an indirect atom,
    //  but it's always safe to print it as if it is one
    //
    u3a_atom* vat_u = (u3a_atom*)box_u;
    u3_atom   veb   = u3a_to_pug(box_w);

    //  skip atoms larger than 10 words
    //  XX print mugs or something
    //
    if ( 10 > vat_u->len_w ) {
#if 0
      /*  For those times when you've really just got to crack open
       *  the box and see what's inside
      */
      {
        int i;
        for ( i = 0; i < box_u->siz_w; i++ ) {
          fprintf(stderr, "%08x ", (unsigned int)(((c3_w*)box_u)[i]));
        }
        fprintf(stderr, "\r\n");
      }
#endif
      return 0;
    }

    return u3m_pretty(veb);
  }
  else {
    //  box_u is definitely a cell
    //
    return u3m_pretty(u3a_to_pom(box_w));
  }
}

/* _ca_print_leak(): print the details of a leaked allocation box.
*/
#ifdef U3_MEMORY_DEBUG

static void
_ca_print_leak(c3_c* cap_c, u3a_box* box_u, c3_w eus_w, c3_w use_w)
{
  fprintf(stderr, "%s: %p mug=%x (marked=%u swept=%u)\r\n",
                  cap_c,
                  box_u,
                  ((u3a_noun *)(u3a_boxto(box_u)))->mug_w,
                  eus_w,
                  use_w);

  if ( box_u->cod_w ) {
    c3_c* cod_c = u3m_pretty(box_u->cod_w);
    fprintf(stderr, "code: %s\r\n", cod_c);
    free(cod_c);
  }

  u3a_print_memory(stderr, "    size", box_u->siz_w);

  {
    c3_c* dat_c = _ca_print_box(box_u);
    fprintf(stderr, "    data: %s\r\n", dat_c);
    free(dat_c);
  }
}

#else

static void
_ca_print_leak(c3_c* cap_c, u3a_box* box_u, c3_ws use_ws)
{
  fprintf(stderr, "%s: %p mug=%x swept=%d\r\n",
                  cap_c,
                  box_u,
                  ((u3a_noun *)(u3a_boxto(box_u)))->mug_w,
                  use_ws);

  u3a_print_memory(stderr, "    size", box_u->siz_w);

  {
    c3_c* dat_c = _ca_print_box(box_u);
    fprintf(stderr, "    data: %s\r\n", dat_c);
    free(dat_c);
  }
}

#endif

/* u3a_sweep(): sweep a fully marked road.
*/
c3_w
u3a_sweep(void)
{
  c3_w neg_w, pos_w, leq_w, weq_w;
#ifdef U3_MEMORY_DEBUG
  c3_w tot_w, caf_w;
#endif

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
#ifdef U3_CPU_DEBUG
    if ( fre_w != u3R->all.fre_w ) {
      u3l_log("fre discrepancy (%x): %x, %x, %x\r\n", u3R->par_p,
              fre_w, u3R->all.fre_w, (u3R->all.fre_w - fre_w));
    }
#endif
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
      /* I suspect these printfs fail hilariously in the case
       * of non-direct atoms. We shouldn't unconditionally run
       * u3a_to_pom(). In general, the condition
       * box_u->siz_w > u3a_minimum is sufficient, but not necessary,
       * for the box to represent an atom.  The atoms between
       * 2^31 and 2^32 are the exceptions.
       *
       * Update: so, apparently u3.md is incorrect, and a pug is just
       * an indirect atom.  This code should be altered to handle
       * that.
      */
      if ( box_u->use_w != box_u->eus_w ) {
        if ( box_u->eus_w != 0 ) {
          if ( box_u->use_w == 0 ) {
            _ca_print_leak("dank", box_u, box_u->eus_w, box_u->use_w);
          }
          else {
            _ca_print_leak("weak", box_u, box_u->eus_w, box_u->use_w);
          }

          weq_w += box_u->siz_w;
        }
        else {
          _ca_print_leak("leak", box_u, box_u->eus_w, box_u->use_w);

          leq_w += box_u->siz_w;
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
        _ca_print_leak("leak", box_u, use_ws);

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

#ifdef U3_MEMORY_DEBUG
  tot_w = _(u3a_is_north(u3R))
                ? u3R->mat_p - u3R->rut_p
                : u3R->rut_p - u3R->mat_p;
  caf_w = _(u3a_is_north(u3R))
                ? u3R->mat_p - u3R->cap_p
                : u3R->cap_p - u3R->mat_p;

#ifdef U3_CPU_DEBUG
  if ( (0 != u3R->par_p) && (u3R->all.max_w > 1000000) ) {
    u3a_print_memory(stderr, "available", (tot_w - pos_w));
    u3a_print_memory(stderr, "allocated", pos_w);
    u3a_print_memory(stderr, "volatile", caf_w);

    u3a_print_memory(stderr, "maximum", u3R->all.max_w);
  }
#else
#if 0
  u3a_print_memory(stderr, "available", (tot_w - pos_w));
  u3a_print_memory(stderr, "allocated", pos_w);
  u3a_print_memory(stderr, "volatile", caf_w);
#endif
#endif
#endif
  u3a_print_memory(stderr, "leaked", leq_w);
  u3a_print_memory(stderr, "weaked", weq_w);

  c3_assert((pos_w + leq_w + weq_w) == neg_w);

  if ( 0 != leq_w || (0 != weq_w) ) { c3_assert(0); }

  return neg_w;
}

/* u3a_slab(): create a length-bounded proto-atom.
*/
c3_w*
u3a_slab(c3_w len_w)
{
  c3_w*     nov_w = u3a_walloc(len_w + c3_wiseof(u3a_atom));
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
