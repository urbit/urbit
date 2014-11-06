/* g/h.c
**
** This file is in the public domain.
*/
#include "all.h"

static void* _ch_some_add(void* han_v, c3_w, c3_w, u3_noun);
static void* _ch_some_new(c3_w lef_w);

/* u3h_new(): create hashtable.
*/
u3p(u3h_root)
u3h_new(void)
{
  u3h_root*     har_u = u3a_walloc(c3_wiseof(u3h_root));
  u3p(u3h_root) har_p = u3of(u3h_root, har_u);
  c3_w        i_w;

  har_u->clk_w = 0;
  for ( i_w = 0; i_w < 64; i_w++ ) {
    har_u->sot_w[i_w] = 0;
  }
  return har_p;
}

/* _ch_popcount(): number of bits set in word.  A standard intrinsic.
*/
static c3_w
_ch_popcount(c3_w num_w)
{
  return __builtin_popcount(num_w);
}

/* _ch_buck_new(): create new, empty bucket.
*/
static u3h_buck*
_ch_buck_new(void)
{
  u3h_buck* hab_u = u3a_walloc(c3_wiseof(u3h_buck));

  hab_u->len_w = 0;
  return hab_u;
}

/* ha_buck_add(): add to bucket.
*/
static u3h_buck*
_ch_buck_add(u3h_buck* hab_u, u3_noun kev)
{
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    if ( c3y == u3r_sing(u3h(kev), u3h(hab_u->kev[i_w])) ) {
      u3a_lose(hab_u->kev[i_w]);
      hab_u->kev[i_w] = kev;

      return hab_u;
    }
  }

  {
    c3_w len_w        = hab_u->len_w;
    u3h_buck* bah_u = u3a_walloc(c3_wiseof(u3h_buck) +
                                     (len_w + 1) * c3_wiseof(u3_noun));

    bah_u->len_w = len_w + 1;
    bah_u->kev[0] = kev;

    // Optimize: use u3a_wealloc().
    //
    for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) { 
      bah_u->kev[i_w + 1] = hab_u->kev[i_w];
    }

    u3a_free(hab_u);
    return bah_u;
  }
}

/* _ch_node_new(): create new, empty node.
*/
static u3h_node*
_ch_node_new(void)
{
  u3h_node* han_u = u3a_walloc(c3_wiseof(u3h_node));

  han_u->map_w = 0;
  return han_u;
}

/* _ch_node_add(): add to node.
*/
static u3h_node*
_ch_node_add(u3h_node* han_u, c3_w lef_w, c3_w rem_w, u3_noun kev)
{
  c3_w bit_w, inx_w, map_w, i_w;

  lef_w -= 5;
  bit_w = (rem_w >> lef_w);
  rem_w = (rem_w & ((1 << lef_w) - 1));
  map_w = han_u->map_w;
  inx_w = _ch_popcount(map_w & ((1 << bit_w) - 1));

  if ( map_w & (1 << bit_w) ) {
    c3_w sot_w = han_u->sot_w[inx_w];

    if ( _(u3h_slot_is_node(sot_w)) ) {
      void* hav_v = u3h_slot_to_node(sot_w);

      hav_v = _ch_some_add(hav_v, lef_w, rem_w, kev);
      han_u->sot_w[inx_w] = u3h_node_to_slot(hav_v);

      return han_u;
    }
    else {
      u3_noun kov = u3h_slot_to_noun(sot_w);

      if ( c3y == u3r_sing(u3h(kev), u3h(kov)) ) {
        u3a_lose(kov);
        han_u->sot_w[inx_w] = u3h_noun_to_slot(kev);
        return han_u;
      }
      else {
        c3_w  rom_w = u3r_mug(u3h(kov)) & ((1 << lef_w) - 1);
        void* hav_v = _ch_some_new(lef_w);

        //  Optimize: need a custom collision create.
        //
        hav_v = _ch_some_add(hav_v, lef_w, rem_w, kev);
        hav_v = _ch_some_add(hav_v, lef_w, rom_w, kov);

        han_u->sot_w[inx_w] = u3h_node_to_slot(hav_v);
        return han_u;
      }
    }
  }
  else {
    //  Optimize: use u3a_wealloc.
    //
    c3_w        len_w = _ch_popcount(map_w);
    u3h_node* nah_u = u3a_walloc(c3_wiseof(u3h_node) + 
                                     ((len_w + 1) * c3_wiseof(u3h_slot)));
    nah_u->map_w = han_u->map_w | (1 << bit_w);
    
    for ( i_w = 0; i_w < inx_w; i_w++ ) {
      nah_u->sot_w[i_w] = han_u->sot_w[i_w]; 
    }
    nah_u->sot_w[inx_w] = u3h_noun_to_slot(kev); 
    
    for ( i_w = inx_w; i_w < len_w; i_w++ ) {
      nah_u->sot_w[i_w + 1] = han_u->sot_w[i_w];
    }
    u3a_free(han_u);
    return nah_u;
  }
}

/* _ch_some_new(): create node or bucket.
*/
static void*
_ch_some_new(c3_w lef_w)
{
  if ( 0 == lef_w ) {
    return _ch_buck_new();
  }
  else {
    return _ch_node_new();
  }
}

/* _ch_some_add(): add to node or bucket.
*/
static void*
_ch_some_add(void* han_v, c3_w lef_w, c3_w rem_w, u3_noun kev)
{
  if ( 0 == lef_w ) {
    return _ch_buck_add(han_v, kev);
  }
  else return _ch_node_add(han_v, lef_w, rem_w, kev);
}

/* u3h_put(): insert in hashtable.
**
** `key` is RETAINED; `val` is transferred.
*/
void
u3h_put(u3p(u3h_root) har_p, u3_noun key, u3_noun val)
{
  u3h_root* har_u = u3to(u3h_root, har_p);
  u3_noun     kev   = u3nc(u3k(key), val);
  c3_w        mug_w = u3r_mug(key);
  c3_w        inx_w = (mug_w >> 25);
  c3_w        rem_w = (mug_w & ((1 << 25) - 1));
  c3_w        sot_w = har_u->sot_w[inx_w];

  if ( _(u3h_slot_is_null(sot_w)) ) {
    har_u->sot_w[inx_w] = u3h_noun_to_slot(kev);
  }
  else {
    u3h_node* han_u;

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kov   = u3h_slot_to_noun(sot_w);
      c3_w    rom_w = u3r_mug(u3h(kov)) & ((1 << 25) - 1);
 
      han_u = _ch_node_new();
      han_u = _ch_node_add(han_u, 25, rem_w, kev);
      han_u = _ch_node_add(han_u, 25, rom_w, kov);
    }
    else {
      han_u = _ch_node_add(u3h_slot_to_node(sot_w), 25, rem_w, kev);
    }
    har_u->sot_w[inx_w] = u3h_node_to_slot(han_u);
  }
}

/* _ch_buck_hum(): read in bucket.
*/
static c3_o
_ch_buck_hum(u3h_buck* hab_u, c3_w mug_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    if ( mug_w == u3r_mug(u3h(hab_u->kev[i_w])) ) {
      return c3y;
    }
  }
  return c3n;
}

/* _ch_node_hum(): read in node.
*/
static c3_o
_ch_node_hum(u3h_node* han_u, c3_w lef_w, c3_w rem_w, c3_w mug_w)
{
  c3_w bit_w, map_w;

  lef_w -= 5;
  bit_w = (rem_w >> lef_w);
  rem_w = (rem_w & ((1 << lef_w) - 1));
  map_w = han_u->map_w;

  if ( !(map_w & (1 << bit_w)) ) {
    return c3n;
  }
  else {
    c3_w inx_w = _ch_popcount(map_w & ((1 << bit_w) - 1));
    c3_w sot_w = han_u->sot_w[inx_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      if ( mug_w == u3r_mug(u3h(kev)) ) {
        return c3y;
      } 
      else {
        return c3n;
      }
    }
    else {
      void* hav_v = u3h_slot_to_node(sot_w);

      if ( 0 == lef_w ) {
        return _ch_buck_hum(hav_v, mug_w);
      }
      else return _ch_node_hum(hav_v, lef_w, rem_w, mug_w);
    }
  }
}

/* u3h_hum(): read from hashtable.
**
** `key` is RETAINED.
*/
c3_o
u3h_hum(u3p(u3h_root) har_p, c3_w mug_w)
{
  u3h_root* har_u = u3to(u3h_root, har_p);
  c3_w        inx_w = (mug_w >> 25);
  c3_w        rem_w = (mug_w & ((1 << 25) - 1));
  c3_w        sot_w = har_u->sot_w[inx_w];

  if ( _(u3h_slot_is_null(sot_w)) ) {
    return c3n;
  }
  else if ( _(u3h_slot_is_noun(sot_w)) ) {
    u3_noun kev = u3h_slot_to_noun(sot_w);

    if ( mug_w == u3r_mug(u3h(kev)) ) {
      return c3y;
    } 
    else {
      return c3n;
    }
  }
  else {
    u3h_node* han_u = u3h_slot_to_node(sot_w);

    return _ch_node_hum(han_u, 25, rem_w, mug_w);
  }
}

/* _ch_buck_get(): read in bucket.
*/
static u3_weak
_ch_buck_get(u3h_buck* hab_u, u3_noun key)
{
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    if ( _(u3r_sing(key, u3h(hab_u->kev[i_w]))) ) {
      return u3a_gain(u3t(hab_u->kev[i_w]));
    }
  }
  return u3_none;
}

/* _ch_node_get(): read in node.
*/
static u3_weak
_ch_node_get(u3h_node* han_u, c3_w lef_w, c3_w rem_w, u3_noun key)
{
  c3_w bit_w, map_w;

  lef_w -= 5;
  bit_w = (rem_w >> lef_w);
  rem_w = (rem_w & ((1 << lef_w) - 1));
  map_w = han_u->map_w;

  if ( !(map_w & (1 << bit_w)) ) {
    return u3_none;
  }
  else {
    c3_w inx_w = _ch_popcount(map_w & ((1 << bit_w) - 1));
    c3_w sot_w = han_u->sot_w[inx_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      if ( _(u3r_sing(key, u3h(kev))) ) {
        return u3a_gain(u3t(kev));
      } 
      else {
        return u3_none;
      }
    }
    else {
      void* hav_v = u3h_slot_to_node(sot_w);

      if ( 0 == lef_w ) {
        return _ch_buck_get(hav_v, key);
      }
      else return _ch_node_get(hav_v, lef_w, rem_w, key);
    }
  }
}

/* u3h_get(): read from hashtable.
**
** `key` is RETAINED.
*/
u3_weak
u3h_get(u3p(u3h_root) har_p, u3_noun key)
{
  u3h_root* har_u = u3to(u3h_root, har_p);
  c3_w        mug_w = u3r_mug(key);
  c3_w        inx_w = (mug_w >> 25);
  c3_w        rem_w = (mug_w & ((1 << 25) - 1));
  c3_w        sot_w = har_u->sot_w[inx_w];

  if ( _(u3h_slot_is_null(sot_w)) ) {
    return u3_none;
  }
  else if ( _(u3h_slot_is_noun(sot_w)) ) {
    u3_noun kev = u3h_slot_to_noun(sot_w);

    if ( _(u3r_sing(key, u3h(kev))) ) {
      har_u->sot_w[inx_w] = u3h_noun_be_warm(sot_w);
      return u3a_gain(u3t(kev));
    } 
    else {
      return u3_none;
    }
  }
  else {
    u3h_node* han_u = u3h_slot_to_node(sot_w);

    return _ch_node_get(han_u, 25, rem_w, key);
  }
}

/* _ch_buck_gut(): read in bucket, unifying key nouns.
*/
static u3_weak
_ch_buck_gut(u3h_buck* hab_u, u3_noun key)
{
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    if ( _(u3r_sung(key, u3h(hab_u->kev[i_w]))) ) {
      return u3a_gain(u3t(hab_u->kev[i_w]));
    }
  }
  return u3_none;
}

/* _ch_node_gut(): read in node, unifying key nouns.
*/
static u3_weak
_ch_node_gut(u3h_node* han_u, c3_w lef_w, c3_w rem_w, u3_noun key)
{
  c3_w bit_w, map_w;

  lef_w -= 5;
  bit_w = (rem_w >> lef_w);
  rem_w = (rem_w & ((1 << lef_w) - 1));
  map_w = han_u->map_w;

  if ( !(map_w & (1 << bit_w)) ) {
    return u3_none;
  }
  else {
    c3_w inx_w = _ch_popcount(map_w & ((1 << bit_w) - 1));
    c3_w sot_w = han_u->sot_w[inx_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      if ( _(u3r_sung(key, u3h(kev))) ) {
        return u3a_gain(u3t(kev));
      } 
      else {
        return u3_none;
      }
    }
    else {
      void* hav_v = u3h_slot_to_node(sot_w);

      if ( 0 == lef_w ) {
        return _ch_buck_gut(hav_v, key);
      }
      else return _ch_node_gut(hav_v, lef_w, rem_w, key);
    }
  }
}

/* u3h_gut(): read from hashtable, unifying key nouns.
**
** `key` is RETAINED.
*/
u3_weak
u3h_gut(u3p(u3h_root) har_p, u3_noun key)
{
  u3h_root* har_u = u3to(u3h_root, har_p);
  c3_w mug_w        = u3r_mug(key);
  c3_w inx_w        = (mug_w >> 25);
  c3_w rem_w        = (mug_w & ((1 << 25) - 1));
  c3_w sot_w        = har_u->sot_w[inx_w];

  if ( _(u3h_slot_is_null(sot_w)) ) {
    return u3_none;
  }
  else if ( _(u3h_slot_is_noun(sot_w)) ) {
    u3_noun kev = u3h_slot_to_noun(sot_w);

    if ( _(u3r_sung(key, u3h(kev))) ) {
      har_u->sot_w[inx_w] = u3h_noun_be_warm(sot_w);
      return u3a_gain(u3t(kev));
    } 
    else {
      return u3_none;
    }
  }
  else {
    u3h_node* han_u = u3h_slot_to_node(sot_w);

    return _ch_node_gut(han_u, 25, rem_w, key);
  }
}

/* _ch_free_buck(): free bucket
*/
static void
_ch_free_buck(u3h_buck* hab_u)
{
  c3_w i_w; 

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    u3a_lose(hab_u->kev[i_w]);
  }
  u3a_free(hab_u);
}

/* _ch_free_node(): free node.
*/
static void
_ch_free_node(u3h_node* han_u, c3_w lef_w)
{
  c3_w len_w = _ch_popcount(han_u->map_w);
  c3_w i_w;

  lef_w -= 5;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_w sot_w = han_u->sot_w[i_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      u3a_lose(kev);
    }
    else {
      void* hav_v = u3h_slot_to_node(sot_w);

      if ( 0 == lef_w ) {
        _ch_free_buck(hav_v);
      } else {
        _ch_free_node(hav_v, lef_w);
      }
    }
  }
  u3a_free(han_u);
}

/* u3h_free(): free hashtable.
*/
void
u3h_free(u3p(u3h_root) har_p)
{
  u3h_root* har_u = u3to(u3h_root, har_p);
  c3_w        i_w;

  for ( i_w = 0; i_w < 64; i_w++ ) {
    c3_w sot_w = har_u->sot_w[i_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      u3a_lose(kev);
    }
    else if ( _(u3h_slot_is_node(sot_w)) ) {
      u3h_node* han_u = u3h_slot_to_node(sot_w);

      _ch_free_node(han_u, 25);
    }
  }
  u3a_free(har_u);
}

/* _ch_walk_buck(): walk bucket for gc.
*/
static void
_ch_walk_buck(u3h_buck* hab_u, void (*fun_f)(u3_noun))
{
  c3_w i_w; 

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    fun_f(hab_u->kev[i_w]);
  }
}

/* _ch_walk_node(): walk node for gc.
*/
static void
_ch_walk_node(u3h_node* han_u, c3_w lef_w, void (*fun_f)(u3_noun))
{
  c3_w len_w = _ch_popcount(han_u->map_w);
  c3_w i_w;

  lef_w -= 5;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_w sot_w = han_u->sot_w[i_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      fun_f(kev);
    }
    else {
      void* hav_v = u3h_slot_to_node(sot_w);

      if ( 0 == lef_w ) {
        _ch_walk_buck(hav_v, fun_f);
      } else {
        _ch_walk_node(hav_v, lef_w, fun_f);
      }
    }
  }
}

/* u3h_walk(): walk hashtable for gc.
*/
void
u3h_walk(u3p(u3h_root) har_p, void (*fun_f)(u3_noun))
{
  u3h_root* har_u = u3to(u3h_root, har_p);
  c3_w        i_w;

  for ( i_w = 0; i_w < 64; i_w++ ) {
    c3_w sot_w = har_u->sot_w[i_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      fun_f(kev);
    }
    else if ( _(u3h_slot_is_node(sot_w)) ) {
      u3h_node* han_u = u3h_slot_to_node(sot_w);

      _ch_walk_node(han_u, 25, fun_f);
    }
  }
}


/* _ch_mark_buck(): mark bucket for gc.
*/
static void
_ch_mark_buck(u3h_buck* hab_u)
{
  c3_w i_w; 

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    u3a_mark_noun(hab_u->kev[i_w]);
  }
  u3a_mark_ptr(hab_u);
}

/* _ch_mark_node(): mark node for gc.
*/
static void
_ch_mark_node(u3h_node* han_u, c3_w lef_w)
{
  c3_w len_w = _ch_popcount(han_u->map_w);
  c3_w i_w;

  lef_w -= 5;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_w sot_w = han_u->sot_w[i_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      u3a_mark_noun(kev);
    }
    else {
      void* hav_v = u3h_slot_to_node(sot_w);

      if ( 0 == lef_w ) {
        _ch_mark_buck(hav_v);
      } else {
        _ch_mark_node(hav_v, lef_w);
      }
    }
  }
  u3a_mark_ptr(han_u);
}

/* u3h_mark(): mark hashtable for gc.
*/
void
u3h_mark(u3p(u3h_root) har_p)
{
  u3h_root* har_u = u3to(u3h_root, har_p);
  c3_w        i_w;

  for ( i_w = 0; i_w < 64; i_w++ ) {
    c3_w sot_w = har_u->sot_w[i_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      u3a_mark_noun(kev);
    }
    else if ( _(u3h_slot_is_node(sot_w)) ) {
      u3h_node* han_u = u3h_slot_to_node(sot_w);

      _ch_mark_node(han_u, 25);
    }
  }
  u3a_mark_ptr(har_u);
}
