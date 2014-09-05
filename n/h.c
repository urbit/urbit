/* n/h.c
**
** This file is in the public domain.
*/
#include "all.h"

static void* _ch_some_add(void* han_v, c3_w, c3_w, u2_noun);
static void* _ch_some_new(c3_w lef_w);

/* u2_ch_new(): create hashtable.
*/
u2_ch_root* 
u2_ch_new(void)
{
  u2_ch_root* har_u = u2_ca_walloc(c3_wiseof(u2_ch_root));
  c3_w        i_w;

  har_u->clk_w = 0;
  for ( i_w = 0; i_w < 64; i_w++ ) {
    har_u->sot_w[i_w] = 0;
  }
  return har_u;
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
static u2_ch_buck*
_ch_buck_new(void)
{
  u2_ch_buck* hab_u = u2_ca_walloc(c3_wiseof(u2_ch_buck));

  hab_u->len_w = 0;
  return hab_u;
}

/* ha_buck_add(): add to bucket.
*/
static u2_ch_buck*
_ch_buck_add(u2_ch_buck* hab_u, u2_noun kev)
{
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    if ( u2_yes == u2_cr_sing(u2h(kev), u2h(hab_u->kev[i_w])) ) {
      u2_ca_lose(hab_u->kev[i_w]);
      hab_u->kev[i_w] = kev;

      return hab_u;
    }
  }

  {
    c3_w len_w        = hab_u->len_w;
    u2_ch_buck* bah_u = u2_ca_walloc(c3_wiseof(u2_ch_buck) +
                                     (len_w + 1) * c3_wiseof(u2_noun));

    bah_u->len_w = len_w + 1;
    bah_u->kev[0] = kev;

    // Optimize: use u2_ca_wealloc().
    //
    for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) { 
      bah_u->kev[i_w + 1] = hab_u->kev[i_w];
    }

    u2_ca_free(hab_u);
    return bah_u;
  }
}

/* _ch_node_new(): create new, empty node.
*/
static u2_ch_node*
_ch_node_new(void)
{
  u2_ch_node* han_u = u2_ca_walloc(c3_wiseof(u2_ch_node));

  han_u->map_w = 0;
  return han_u;
}

/* _ch_node_add(): add to node.
*/
static u2_ch_node*
_ch_node_add(u2_ch_node* han_u, c3_w lef_w, c3_w rem_w, u2_noun kev)
{
  c3_w bit_w, inx_w, map_w, i_w;

  lef_w -= 5;
  bit_w = (rem_w >> lef_w);
  rem_w = (rem_w & ((1 << lef_w) - 1));
  map_w = han_u->map_w;
  inx_w = _ch_popcount(map_w & ((1 << bit_w) - 1));

  if ( map_w & (1 << bit_w) ) {
    c3_w sot_w = han_u->sot_w[inx_w];

    if ( u2_so(u2_ch_slot_is_node(sot_w)) ) {
      void* hav_v = u2_ch_slot_to_node(sot_w);

      hav_v = _ch_some_add(hav_v, lef_w, rem_w, kev);
      han_u->sot_w[inx_w] = u2_ch_node_to_slot(hav_v);

      return han_u;
    }
    else {
      u2_noun kov = u2_ch_slot_to_noun(sot_w);

      if ( u2_yes == u2_cr_sing(u2h(kev), u2h(kov)) ) {
        u2_ca_lose(kov);
        han_u->sot_w[inx_w] = u2_ch_noun_to_slot(kev);
        return han_u;
      }
      else {
        c3_w  rom_w = u2_cr_mug(u2h(kov)) & ((1 << lef_w) - 1);
        void* hav_v = _ch_some_new(lef_w);

        //  Optimize: need a custom collision create.
        //
        hav_v = _ch_some_add(hav_v, lef_w, rem_w, kev);
        hav_v = _ch_some_add(hav_v, lef_w, rom_w, kov);

        han_u->sot_w[inx_w] = u2_ch_node_to_slot(hav_v);
        return han_u;
      }
    }
  }
  else {
    //  Optimize: use u2_ca_wealloc.
    //
    c3_w        len_w = _ch_popcount(map_w);
    u2_ch_node* nah_u = u2_ca_walloc(c3_wiseof(u2_ch_node) + 
                                     ((len_w + 1) * c3_wiseof(u2_ch_slot)));
    nah_u->map_w = han_u->map_w | (1 << bit_w);
    
    for ( i_w = 0; i_w < inx_w; i_w++ ) {
      nah_u->sot_w[i_w] = han_u->sot_w[i_w]; 
    }
    nah_u->sot_w[inx_w] = u2_ch_noun_to_slot(kev); 
    
    for ( i_w = inx_w; i_w < len_w; i_w++ ) {
      nah_u->sot_w[i_w + 1] = han_u->sot_w[i_w];
    }
    u2_ca_free(han_u);
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
_ch_some_add(void* han_v, c3_w lef_w, c3_w rem_w, u2_noun kev)
{
  if ( 0 == lef_w ) {
    return _ch_buck_add(han_v, kev);
  }
  else return _ch_node_add(han_v, lef_w, rem_w, kev);
}

/* u2_ch_put(): insert in hashtable.
*/
void
u2_ch_put(u2_ch_root* har_u, u2_noun key, u2_noun val)
{
  u2_noun kev    = u2nc(u2k(key), val);
  c3_w    mug_w  = u2_cr_mug(key);
  c3_w    inx_w  = (mug_w >> 25);
  c3_w    rem_w  = (mug_w & ((1 << 25) - 1));
  c3_w    sot_w  = har_u->sot_w[inx_w];

  if ( u2_so(u2_ch_slot_is_null(sot_w)) ) {
    har_u->sot_w[inx_w] = u2_ch_noun_to_slot(kev);
  }
  else {
    u2_ch_node* han_u;

    if ( u2_so(u2_ch_slot_is_noun(sot_w)) ) {
      u2_noun kov   = u2_ch_slot_to_noun(sot_w);
      c3_w    rom_w = u2_cr_mug(u2h(kov)) & ((1 << 25) - 1);
 
      han_u = _ch_node_new();
      han_u = _ch_node_add(han_u, 25, rem_w, kev);
      han_u = _ch_node_add(han_u, 25, rom_w, kov);
    }
    else {
      han_u = _ch_node_add(u2_ch_slot_to_node(sot_w), 25, rem_w, kev);
    }
    har_u->sot_w[inx_w] = u2_ch_node_to_slot(han_u);
  }
}

/* _ch_buck_get(): read in bucket.
*/
static u2_weak
_ch_buck_get(u2_ch_buck* hab_u, u2_noun key)
{
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    if ( u2_so(u2_cr_sing(key, u2h(hab_u->kev[i_w]))) ) {
      return u2_ca_gain(u2t(hab_u->kev[i_w]));
    }
  }
  return u2_none;
}

/* _ch_node_get(): read in node.
*/
static u2_weak
_ch_node_get(u2_ch_node* han_u, c3_w lef_w, c3_w rem_w, u2_noun key)
{
  c3_w bit_w, map_w;

  lef_w -= 5;
  bit_w = (rem_w >> lef_w);
  rem_w = (rem_w & ((1 << lef_w) - 1));
  map_w = han_u->map_w;

  if ( !(map_w & (1 << bit_w)) ) {
    return u2_none;
  }
  else {
    c3_w inx_w = _ch_popcount(map_w & ((1 << bit_w) - 1));
    c3_w sot_w = han_u->sot_w[inx_w];

    if ( u2_so(u2_ch_slot_is_noun(sot_w)) ) {
      u2_noun kev = u2_ch_slot_to_noun(sot_w);

      if ( u2_so(u2_cr_sing(key, u2h(kev))) ) {
        return u2_ca_gain(u2t(kev));
      } 
      else {
        return u2_none;
      }
    }
    else {
      void* hav_v = u2_ch_slot_to_node(sot_w);

      if ( 0 == lef_w ) {
        return _ch_buck_get(hav_v, key);
      }
      else return _ch_node_get(hav_v, lef_w, rem_w, key);
    }
  }
}

/* u2_ch_get(): read from hashtable.
*/
u2_weak
u2_ch_get(u2_ch_root* har_u, u2_noun key)
{
  c3_w mug_w = u2_cr_mug(key);
  c3_w inx_w = (mug_w >> 25);
  c3_w rem_w  = (mug_w & ((1 << 25) - 1));
  c3_w sot_w  = har_u->sot_w[inx_w];

  if ( u2_so(u2_ch_slot_is_null(sot_w)) ) {
    return u2_none;
  }
  else if ( u2_so(u2_ch_slot_is_noun(sot_w)) ) {
    u2_noun kev = u2_ch_slot_to_noun(sot_w);

    if ( u2_so(u2_cr_sing(key, u2h(kev))) ) {
      har_u->sot_w[inx_w] = u2_ch_noun_be_warm(sot_w);
      return u2_ca_gain(u2t(kev));
    } 
    else {
      return u2_none;
    }
  }
  else {
    u2_ch_node* han_u = u2_ch_slot_to_node(sot_w);

    return _ch_node_get(han_u, 25, rem_w, key);
  }
}

/* _ch_free_buck(): free bucket
*/
static void
_ch_free_buck(u2_ch_buck* hab_u)
{
  c3_w i_w; 

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    u2_ca_lose(hab_u->kev[i_w]);
  }
  u2_ca_free(hab_u);
}

/* _ch_free_node(): free node.
*/
static void
_ch_free_node(u2_ch_node* han_u, c3_w lef_w)
{
  c3_w len_w = _ch_popcount(han_u->map_w);
  c3_w i_w;

  lef_w -= 5;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_w sot_w = han_u->sot_w[i_w];

    if ( u2_so(u2_ch_slot_is_noun(sot_w)) ) {
      u2_noun kev = u2_ch_slot_to_noun(sot_w);

      u2_ca_lose(kev);
    }
    else {
      void* hav_v = u2_ch_slot_to_node(sot_w);

      if ( 0 == lef_w ) {
        _ch_free_buck(hav_v);
      } else {
        _ch_free_node(hav_v, lef_w);
      }
    }
  }
  u2_ca_free(han_u);
}

/* u2_ch_free(): free hashtable.
*/
void
u2_ch_free(u2_ch_root* har_u)
{
  c3_w i_w;

  for ( i_w = 0; i_w < 64; i_w++ ) {
    c3_w sot_w = har_u->sot_w[i_w];

    if ( u2_so(u2_ch_slot_is_noun(sot_w)) ) {
      u2_noun kev = u2_ch_slot_to_noun(sot_w);

      u2_ca_lose(kev);
    }
    else if ( u2_so(u2_ch_slot_is_node(sot_w)) ) {
      u2_ch_node* han_u = u2_ch_slot_to_node(sot_w);

      _ch_free_node(han_u, 25);
    }
  }
  u2_ca_free(har_u);
}

