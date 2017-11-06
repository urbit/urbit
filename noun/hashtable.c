/* g/h.c
**
*/
#include "all.h"

static void _ch_slot_put(u3h_slot* sot_w, u3_noun kev, c3_w lef_w, c3_w rem_w, c3_w* use_w);
static c3_o _ch_trim_slot(u3h_slot *sot_w, c3_w lef_w);

/* u3h_new_cache(): create hashtable with bounded size.
*/
u3p(u3h_root)
u3h_new_cache(c3_w clk_w)
{
  u3h_root*     har_u = u3a_walloc(c3_wiseof(u3h_root));
  u3p(u3h_root) har_p = u3of(u3h_root, har_u);
  c3_w        i_w;

  har_u->clk_w = clk_w;
  har_u->use_w = 0;
  for ( i_w = 0; i_w < 64; i_w++ ) {
    har_u->sot_w[i_w] = 0;
  }
  return har_p;
}

/* u3h_new(): create hashtable.
*/
u3p(u3h_root)
u3h_new(void)
{
  return u3h_new_cache(0);
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
  hab_u->arm_w = 0;
  return hab_u;
}

/* ha_buck_add(): add to bucket.
*/
static u3h_buck*
_ch_buck_add(u3h_buck* hab_u, u3_noun kev, c3_w *use_w)
{
  c3_w i_w;

  //  if our key is equal to any of the existing keys in the bucket,
  //  then replace that key-value pair with kev.
  //
  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    u3_noun kov = u3h_slot_to_noun(hab_u->sot_w[i_w]);
    if ( c3y == u3r_sing(u3h(kev), u3h(kov)) ) {
      u3a_lose(kov);
      hab_u->sot_w[i_w] = u3h_noun_to_slot(kev);

      return hab_u;
    }
  }

  //  create mutant bucket with added key-value pair.
  {
    c3_w len_w      = hab_u->len_w;
    u3h_buck* bah_u = u3a_walloc(c3_wiseof(u3h_buck) +
                                 (len_w + 1) * c3_wiseof(u3h_slot));

    bah_u->arm_w    = hab_u->arm_w + 1;
    bah_u->len_w    = len_w + 1;
    bah_u->sot_w[0] = u3h_noun_to_slot(kev);

    // Optimize: use u3a_wealloc().
    //
    for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
      bah_u->sot_w[i_w + 1] = hab_u->sot_w[i_w];
    }

    u3a_wfree(hab_u);
    *use_w += 1;
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
  han_u->arm_w = 0;
  return han_u;
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

/* _ch_node_add(): add to node.
*/
static u3h_node*
_ch_node_add(u3h_node* han_u, c3_w lef_w, c3_w rem_w, u3_noun kev, c3_w *use_w)
{
  c3_w bit_w, inx_w, map_w, i_w;

  lef_w -= 5;
  bit_w = (rem_w >> lef_w);
  rem_w = (rem_w & ((1 << lef_w) - 1));
  map_w = han_u->map_w;
  inx_w = _ch_popcount(map_w & ((1 << bit_w) - 1));

  if ( map_w & (1 << bit_w) ) {
    _ch_slot_put(&(han_u->sot_w[inx_w]), kev, lef_w, rem_w, use_w);
    return han_u;
  }
  else {
    //  nothing was at this slot.
    //  Optimize: use u3a_wealloc.
    //
    c3_w      len_w = _ch_popcount(map_w);
    u3h_node* nah_u = u3a_walloc(c3_wiseof(u3h_node) +
                                 ((len_w + 1) * c3_wiseof(u3h_slot)));
    nah_u->map_w = han_u->map_w | (1 << bit_w);
    nah_u->arm_w = han_u->arm_w;

    for ( i_w = 0; i_w < inx_w; i_w++ ) {
      nah_u->sot_w[i_w] = han_u->sot_w[i_w];
    }
    u3m_p("empty node slot store", kev);
    nah_u->sot_w[inx_w] = u3h_noun_be_warm(u3h_noun_to_slot(kev));
    for ( i_w = inx_w; i_w < len_w; i_w++ ) {
      nah_u->sot_w[i_w + 1] = han_u->sot_w[i_w];
    }

    u3a_wfree(han_u);
    *use_w += 1;
    return nah_u;
  }
}


/* _ch_some_add(): add to node or bucket.
*/
static void*
_ch_some_add(void* han_v, c3_w lef_w, c3_w rem_w, u3_noun kev, c3_w *use_w)
{
  if ( 0 == lef_w ) {
    return _ch_buck_add((u3h_buck*)han_v, kev, use_w);
  }
  else return _ch_node_add((u3h_node*)han_v, lef_w, rem_w, kev, use_w);
}

/* store a key-value pair in a u3h_slot (root or node, bucks work differently) */
static void
_ch_slot_put(u3h_slot* sot_w, u3_noun kev, c3_w lef_w, c3_w rem_w, c3_w* use_w)
{
  if ( c3y == u3h_slot_is_null(*sot_w) ) {
    u3m_p("empty slot store", kev);
    *sot_w = u3h_noun_be_warm(u3h_noun_to_slot(kev));
    *use_w += 1;
  }
  else if ( c3y == u3h_slot_is_noun(*sot_w) ) {
    u3_noun kov = u3h_slot_to_noun(*sot_w);
    if ( c3y == u3r_sing(u3h(kev), u3h(kov)) ) {
      u3m_p("same key store", u3nc(u3k(kev), u3k(kov)));
      *sot_w = u3h_noun_be_warm(u3h_noun_to_slot(kev));
      u3z(kov);
    }
    else {
      c3_w  rom_w = u3r_mug(u3h(kov)) & ((1 << lef_w) - 1);
      void* hav_v = _ch_some_new(lef_w);

      *use_w -= 1; // take one out, add two
      hav_v = _ch_some_add(hav_v, lef_w, rem_w, kev, use_w);
      hav_v = _ch_some_add(hav_v, lef_w, rom_w, kov, use_w);
      *sot_w = u3h_node_to_slot(hav_v);
    }
  }
  else {
    c3_assert( c3y == u3h_slot_is_node(*sot_w) );
    void* hav_v = _ch_some_add(u3h_slot_to_node(*sot_w), lef_w, rem_w, kev, use_w);
    *sot_w = u3h_node_to_slot(hav_v);
  }
}

/* u3h_put(): insert in hashtable.
**
** `key` is RETAINED; `val` is transferred.
*/
void
u3h_put(u3p(u3h_root) har_p, u3_noun key, u3_noun val)
{
  u3h_root*   har_u = u3to(u3h_root, har_p);
  u3_noun     kev   = u3nc(u3k(key), val);
  c3_w        mug_w = u3r_mug(key);
  c3_w        inx_w = (mug_w >> 25);  //  6 bits
  c3_w        rem_w = (mug_w & ((1 << 25) - 1));

  _ch_slot_put(&(har_u->sot_w[inx_w]), kev, 25, rem_w, &(har_u->use_w));
  if ( har_u->clk_w > 0 ) {
    u3h_trim_to(har_p, har_u->clk_w);
  }
}

static c3_o
_ch_trim_buck(u3h_slot* sot_w)
{
  u3h_buck* hab_u = (u3h_buck*) u3h_slot_to_node(*sot_w);
  c3_w i_w, j_w, len_w = hab_u->len_w;

  for ( i_w = hab_u->arm_w; i_w < len_w; hab_u->arm_w = ++i_w ) {
    u3h_slot* tos_w = &(hab_u->sot_w[i_w]);
    if ( c3y == _ch_trim_slot(tos_w, 0) ) {
      // child always shrinks in bucket
      if ( 2 == len_w ) {
        // pick one of two items in bucket
        *sot_w = hab_u->sot_w[ (0 == i_w) ? 1 : 0 ];
      }
      else {
        // make smaller bucket
        len_w -= 1;
        u3h_buck* bah_u = u3a_walloc(c3_wiseof(u3h_buck) +
                                     len_w * c3_wiseof(u3h_slot));
        bah_u->len_w    = len_w;
        bah_u->arm_w    = hab_u->arm_w;

        for ( j_w = 0; j_w < i_w; ++j_w ) {
          bah_u->sot_w[j_w] = hab_u->sot_w[j_w];
        }
        for ( j_w = i_w; j_w < len_w; ++j_w ) {
          bah_u->sot_w[j_w] = hab_u->sot_w[j_w + 1];
        }

        *sot_w = u3h_node_to_slot(bah_u);
      }
      u3a_wfree(hab_u);
      return c3y;
    }
  }
}

static c3_o
_ch_trim_node(u3h_slot* sot_w, c3_w lef_w)
{
  u3h_node* han_u = (u3h_node*) u3h_slot_to_node(*sot_w);
  c3_w i_w, j_w, len_w = _ch_popcount(han_u->map_w);
  lef_w -= 5;

  for ( i_w = han_u->arm_w; i_w < len_w; han_u->arm_w = ++i_w ) {
    u3h_slot* tos_w = &(han_u->sot_w[i_w]);
    if ( c3y == _ch_trim_slot(tos_w, lef_w) ) {
      if ( c3y == u3h_slot_is_null(*tos_w) ) {
        // child shrank
        if ( 2 == len_w ) {
          // shrink to a single key-value pair
          *sot_w = han_u->sot_w[ (0 == i_w) ? 1 : 0 ];
        }
        else {
          // shrink to a smaller node
          len_w -= 1;
          u3h_node* nah_u = u3a_walloc(c3_wiseof(u3h_node) +
                                       (len_w * c3_wiseof(u3h_slot)));
          nah_u->arm_w = han_u->arm_w;
          nah_u->map_w = han_u->map_w & ~(1 << i_w);

          for ( j_w = 0; j_w < i_w; j_w++ ) {
            nah_u->sot_w[j_w] = han_u->sot_w[j_w];
          }
          for ( j_w = i_w; j_w < len_w; ++j_w ) {
            nah_u->sot_w[j_w] = han_u->sot_w[j_w + 1];
          }
          *sot_w = u3h_node_to_slot(nah_u);
        }
        u3a_wfree(han_u);
      }
      return c3y;
    }
  }

  han_u->arm_w = 0;
  return c3n;
}

static c3_o
_ch_trim_some(u3h_slot* sot_w, c3_w lef_w)
{
  if ( 0 == lef_w ) {
    return _ch_trim_buck(sot_w);
  }
  else {
    return _ch_trim_node(sot_w, lef_w);
  }
}

/* _ch_trim_slot(): trim one key-value pair from slot */
static c3_o
_ch_trim_slot(u3h_slot *sot_w, c3_w lef_w)
{
  if ( c3y == u3h_slot_is_null(*sot_w) ) {
    return c3n;
  }
  else {
    if ( c3y == u3h_slot_is_noun(*sot_w) ) {
      if ( c3y == u3h_slot_is_warm(*sot_w) ) {
        *sot_w = u3h_noun_be_cold(*sot_w);
        return c3n;
      }
      else {
        u3_noun kev = u3h_slot_to_noun(*sot_w);
        u3z(kev);
        *sot_w = 0;
        return c3y;
      }
    }
    else {
      c3_assert( c3y == u3h_slot_is_node(*sot_w) );
      return _ch_trim_some(sot_w, lef_w);
    }
  }
}

/* _ch_trim_root(): trim one key-value pair from root
*/
static void
_ch_trim_root(u3h_root *har_u)
{
  u3h_slot* sot_w;
  c3_assert(har_u->use_w > 0);

  while ( 1 ) {
    sot_w = &(har_u->sot_w[har_u->arm_w]);
    if ( c3y == _ch_trim_slot(sot_w, 25) ) {
      return;
    }
    har_u->arm_w = (har_u->arm_w + 1) % 64;
  }
}

/* u3h_trim_to(): trim to n key-value pairs
*/
void
u3h_trim_to(u3p(u3h_root) har_p, c3_w n_w)
{
  u3h_root*   har_u = u3to(u3h_root, har_p);
  while ( har_u->use_w > n_w ) {
    _ch_trim_root(har_u);
  }
}

/* _ch_trim_one_buck(): trim one key-value pair from a bucket.
*/
static c3_o
_ch_trim_one_buck(u3h_slot* hal_w)
{
  u3h_buck* hab_u = (u3h_buck*)u3h_slot_to_node(*hal_w);
  u3h_slot sot_w = hab_u->sot_w[hab_u->arm_w];

  c3_assert(hab_u->len_w != 1);

  //  instead of assigning a bucket with only one element,
  //  assign a single key-value pair as a slot.
  //
  if ( hab_u->len_w == 2 ) {
    u3h_slot fir_w = hab_u->sot_w[0];
    u3h_slot sec_w = hab_u->sot_w[1];

    if ( _(u3h_slot_is_warm(fir_w)) ) {
      if ( _(u3h_slot_is_warm(sec_w)) ) {
        hab_u->sot_w[0] = u3h_noun_be_cold(fir_w);
        hab_u->sot_w[1] = u3h_noun_be_cold(sec_w);
        return c3n;
      }
      else {
        u3a_lose(u3h_slot_to_noun(sec_w));
        *hal_w = u3h_noun_be_cold(u3h_slot_to_noun(fir_w));
        return c3y;
      }
    }
    else {
      u3a_lose(u3h_slot_to_noun(fir_w));
      *hal_w = u3h_slot_to_noun(sec_w);
      return c3y;
    }
  }
  else {
    while ( hab_u->arm_w < hab_u->len_w ) {
      sot_w = hab_u->sot_w[hab_u->arm_w];

      if ( _(u3h_slot_is_warm(sot_w)) ) {
        hab_u->sot_w[hab_u->arm_w] = u3h_noun_be_cold(sot_w);
        hab_u->arm_w++;
        //  next
      }
      else {
        c3_w i_w;
        c3_w len_w      = hab_u->len_w;
        u3h_buck* bah_u = u3a_walloc(c3_wiseof(u3h_buck) +
                                     (len_w - 1) * c3_wiseof(u3h_slot));

        bah_u->len_w    = len_w - 1;
        bah_u->arm_w    = hab_u->arm_w;

        for ( i_w = 0; i_w < hab_u->arm_w; i_w++ ) {
          bah_u->sot_w[i_w] = hab_u->sot_w[i_w];
        }

        u3a_lose(u3h_slot_to_noun(hab_u->sot_w[hab_u->arm_w]));

        for ( i_w = hab_u->arm_w; i_w < bah_u->len_w; i_w++ ) {
          bah_u->sot_w[i_w] = hab_u->sot_w[i_w + 1];
        }

        *hal_w = u3h_node_to_slot(bah_u);
        return c3y;
      }
    }
    hab_u->arm_w = 0;
    return c3n;
  }
}

/* _ch_trim_one_node(): trim one key-value pair from a node.
*/
static c3_o
_ch_trim_one_node(u3h_slot* hal_w, c3_w lef_w)
{
  u3h_node* han_u = (u3h_node*) u3h_slot_to_node(*hal_w);
  u3h_slot sot_w = han_u->sot_w[han_u->arm_w];
  c3_w len_w = _ch_popcount(han_u->map_w);
  c3_c* pre = _ch_pre(lef_w);

  c3_assert(len_w != 1);

  while (han_u->arm_w < len_w) {
    sot_w = han_u->sot_w[han_u->arm_w];

    if ( _(u3h_slot_is_node(sot_w)) ) {
      if ( _(_ch_trim_one_some(&sot_w, lef_w - 5)) ) {
        return c3y;
      }
      han_u->arm_w++;  //  next
    }

    else if ( _(u3h_slot_is_warm(sot_w)) ) {
      han_u->sot_w[han_u->arm_w] = u3h_noun_be_cold(sot_w);
      han_u->arm_w++;  //  next
    }
    else {
      c3_w bit_w, i_w, inx_w;

      u3h_node* nah_u = u3a_walloc(c3_wiseof(u3h_node) +
                                   ((len_w - 1) * c3_wiseof(u3h_slot)));

      bit_w = han_u->arm_w;
      nah_u->map_w = han_u->map_w & ~(1 << bit_w);
      inx_w = _ch_popcount(nah_u->map_w & ((1 << bit_w) - 1));
      nah_u->arm_w = han_u->arm_w;

      for ( i_w = 0; i_w < inx_w; i_w++ ) {
        nah_u->sot_w[i_w] = han_u->sot_w[i_w];
      }

      u3a_lose(u3h_slot_to_noun(han_u->sot_w[inx_w]));

      for ( i_w = inx_w; i_w < len_w - 1; i_w++ ) {
        nah_u->sot_w[i_w] = han_u->sot_w[i_w + 1];
      }

      *hal_w = u3h_node_to_slot(nah_u);
      return c3y;
    }

  }
  han_u->arm_w = 0;
  return c3n;
}

/* _ch_buck_hum(): read in bucket.
*/
static c3_o
_ch_buck_hum(u3h_buck* hab_u, c3_w mug_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    if ( mug_w == u3r_mug(u3h(u3h_slot_to_noun(hab_u->sot_w[i_w]))) ) {
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

/* u3h_hum(): check presence in hashtable.
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

/* _ch_buck_git(): read in bucket.
*/
static u3_weak
_ch_buck_git(u3h_buck* hab_u, u3_noun key)
{
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    u3_noun kev = u3h_slot_to_noun(hab_u->sot_w[i_w]);
    if ( _(u3r_sing(key, u3h(kev))) ) {
      return u3t(kev);
    }
  }
  return u3_none;
}

/* _ch_node_git(): read in node.
*/
static u3_weak
_ch_node_git(u3h_node* han_u, c3_w lef_w, c3_w rem_w, u3_noun key)
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
        return u3t(kev);
      }
      else {
        return u3_none;
      }
    }
    else {
      void* hav_v = u3h_slot_to_node(sot_w);

      if ( 0 == lef_w ) {
        return _ch_buck_git(hav_v, key);
      }
      else return _ch_node_git(hav_v, lef_w, rem_w, key);
    }
  }
}

/* u3h_git(): read from hashtable.
**
** `key` is RETAINED; result is RETAINED.
*/
u3_weak
u3h_git(u3p(u3h_root) har_p, u3_noun key)
{
  u3h_root* har_u = u3to(u3h_root, har_p);
  c3_w      mug_w = u3r_mug(key);
  c3_w      inx_w = (mug_w >> 25);
  c3_w      rem_w = (mug_w & ((1 << 25) - 1));
  c3_w      sot_w = har_u->sot_w[inx_w];

  if ( _(u3h_slot_is_null(sot_w)) ) {
    return u3_none;
  }
  else if ( _(u3h_slot_is_noun(sot_w)) ) {
    u3_noun kev = u3h_slot_to_noun(sot_w);

    if ( _(u3r_sing(key, u3h(kev))) ) {
      har_u->sot_w[inx_w] = u3h_noun_be_warm(sot_w);
      return u3t(kev);
    }
    else {
      return u3_none;
    }
  }
  else {
    u3h_node* han_u = u3h_slot_to_node(sot_w);

    return _ch_node_git(han_u, 25, rem_w, key);
  }
}

/* u3h_get(): read from hashtable, incrementing refcount.
**
** `key` is RETAINED; result is PRODUCED.
*/
u3_weak
u3h_get(u3p(u3h_root) har_p, u3_noun key)
{
  u3_noun pro = u3h_git(har_p, key);

  if ( u3_none != pro ) {
    u3a_gain(pro);
  }
  return pro;
}

/* _ch_buck_gut(): read in bucket, unifying key nouns.
*/
static u3_weak
_ch_buck_gut(u3h_buck* hab_u, u3_noun key)
{
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    u3_noun kev = u3h_slot_to_noun(hab_u->sot_w[i_w]);
    if ( _(u3r_sung(key, u3h(kev))) ) {
      return u3a_gain(u3t(kev));
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
    u3a_lose(u3h_slot_to_noun(hab_u->sot_w[i_w]));
  }
  u3a_wfree(hab_u);
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
  u3a_wfree(han_u);
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
  u3a_wfree(har_u);
}

/* _ch_walk_buck(): walk bucket for gc.
*/
static void
_ch_walk_buck(u3h_buck* hab_u, void (*fun_f)(u3_noun))
{
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    fun_f(u3h_slot_to_noun(hab_u->sot_w[i_w]));
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
c3_w
_ch_mark_buck(u3h_buck* hab_u)
{
  c3_w tot_w = 0;
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    tot_w += u3a_mark_noun(u3h_slot_to_noun(hab_u->sot_w[i_w]));
  }
  tot_w += u3a_mark_ptr(hab_u);

  return tot_w;
}

/* _ch_mark_node(): mark node for gc.
*/
c3_w
_ch_mark_node(u3h_node* han_u, c3_w lef_w)
{
  c3_w tot_w = 0;
  c3_w len_w = _ch_popcount(han_u->map_w);
  c3_w i_w;

  lef_w -= 5;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_w sot_w = han_u->sot_w[i_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      tot_w += u3a_mark_noun(kev);
    }
    else {
      void* hav_v = u3h_slot_to_node(sot_w);

      if ( 0 == lef_w ) {
        tot_w += _ch_mark_buck(hav_v);
      } else {
        tot_w += _ch_mark_node(hav_v, lef_w);
      }
    }
  }

  tot_w += u3a_mark_ptr(han_u);

  return tot_w;
}

/* u3h_mark(): mark hashtable for gc.
*/
c3_w
u3h_mark(u3p(u3h_root) har_p)
{
  c3_w tot_w = 0;
  u3h_root* har_u = u3to(u3h_root, har_p);
  c3_w        i_w;

  for ( i_w = 0; i_w < 64; i_w++ ) {
    c3_w sot_w = har_u->sot_w[i_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      tot_w += u3a_mark_noun(kev);
    }
    else if ( _(u3h_slot_is_node(sot_w)) ) {
      u3h_node* han_u = u3h_slot_to_node(sot_w);

      tot_w += _ch_mark_node(han_u, 25);
    }
  }

  tot_w += u3a_mark_ptr(har_u);

  return tot_w;
}
