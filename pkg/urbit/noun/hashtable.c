/* g/h.c
**
*/
#include "all.h"

/* CUT_END(): extract [b_w] low bits from [a_w]
*/
#define CUT_END(a_w, b_w) (a_w & ((1 << b_w) - 1))

/* BIT_SET(): [1] if bit [b_w] is set in [a_w]
*/
#define BIT_SET(a_w, b_w) (a_w & (1 << b_w))

static void
_ch_slot_put(u3h_slot* sot_w, u3_noun kev, c3_w lef_w, c3_w rem_w, c3_w* use_w);

static c3_o
_ch_trim_slot(u3h_root* har_u, u3h_slot *sot_w, c3_w lef_w, c3_w rem_w);

c3_w
_ch_skip_slot(c3_w mug_w, c3_w lef_w);

/* u3h_new_cache(): create hashtable with bounded size.
*/
u3p(u3h_root)
u3h_new_cache(c3_w max_w)
{
  u3h_root*     har_u = u3a_walloc(c3_wiseof(u3h_root));
  u3p(u3h_root) har_p = u3of(u3h_root, har_u);
  c3_w        i_w;

  har_u->max_w       = max_w;
  har_u->use_w       = 0;
  har_u->arm_u.mug_w = 0;
  har_u->arm_u.inx_w = 0;
  har_u->arm_u.buc_o = c3n;

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

/* _ch_buck_new(): create new bucket.
*/
static u3h_buck*
_ch_buck_new(c3_w len_w)
{
  u3h_buck* hab_u = u3a_walloc(c3_wiseof(u3h_buck) +
                               (len_w * c3_wiseof(u3h_slot)));
  hab_u->len_w = len_w;
  return hab_u;
}

/* _ch_node_new(): create new node.
*/
static u3h_node*
_ch_node_new(c3_w len_w)
{
  u3h_node* han_u = u3a_walloc(c3_wiseof(u3h_node) +
                               (len_w * c3_wiseof(u3h_slot)));
  han_u->map_w = 0;
  return han_u;
}

/* _ch_some_new(): create new, empty node or bucket.
*/
static void*
_ch_some_new(c3_w lef_w)
{
  if ( 0 == lef_w ) {
    return _ch_buck_new(0);
  }
  else {
    return _ch_node_new(0);
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
  rem_w = CUT_END(rem_w, lef_w);
  map_w = han_u->map_w;
  inx_w = _ch_popcount(CUT_END(map_w, bit_w));

  if ( BIT_SET(map_w, bit_w) ) {
    _ch_slot_put(&(han_u->sot_w[inx_w]), kev, lef_w, rem_w, use_w);
    return han_u;
  }
  else {
    //  nothing was at this slot.
    //  Optimize: use u3a_wealloc.
    //
    c3_w      len_w = _ch_popcount(map_w);
    u3h_node* nah_u = _ch_node_new(1 + len_w);
    nah_u->map_w    = han_u->map_w | (1 << bit_w);

    for ( i_w = 0; i_w < inx_w; i_w++ ) {
      nah_u->sot_w[i_w] = han_u->sot_w[i_w];
    }
    nah_u->sot_w[inx_w] = u3h_noun_be_warm(u3h_noun_to_slot(kev));
    for ( i_w = inx_w; i_w < len_w; i_w++ ) {
      nah_u->sot_w[i_w + 1] = han_u->sot_w[i_w];
    }

    u3a_wfree(han_u);
    *use_w += 1;
    return nah_u;
  }
}

/* ch_buck_add(): add to bucket.
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
      hab_u->sot_w[i_w] = u3h_noun_to_slot(kev);
      u3z(kov);
      return hab_u;
    }
  }

  //  create mutant bucket with added key-value pair.
  //  Optimize: use u3a_wealloc().
  {
    u3h_buck* bah_u = _ch_buck_new(1 + hab_u->len_w);
    bah_u->sot_w[0] = u3h_noun_to_slot(kev);

    for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
      bah_u->sot_w[i_w + 1] = hab_u->sot_w[i_w];
    }

    u3a_wfree(hab_u);
    *use_w += 1;
    return bah_u;
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

/* _ch_slot_put(): store a key-value pair in a u3h_slot (root or node)
*/
static void
_ch_slot_put(u3h_slot* sot_w, u3_noun kev, c3_w lef_w, c3_w rem_w, c3_w* use_w)
{
  if ( c3y == u3h_slot_is_null(*sot_w) ) {
    *sot_w = u3h_noun_be_warm(u3h_noun_to_slot(kev));
    *use_w += 1;
  }
  else if ( c3y == u3h_slot_is_noun(*sot_w) ) {
    u3_noun kov = u3h_slot_to_noun(*sot_w);
    if ( c3y == u3r_sing(u3h(kev), u3h(kov)) ) {
      *sot_w = u3h_noun_be_warm(u3h_noun_to_slot(kev));
      u3z(kov);
    }
    else {
      c3_w  rom_w = CUT_END(u3r_mug(u3h(kov)), lef_w);
      void* hav_v = _ch_some_new(lef_w);

      *use_w -= 1; // take one out, add two
      hav_v = _ch_some_add(hav_v, lef_w, rom_w, kov, use_w);
      hav_v = _ch_some_add(hav_v, lef_w, rem_w, kev, use_w);
      *sot_w = u3h_node_to_slot(hav_v);
    }
  }
  else {
    void* hav_v = _ch_some_add(u3h_slot_to_node(*sot_w),
                               lef_w,
                               rem_w,
                               kev,
                               use_w);

    c3_assert( c3y == u3h_slot_is_node(*sot_w) );
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
  u3h_root* har_u = u3to(u3h_root, har_p);
  u3_noun   kev   = u3nc(u3k(key), val);
  c3_w      mug_w = u3r_mug(key);
  c3_w      inx_w = (mug_w >> 25);  //  6 bits
  c3_w      rem_w = CUT_END(mug_w, 25);

  _ch_slot_put(&(har_u->sot_w[inx_w]), kev, 25, rem_w, &(har_u->use_w));
  if ( har_u->max_w > 0 ) {
    u3h_trim_to(har_p, har_u->max_w);
  }
}

/* _ch_trim_node(): trim one entry from a node slot or its children
*/
static c3_o
_ch_trim_node(u3h_root* har_u, u3h_slot* sot_w, c3_w lef_w, c3_w rem_w)
{
  c3_w bit_w, map_w, inx_w;
  u3h_slot* tos_w;
  u3h_node* han_u = (u3h_node*) u3h_slot_to_node(*sot_w);

  lef_w -= 5;
  bit_w = (rem_w >> lef_w);
  map_w = han_u->map_w;

  if ( !BIT_SET(map_w, bit_w) ) {
    har_u->arm_u.mug_w = _ch_skip_slot(har_u->arm_u.mug_w, lef_w);
    return c3n;
  }

  rem_w = CUT_END(rem_w, lef_w);
  inx_w = _ch_popcount(CUT_END(map_w, bit_w));
  tos_w = &(han_u->sot_w[inx_w]);

  if ( c3n == _ch_trim_slot(har_u, tos_w, lef_w, rem_w) ) {
    // nothing trimmed
    return c3n;
  }
  else if ( 0 != *tos_w  ) {
    // something trimmed, but slot still has value
    return c3y;
  }
  else {
    // shrink!
    c3_w i_w, len_w = _ch_popcount(map_w);

    if ( 2 == len_w ) {
      // only one left, pick the other
      *sot_w = han_u->sot_w[ 0 == inx_w ? 1 : 0 ];

      u3a_wfree(han_u);
    }
    else {
      // shrink node in place; don't reallocate, we could be low on memory
      //
      han_u->map_w = han_u->map_w & ~(1 << bit_w);

      for ( i_w = inx_w; i_w < (len_w - 1); i_w++ ) {
        han_u->sot_w[i_w] = han_u->sot_w[i_w + 1];
      }
    }
    return c3y;
  }
}

/* _ch_trim_node(): trim one entry from a bucket slot
*/
static c3_o
_ch_trim_buck(u3h_root* har_u, u3h_slot* sot_w)
{
  c3_w i_w, len_w;
  u3h_buck* hab_u = u3h_slot_to_node(*sot_w);

  for ( har_u->arm_u.buc_o = c3y, len_w = hab_u->len_w;
        har_u->arm_u.inx_w < len_w;
        har_u->arm_u.inx_w += 1 )
  {
    u3h_slot* tos_w = &(hab_u->sot_w[har_u->arm_u.inx_w]);
    if ( c3y == _ch_trim_slot(har_u, tos_w, 0, 0) ) {
      if ( 2 == len_w ) {
        // 2 things in bucket: debucketize to key-value pair, the next
        // run will point at this pair (same mug_w, no longer in bucket)
        *sot_w = hab_u->sot_w[ (0 == har_u->arm_u.inx_w) ? 1 : 0 ];
        u3a_wfree(hab_u);
        har_u->arm_u.inx_w = 0;
        har_u->arm_u.buc_o = c3n;
      }
      else {
        // shrink bucket in place; don't reallocate, we could be low on memory
        //
        hab_u->len_w = len_w - 1;

        for ( i_w = har_u->arm_u.inx_w; i_w < (len_w - 1); ++i_w ) {
          hab_u->sot_w[i_w] = hab_u->sot_w[i_w + 1];
        }
      }
      return c3y;
    }
  }

  har_u->arm_u.mug_w = (har_u->arm_u.mug_w + 1) & 0x7FFFFFFF; // modulo 2^31
  har_u->arm_u.inx_w = 0;
  har_u->arm_u.buc_o = c3n;
  return c3n;
}

/* _ch_trim_some(): trim one entry from a bucket or node slot
*/
static c3_o
_ch_trim_some(u3h_root* har_u, u3h_slot* sot_w, c3_w lef_w, c3_w rem_w)
{
  if ( 0 == lef_w ) {
    return _ch_trim_buck(har_u, sot_w);
  }
  else {
    return _ch_trim_node(har_u, sot_w, lef_w, rem_w);
  }
}

/* _ch_skip_slot(): increment arm over hash prefix.
*/
c3_w
_ch_skip_slot(c3_w mug_w, c3_w lef_w)
{
  c3_w hig_w = mug_w >> lef_w;
  c3_w new_w = CUT_END(hig_w + 1, (31 - lef_w)); // modulo 2^(31 - lef_w)
  return new_w << lef_w;
}

/* _ch_trim_slot(): trim one entry from a slot
*/
static c3_o
_ch_trim_slot(u3h_root* har_u, u3h_slot *sot_w, c3_w lef_w, c3_w rem_w)
{
  if ( _(u3h_slot_is_null(*sot_w)) ) {
    har_u->arm_u.mug_w = _ch_skip_slot(har_u->arm_u.mug_w, lef_w);
    return c3n;
  }
  else if ( _(u3h_slot_is_node(*sot_w)) ) {
    return _ch_trim_some(har_u, sot_w, lef_w, rem_w);
  }
  else if ( _(u3h_slot_is_warm(*sot_w)) ) {
    *sot_w = u3h_noun_be_cold(*sot_w);
    if ( c3n == har_u->arm_u.buc_o ) {
      har_u->arm_u.mug_w = (har_u->arm_u.mug_w + 1) & 0x7FFFFFFF; // modulo 2^31
    }
    return c3n;
  }
  else {
    u3_noun kev = u3h_slot_to_noun(*sot_w);
    *sot_w = 0;
    // u3l_log("trim: freeing %x, use count %d\r\n", kev, u3a_use(kev)));
    u3z(kev);

    har_u->arm_u.mug_w = _ch_skip_slot(har_u->arm_u.mug_w, lef_w);
    return c3y;
  }
}

/* _ch_trim_slot(): trim one entry from a hashtable
*/
static c3_o
_ch_trim_root(u3h_root* har_u)
{
  c3_w      mug_w = har_u->arm_u.mug_w;
  c3_w      inx_w = mug_w >> 25; // 6 bits
  c3_w      rem_w = CUT_END(mug_w, 25);
  u3h_slot* sot_w = &(har_u->sot_w[inx_w]);

  return _ch_trim_slot(har_u, sot_w, 25, rem_w);
}

/* u3h_trim_to(): trim to n key-value pairs
*/
void
u3h_trim_to(u3p(u3h_root) har_p, c3_w n_w)
{
  u3h_root* har_u = u3to(u3h_root, har_p);

  while ( har_u->use_w > n_w ) {
    if ( c3y == _ch_trim_root(har_u) ) {
      har_u->use_w -= 1;
    }
    /* TODO: remove
    if ( c3n == har_u->arm_u.buc_o ) {
      // lower 31 bits of increment (next mug)
      har_u->arm_u.mug_w = (har_u->arm_u.mug_w + 1) & 0x7FFFFFFF;
      har_u->arm_u.inx_w = 0;
    }
    */
  }
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
  rem_w = CUT_END(rem_w, lef_w);
  map_w = han_u->map_w;

  if ( !BIT_SET(map_w, bit_w) ) {
    return c3n;
  }
  else {
    c3_w inx_w = _ch_popcount(CUT_END(map_w, bit_w));
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
  c3_w      inx_w = (mug_w >> 25);
  c3_w      rem_w = CUT_END(mug_w, 25);
  c3_w      sot_w = har_u->sot_w[inx_w];

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
  rem_w = CUT_END(rem_w, lef_w);
  map_w = han_u->map_w;

  if ( !BIT_SET(map_w, bit_w) ) {
    return u3_none;
  }
  else {
    c3_w inx_w = _ch_popcount(CUT_END(map_w, bit_w));
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
  c3_w      rem_w = CUT_END(mug_w, 25);
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
    u3k(pro);
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
      return u3t(kev);
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
  rem_w = CUT_END(rem_w, lef_w);
  map_w = han_u->map_w;

  if ( !BIT_SET(map_w, bit_w) ) {
    return u3_none;
  }
  else {
    c3_w inx_w = _ch_popcount(CUT_END(map_w, bit_w));
    c3_w sot_w = han_u->sot_w[inx_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      if ( _(u3r_sung(key, u3h(kev))) ) {
        return u3t(kev);
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
  c3_w      mug_w = u3r_mug(key);
  c3_w      inx_w = (mug_w >> 25);
  c3_w      rem_w = CUT_END(mug_w, 25);
  c3_w      sot_w = har_u->sot_w[inx_w];

  if ( _(u3h_slot_is_null(sot_w)) ) {
    return u3_none;
  }
  else if ( _(u3h_slot_is_noun(sot_w)) ) {
    u3_noun kev = u3h_slot_to_noun(sot_w);

    if ( _(u3r_sung(key, u3h(kev))) ) {
      har_u->sot_w[inx_w] = u3h_noun_be_warm(sot_w);
      return u3k(u3t(kev));
    }
    else {
      return u3_none;
    }
  }
  else {
    u3h_node* han_u = u3h_slot_to_node(sot_w);
    u3_weak     pro = _ch_node_gut(han_u, 25, rem_w, key);

    if ( u3_none == pro ) {
      return u3_none;
    }
    else {
      return u3k(pro);
    }
  }
}

/* _ch_free_buck(): free bucket
*/
static void
_ch_free_buck(u3h_buck* hab_u)
{
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    u3z(u3h_slot_to_noun(hab_u->sot_w[i_w]));
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
      u3z(u3h_slot_to_noun(sot_w));
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
      u3z(u3h_slot_to_noun(sot_w));
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
_ch_walk_buck(u3h_buck* hab_u, void (*fun_f)(u3_noun, void*), void* wit)
{
  c3_w i_w;

  for ( i_w = 0; i_w < hab_u->len_w; i_w++ ) {
    fun_f(u3h_slot_to_noun(hab_u->sot_w[i_w]), wit);
  }
}

/* _ch_walk_node(): walk node for gc.
*/
static void
_ch_walk_node(u3h_node* han_u, c3_w lef_w, void (*fun_f)(u3_noun, void*), void* wit)
{
  c3_w len_w = _ch_popcount(han_u->map_w);
  c3_w i_w;

  lef_w -= 5;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_w sot_w = han_u->sot_w[i_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      fun_f(kev, wit);
    }
    else {
      void* hav_v = u3h_slot_to_node(sot_w);

      if ( 0 == lef_w ) {
        _ch_walk_buck(hav_v, fun_f, wit);
      } else {
        _ch_walk_node(hav_v, lef_w, fun_f, wit);
      }
    }
  }
}

/* u3h_walk_with(): traverse hashtable with key, value fn and data
 *                  argument; RETAINS.
*/
void
u3h_walk_with(u3p(u3h_root) har_p,
              void (*fun_f)(u3_noun, void*),
              void* wit)
{
  u3h_root* har_u = u3to(u3h_root, har_p);
  c3_w        i_w;

  for ( i_w = 0; i_w < 64; i_w++ ) {
    c3_w sot_w = har_u->sot_w[i_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      fun_f(kev, wit);
    }
    else if ( _(u3h_slot_is_node(sot_w)) ) {
      u3h_node* han_u = u3h_slot_to_node(sot_w);

      _ch_walk_node(han_u, 25, fun_f, wit);
    }
  }
}

/* _ch_walk_plain(): use plain u3_noun fun_f for each node
 */
static void
_ch_walk_plain(u3_noun kev, void* wit)
{
  void (*fun_f)(u3_noun) = wit;
  fun_f(kev);
}

/* u3h_walk(): u3h_walk_with, but with no data argument
*/
void
u3h_walk(u3p(u3h_root) har_p, void (*fun_f)(u3_noun))
{
  u3h_walk_with(har_p, _ch_walk_plain, fun_f);
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
