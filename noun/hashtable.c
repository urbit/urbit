/* g/h.c
**
*/
#include "all.h"

static void* _ch_some_add(void* han_v, c3_w, c3_w, u3_noun, c3_w*);
static void* _ch_some_new(c3_w lef_w);
static void _ch_trim_one(u3h_root*);
static c3_o _ch_trim_one_some(u3h_slot*, c3_w);
static c3_o _ch_trim_one_buck(u3h_slot*);
static c3_o _ch_trim_one_node(u3h_slot*, c3_w);
static void _ch_print_node(u3h_node*, c3_w);
static void _ch_walk_node(u3h_node*, c3_w, void (*fun_f)(u3_noun));

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
  fprintf(stderr, "node_new\r\n");
  u3h_node* han_u = u3a_walloc(c3_wiseof(u3h_node));

  han_u->map_w = 0;
  han_u->arm_w = 0;
  return han_u;
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

  //  there is already an entry at the slot with this index
  //
  if ( map_w & (1 << bit_w) ) {
    c3_w sot_w = han_u->sot_w[inx_w];

    //  this slot contains a node, so recurse into that node.
    //
    if ( _(u3h_slot_is_node(sot_w)) ) {
      fprintf(stderr, "nod recurse\r\n");
      void* hav_v = u3h_slot_to_node(sot_w);

      hav_v = _ch_some_add(hav_v, lef_w, rem_w, kev, use_w);
      han_u->sot_w[inx_w] = u3h_node_to_slot(hav_v);

      return han_u;
    }
    //  this slot contains just one key-value pair
    //
    else {
      u3_noun kov = u3h_slot_to_noun(sot_w);

      //  the key is the same, so replace the value
      //
      if ( c3y == u3r_sing(u3h(kev), u3h(kov)) ) {
        fprintf(stderr, "nod replace\r\n");
        u3m_p("nod kev", kev);
        u3m_p("nod kov", kov);
        u3a_lose(kov);
        han_u->sot_w[inx_w] = u3h_noun_to_slot(kev);
        return han_u;
      }
      //  the hash is the same, but the keys are different,
      //  so create a new bucket or node.
      //
      else {
        c3_w  rom_w = u3r_mug(u3h(kov)) & ((1 << lef_w) - 1);
        void* hav_v = _ch_some_new(lef_w);

        //  Optimize: need a custom collision create.
        //
        fprintf(stderr, "nod pre-kev use_w: %d\r\n", *use_w);
        hav_v = _ch_some_add(hav_v, lef_w, rem_w, kev, use_w);
        fprintf(stderr, "nod pre-kov use_w: %d\r\n", *use_w);
        hav_v = _ch_some_add(hav_v, lef_w, rom_w, kov, use_w);
        fprintf(stderr, "nod post-kev use_w: %d\r\n", *use_w);
        (*use_w)--;
        fprintf(stderr, "nod post-dec use_w: %d\r\n", *use_w);

        han_u->sot_w[inx_w] = u3h_node_to_slot(hav_v);
        return han_u;
      }
    }
  }
  else {
    //  nothing was at this slot.
    //  Optimize: use u3a_wealloc.
    //
    c3_w      len_w = _ch_popcount(map_w);
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
    nah_u->arm_w = han_u->arm_w;

    u3a_wfree(han_u);

    /* TODO: remove
    //  make sure we don't immediately trim off the new value.
    //
    if ( nah_u->arm_w == inx_w ) {
      nah_u->arm_w++;
    }
    */

    *use_w += 1;
    fprintf(stderr, "nod post-inc use_w: %d\r\n", *use_w);
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
_ch_some_add(void* han_v, c3_w lef_w, c3_w rem_w, u3_noun kev, c3_w *use_w)
{
  if ( 0 == lef_w ) {
    return _ch_buck_add((u3h_buck*)han_v, kev, use_w);
  }
  else return _ch_node_add((u3h_node*)han_v, lef_w, rem_w, kev, use_w);
}

/* _ch_print(): print hashtable.
*/
static void
_ch_print_table(u3h_root* har_u)
{
  u3h_slot sot_w;
  c3_w i_w;

  for ( i_w = 0; i_w < 64; i_w++ ) {
    sot_w = har_u->sot_w[i_w];
    if ( _(u3h_slot_is_null(sot_w)) ) {
      continue;
    }
    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun elm = u3h_slot_to_noun(har_u->sot_w[i_w]);
      fprintf(stderr, "sot_w[%d]: {%d: %d}\r\n", i_w, u3h(elm), u3t(elm));
    }
    else if ( _(u3h_slot_is_node(sot_w)) ) {
      fprintf(stderr, "sot_w[%d]: <node>\r\n", i_w);
      _ch_print_node(u3h_slot_to_node(sot_w), 25);
      fprintf(stderr, "sot_w[%d]: </node>\r\n", i_w);
    }
  }
}

/* _ch_print_node(): print a node.
*/
static void
_ch_print_node(u3h_node* han_u, c3_w lef_w)
{
  c3_w len_w = _ch_popcount(han_u->map_w);
  c3_w i_w;
  c3_c* pre;

  lef_w -= 5;
  if ( 20 == lef_w ) { pre = "  "; }
  else if ( 15 == lef_w ) { pre = "    "; }
  else if ( 10 == lef_w ) { pre = "      "; }
  else if ( 5 == lef_w ) { pre = "        "; }
  else {
    pre = "          ";
    fprintf(stderr, "%s<bucket/>\r\n", pre);
  }

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_w sot_w = han_u->sot_w[i_w];

    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kev = u3h_slot_to_noun(sot_w);

      fprintf(stderr, "%snode[%d]: {%d: %d}\r\n", pre, i_w, u3h(kev), u3t(kev));
    }
    else if ( _(u3h_slot_is_node(sot_w)) ) {
      _ch_print_node(u3h_slot_to_node(sot_w), lef_w);
    }
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
  c3_w        rem_w = (mug_w & ((1 << 25) - 1));  // TODO: macro
  c3_w        sot_w = har_u->sot_w[inx_w];

  u3m_p("put", kev);
  fprintf(stderr, "inx_w: %d\r\n", inx_w);
  _ch_print_table(har_u);

  //  nothing stored for this 6-bit prefix
  //
  if ( _(u3h_slot_is_null(sot_w)) ) {
    har_u->sot_w[inx_w] = u3h_noun_to_slot(kev);
    har_u->use_w++;

    fprintf(stderr, "post-nul use_w: %d\r\n", har_u->use_w);
    //  make sure we don't immediately trim off the new value.
    //  cycle arm_w around until it points to a non-null slot.
    //  if it landed on a null value, the next value inserted might
    //  be the first one to be evicted, which would break the semantics.
    //  if we're adding the first non-null element, arm_w will end up
    //  pointing at the new element. this is ok.
    //
    /* TODO: remove
    if ( har_u->arm_w == inx_w ) {
      fprintf(stderr, "put arm_w %d -> ", har_u->arm_w);
      do {
        har_u->arm_w = (har_u->arm_w + 1) % 64;
      }
      while ( _(u3h_slot_is_null(har_u->sot_w[har_u->arm_w])) );
      fprintf(stderr, "%d\r\n", har_u->arm_w);
    }
    */
  }
  else {
    u3h_node* han_u;
    c3_w *use_w = &(har_u->use_w);

    //  one key-value pair for this prefix
    //
    if ( _(u3h_slot_is_noun(sot_w)) ) {
      u3_noun kov   = u3h_slot_to_noun(sot_w);
      c3_w    rom_w = u3r_mug(u3h(kov)) & ((1 << 25) - 1);

        fprintf(stderr, "sot_w: %08x kev: %08x kov: %08x\r\n", sot_w, kev, kov);
        u3m_p("put kov", u3h(kov));
        u3m_p("put kov", u3t(kov));
        u3m_p("put kev", u3h(kev));
        u3m_p("put kev", u3t(kev));

      han_u = _ch_node_new();
      fprintf(stderr, "put: pre-kev use_w: %d\r\n", har_u->use_w);
      han_u = _ch_node_add(han_u, 25, rem_w, kev, use_w);
      fprintf(stderr, "put: pre-kov use_w: %d\r\n", har_u->use_w);
      han_u = _ch_node_add(han_u, 25, rom_w, kov, use_w);
      fprintf(stderr, "put: post-kov use_w: %d\r\n", har_u->use_w);
      (*use_w)--;
      fprintf(stderr, "put: post-dec use_w: %d\r\n", har_u->use_w);
    }
    //  more than one key-value pair for this prefix; use a node
    //
    else {
      han_u = _ch_node_add(u3h_slot_to_node(sot_w), 25, rem_w, kev, use_w);
      fprintf(stderr, "put: single use_w: %d\r\n", har_u->use_w);
    }
    har_u->sot_w[inx_w] = u3h_node_to_slot(han_u);
  }

  if ( har_u->clk_w > 0 ) {
    u3h_trim_to(har_p, har_u->clk_w);
  }
  fprintf(stderr, "put: final use_w: %d\r\n", har_u->use_w);

  _ch_print_table(har_u);
}

/* u3h_trim_to(): trim to n key-value pairs
*/
void
u3h_trim_to(u3p(u3h_root) har_p, c3_w n_w)
{
  u3h_root*   har_u = u3to(u3h_root, har_p);
  while ( har_u->use_w > n_w ) {
    _ch_trim_one(har_u);
  }
}

/* _ch_trim_one(): trim off one key-value pair
*/
static void
_ch_trim_one(u3h_root *har_u)
{
  u3h_slot root_sot_w = har_u->sot_w[har_u->arm_w];

  c3_assert(har_u->use_w > 0);

  while ( 1 ) {

    if ( _(u3h_slot_is_null(root_sot_w)) ) {
      //  next
    }
    else if ( _(u3h_slot_is_node(root_sot_w)) ) {
      c3_o trimmed = _ch_trim_one_some(&root_sot_w, 25);
      if ( _(trimmed) ) {
        har_u->use_w--;
        return;
      }
      //  next
    }
    else if ( _(u3h_slot_is_warm(root_sot_w)) ) {
      har_u->sot_w[har_u->arm_w] = u3h_noun_be_cold(har_u->sot_w[har_u->arm_w]);
    }
    else {
      c3_assert(_(u3h_slot_is_noun(root_sot_w)));

      u3_noun kev = u3h_slot_to_noun(root_sot_w);
      u3a_lose(kev);
      har_u->sot_w[har_u->arm_w] = 0;

      har_u->use_w--;
      return;
    }


    do {
      har_u->arm_w = (har_u->arm_w + 1) % 64;
    }
    while ( _(u3h_slot_is_null(har_u->sot_w[har_u->arm_w])) );
    root_sot_w = har_u->sot_w[har_u->arm_w];
  }
}

/* _ch_trim_one_some(): trim one key-value pair from either a node or a bucket.
*/
static c3_o
_ch_trim_one_some(u3h_slot* hal_w, c3_w lef_w)
{
  if ( 0 == lef_w ) {
    return _ch_trim_one_buck(hal_w);
  }
  else {
    return _ch_trim_one_node(hal_w, lef_w);
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
  u3h_node* han_u = (u3h_node*)u3h_slot_to_node(*hal_w);
  u3h_slot sot_w = han_u->sot_w[han_u->arm_w];
  c3_w len_w = _ch_popcount(han_u->map_w);

  c3_assert(len_w != 1);

  while (han_u->arm_w < len_w) {
    sot_w = han_u->sot_w[han_u->arm_w];

    if ( _(u3h_slot_is_node(sot_w)) ) {
      if ( _(_ch_trim_one_some(&sot_w, lef_w - 5)) ) {
        return c3y;
      }
      //  next
    }

    else if ( _(u3h_slot_is_warm(sot_w)) ) {
      han_u->sot_w[han_u->arm_w] = u3h_noun_be_cold(sot_w);
      han_u->arm_w++;
      //  next
    }
    else {
      c3_w bit_w, i_w, inx_w;

      if ( len_w == 2) {
        c3_assert(c3n == u3h_slot_is_null(sot_w));
        u3a_lose(u3h_slot_to_noun(sot_w));

        //  if arm_w is 0, assign han_u->sot_w[1] and vice versa.
        //
        *hal_w = u3h_slot_to_noun(han_u->sot_w[!han_u->arm_w]);
        return c3y;
      }

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
