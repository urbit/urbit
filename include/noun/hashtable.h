/* include/n/h.h
**
** This file is in the public domain.
*/
  /**  Data structures.
  **/
    /**  Straightforward implementation of the classic Bagwell
    ***  HAMT (hash array mapped trie), using a mug hash.
    ***
    ***  Because a mug is 31 bits, the root table has 64 slots.
    ***  The 31 bits of a mug are divided into the first lookup,
    ***  which is 6 bits (corresponding to the 64 entries in the
    ***  root table), followed by 5 more branchings of 5 bits each,
    ***  corresponding to the 32-slot nodes for everything under
    ***  the root node.
    ***
    ***  We store an extra "freshly warm" bit for a simple
    ***  clock-algorithm reclamation policy, not yet implemented.
    ***  Search "clock algorithm" to figure it out.
    **/
      /* u3h_slot: map slot.
      **
      **   Either a key-value cell or a loom offset, decoded as a pointer
      **   to a u3h_node, or a u3h_buck at the bottom.  Matches the u3_noun
      **   format - coordinate with allocate.h.  The top two bits are:
      **
      **     00 - empty (in the root table only)
      **     01 - table
      **     02 - entry, stale
      **     03 - entry, fresh
      */
        typedef c3_w u3h_slot;

      /* u3h_node: map node.
      */
        typedef struct {
          c3_w     map_w;
          u3h_slot sot_w[0];
        } u3h_node;

      /* u3h_root: hash root table
      */
        typedef struct {
          c3_w     max_w;     // number of cache lines (0 for no trimming)
          c3_w     use_w;     // number of lines currently filled
          struct {
            c3_w  mug_w;      // current hash
            c3_w  inx_w;      // index into current hash bucket iff buc_o
            c3_o  buc_o;      // yes if in middle of hash bucket
          } arm_u;            // clock arm
          u3h_slot sot_w[64]; // slots
        } u3h_root;

      /* u3h_buck: bottom bucket.
      */
        typedef struct {
          c3_w    len_w;
          u3h_slot sot_w[0];
        } u3h_buck;

    /**  HAMT macros.
    ***
    ***  Coordinate with u3_noun definition!
    **/
      /* u3h_slot_is_null(): yes iff slot is empty
      ** u3h_slot_is_noun(): yes iff slot contains a key/value cell
      ** u3h_slot_is_node(): yes iff slot contains a subtable/bucket
      ** u3h_slot_is_warm(): yes iff fresh bit is set
      ** u3h_slot_to_node(): slot to node pointer
      ** u3h_node_to_slot(): node pointer to slot
      ** u3h_slot_to_noun(): slot to cell
      ** u3h_noun_to_slot(): cell to slot
      ** u3h_noun_be_warm(): warm mutant
      ** u3h_noun_be_cold(): cold mutant
      */
#     define  u3h_slot_is_null(sot)  ((0 == ((sot) >> 30)) ? c3y : c3n)
#     define  u3h_slot_is_node(sot)  ((1 == ((sot) >> 30)) ? c3y : c3n)
#     define  u3h_slot_is_noun(sot)  ((1 == ((sot) >> 31)) ? c3y : c3n)
#     define  u3h_slot_is_warm(sot)  (((sot) & 0x40000000) ? c3y : c3n)
#     define  u3h_slot_to_node(sot)  (u3a_into((sot) & 0x3fffffff))
#     define  u3h_node_to_slot(ptr)  (u3a_outa(ptr) | 0x40000000)
#     define  u3h_noun_be_warm(sot)  ((sot) | 0x40000000)
#     define  u3h_noun_be_cold(sot)  ((sot) & ~0x40000000)
#     define  u3h_slot_to_noun(sot)  (0x40000000 | (sot))
#     define  u3h_noun_to_slot(som)  (u3h_noun_be_warm(som))

    /**  Functions.
    ***
    ***  Needs: delete and merge functions; clock reclamation function.
    **/
      /* u3h_new_cache(): create hashtable with bounded size.
      */
        u3p(u3h_root)
        u3h_new_cache(c3_w clk_w);

      /* u3h_new(): create hashtable.
      */
        u3p(u3h_root)
        u3h_new(void);

      /* u3h_put(): insert in hashtable.
      **
      ** `key` is RETAINED; `val` is transferred.
      */
        void
        u3h_put(u3p(u3h_root) har_p, u3_noun key, u3_noun val);

      /* u3h_get(): read from hashtable.
      **
      ** `key` is RETAINED; result is PRODUCED.
      */
        u3_weak
        u3h_get(u3p(u3h_root) har_p, u3_noun key);

      /* u3h_git(): read from hashtable, retaining result.
      **
      ** `key` is RETAINED; result is RETAINED.
      */
        u3_weak
        u3h_git(u3p(u3h_root) har_p, u3_noun key);

      /* u3h_gut(): read from hashtable, unifying key nouns.
      **
      ** `key` is RETAINED.
      */
        u3_weak
        u3h_gut(u3p(u3h_root) har_p, u3_noun key);

      /* u3h_trim_to(): trim to n key-value pairs
      */
        void
        u3h_trim_to(u3p(u3h_root) har_p, c3_w n_w);

      /* u3h_free(): free hashtable.
      */
        void
        u3h_free(u3p(u3h_root) har_p);

      /* u3h_mark(): mark hashtable for gc.
      */
        c3_w
        u3h_mark(u3p(u3h_root) har_p);

      /* u3h_walk_with(): traverse hashtable with key, value fn and data
       *                  argument; RETAINS.
      */
        void
        u3h_walk_with(u3p(u3h_root) har_p,
                      void (*fun_f)(u3_noun, void*),
                      void* wit);

      /* u3h_walk(): u3h_walk_with, but with no data argument
      */
        void
        u3h_walk(u3p(u3h_root) har_p, void (*fun_f)(u3_noun));
