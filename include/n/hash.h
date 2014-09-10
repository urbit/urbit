/* include/n/hash.h
**
** This file is in the public domain.
*/
  /**  Straightforward implementation of the classic Bagwell 
  ***  HAMT (hash array mapped trie), using a mug hash.
  ***
  ***  Because a mug is 31 bits, the root table is 64 wide.
  ***  Thereupon 5 bits each are warm for each layer.  The
  ***  final leaf is simply a linear search.
  ***
  ***  We store an extra "freshly warm" bit for a simple
  ***  clock-algorithm reclamation policy, not yet implemented.
  ***  Search "clock algorithm" to figure it out.
  **/
    /* u3_ch_slot: map slot. 
    **
    **   Either a key-value cell or a loom offset, decoded as a pointer 
    **   to a u3_ch_node.  Matches the u3_noun format - coordinate with
    **   meme.h.  The top two bits are:
    **
    **     00 - empty (in the root table only)
    **     01 - table
    **     02 - entry, stale
    **     03 - entry, fresh
    */
      typedef c3_w u3_ch_slot;

    /* u3_ch_node: map node.
    */
      typedef struct {
        c3_w       map_w;
        u3_ch_slot sot_w[0];
      } u3_ch_node;

    /* u3_ch_root: hash root table, with future-proof clock.
    */
      typedef struct {
        c3_w       clk_w;
        u3_ch_slot sot_w[64];
      } u3_ch_root;

    /* u3_ch_buck: bottom bucket.
    */
      typedef struct {
        c3_w    len_w;
        u3_noun kev[0];
      } u3_ch_buck;

  /**  HAMT macros.
  ***
  ***  Coordinate with u3_noun definition!
  **/
    /* u3_ch_slot_is_null(): yes iff slot is empty
    ** u3_ch_slot_is_noun(): yes iff slot contains a key/value cell
    ** u3_ch_slot_is_node(): yes iff slot contains a subtable/bucket
    ** u3_ch_slot_is_warm(): yes iff fresh bit is set
    ** u3_ch_slot_to_node(): slot to node pointer
    ** u3_ch_node_to_slot(): node pointer to slot
    ** u3_ch_slot_to_noun(): slot to cell
    ** u3_ch_noun_to_slot(): cell to slot
    */
#     define  u3_ch_slot_is_null(sot)  ((0 == ((sot) >> 30)) ? u3_yes : u3_no)
#     define  u3_ch_slot_is_node(sot)  ((1 == ((sot) >> 30)) ? u3_yes : u3_no)
#     define  u3_ch_slot_is_noun(sot)  ((1 == ((sot) >> 31)) ? u3_yes : u3_no)
#     define  u3_ch_slot_is_warm(sot)  (((sot) & 0x40000000) ? u3_yes : u3_no)
#     define  u3_ch_slot_to_node(sot)  (u3_co_into((sot) & 0x3fffffff))
#     define  u3_ch_node_to_slot(ptr)  (u3_co_outa(ptr) | 0x40000000)
#     define  u3_ch_slot_to_noun(sot)  (0x40000000 | (sot))
#     define  u3_ch_noun_to_slot(som)  (som)
#     define  u3_ch_noun_be_warm(sot)  ((sot) | 0x40000000)
#     define  u3_ch_noun_be_cold(sot)  ((sot) & ~0x40000000)

