/* include/cash.h
**
** This file is in the public domain.
*/
  /** Hash-table design:
  ***
  ***   The cash system is a 16-way hash tree designed to
  ***   scale smoothly, remaining small for small usage
  ***   and fast for big usage.  It is also salted for use
  ***   of multiple associations in the same table.
  ***
  ***   A logical key is the combination of an opaque
  ***   function salt, any number < (1 << 31), and a
  ***   sample list.  Matches are in every case within
  ***   the salt.
  ***
  ***   All the nouns in the sample list are equal, but
  ***   duplicate.  Comparing duplicates is expensive, so
  ***   the hash-table stores every duplicate it finds.
  ***
  ***   The search key is the mug of the salt, XORed
  ***   with the mug of the sample.  [XX - This sacrifices
  ***   efficiency to internal convenience and should
  ***   be replaced with direct salt.]
  ***
  ***   A single slot stores a single entry.  If a new entry
  ***   is added, the slot expands to a 16-way table.  If the
  ***   table is lightly filled, slots are collision-mapped.
  ***   If it is heavily filled, slots are radix-mapped.
  ***
  ***   Full key collisions, which are inevitable and not at
  ***   all improbable, revert to linear search.  16 collisions
  ***   on the same 31-bit key will produce storage failure.
  ***
  ***   Future revisions should add a reclamation mode based on
  ***   the "clock algorithm" (a variant on LRU).  The clock
  ***   rotates around search-key space.  Entries are reclaimed
  ***   if they are clocked out and either key or value has a
  ***   reference count of 1.
  **/

  /** Tunable constants.
  **/
    /* A 16-way subtable with remaining keyspace switches up to laminar
    ** mode when it exceeds `cash_hi` recursively counted entries, and
    ** reverts to collision mode when it falls back below `cash_lo`.
    */
#     define u2_tune_cash_lo  6
#     define u2_tune_cash_hi  10

  /** Data types.
  **/
    /* Slot: a discriminated union.
    */
#       define u2_slot_is_a(lot_r)  (0 == (*u2_at_ray(lot_r) >> 31))
#       define u2_slot_is_b(lot_r)  (3 == (*u2_at_ray(lot_r) >> 30))
#       define u2_slot_is_c(lot_r)  (2 == (*u2_at_ray(lot_r) >> 30))

    /* u2_cash_slot_a: cache slot, containing exactly one entry.
    */
      typedef struct {
        /*  sel_m: opaque, unique function identity (0 for nock)
        **  sap:   sample list
        **  pro:   product
        */
        c3_m    sel_m;
        u2_noun sap;
        u2_noun pro;
      } u2_cash_slot_a;

      typedef u2_cash_slot_a u2_cash_slot;

#       define u2_slot_a_sel(lot_r)  *u2_at(lot_r, u2_cash_slot_a, sel_m)
#       define u2_slot_a_sap(lot_r)  *u2_at(lot_r, u2_cash_slot_a, sap)
#       define u2_slot_a_pro(lot_r)  *u2_at(lot_r, u2_cash_slot_a, pro)

#       define u2_slot_gunk_coll (7 << 29)
#       define u2_slot_gunk_radx (6 << 29)

#       define u2_slot_gunk_is_coll(gun_w) (!!((gun_w) & (1 << 29)))
#       define u2_slot_gunk_is_radx(gun_w) (!((gun_w) & (1 << 29)))

    /* u2_cash_slot_b: cache slot, with 16-way table - if not empty.
    */
      typedef struct {
        /*  gun_w: gunk word - (1 << 31) at present
        **  rag_w: recursive entry count
        **  sid_r: ray to 16-way slot array, or 0 if rag_w is 0
        */
        c3_w   gun_w;
        c3_w   rag_w;
        u2_ray sid_r;
      } u2_cash_slot_b;

#       define u2_slot_b_gun(lot_r)  *u2_at(lot_r, u2_cash_slot_b, gun_w)
#       define u2_slot_b_rag(lot_r)  *u2_at(lot_r, u2_cash_slot_b, rag_w)
#       define u2_slot_b_sid(lot_r)  *u2_at(lot_r, u2_cash_slot_b, sid_r)
#       define u2_slot_b_sid_i(lot_r, i_w) \
          (u2_slot_b_sid(lot_r) + ((i_w) * c3_wiseof(u2_cash_slot_a)))

    /* u2_cash_slot_c: cache slot, empty
    */
      typedef struct {
        /*  emt_w: (1 << 31)
        */
        c3_w emt_w;
        c3_w xxx_w;         //  unused
        c3_w xxy_w;         //  unused
      } u2_cash_slot_c;

#       define u2_slot_emty          (1 << 31)
#       define u2_slot_c_emt(lot_r)  *u2_at(lot_r, u2_cash_slot_c, emt_w)

  /** Functions.
  **/
    /* u2_cs_free():
    **
    **   Release an old hashtable.
    */
      void
      u2_cs_free(u2_ray  ral_r,
                 u2_ray  lot_r);                                    //  submit

    /* u2_cs_mark():
    **
    **   Mark traverse of slot.  Return allocated words.
    */
      c3_w
      u2_cs_mark(u2_ray ral_r,
                 u2_ray lot_r);

    /* u2_cs_init():
    **
    **  Initialize slot to empty.
    */
      void
      u2_cs_init(u2_ray lot_r);

    /* u2_cs_lose():
    **
    **   Release all resources in and under slot (but not slot itself).
    */
      void
      u2_cs_lose(u2_ray ral_r,
                 u2_ray lot_r);                                     //  submit

    /* u2_cs_make():
    **
    **  Create a new hashtable.
    */
      u2_ray                                                        //  produce
      u2_cs_make(u2_ray  ral_r);

    /* u2_cs_find():
    **
    **   Find `sam` for `sel`, or return `u2_none`.
    */
      u2_weak                                                     //  discover
      u2_cs_find(u2_ray  ral_r,
                 u2_ray  lot_r,
                 c3_m    sel_m,
                 u2_noun sam);                                    //  retain

    /* u2_cs_find_cell():
    **
    **   Find `[a b]` for `sel`, or return `u2_none`.
    */
      u2_weak                                                     //  retain
      u2_cs_find_cell(u2_ray  ral_r,
                      u2_ray  lot_r,
                      c3_m    sel_m,
                      u2_noun a,                                  //  retain
                      u2_noun b);                                 //  retain

    /* u2_cs_find_mixt():
    **
    **   Find `[a b]` for `sel`, or return `u2_none`.
    */
      u2_weak                                                     //  retain
      u2_cs_find_mixt(u2_ray      ral_r,
                      u2_ray      lot_r,
                      c3_m        sel_m,
                      const c3_c* a_c,                            //  retain
                      u2_noun     b);                             //  retain

    /* u2_cs_find_trel():
    **
    **   Find `[a b c]` for `sel`, or return `u2_none`.
    */
      u2_weak                                                     //  retain
      u2_cs_find_trel(u2_ray  ral_r,
                      u2_ray  lot_r,
                      c3_m    sel_m,
                      u2_noun a,                                  //  retain
                      u2_noun b,                                  //  retain
                      u2_noun c);                                 //  retain

    /* u2_cs_find_qual():
    **
    **   Find `[a b c d]` for `sel`, or return `u2_none`.
    */
      u2_weak                                                     //  retain
      u2_cs_find_qual(u2_ray  ral_r,
                      u2_ray  lot_r,
                      c3_m    sel_m,
                      u2_noun a,                                  //  retain
                      u2_noun b,                                  //  retain
                      u2_noun c,                                  //  retain
                      u2_noun d);                                 //  retain

    /* u2_cs_save():
    **
    **   Save `sam` as `pro` for `sel`.  Replace existing `pro`, if any.
    */
      u2_noun                                                     //  produce
      u2_cs_save(u2_ray  ral_r,
                 u2_ray  lot_r,
                 c3_m    sel_m,
                 u2_noun sam,                                     //  retain
                 u2_noun pro);                                    //  submit

    /* u2_cs_save_cell():
    **
    **   Save `[a b]` as `pro` for `sel`.
    */
      u2_noun                                                     //  transfer
      u2_cs_save_cell(u2_ray  ral_r,
                      u2_ray  lot_r,
                      c3_m    sel_m,
                      u2_noun a,                                  //  retain
                      u2_noun b,                                  //  retain
                      u2_noun pro);                               //  transfer

    /* u2_cs_save_mixt():
    **
    **   Save `[a b]` as `pro` for `sel`.
    */
      u2_noun                                                     //  transfer
      u2_cs_save_mixt(u2_ray      ral_r,
                      u2_ray      lot_r,
                      c3_m        sel_m,
                      const c3_c* a_c,                            //  retain
                      u2_noun     b,                              //  retain
                      u2_noun     pro);                           //  transfer

    /* u2_cs_save_trel():
    **
    **   Save `[a b c]` as `pro` for `sel`.
    */
      u2_noun                                                     //  transfer
      u2_cs_save_trel(u2_ray  ral_r,
                      u2_ray  lot_r,
                      c3_m    sel_m,
                      u2_noun a,                                  //  retain
                      u2_noun b,                                  //  retain
                      u2_noun c,                                  //  retain
                      u2_noun pro);                               //  transfer

    /* u2_cs_save_qual():
    **
    **   Save `[a b c d]` as `pro` for `sel`.
    */
      u2_noun                                                     //  transfer
      u2_cs_save_qual(u2_ray  ral_r,
                      u2_ray  lot_r,
                      c3_m    sel_m,
                      u2_noun a,                                  //  retain
                      u2_noun b,                                  //  retain
                      u2_noun c,                                  //  retain
                      u2_noun d,                                  //  retain
                      u2_noun pro);                               //  transfer
