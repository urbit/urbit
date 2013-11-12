/* include/rail.h
**
** This file is in the public domain.
*/
  /** Configurations.
  **/
#   undef U2_LEAK_DEBUG

#   ifdef U2_LEAK_DEBUG
#     define  u2_leak_on(x) (COD_w = x)
        extern  c3_w COD_w;
#     define  u2_leak_off  (COD_w = 0)
#   endif

  /** Data types.
  **/
    /** Ray types.
    **/
      /* u2_rail: an allocation control frame.
      */
        typedef u2_ray u2_rail;

      /* u2_wire: an execution context, inheriting rail
      */
        typedef u2_ray u2_wire;

    /** Structures - in loom space.
    **/
      /* Base rail.
      */
        typedef struct _u2_loom_rail {
          u2_ray cap_r;   // top of transient region
          u2_ray hat_r;   // top of new durable region
          u2_ray mat_r;   // bottom of transient region
          u2_ray rut_r;   // bottom of new durable region
          c3_m   hip_m;   // memory model in durable; c3__rock, c3__sand
        } u2_loom_rail;

#         define  u2_rail_cap_r(ral_r)  *u2_at(ral_r, u2_loom_rail, cap_r)
#         define  u2_rail_hat_r(ral_r)  *u2_at(ral_r, u2_loom_rail, hat_r)
#         define  u2_rail_mat_r(ral_r)  *u2_at(ral_r, u2_loom_rail, mat_r)
#         define  u2_rail_rut_r(ral_r)  *u2_at(ral_r, u2_loom_rail, rut_r)
#         define  u2_rail_hip_m(ral_r)  *u2_at(ral_r, u2_loom_rail, hip_m)

      /* Pork - base of frame.
      */
        typedef struct {
          u2_ray mut_r;   // parent mat
          u2_ray rit_r;   // parent rut
          c3_m   hap_m;   // parent hip
        } u2_loom_pork;
#         define u2_pork_mut_r(pik_r)  *u2_at(pik_r, u2_loom_pork, mut_r)
#         define u2_pork_rit_r(pik_r)  *u2_at(pik_r, u2_loom_pork, rit_r)
#         define u2_pork_hap_m(pik_r)  *u2_at(pik_r, u2_loom_pork, hap_m)

      /* Floe - a solid rail allocator.  Implied by `hip_m == c3__sand`.
      */
        typedef struct {
        } u2_loom_floe;

      /* Soup - a liquid rail allocator.
      */
#       define u2_soup_free_no 28

        typedef struct {
          u2_ray         fre_r[u2_soup_free_no];    // doubly-linked free lists
          u2_cash_slot   lot_s;                     // modern memo cache
#ifdef U2_PROFILE_MEMORY
          c3_w           liv_w;                     // number of words live
#endif
        } u2_loom_soup;
#         define u2_soup_fre_r(sop_r, x)  *u2_at(sop_r, u2_loom_soup, fre_r[x])
#         define u2_soup_lot_r(sop_r)     u2_aftr(sop_r, u2_loom_soup, lot_s)
#         define u2_soup_liv_w(sop_r)     *u2_at(sop_r, u2_loom_soup, liv_w)

      /* A noun box, for liquid hats.  Behind pointer, addressed fwd.
      **
      ** The box size is also stored at the end of the box in classic
      ** bad ass malloc style.  Hence a box is:
      **
      **    ---
      **    siz_w
      **    use_w
      **      user data
      **    siz_w
      **    ---
      **
      ** Do not attempt to adjust this structure!
      */
        typedef struct _u2_loom_rail_box {
          c3_w   siz_w;     // size of this box
          c3_w   use_w;     // reference count; free if 0
#ifdef U2_LEAK_DEBUG
          c3_w   cod_w;     // allocation code
#endif
        } u2_loom_rail_box;

#         define u2_rail_box_siz(box) *u2_at(box, u2_loom_rail_box, siz_w)
#         define u2_rail_box_use(box) *u2_at(box, u2_loom_rail_box, use_w)
#         define u2_rail_box_cod(box) *u2_at(box, u2_loom_rail_box, cod_w)

      /* A free node.  Addressed from the box.
      */
        typedef struct _u2_loom_rail_hut {
          u2_loom_rail_box b;
          u2_ray pre_r;   // next on free list
          u2_ray nex_r;   // next on free list
        } u2_loom_rail_hut;

#         define u2_rail_hut_siz(hut) *u2_at(hut, u2_loom_rail_hut, b.siz_w)
#         define u2_rail_hut_use(hut) *u2_at(hut, u2_loom_rail_hut, b.use_w)
#         define u2_rail_hut_pre(hut) *u2_at(hut, u2_loom_rail_hut, pre_r)
#         define u2_rail_hut_nex(hut) *u2_at(hut, u2_loom_rail_hut, nex_r)

#         define u2_rail_box(som) \
            ( u2_fly_is_cat(som) \
                ? 0 \
                : (u2_dog_a(som) - c3_wiseof(u2_loom_rail_box)) )

    /** Abbreviations.
    **/
#     define u2_rc(ral_r, a, b)           u2_rl_cell(ral_r, a, b)
#     define u2_rt(ral_r, a, b, c)        u2_rl_trel(ral_r, a, b, c)
#     define u2_rq(ral_r, a, b, c, d)     u2_rl_qual(ral_r, a, b, c, d)
#     define u2_ri(ral_r, a, b, c, d, e)  u2_rl_quil(ral_r, a, b, c, d, e)
#     define u2_ro(ral_r, a)              u2_rl_lone(ral_r, a)
#     define u2_ru(ral_r, a)              u2_rl_unit(ral_r, a)
#     define u2_rx(ral_r, a)              u2_rl_take(ral_r, a)
#     define u2_rz(ral_r, a)              u2_rl_lose(ral_r, a)
#     define u2_rl                        u2_rl_list
#     define u2_rk                        u2_rl_rack


    /** Functions.
    **/
      /** Miscellaneous and interesting.
      **/
        /* u2_rl_boot():
        **
        **   Create an empty rail in an empty loom, with memory model `hip`.
        **   See u2_rl_leap() for storage policies.
        */
          u2_ray
          u2_rl_boot(c3_m hip_m);

        /* u2_rl_clear():
        **
        **   Yes iff [lef] does not point to any word >= [net]
        **   and < [bat].
        */
          u2_bean
          u2_rl_clear(u2_noun lef,
                      u2_ray  net_r,
                      u2_ray  bat_r);

        /* u2_rl_init():
        **
        **   Install an empty rail within `hat_r` and `mat_r` in the loom,
        **   with memory model `hip`.
        **
        **   Returns ray to rail, which always equalls the passed `mat_r`.
        */
          u2_ray
          u2_rl_init(c3_m   hip_m,
                     u2_ray hat_r,
                     u2_ray mat_r);

        /* u2_rl_dump():
        **
        **  Print memory structure for benefit of archeologists.
        */
          void
          u2_rl_dump(u2_ray ral_r);

        /* u2_rl_drain():
        **
        **   Clear the memo cache (soup).
        */
          void
          u2_rl_drain(u2_ray ral_r);

        /* u2_rl_fall():
        **
        **   Reverse the beams backward, restoring the old frame.
        */
          void
          u2_rl_fall(u2_ray ral_r);

        /* u2_rl_fall_part():
        **
        **   Fall on `ral`, also releasing the partition `aux`.
        */
          void
          u2_rl_fall_part(u2_ray ral_r,
                          u2_ray aux_r);

        /* u2_rl_flog():
        **
        **   Release the can, setting cap to mat.
        */
          void
          u2_rl_flog(u2_ray ral_r);

        /* u2_rl_gain():
        **
        **   Gain a reference to `som`, returning it.
        */
          u2_weak                                                 //  transfer
          u2_rl_gain(u2_ray  ral_r,
                     u2_weak som);                                //  retain

        /* u2_rl_free():
        **
        **   Free storage allocated by u2_rl_malloc().
        */
          void
          u2_rl_free(u2_ray ral_r,
                     void*  lag_v);

        /* u2_rl_ok():
        **
        **   Ensure that all reference counts are valid in `som`.
        */
          void
          u2_rl_ok(u2_ray  ral_r,
                   u2_noun som);                                  //  retain
          
        /* u2_rl_junior():
        **
        **   Yes iff `dus` is junior in `ral` - ie, must be copied
        **   to be referenced on the hat.
        */
          u2_bean
          u2_rl_junior(u2_ray  ral_r,
                       u2_noun dus);                              //  retain

        /* u2_rl_leap():
        **
        **   Reverse the beams forward, with memory model `hip`.
        **   Memory models at present:
        **
        **    c3__sand    solid, no boxes or reference counts
        **    c3__rock    liquid, boxes, reference-counted heap
        **
        **   Returns u2_yes on success.
        */
          u2_bean
          u2_rl_leap(u2_ray ral_r,
                     c3_m   hip_m);

        /* u2_rl_leap_part():
        **
        **   Reverse and split rail, inserting partition of size `num/dem`
        **   plus `tip`. 
        **
        **   Returns partition rail, `aux_r`.
        */
          u2_ray
          u2_rl_leap_part(u2_ray ral_r,
                          c3_m   hop_m,
                          c3_w   num_w,
                          c3_w   dem_w,
                          c3_w   tip_w);

        /* u2_rl_lose():
        **
        **   Lose a reference to `som`.  Free it if refcount == 0.
        */
          void
          u2_rl_lose(u2_ray  ral_r,
                     u2_weak som);                                //  transfer

        /* u2_rl_gc_mark_noun():
        **
        **   Mark a noun for gc.  Return allocated words.
        */
          c3_w
          u2_rl_gc_mark_noun(u2_ray  ral_r,
                             u2_noun som);

        /* u2_rl_gc_mark_ptr():
        **
        **   Mark a pointer allocated with ralloc.  Return allocated words.
        */
          c3_w
          u2_rl_gc_mark_ptr(u2_ray ral_r,
                            u2_ray ptr_r);

        /* u2_rl_gc_mark():
        **
        **   Mark a rail (mainly memo cache).  Return allocated words.
        */
          c3_w
          u2_rl_gc_mark(u2_ray ral_r);

        /* u2_rl_gc_sweep(): 
        **
        **   Sweep memory, freeing unused blocks.  Match live, save leaked.
        */
          c3_w
          u2_rl_gc_sweep(u2_ray ral_r, c3_w sav_w);

        /* u2_rl_malloc():
        **
        **   Allocate `sib_w` *bytes* of raw C storage.
        */
          void*
          u2_rl_malloc(u2_ray ral_r,
                       c3_w   sib_w);

        /* u2_rl_malt():
        **
        **   Initialize slab `sal` as an atom, internally measured.
        */
          u2_atom                                                 //  transfer
          u2_rl_malt(u2_rail ral_r,
                     u2_ray  sal_r);

        /* u2_rl_mint():
        **
        **   Initialize slab `sal` as an atom, externally measured.
        */
          u2_atom                                                 //  transfer
          u2_rl_mint(u2_rail ral_r,
                     u2_ray  sal_r,
                     c3_w    len_w);

        /* u2_rl_moot():
        **
        **   Initialize slab `sal` as an atom, originally measured.
        */
          u2_atom                                                 //  transfer
          u2_rl_moot(u2_rail ral_r,
                     u2_ray  sal_r);

        /* u2_rl_open():
        **
        **   Yes iff [a] more words remain in the pad.
        */
          u2_bean
          u2_rl_open(u2_ray ral_r,
                     c3_w   a_w);

        /* u2_rl_ralloc():
        **
        **   Allocate `siz_w` words of raw ray storage.
        */
          u2_ray
          u2_rl_ralloc(u2_ray ral_r,
                       c3_w   siz_w);

        /* u2_rl_rfree():
        **
        **   Free raw ray storage allocated by `u2_rl_ralloc()`.
        */
          void
          u2_rl_rfree(u2_ray ral_r,
                      u2_ray nov_r);

        /* u2_rl_senior():
        **
        **   Yes iff `dus` is senior in `ral` - ie, does not
        **   require reference counting.
        */
          u2_bean
          u2_rl_senior(u2_ray  ral_r,
                       u2_noun dus);                              //  retain

        /* u2_rl_slab():
        **
        **   Create a blank atomic slab of `len` words.
        */
          u2_ray
          u2_rl_slab(u2_rail ral_r,
                     c3_w    len_w);

        /* u2_rl_slaq():
        **
        **   Create a blank atomic slab of `len` bloqs of size `met`.
        */
          u2_ray
          u2_rl_slaq(u2_wire wir_r,
                     c3_g    met_g,
                     c3_w    len_w);

        /* u2_rl_refs():
        **
        **   Return the reference count of (som).  For debugging, etc.
        */
          c3_w
          u2_rl_refs(u2_ray  ral_r,
                     u2_noun som);

        /* u2_rl_copy():
        **
        **   Copy indirect noun `fiz` into main storage, preserving dags.
        **   Must be followed by `rl_wash(fiz)` if `fiz` is to be preserved.
        */
          u2_weak                                                 //  transfer
          u2_rl_copy(u2_ray ral_r,
                     u2_dog fiz);                                 //  retain

        /* u2_rl_take():
        **
        **   Produce `a`, as eligible result.  Copy juniors; reference peers.
        */
          u2_weak                                                 //  transfer
          u2_rl_take(u2_ray  ral_r,
                     u2_weak a);                                  //  retain
#         define u2_rl_ice(ral_r, a) \
            u2_rl_take(ral_r, a)

        /* u2_rl_tamp():
        **
        **   Tamp, eliding the segment from `net` up to `bat`,
        **   preserving the root `lef`.
        **
        **   Assumes u2_rl_clear() with the same arguments.
        */
          u2_noun
          u2_rl_tamp(u2_ray  ral_r,
                     u2_noun lef,
                     u2_ray  net_r,
                     u2_ray  bat_r);

        /* u2_rl_valid():
        **
        **   Validate rail for memory bugs.
        */
          void
          u2_rl_valid(u2_ray ral_r);

        /* u2_rl_water():
        **
        **   Return east and west watermarks, respectively.
        */
          void
          u2_rl_water(u2_ray ral_r,
                      c3_w*  maz_w,
                      c3_w*  buc_w);

      /** Basic noun fabrication.
      **/
        /* u2_rl_bytes():
        **
        **   Copy `a` bytes from `b` to an LSB first atom.
        */
          u2_weak                                                 // transfer
          u2_rl_bytes(u2_ray      ral_r,
                      c3_w        a_w,
                      const c3_y* b_y);

        /* u2_rl_cell(): 
        **
        **   Produce the cell `[a b]`.
        */
          u2_weak                                                 //  transfer
          u2_rl_cell(u2_ray  ral_r,
                     u2_weak a,                                   //  transfer
                     u2_weak b);                                  //  transfer

        /* u2_rl_list():
        **
        **   Produce a null-terminated list, terminating `...` with `u2_none`.
        */
          u2_weak                                                 //  transfer
          u2_rl_list(u2_rail ral_r,
                     ...);                                        //  transfer

        /* u2_rl_lone():
        **
        **   Create the unit `[0 a]`.
        */
#if 0
          u2_weak                                                 //  transfer
          u2_rl_lone(u2_rail ral_r,
                     u2_weak a);                                  //  transfer
#else
#         define u2_rl_lone(ral_r, a) \
            u2_rc(ral_r, a, u2_nul)
#endif

        /* u2_rl_molt():
        **
        **   Mutate `som` with a 0-terminated list of axis, noun pairs.
        **   Axes must be cats (31 bit).
        */
          u2_weak                                                 //  transfer
          u2_rl_molt(u2_rail ral_r,
                     u2_weak som,                                 //  retain
                     ...);                                        //  transfer
       
        /* u2_rl_molv():
        **
        **   As u2_rl_molt(), by argument pointer.
        */
          u2_weak                                                 //  transfer
          u2_rl_molv(u2_rail ral_r,
                     u2_weak som,                                 //  retain
                     va_list vap);                                //  transfer

        /* u2_rl_mp():
        **
        **   Copy the GMP integer [a] into an atom.
        */
          u2_weak                                                 //  transfer
          u2_rl_mp(u2_ray ral_r,
                   mpz_t  a_mp);                                  //  transfer

        /* u2_rl_qual():
        **
        **   Produce the triple `[a b c d]`.
        */
#if 0
          u2_weak                                                 //  transfer
          u2_rl_qual(u2_rail ral_r,
                     u2_weak a,                                   //  transfer
                     u2_weak b,                                   //  transfer
                     u2_weak c,                                   //  transfer
                     u2_weak d);                                  //  transfer
#else
#         define u2_rl_qual(ral_r, a, b, c, d) \
            u2_rc(ral_r, a, u2_rt(ral_r, b, c, d))
#endif

        /* u2_rl_rack():
        **
        **   Produce an n-tuple, terminating `...` with `u2_none`.
        */
          u2_weak                                                 //  transfer
          u2_rl_rack(u2_rail ral_r,
                     ...);                                        //  transfer

        /* u2_rl_string():
        **
        **   Produce an LSB-first atom from the C string `a`.
        */
          u2_weak                                                 //  transfer
          u2_rl_string(u2_ray      ral_r,
                       const c3_c* a_c);

        /* u2_rl_trel():
        **
        **   Create the triple `[a b c]`.
        */
#if 0
          u2_weak                                                 //  transfer
          u2_rl_trel(u2_rail ral_r,
                     u2_weak a,                                   //  transfer
                     u2_weak b,                                   //  transfer
                     u2_weak c);                                  //  transfer
#else
#         define u2_rl_trel(ral_r, a, b, c) \
            u2_rc(ral_r, a, u2_rc(ral_r, b, c))
#endif

        /* u2_rl_unit():
        **
        **   Create the unit `[0 a]`.
        */
#if 0
          u2_weak                                                 //  transfer
          u2_rl_unit(u2_rail ral_r,
                     u2_weak a);                                  //  transfer
#else
#         define u2_rl_unit(ral_r, a) \
            u2_rc(ral_r, u2_nul, a)
#endif

        /* u2_rl_vint():
        **
        **   Create `a + 1`.
        */
          u2_weak                                                 //  transfer
          u2_rl_vint(u2_rail ral_r,
                     u2_weak a);                                  //  transfer

        /* u2_rl_words():
        **
        **   Copy [a] words from [b] into an atom.
        */
          u2_weak                                                 //  transfer
          u2_rl_words(u2_ray      ral_r,
                      c3_w        a_w,
                      const c3_w* b_w);

      /** Caching.
      **/
        /* u2_rl_find():
        **
        **   Cache search for function (0 means nock) and sample.
        */
          u2_weak                                                 //  transfer
          u2_rl_find(u2_ray  ral_r,
                     u2_mote fun_m,
                     u2_noun sam);                                //  retain

        /* u2_rl_save():
        **
        **   Cache store for function (0 means nock), sample and product.
        */
          u2_weak                                                 //  transfer
          u2_rl_save(u2_ray  ral_r,
                     u2_mote fun_m,                               //  retain
                     u2_noun sam,                                 //  retain
                     u2_noun pro);                                //  transfer

        /* u2_rl_uniq():
        **
        **   Use cache to render object unique.
        */
          u2_noun                                                 //  produce
          u2_rl_uniq(u2_ray  ral_r,
                     u2_noun som);                                //  submit

        /* u2_rl_find_cell(): as u2_rl_find(), for `sam=[a b]`.
        ** u2_rl_find_trel(): as u2_rl_find(), for `sam=[a b c]`.
        ** u2_rl_find_qual(): as u2_rl_find(), for `sam=[a b d]`.
        ** u2_rl_find_quil(): as u2_rl_find(), for `sam=[a b c d e]`.
        **
        ** Extend as needed...
        */
          u2_weak                                                 //  transfer
          u2_rl_find_cell(u2_ray, u2_mote, u2_noun,               //  retain 
                                           u2_noun);              //  retain
          u2_weak                                                 //  transfer
          u2_rl_find_trel(u2_ray, u2_mote, u2_noun,               //  retain 
                                           u2_noun,               //  retain
                                           u2_noun);              //  retain
          u2_weak                                                 //  transfer
          u2_rl_find_qual(u2_ray, u2_mote, u2_noun,               //  retain 
                                           u2_noun,               //  retain
                                           u2_noun,               //  retain
                                           u2_noun);              //  retain
          u2_weak                                                 //  transfer
          u2_rl_find_quil(u2_ray, u2_mote, u2_noun,               //  retain 
                                           u2_noun,               //  retain
                                           u2_noun,               //  retain
                                           u2_noun,               //  retain
                                           u2_noun);              //  retain

        /* u2_rl_save_cell(): as u2_rl_save(), for `sam=[a b]`.
        ** u2_rl_save_trel(): as u2_rl_save(), for `sam=[a b c]`.
        ** u2_rl_save_qual(): as u2_rl_save(), for `sam=[a b c d]`.
        ** u2_rl_save_quil(): as u2_rl_save(), for `sam=[a b c d e]`.
        **
        ** Extended
        */
          u2_weak                                                 //  transfer
          u2_rl_save_cell(u2_ray, u2_mote, u2_noun,               //  retain 
                                           u2_noun,               //  retain
                                           u2_noun);              //  transfer

          u2_weak                                                 //  transfer
          u2_rl_save_trel(u2_ray, u2_mote, u2_noun,               //  retain 
                                           u2_noun,               //  retain
                                           u2_noun,               //  retain
                                           u2_noun);              //  transfer

          u2_weak                                                 //  transfer
          u2_rl_save_qual(u2_ray, u2_mote, u2_noun,               //  retain 
                                           u2_noun,               //  retain
                                           u2_noun,               //  retain
                                           u2_noun,               //  retain
                                           u2_noun);              //  transfer

          u2_weak                                                 //  transfer
          u2_rl_save_quil(u2_ray, u2_mote, u2_noun,               //  retain 
                                           u2_noun,               //  retain
                                           u2_noun,               //  retain
                                           u2_noun,               //  retain
                                           u2_noun,               //  retain
                                           u2_noun);              //  transfer
