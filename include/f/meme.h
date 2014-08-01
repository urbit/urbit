/* include/meme.h
**
** This file is in the public domain.
*/
  /** Tuning and configuration.
  **/
#   define u2_me_free_no  28

#   undef U2_MEMORY_DEBUG
#   ifdef U2_MEMORY_DEBUG
#     define  u2_leak_on(x) (COD_w = x)
        extern  c3_w COD_w;
#     define  u2_leak_off  (COD_w = 0)
#   endif


  /** Data structures.
  **/
    /* u2_me_box: classic allocation box.
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
      typedef struct _u2_me_box {
        c3_w   siz_w;                       // size of this box
        c3_w   use_w;                       // reference count; free if 0
#       ifdef U2_LEAK_DEBUG
          c3_w   cod_w;                     // tracing code
#       endif
      } u2_me_box;

#     define u2_me_boxed(len_w)  (len_w + c3_wiseof(u2_me_box) + 1)

    /* u2_me_free: free node in heap.  Sets minimum node size.
    **
    ** XXO: pre_u and nex_u should live in box.
    */
      typedef struct _u2_me_free {
        u2_me_box           box_u;
        struct _u2_me_free* pre_u;
        struct _u2_me_free* nex_u;
      } u2_me_free;

#     define u2_me_minimum   (c3_wiseof(u2_me_free))

    /* u2_me_road: contiguous allocation and execution context.
    **
    **  The road can point in either direction.  If cap > hat, it
    **  looks like this ("north"):
    **
    **  0           rut   hat                                    ffff
    **  |            |     |                                       |
    **  |~~~~~~~~~~~~-------##########################+++++++$~~~~~|
    **  |                                             |      |     |
    **  0                                            cap    mat  ffff
    **
    **  Otherwise, it looks like this ("south"):
    ** 
    **  0           mat   cap                                    ffff
    **  |            |     |                                       |
    **  |~~~~~~~~~~~~$++++++##########################--------~~~~~|
    **  |                                             |      |     |
    **  0                                            hat    rut  ffff
    **
    **  Legend: - is durable storage (heap); + is temporary storage
    **  (stack); ~ is deep storage (immutable); $ is the allocation block;
    **  # is free memory.
    **
    **  Pointer restrictions: pointers stored in + can point anywhere; 
    **  pointers in - can only point to - or ~; pointers in ~ can only
    **  point to ~.
    **
    **  To "leap" is to create a new inner road in the ### free space.
    **  but in the reverse direction, so that when the inner road
    **  "falls" (terminates), its durable storage is left on the
    **  temporary storage of the outer road.
    **
    **  In all cases, the pointer in a u2_noun is a word offset into
    **  u2H, the top-level road.
    */
      typedef struct _u2_me_road {
        struct _u2_me_road* par_u;          //  parent road

        struct {                            //  layout information
          c3_w* cap_w;                      //  top of transient region
          c3_w* hat_w;                      //  top of durable region
          c3_w* mat_w;                      //  bottom of transient region
          c3_w* rut_w;                      //  bottom of durable region
        } lay;

        struct {                            //  allocation pools
          u2_me_free* fre_u[u2_me_free_no]; //  heap by node size log
#         ifdef U2_MEMORY_DEBUG
            c3_w liv_w;                     //  number of live words
#         endif
        } all;

        struct {                            //  jet dashboard
          u2_noun dax;                      //  (map ,* (list chub))
        } jed;

        struct {                            //  namespace
          u2_noun fly;                      //  $+(* (unit))
        } ski;

        struct {                            //  trace stack
          u2_noun tax;                      //  (list ,*)
        } bug;

        struct {                            //  profiling stack
          u2_noun don;                      //  (list ,*)
        } pro;

        struct {                            //  memoization
          u2_noun sav;                      //  (map (pair term noun) noun)
        } cax;
      } u2_me_road;
      typedef u2_me_road u2_road;


  /** Globals.
  **/
    /* u2_Loom: base of loom, as a word pointer.
    */
      c3_global c3_w* u2_Loom;
#       define u2L  u2_Loom;

    /* u2_Home / u2H: root of thread.  Always north.
    */
      c3_global u2_road* u2_Home;
#       define u2H  u2_Home

    /* u2_Road / u2R: current road (thread-local).
    */
      c3_global u2_road* u2_Road;
#       define u2R  u2_Road

  /**  Macros.
  **/
#     define  u2_me_is_north  ((u2R->cap > u2R->hat) ? u2_yes : u2_no)
#     define  u2_me_is_south  (!u2_me_is_north)

#     define  u2_me_into(x) (((c3_w*)(void*)u2H) + (x))
#     define  u2_me_outa(p) (((c3_w*)(void*)(p)) - (c3w*)(void*)u2H)


  /** Functions.
  **/
      /* u2_me_boot(): make u2R and u2H from `len` words at `mem`.
      */
        void
        u2_me_boot(void* mem_v, c3_w len_w);

      /* u2_me_grab(): garbage-collect memory.  Assumes u2R == u2H.
      */
        void
        u2_me_grab(void);

      /* u2_me_check(): checkpoint memory to file.
      **
      ** Assumes u2R == u2H.
      */
        void
        u2_me_check(void);

      /* u2_me_fall(): return to parent road.
      */
        void
        u2_me_fall(void):

      /* u2_me_leap(): advance to inner road.
      */
        void
        u2_me_leap(void);

      /* u2_me_flog(): release inner-allocated storage.
      **
      ** The proper sequence for inner allocation is:
      **
      **    u2_me_leap();
      **    //  allocate some inner stuff...
      **    u2_me_fall();
      **    //  inner stuff is still valid, but on cap
      */
        void
        u2_me_flog(void);

        
    /**  Allocation.
    **/
      /* Basic allocation.
      */
        /* u2_me_walloc(): allocate storage measured in words.
        */
          void*
          u2_me_walloc(c3_w len_w);

        /* u2_me_malloc(): allocate storage measured in bytes.
        */
          void*
          u2_me_malloc(c3_w len_w);

        /* u2_me_free(): free storage.
        */
          void
          u2_me_free(void* lag_v);


      /* Reference and arena control.
      */
        /* u2_me_open(): u2_yes iff a free block is available.
        */
          u2_bean
          u2_me_open(c3_w len_w);

        /* u2_me_gain(): gain and/or copy juniors.
        */
          u2_weak
          u2_me_gain(u2_weak som);

        /* u2_me_lose(): lose a reference.
        */
          void
          u2_me_lose(u2_weak som);

        /* u2_me_junior(): yes iff reference cannot be saved.
        */
          u2_bean
          u2_me_junior(u2_noun som);

        /* u2_me_senior(): yes iff references need not be counted.
        */
          u2_bean
          u2_me_senior(u2_noun som);

        /* u2_me_refs(): reference count.
        */
          c3_w
          u2_me_refs(u2_noun som);


      /* Atoms from proto-atoms.
      */
        /* u2_me_slab(): create a length-bounded proto-atom.
        */
          c3_w*
          u2_me_slab(c3_w len_w);

        /* u2_me_slaq(): u2_me_slaq() with a defined blocksize.
        */
          c3_w*
          u2_me_slaq(c3_g met_g, c3_w len_w);

        /* u2_me_malt(): measure and finish a proto-atom.
        */
          u2_noun
          u2_me_malt(c3_w* sal_w);

        /* u2_me_moot(): finish a pre-measured proto-atom; dangerous.
        */
          u2_noun
          u2_me_moot(c3_w* sal_w);

        /* u2_me_mint(): finish a measured proto-atom.
        */
          u2_noun
          u2_me_mint(c3_w* sal_w, c3_w len_w);

      /* Garbage collection (for debugging only).
      */
        /* u2_me_mark(): mark for gc, returning allocated words.
        */
          c3_w
          u2_me_mark(u2_noun som);

        /* u2_me_sweep(): sweep after gc, freeing, matching live count.
        */
          c3_w
          u2_me_sweep(c3_w liv_w);
  

    /**  Generic computation.
    **/
      /* u2_me_nock_on(): produce .*(bus fol).
      */
        u2_noun
        u2_me_nock_on(u2_noun bus, u2_noun fol);

      /* u2_me_slam_on(): produce (gat sam).
      */
        u2_noun
        u2_me_slam_on(u2_noun gat, u2_noun sam);

      /* u2_me_nock_un(): produce .*(bus fol), as ++toon.
      */
        u2_noun
        u2_me_nock_un(u2_noun bus, u2_noun fol);

      /* u2_me_slam_un(): produce (gat sam), as ++toon.
      */
        u2_noun
        u2_me_slam_un(u2_noun gat, u2_noun sam);

      /* u2_me_nock_in(): produce .*(bus fol), as ++toon, in namespace.
      */
        u2_noun
        u2_me_nock_in(u2_noun fly, u2_noun bus, u2_noun fol);

      /* u2_me_slam_in(): produce (gat sam), as ++toon, in namespace.
      */
        u2_noun
        u2_me_slam_in(u2_noun fly, u2_noun gat, u2_noun sam);


    /**  Memoization.
    ***
    ***  The memo cache is keyed by an arbitrary symbolic function
    ***  and a noun argument to that (logical) function.  Functions
    ***  are predefined by C-level callers, but 0 means nock.
    ***
    ***  The memo cache is within its road and dies when it falls.
    **/
      /* u2_me_find*(): find in memo cache.
      */
        u2_weak u2_me_find(u2_mote, u2_noun);
        u2_weak u2_me_find_2(u2_mote, u2_noun, u2_noun);
        u2_weak u2_me_find_3(u2_mote, u2_noun, u2_noun, u2_noun);
        u2_weak u2_me_find_4(u2_mote, u2_noun, u2_noun, u2_noun, u2_noun);

      /* u2_me_save*(): save in memo cache.
      */
        u2_weak u2_me_save(u2_mote, u2_noun, u2_noun);
        u2_weak u2_me_save_2(u2_mote, u2_noun, u2_noun, u2_noun);
        u2_weak u2_me_save_3(u2_mote, u2_noun, u2_noun, u2_noun, u2_noun);
        u2_weak u2_me_save_4
                  (u2_mote, u2_noun, u2_noun, u2_noun, u2_noun, u2_noun);

      /* u2_me_uniq(): uniquify with memo cache.
      */
        u2_weak 
        u2_me_uniq(u2_noun som);

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

        /* u2_rl_malloc():
        **
        **   Allocate `sib_w` *bytes* of raw C storage.
        */
          void*
          u2_rl_malloc(u2_ray ral_r,
                       c3_w   sib_w);

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
