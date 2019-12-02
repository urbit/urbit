/* i/n/a.h
**
** This file is in the public domain.
*/
  /**  Constants.
  **/
    /* u3a_bits: number of bits in word-addressed pointer.  29 == 2GB.
    */
#     define u3a_bits  U3_OS_LoomBits

    /* u3a_page: number of bits in word-addressed page.  12 == 16Kbyte page.
    */
#     define u3a_page   12

    /* u3a_pages: number of pages in memory.
    */
#     define u3a_pages  (1 << (u3a_bits - u3a_page))

    /* u3a_words: number of words in memory.
    */
#     define u3a_words  (1 << u3a_bits)

    /* u3a_bytes: number of bytes in memory.
    */
#     define u3a_bytes  (c3_w)((1 << (2 + u3a_bits)))

    /* u3a_minimum: minimum number of words in a box.
    **
    **  wiseof(u3a_cell) + wiseof(u3a_box) + 1 (trailing siz_w)
    */
#ifdef U3_MEMORY_DEBUG
#     define u3a_minimum   8
#else
#     define u3a_minimum   6
#endif

    /* u3a_fbox_no: number of free lists per size.
    */
#     define u3a_fbox_no   27


  /**  Structures.
  **/
    /* u3a_atom, u3a_cell: logical atom and cell structures.
    */
      typedef struct {
        c3_w mug_w;
      } u3a_noun;

      typedef struct {
        c3_w mug_w;
        c3_w len_w;
        c3_w buf_w[0];
      } u3a_atom;

      typedef struct {
        c3_w    mug_w;
        u3_noun hed;
        u3_noun tel;
      } u3a_cell;

    /* u3a_box: classic allocation box.
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
      typedef struct _u3a_box {
        c3_w   siz_w;                       // size of this box
        c3_w   use_w;                       // reference count; free if 0
#       ifdef U3_MEMORY_DEBUG
          c3_w   eus_w;                     // recomputed refcount
          c3_w   cod_w;                     // tracing code
#       endif
      } u3a_box;

    /* u3a_fbox: free node in heap.  Sets minimum node size.
    */
      typedef struct _u3a_fbox {
        u3a_box               box_u;
        u3p(struct _u3a_fbox) pre_p;
        u3p(struct _u3a_fbox) nex_p;
      } u3a_fbox;

    /* u3a_jets: jet dashboard
    */
      typedef struct _u3a_jets {
        u3p(u3h_root) hot_p;                  //  hot state (home road only)
        u3p(u3h_root) war_p;                  //  warm state
        u3p(u3h_root) cod_p;                  //  cold state
        u3p(u3h_root) han_p;                  //  hank cache
        u3p(u3h_root) bas_p;                  //  battery hashes
      } u3a_jets;

    /* u3a_road: contiguous allocation and execution context.
    */
      typedef struct _u3a_road {
        u3p(struct _u3a_road) par_p;          //  parent road
        u3p(struct _u3a_road) kid_p;          //  child road list
        u3p(struct _u3a_road) nex_p;          //  sibling road

        u3p(c3_w) cap_p;                      //  top of transient region
        u3p(c3_w) hat_p;                      //  top of durable region
        u3p(c3_w) mat_p;                      //  bottom of transient region
        u3p(c3_w) rut_p;                      //  bottom of durable region
        u3p(c3_w) ear_p;                      //  original cap if kid is live

        c3_w fut_w[32];                       //  futureproof buffer

        struct {                              //  escape buffer
          union {
            jmp_buf buf;
            c3_w buf_w[256];                  //  futureproofing
          };
        } esc;

        struct {                              //  miscellaneous config
          c3_w fag_w;                         //  flag bits
        } how;                                //

        struct {                              //  allocation pools
          u3p(u3a_fbox) fre_p[u3a_fbox_no];   //  heap by node size log
          u3p(u3a_fbox) cel_p;                //  custom cell allocator
          c3_w fre_w;                         //  number of free words
          c3_w max_w;                         //  maximum allocated
        } all;

        u3a_jets jed;                         //  jet dashboard

        struct {                              // bytecode state
          u3p(u3h_root) har_p;                // formula->post of bytecode
        } byc;

        struct {                              //  namespace
          u3_noun gul;                        //  (list $+(* (unit (unit)))) now
        } ski;

        struct {                              //  trace stack
          u3_noun tax;                        //  (list ,*)
          u3_noun mer;                        //  emergency buffer to release
        } bug;

        struct {                              //  profile stack
          c3_d    nox_d;                      //  nock steps
          c3_d    cel_d;                      //  cell allocations
          u3_noun don;                        //  (list batt)
          u3_noun trace;                      //  (list trace)
          u3_noun day;                        //  doss, only in u3H (moveme)
        } pro;

        struct {                              //  memoization
          u3p(u3h_root) har_p;                //  (map (pair term noun) noun)
        } cax;
      } u3a_road;
      typedef u3a_road u3_road;

    /* u3a_flag: flags for how.fag_w.  All arena related.
    */
      enum u3a_flag {
        u3a_flag_sand  = 0x1,                 //  bump allocation (XX not impl)
      };


  /**  Macros.  Should be better commented.
  **/
    /* In and out of the box.
    */
#     define u3a_boxed(len_w)  (len_w + c3_wiseof(u3a_box) + 1)
#     define u3a_boxto(box_v)  ( (void *) \
                                   ( ((c3_w *)(void*)(box_v)) + \
                                     c3_wiseof(u3a_box) ) )
#     define u3a_botox(tox_v)  ( (struct _u3a_box *) \
                                   (void *) \
                                   ( ((c3_w *)(void*)(tox_v)) - \
                                      c3_wiseof(u3a_box)  ) )
    /* Inside a noun.
    */
#     define u3a_is_cat(som)    (((som) >> 31) ? c3n : c3y)
#     define u3a_is_dog(som)    (((som) >> 31) ? c3y : c3n)

#     define u3a_is_pug(som)    ((2 == ((som) >> 30)) ? c3y : c3n)
#     define u3a_is_pom(som)    ((3 == ((som) >> 30)) ? c3y : c3n)
#     define u3a_to_off(som)    ((som) & 0x3fffffff)
#     define u3a_to_ptr(som)    (u3a_into(u3a_to_off(som)))
#     define u3a_to_wtr(som)    ((c3_w *)u3a_to_ptr(som))
#     define u3a_to_pug(off)    (off | 0x80000000)
#     define u3a_to_pom(off)    (off | 0xc0000000)

#     define u3a_is_atom(som)    c3o(u3a_is_cat(som), \
                                         u3a_is_pug(som))
#     define u3a_is_cell(som)    u3a_is_pom(som)

#     define u3a_h(som) \
        ( _(u3a_is_cell(som)) \
           ? ( ((u3a_cell *)u3a_to_ptr(som))->hed )\
           : u3m_bail(c3__exit) )

#     define u3a_t(som) \
        ( _(u3a_is_cell(som)) \
           ? ( ((u3a_cell *)u3a_to_ptr(som))->tel )\
           : u3m_bail(c3__exit) )

#     define  u3a_into(x) ((void *)(u3_Loom + (x)))
#     define  u3a_outa(p) (((c3_w*)(void*)(p)) - u3_Loom)

#     define  u3a_is_north(r)  __(r->cap_p > r->hat_p)
#     define  u3a_is_south(r)  !u3a_is_north(r)

    /* u3a_open(): words of contiguous free space in [r]
    */
#     define  u3a_open(r)  ( (c3y == u3a_is_north(r)) \
                             ? (c3_w)(r->cap_p - r->hat_p) \
                             : (c3_w)(r->hat_p - r->cap_p) )

    /* u3a_full(): words of [r];
    ** u3a_full(r) == u3a_heap(r) + u3a_temp(r) + u3a_open(r)
    */
#     define  u3a_full(r)  ( (c3y == u3a_is_north(r)) \
                             ? (c3_w)(r->mat_p - r->rut_p) \
                             : (c3_w)(r->rut_p - r->mat_p) )

    /* u3a_heap(): words of heap in [r]
    */
#     define  u3a_heap(r)  ( (c3y == u3a_is_north(r)) \
                             ? (c3_w)(r->hat_p - r->rut_p) \
                             : (c3_w)(r->rut_p - r->hat_p) )

    /* u3a_temp(): words of stack in [r]
    */
#     define  u3a_temp(r)  ( (c3y == u3a_is_north(r)) \
                             ? (c3_w)(r->mat_p - r->cap_p) \
                             : (c3_w)(r->cap_p - r->mat_p) )

#     define  u3a_north_is_senior(r, dog) \
                __((u3a_to_off(dog) < r->rut_p) ||  \
                       (u3a_to_off(dog) >= r->mat_p))

#     define  u3a_north_is_junior(r, dog) \
                __((u3a_to_off(dog) >= r->cap_p) && \
                       (u3a_to_off(dog) < r->mat_p))

#     define  u3a_north_is_normal(r, dog) \
                c3a(!(u3a_north_is_senior(r, dog)),  \
                       !(u3a_north_is_junior(r, dog)))

#     define  u3a_south_is_senior(r, dog) \
                __((u3a_to_off(dog) < r->mat_p) || \
                       (u3a_to_off(dog) >= r->rut_p))

#     define  u3a_south_is_junior(r, dog) \
                __((u3a_to_off(dog) < r->cap_p) && \
                       (u3a_to_off(dog) >= r->mat_p))

#     define  u3a_south_is_normal(r, dog) \
                c3a(!(u3a_south_is_senior(r, dog)),  \
                       !(u3a_south_is_junior(r, dog)))

#     define  u3a_is_junior(r, som) \
                ( _(u3a_is_cat(som)) \
                      ?  c3n \
                      :  _(u3a_is_north(r)) \
                         ?  u3a_north_is_junior(r, som) \
                         :  u3a_south_is_junior(r, som) )

#     define  u3a_is_senior(r, som) \
                ( _(u3a_is_cat(som)) \
                      ?  c3y \
                      :  _(u3a_is_north(r)) \
                         ?  u3a_north_is_senior(r, som) \
                         :  u3a_south_is_senior(r, som) )

#     define  u3a_is_mutable(r, som) \
                ( _(u3a_is_atom(som)) \
                  ? c3n \
                  : _(u3a_is_senior(r, som)) \
                  ? c3n \
                  : _(u3a_is_junior(r, som)) \
                  ? c3n \
                  : (u3a_botox(u3a_to_ptr(som))->use_w == 1) \
                  ? c3y : c3n )

  /**  Globals.
  **/
    /* u3_Road / u3R: current road (thread-local).
    */
      c3_global u3_road* u3a_Road;
#       define u3R  u3a_Road

    /* u3_Code: memory code.
    */
#ifdef U3_MEMORY_DEBUG
      c3_global c3_w u3_Code;
#endif

#   define u3_Loom      ((c3_w *)(void *)U3_OS_LoomBase)

  /**  Functions.
  **/
    /**  Allocation.
    **/
      /* Word-aligned allocation.
      */
        /* u3a_walloc(): allocate storage measured in words.
        */
          void*
          u3a_walloc(c3_w len_w);

        /* u3a_celloc(): allocate a cell.  Faster, sometimes.
        */
          c3_w*
          u3a_celloc(void);

        /* u3a_wfree(): free storage.
        */
          void
          u3a_wfree(void* lag_v);

        /* u3a_wealloc(): word realloc.
        */
          void*
          u3a_wealloc(void* lag_v, c3_w len_w);

        /* u3a_push(): allocate space on the road stack
        */
          void*
          u3a_push(c3_w len_w);

        /* u3a_pop(): deallocate space on the road stack
        */
          void
          u3a_pop(c3_w len_w);

        /* u3a_peek(): examine the top of the road stack
        */
          void*
          u3a_peek(c3_w len_w);


      /* C-style aligned allocation - *not* compatible with above.
      */
        /* u3a_malloc(): aligned storage measured in bytes.
        */
          void*
          u3a_malloc(size_t len_i);

        /* u3a_calloc(): aligned storage measured in bytes.
        */
          void*
          u3a_calloc(size_t num_i, size_t len_i);

        /* u3a_realloc(): aligned realloc in bytes.
        */
          void*
          u3a_realloc(void* lag_v, size_t len_i);

        /* u3a_realloc2(): gmp-shaped realloc.
        */
          void*
          u3a_realloc2(void* lag_v, size_t old_i, size_t new_i);

        /* u3a_free(): free for aligned malloc.
        */
          void
          u3a_free(void* tox_v);

        /* u3a_free2(): gmp-shaped free.
        */
          void
          u3a_free2(void* tox_v, size_t siz_i);

      /* Reference and arena control.
      */
        /* u3a_gain(): gain a reference count in normal space.
        */
          u3_weak
          u3a_gain(u3_weak som);

        /* u3a_take(): gain, copying juniors.
        */
          u3_noun
          u3a_take(u3_noun som);

        /* u3a_left(): true of junior if preserved.
        */
          c3_o
          u3a_left(u3_noun som);

        /* u3a_lose(): lose a reference.
        */
          void
          u3a_lose(u3_weak som);

        /* u3a_wash(): wash all lazy mugs in subtree.  RETAIN.
        */
          void
          u3a_wash(u3_noun som);

        /* u3a_use(): reference count.
        */
          c3_w
          u3a_use(u3_noun som);

        /* u3a_wed(): unify noun references.
        */
          void
          u3a_wed(u3_noun* a, u3_noun* b);

        /* u3a_luse(): check refcount sanity.
        */
          void
          u3a_luse(u3_noun som);

        /* u3a_mark_ptr(): mark a pointer for gc.  Produce size.
        */
          c3_w
          u3a_mark_ptr(void* ptr_v);

        /* u3a_mark_mptr(): mark a u3_malloc-allocated ptr for gc.
        */
          c3_w
          u3a_mark_mptr(void* ptr_v);

        /* u3a_mark_noun(): mark a noun for gc.  Produce size.
        */
          c3_w
          u3a_mark_noun(u3_noun som);

        /* u3a_mark_road(): mark ad-hoc persistent road structures.
        */
          c3_w
          u3a_mark_road(FILE* fil_u);

        /* u3a_idle(): measure free-lists in [rod_u]
        */
          c3_w
          u3a_idle(u3a_road* rod_u);

        /* u3a_sweep(): sweep a fully marked road.
        */
          c3_w
          u3a_sweep(void);

        /* u3a_sane(): check allocator sanity.
        */
          void
          u3a_sane(void);

        /* u3a_lush(): leak push.
        */
          c3_w
          u3a_lush(c3_w lab_w);

        /* u3a_lop(): leak pop.
        */
          void
          u3a_lop(c3_w lab_w);

        /* u3a_print_memory(): print memory amount.
        */
          void
          u3a_print_memory(FILE* fil_u, c3_c* cap_c, c3_w wor_w);

        /* u3a_maid(): maybe print memory.
        */
          c3_w
          u3a_maid(FILE* fil_u, c3_c* cap_c, c3_w wor_w);

        /* u3a_deadbeef(): write 0xdeadbeef from hat to cap.
        */
          void
          u3a_deadbeef(void);

      /* Atoms from proto-atoms.
      */
        /* u3a_slab(): create a length-bounded proto-atom.
        */
          c3_w*
          u3a_slab(c3_w len_w);

        /* u3a_slaq(): u3a_slab() with a defined blocksize.
        */
          c3_w*
          u3a_slaq(c3_g met_g, c3_w len_w);

        /* u3a_malt(): measure and finish a proto-atom.
        */
          u3_noun
          u3a_malt(c3_w* sal_w);

        /* u3a_moot(): finish a pre-measured proto-atom; dangerous.
        */
          u3_noun
          u3a_moot(c3_w* sal_w);

        /* u3a_mint(): finish a measured proto-atom.
        */
          u3_noun
          u3a_mint(c3_w* sal_w, c3_w len_w);

        /* u3a_walk_fore(): preorder traversal, visits ever limb of a noun.
        **
        **   cells are visited *before* their heads and tails
        **   and can shortcircuit traversal by returning [c3n]
        */
          void
          u3a_walk_fore(u3_noun    a,
                        void*      ptr_v,
                        void     (*pat_f)(u3_atom, void*),
                        c3_o     (*cel_f)(u3_noun, void*));

        /* u3a_walk_fore_unsafe(): u3a_walk_fore(), without overflow checks
        */
          void
          u3a_walk_fore_unsafe(u3_noun    a,
                               void*      ptr_v,
                               void     (*pat_f)(u3_atom, void*),
                               c3_o     (*cel_f)(u3_noun, void*));
