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

    /* u3a_cells: number of representable cells.
    */
#     define u3a_cells  (c3_w)(u3a_words / u3a_minimum)

    /* u3a_maximum: maximum loom object size (largest possible atom).
    */
#     define u3a_maximum   \
        (c3_w)(u3a_words - (c3_wiseof(u3a_box) + c3_wiseof(u3a_atom)))

    /* u3a_minimum: minimum loom object size (actual size of a cell).
    */
#     define u3a_minimum   (c3_w)(1 + c3_wiseof(u3a_box) + c3_wiseof(u3a_cell))

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

    /* u3a_pile: stack control, abstracted over road direction.
    */
      typedef struct _u3a_pile {
        c3_ws    mov_ws;
        c3_ws    off_ws;
        u3_post   top_p;
#ifdef U3_MEMORY_DEBUG
        u3a_road* rod_u;
#endif
      } u3a_pile;

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

  /**  inline functions.
  **/
    /**  road stack.
    **/
        /* u3a_drop(): drop a road stack frame per [pil_u].
        */
          inline void
          u3a_drop(const u3a_pile* pil_u)
          {
            u3R->cap_p -= pil_u->mov_ws;
          }

        /* u3a_peek(): examine the top of the road stack.
        */
          inline void*
          u3a_peek(const u3a_pile* pil_u)
          {
            return u3to(void, (u3R->cap_p + pil_u->off_ws));
          }

        /* u3a_pop(): drop a road stack frame, peek at the new top.
        */
          inline void*
          u3a_pop(const u3a_pile* pil_u)
          {
            u3a_drop(pil_u);
            return u3a_peek(pil_u);
          }

        /* u3a_push(): push a frame onto the road stack, per [pil_u].
        */
          inline void*
          u3a_push(const u3a_pile* pil_u)
          {
            u3R->cap_p += pil_u->mov_ws;
            return u3a_peek(pil_u);
          }

        //  we have to forward-declare u3m_bail() here, as our
        //  headers don't have the necessary guards.
        //
          c3_i
          u3m_bail(c3_m how_m) __attribute__((noreturn));

        /* u3a_pile_sane(): bail on invalid road stack state.
        */
          inline void
          u3a_pile_sane(const u3a_pile* pil_u)
          {
            //  !off means we're on a north road
            //
            if ( !pil_u->off_ws ) {
              if( !(u3R->cap_p > u3R->hat_p) ) {
                u3m_bail(c3__meme);
              }
#ifdef U3_MEMORY_DEBUG
              c3_assert( pil_u->top_p >= u3R->cap_p );
#endif
            }
            else {
              if( !(u3R->cap_p < u3R->hat_p) ) {
                u3m_bail(c3__meme);
              }
#ifdef U3_MEMORY_DEBUG
              c3_assert( pil_u->top_p <= u3R->cap_p );
#endif
            }

#ifdef U3_MEMORY_DEBUG
            c3_assert( pil_u->rod_u == u3R );
#endif
          }

        /* u3a_pile_done(): assert valid upon completion.
        */
          inline c3_o
          u3a_pile_done(const u3a_pile* pil_u)
          {
            return (pil_u->top_p == u3R->cap_p) ? c3y : c3n;
          }

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

        /* u3a_wtrim(): trim storage.
        */
          void
          u3a_wtrim(void* tox_v, c3_w old_w, c3_w len_w);

        /* u3a_wealloc(): word realloc.
        */
          void*
          u3a_wealloc(void* lag_v, c3_w len_w);

        /* u3a_pile_prep(): initialize stack control.
        */
          void
          u3a_pile_prep(u3a_pile* pil_u, c3_w len_w);

      /* C-style aligned allocation - *not* compatible with above.
      */
        /* u3a_malloc(): aligned storage measured in bytes.
        */
          void*
          u3a_malloc(size_t len_i);

        /* u3a_malloc_ssl(): openssl-shaped malloc
        */
          void*
          u3a_malloc_ssl(size_t len_i
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
                          , const char* file, int line
#endif
                          );

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

        /* u3a_realloc_ssl(): openssl-shaped realloc.
        */
          void*
          u3a_realloc_ssl(void* lag_v, size_t len_i
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
                          , const char* file, int line
#endif
                          );

        /* u3a_free(): free for aligned malloc.
        */
          void
          u3a_free(void* tox_v);

        /* u3a_free2(): gmp-shaped free.
        */
          void
          u3a_free2(void* tox_v, size_t siz_i);

        /* u3a_free_ssl(): openssl-shaped free.
        */
          void
          u3a_free_ssl(void* tox_v
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
                          , const char* file, int line
#endif
                          );

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

        /* u3a_reclaim(): clear ad-hoc persistent caches to reclaim memory.
        */
          void
          u3a_reclaim(void);

        /* u3a_rewrite_compact(): rewrite pointers in ad-hoc persistent road structures.
        */
          void
          u3a_rewrite_compact(void);

        /* u3a_rewrite_ptr(): mark a pointer as already having been rewritten
        */
          c3_o
          u3a_rewrite_ptr(void* ptr_v);

        /* u3a_rewrite_noun(): rewrite a noun for compaction.
        */
          void
          u3a_rewrite_noun(u3_noun som);

        /* u3a_rewritten(): rewrite a pointer for compaction.
        */
          u3_post
          u3a_rewritten(u3_post som_p);

        /* u3a_rewritten(): rewritten noun pointer for compaction.
        */
          u3_noun
          u3a_rewritten_noun(u3_noun som);

        /* u3a_count_noun(): count size of noun.
        */
          c3_w
          u3a_count_noun(u3_noun som);

        /* u3a_discount_noun(): clean up after counting a noun.
        */
          c3_w
          u3a_discount_noun(u3_noun som);

        /* u3a_count_ptr(): count a pointer for gc.  Produce size.  */
          c3_w
          u3a_count_ptr(void* ptr_v);

        /* u3a_discount_ptr(): discount a pointer for gc.  Produce size.  */
          c3_w
          u3a_discount_ptr(void* ptr_v);

        /* u3a_idle(): measure free-lists in [rod_u]
        */
          c3_w
          u3a_idle(u3a_road* rod_u);

        /* u3a_sweep(): sweep a fully marked road.
        */
          c3_w
          u3a_sweep(void);

        /* u3a_pack_seek(): sweep the heap, modifying boxes to record new addresses.
        */
          void
          u3a_pack_seek(u3a_road* rod_u);

        /* u3a_pack_move(): sweep the heap, moving boxes to new addresses.
        */
          void
          u3a_pack_move(u3a_road* rod_u);

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

        /* u3a_string(): `a` as an on-loom c-string.
        */
          c3_c*
          u3a_string(u3_atom a);

        /* u3a_stat(): print loom stats
        */
          void
          u3a_stat(void);
