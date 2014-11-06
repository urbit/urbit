/* include/g/a.h
**
** This file is in the public domain.
*/
  /** Tunables.
  **/
#   undef U3_MEMORY_DEBUG
#   ifdef U3_MEMORY_DEBUG
#     define  u3a_leak_on(x) (u3_Code = x)
#     define  u3a_leak_off  (u3_Code = 0)
#   endif

#   define u3a_bits   U3_OS_LoomBits                    // 28, max 29
#   define u3a_page   12                                // 16Kbyte pages
#   define u3a_pages  (1 << (u3a_bits - u3a_page))  // 2^16 pages
#   define u3a_words  (1 << u3a_bits)
#   define u3a_bytes  (c3_w)((1 << (2 + u3a_bits)))


  /** Data structures.
  **/
  /**  Typedefs.
  **/
    /* u3_atom, u3_cell: logical atom and cell structures.
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
#     define u3a_de_twin(dog, dog_w)  ((dog & 0xc0000000) | u3a_outa(dog_w))

#     define u3a_h(som) \
        ( _(u3a_is_cell(som)) \
           ? ( ((u3a_cell *)u3a_to_ptr(som))->hed )\
           : u3m_bail(c3__exit) )

#     define u3a_t(som) \
        ( _(u3a_is_cell(som)) \
           ? ( ((u3a_cell *)u3a_to_ptr(som))->tel )\
           : u3m_bail(c3__exit) )

    /* u3a_box: classic allocation box.
    **
    ** The box size is also stored at the end of the box in classic
    ** bad ass malloc style.  Hence a box is:
    **
    **    ---
    **    siz_w
    **    use_w
    **    if(debug) cod_w
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

#     define u3a_boxed(len_w)  (len_w + c3_wiseof(u3a_box) + 1)
#     define u3a_boxto(box_v)  ( (void *) \
                                   ( ((c3_w *)(void*)(box_v)) + \
                                     c3_wiseof(u3a_box) ) )
#     define u3a_botox(tox_v)  ( (struct _u3a_box *) \
                                   (void *) \
                                   ( ((c3_w *)(void*)(tox_v)) - \
                                      c3_wiseof(u3a_box)  ) )

    /* u3a_fbox: free node in heap.  Sets minimum node size.
    **
    */
      typedef struct _u3a_fbox {
        u3a_box               box_u;
        u3p(struct _u3a_fbox) pre_p;
        u3p(struct _u3a_fbox) nex_p;
      } u3a_fbox;

#     define u3_cc_minimum   6
#     define u3_cc_fbox_no   28


    /* u3a_road: contiguous allocation and execution context.
    **
    **  A road is a normal heap-stack system, except that the heap
    **  and stack can point in either direction.  Therefore, inside
    **  a road, we can nest another road in the opposite direction.
    **
    **  When the opposite road completes, its heap is left on top of
    **  the opposite heap's stack.  It's no more than the normal 
    **  behavior of a stack machine for all subcomputations to push
    **  their results, internally durable, on the stack.
    **
    **  The performance tradeoff of "leaping" - reversing directions 
    **  in the road - is that if the outer computation wants to
    **  preserve the results of the inner one, not just use them for
    **  temporary purposes, it has to copy them.  
    **
    **  This is a trivial cost in some cases, a prohibitive cost in 
    **  others.  The upside, of course, is that all garbage accrued
    **  in the inner computation is discarded at zero cost.
    **
    **  The goal of the road system is the ability to *layer* memory
    **  models.  If you are allocating on a road, you have no idea
    **  how deep within a nested road system you are - in other words,
    **  you have no idea exactly how durable your result may be.
    **  But free space is never fragmented within a road.
    **
    **  Roads do not reduce the generality or performance of a memory
    **  system, since even the most complex GC system can be nested
    **  within a road at no particular loss of performance - a road
    **  is just a block of memory.  The cost of road allocation is,
    **  at least in theory, the branch prediction hits when we try to
    **  decide which side of the road we're allocating on.  The road
    **  system imposes no pointer read or write barriers, of course.
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
    **  Pointer restrictions: pointers stored in + can point anywhere,
    **  except to more central pointers in +.  (Ie, all pointers from
    **  stack to stack must point downward on the stack.)  Pointers in
    **  - can only point to - or ~; pointers in ~ only point to ~.
    **
    **  To "leap" is to create a new inner road in the ### free space.
    **  but in the reverse direction, so that when the inner road
    **  "falls" (terminates), its durable storage is left on the
    **  temporary storage of the outer road.
    **
    **  In all cases, the pointer in a u3_noun is a word offset into
    **  u3H, the top-level road.
    */
      typedef struct _u3a_road {
        struct _u3a_road* par_u;              //  parent road

        struct _u3a_road* kid_u;              //  child road list
        struct _u3a_road* nex_u;              //  sibling road
        struct _u3a_road* now_u;              //  current road pointer

        u3p(c3_w) cap_p;                        //  top of transient region
        u3p(c3_w) hat_p;                        //  top of durable region
        u3p(c3_w) mat_p;                        //  bottom of transient region
        u3p(c3_w) rut_p;                        //  bottom of durable region
        u3p(c3_w) ear_p;                        //  original cap if kid is live

        c3_w fut_w[32];                         //  futureproof buffer

        struct {                                //  escape buffer
          union {
            jmp_buf buf;
            c3_w buf_w[256];                    //  futureproofing
          };
        } esc;

        struct {                                //  miscellaneous config
          c3_w fag_w;                           //  flag bits
        } how;                                  //

        struct {                                //  allocation pools
          u3p(u3a_fbox) fre_p[u3_cc_fbox_no]; //  heap by node size log
          c3_w fre_w;                           //  number of free words
        } all;

        struct {                                //  jet dashboard
          u3p(u3h_root) har_p;                //  jet index (old style)
          u3_noun         das;                  //  dashboard (new style)
        } jed;

        struct {                                //  namespace
          u3_noun flu;                          //  (list $+(* (unit))), inward
        } ski;

        struct {                                //  trace stack
          u3_noun tax;                          //  (list ,*)
          u3_noun mer;                          //  emergency buffer to release
        } bug;

        struct {                                //  profile stack
          c3_d    nox_d;                        //  nock steps
          u3_noun don;                          //  ++path
          u3_noun day;                          //  profile data, ++doss
        } pro;

        struct {                                //  memoization
          u3p(u3h_root) har_p;                //  (map (pair term noun) noun)
        } cax;
      } u3a_road;
      typedef u3a_road u3_road;


  /**  Flags.
  **/
      enum u3a_flag {
        u3a_flag_debug = 0x1,                 //  debug memory
        u3a_flag_gc    = 0x2,                 //  garbage collect once
        u3a_flag_sand  = 0x4,                 //  sand mode, bump allocation
        u3a_flag_die   = 0x8                  //  process was asked to exit
      };


  /**  Macros.
  **/
#     define  u3a_into(x) ((void *)(u3_Loom + (x)))
#     define  u3a_outa(p) (((c3_w*)(void*)(p)) - u3_Loom)

#     define  u3to(type, x) ((type *) u3a_into(x))
#     define  u3of(type, x) (u3a_outa((type *)x))

#     define  u3a_is_north(r)  __(r->cap_p > r->hat_p)
#     define  u3a_is_south(r)  !u3a_is_north(r)

#     define  u3a_open(r)      ( (c3y == u3a_is_north(r)) \
                                  ? (c3_w)(r->cap_p - r->hat_p) \
                                  : (c3_w)(r->hat_p - r->cap_p) )

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

    /* Word axis macros.  For 31-bit axes only.
    */
      /* u3_ax_dep(): number of axis bits.
      */
#       define u3_ax_dep(a_w)   (c3_bits_word(a_w) - 1)

      /* u3_ax_cap(): root axis, 2 or 3.
      */
#       define u3_ax_cap(a_w)   (0x2 | (a_w >> (u3_ax_dep(a_w) - 1)))

      /* u3_ax_mas(): remainder after cap.
      */
#       define u3_ax_mas(a_w) \
          ( (a_w & ~(1 << u3_ax_dep(a_w))) | (1 << (u3_ax_dep(a_w) - 1)) )

      /* u3_ax_peg(): connect two axes.
      */
#       define u3_ax_peg(a_w, b_w) \
          ( (a_w << u3_ax_dep(b_w)) | (b_w &~ (1 << u3_ax_dep(b_w))) )

    /* Conventional axes for gate call.
    */
#     define u3v_pay      3       //  payload
#     define u3v_sam      6       //  sample
#       define u3v_sam_1  6
#       define u3v_sam_2  12
#       define u3v_sam_3  13
#       define u3v_sam_4  24
#       define u3v_sam_5  25
#       define u3v_sam_6  26
#       define u3v_sam_12 52
#       define u3v_sam_13 53
#       define u3v_sam_7  27
#     define u3v_con      7       //  context
#     define u3v_con_2    14      //  context
#     define u3v_con_3    15      //  context
#     define u3v_con_sam  30      //  sample in gate context
#     define u3v_noc      2       //  deprecated
#     define u3v_bat      2       //  battery


  /**  Globals.
  **/
    /* u3_Road / u3R: current road (thread-local).
    */
      c3_global u3_road* u3_Road;
#       define u3R  u3_Road

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
      /* Basic allocation.
      */
        /* u3a_walloc(): allocate storage measured in words.
        */
          void*
          u3a_walloc(c3_w len_w);

        /* u3a_malloc(): allocate storage measured in bytes.
        */
          void*
          u3a_malloc(c3_w len_w);

        /* u3a_free(): free storage.
        */
          void
          u3a_free(void* lag_v);

        /* u3a_wealloc(): word realloc.
        */
          void*
          u3a_wealloc(void* lag_v, c3_w len_w);

        /* u3a_realloc(): byte realloc.
        */
          void*
          u3a_realloc(void* lag_v, c3_w len_w);


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

        /* u3a_mark_ptr(): mark a pointer for gc.  Produce size.
        */
          c3_w
          u3a_mark_ptr(void* ptr_v);

        /* u3a_mark_noun(): mark a noun for gc.  Produce size.
        */
          c3_w
          u3a_mark_noun(u3_noun som);

        /* u3a_sweep(): sweep a fully marked road.
        */
          void
          u3a_sweep(c3_c* cap_c);

        /* u3a_sane(): check allocator sanity.
        */
          void
          u3a_sane(void);

        /* u3a_detect(): axis (som) is referenced from (fum). 
        **
        ** (som) and (fum) are both RETAINED.
        */
          c3_d
          u3a_detect(u3_noun fum, u3_noun som);

        /* u3a_lush(): leak push.
        */
          c3_w 
          u3a_lush(c3_w lab_w);

        /* u3a_lop(): leak pop.
        */
          void
          u3a_lop(c3_w lab_w);

        /* u3a_print_memory: print memory amount.
        */
          void
          u3a_print_memory(c3_c* cap_c, c3_w wor_w);

      /* Atoms from proto-atoms.
      */
        /* u3a_slab(): create a length-bounded proto-atom.
        */
          c3_w*
          u3a_slab(c3_w len_w);

        /* u3a_slaq(): u3a_slaq() with a defined blocksize.
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


