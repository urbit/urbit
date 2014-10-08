/* include/n/road.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u3_cs_box: classic allocation box.
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
      typedef struct _u3_cs_box {
        c3_w   siz_w;                       // size of this box
        c3_w   use_w;                       // reference count; free if 0
#       ifdef U3_MEMORY_DEBUG
          c3_w   cod_w;                     // tracing code
#       endif
      } u3_cs_box;

#     define u3_co_boxed(len_w)  (len_w + c3_wiseof(u3_cs_box) + 1)
#     define u3_co_boxto(box_v)  ( (void *) \
                                   ( ((c3_w *)(void*)(box_v)) + \
                                     c3_wiseof(u3_cs_box) ) )
#     define u3_co_botox(tox_v)  ( (struct _u3_cs_box *) \
                                   (void *) \
                                   ( ((c3_w *)(void*)(tox_v)) - \
                                      c3_wiseof(u3_cs_box)  ) )

    /* u3_cs_fbox: free node in heap.  Sets minimum node size.
    **
    */
      typedef struct _u3_cs_fbox {
        u3_cs_box           box_u;
        struct _u3_cs_fbox* pre_u;
        struct _u3_cs_fbox* nex_u;
      } u3_cs_fbox;

#     define u3_cc_minimum   (1 + c3_wiseof(u3_cs_fbox))

    /* u3_cs_road: contiguous allocation and execution context.
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
    **  This is a trivial cost in some cases, a prohibitive case in 
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
      typedef struct _u3_cs_road {
        struct _u3_cs_road* par_u;          //  parent road

        struct _u3_cs_road* kid_u;          //  child road list
        struct _u3_cs_road* nex_u;          //  sibling road
        struct _u3_cs_road* now_u;          //  current road pointer

        c3_w* cap_w;                        //  top of transient region
        c3_w* hat_w;                        //  top of durable region
        c3_w* mat_w;                        //  bottom of transient region
        c3_w* rut_w;                        //  bottom of durable region
#if 0
          c3_w* gar_w;                      //  bottom of guard region (future)
          c3_w* rag_w;                      //  top of guard region (future)
          c3_w  pad_w[4];                   //  future interesting info
#endif

        struct {                            //  escape buffer
          union {
            jmp_buf buf;
            c3_w buf_w[256];                //  futureproofing
          };
        } esc;

        struct {                            //  allocation pools
          u3_cs_fbox* fre_u[u3_cc_fbox_no]; //  heap by node size log
          c3_w fre_w;                       //  number of free words
        } all;

        struct {                            //  jet dashboard
          u3_ch_root* har_u;                //  jet index
        } jed;

        struct {                            //  namespace
          u3_noun flu;                      //  (list $+(* (unit))), inward
        } ski;

        struct {                            //  trace stack
          u3_noun tax;                      //  (list ,*)
          u3_noun mer;                      //  emergency buffer to release
        } bug;

        struct {                            //  profile stack
          c3_d    nox_d;                    //  nock steps
          u3_noun don;                      //  ++path
          u3_noun day;                      //  profile data, ++doss
        } pro;

        struct {                            //  memoization
          u3_ch_root* har_u;                //  (map (pair term noun) noun)
        } cax;
      } u3_cs_road;
      typedef u3_cs_road u3_road;


  /**  Macros.
  **/
#     define  u3_co_into(x) ((void *)(u3_Loom + (x)))
#     define  u3_co_outa(p) (((c3_w*)(void*)(p)) - u3_Loom)

#     define  u3to(type, x) ((type *) u3_co_into(x))

#     define  u3_co_is_north(r)  ((r->cap_w > r->hat_w) ? u3_yes : u3_no)
#     define  u3_co_is_south(r)  ((u3_so(u3_co_is_north(r))) ? u3_no : u3_yes)

#     define  u3_co_open(r)      ( (u3_yes == u3_co_is_north(r)) \
                                  ? (c3_w)(r->cap_w - r->hat_w) \
                                  : (c3_w)(r->hat_w - r->cap_w) )

#     define  u3_co_north_is_senior(r, dog) \
                u3_say((u3_co_to_wtr(dog) < r->rut_w) ||  \
                       (u3_co_to_wtr(dog) >= r->mat_w))
              
#     define  u3_co_north_is_junior(r, dog) \
                u3_say((u3_co_to_wtr(dog) >= r->cap_w) && \
                       (u3_co_to_wtr(dog) < r->mat_w))

#     define  u3_co_north_is_normal(r, dog) \
                u3_and(u3_not(u3_co_north_is_senior(r, dog)),  \
                       u3_not(u3_co_north_is_junior(r, dog)))

#     define  u3_co_south_is_senior(r, dog) \
                u3_say((u3_co_to_wtr(dog) < r->mat_w) || \
                       (u3_co_to_wtr(dog) >= r->rut_w))

#     define  u3_co_south_is_junior(r, dog) \
                u3_say((u3_co_to_wtr(dog) < r->cap_w) && \
                       (u3_co_to_wtr(dog) >= r->mat_w))

#     define  u3_co_south_is_normal(r, dog) \
                u3_and(u3_not(u3_co_south_is_senior(r, dog)),  \
                       u3_not(u3_co_south_is_junior(r, dog)))

#     define  u3_co_is_junior(r, som) \
                ( u3_so(u3_co_is_cat(som)) \
                      ?  u3_no \
                      :  u3_so(u3_co_is_north(r)) \
                         ?  u3_co_north_is_junior(r, som) \
                         :  u3_co_south_is_junior(r, som) )

#     define  u3_co_is_senior(r, som) \
                ( u3_so(u3_co_is_cat(som)) \
                      ?  u3_yes \
                      :  u3_so(u3_co_is_north(r)) \
                         ?  u3_co_north_is_senior(r, som) \
                         :  u3_co_south_is_senior(r, som) )
