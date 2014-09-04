/* include/f/road.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u2_cs_box: classic allocation box.
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
      typedef struct _u2_cs_box {
        c3_w   siz_w;                       // size of this box
        c3_w   use_w;                       // reference count; free if 0
#       ifdef U2_MEMORY_DEBUG
          c3_w   cod_w;                     // tracing code
#       endif
      } u2_cs_box;

#     define u2_co_boxed(len_w)  (len_w + c3_wiseof(u2_cs_box) + 1)
#     define u2_co_boxto(box_v)  ( (void *) \
                                   ( ((c3_w *)(void*)(box_v)) + \
                                     c3_wiseof(u2_cs_box) ) )
#     define u2_co_botox(tox_v)  ( (struct _u2_cs_box *) \
                                   (void *) \
                                   ( ((c3_w *)(void*)(tox_v)) - \
                                      c3_wiseof(u2_cs_box)  ) )

    /* u2_cs_fbox: free node in heap.  Sets minimum node size.
    **
    */
      typedef struct _u2_cs_fbox {
        u2_cs_box           box_u;
        struct _u2_cs_fbox* pre_u;
        struct _u2_cs_fbox* nex_u;
      } u2_cs_fbox;

#     define u2_cc_minimum   (1 + c3_wiseof(u2_cs_fbox))

    /* u2_cs_road: contiguous allocation and execution context.
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
    **  In all cases, the pointer in a u2_noun is a word offset into
    **  u2H, the top-level road.
    */
      typedef struct _u2_cs_road {
        struct _u2_cs_road* par_u;          //  parent road
        struct _u2_cs_road* kid_u;          //  child road list
        struct _u2_cs_road* nex_u;          //  sibling road

        c3_w* cap_w;                      //  top of transient region
        c3_w* hat_w;                      //  top of durable region
        c3_w* mat_w;                      //  bottom of transient region
        c3_w* rut_w;                      //  bottom of durable region
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
          u2_cs_fbox* fre_u[u2_cc_fbox_no]; //  heap by node size log
#         ifdef U2_MEMORY_DEBUG
            c3_w liv_w;                     //  number of live words
#         endif
        } all;

        struct {                            //  jet dashboard
          u2_ch_root* har_u;                //  jet index by 
        } jed;

        struct {                            //  namespace
          u2_noun fly;                      //  $+(* (unit))
        } ski;

        struct {                            //  need state
          u2_noun nyd;                      //  (list path)
        } net;

        struct {                            //  trace stack
          u2_noun tax;                      //  (list ,*)
        } bug;

        struct {                            //  profiling stack
          u2_noun don;                      //  (list ,*)
        } pro;

        struct {                            //  memoization
          u2_ch_root* har_u;                //  (map (pair term noun) noun)
        } cax;
      } u2_cs_road;
      typedef u2_cs_road u2_road;

  /**  Macros.
  **/
#     define  u2_co_is_north  ((u2R->cap_w > u2R->hat_w) ? u2_yes : u2_no)
#     define  u2_co_is_south  ((u2_yes == u2_co_is_north) ? u2_no : u2_yes)

#     define  u2_co_open      ( (u2_yes == u2_co_is_north) \
                                  ? (c3_w)(u2R->cap_w - u2R->hat_w) \
                                  : (c3_w)(u2R->hat_w - u2R->cap_w) )

#     define  u2_co_into(x) ((void *)(u2_Loom + (x)))
#     define  u2_co_outa(p) (((c3_w*)(void*)(p)) - u2_Loom)


