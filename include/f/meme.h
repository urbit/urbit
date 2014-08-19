/* include/meme.h
**
** This file is in the public domain.
*/
  /**  Prefix definitions:
  ***
  ***  u2_ca_: fundamental allocators.
  ***  u2_cc_: constants.
  ***  u2_ch_: memoization.
  ***  u2_ci_: noun constructors
  ***  u2_cm_: system management etc.
  ***  u2_cn_: nock interpreter.
  ***  u2_co_: fundamental macros.
  ***  u2_cr_: read functions which never bail out.
  ***  u2_cs_: structures and definitions.
  ***  u2_cx_: read functions which do bail out.
  ***
  ***  u2_cr_ and u2_cx_ functions use retain conventions; the caller
  ***  retains ownership of passed-in nouns, the callee preserves 
  ***  ownership of returned nouns.
  ***
  ***  All other functions use transfer conventions; the caller logically
  ***  releases passed-in nouns, the callee logically releases returned nouns.
  **/

  /**  Subordinate includes.
  **/
    /** c3: the C layer.
    **/
#     include "c/portable.h"
#     include "c/tune.h"
#     include "c/types.h"
#     include "c/defs.h"
#     include "c/motes.h"
#     include "c/comd.h"

  /**  Nock-specific typedefs.
  **/
    /* u2_yes, u2_no, u2_nul;
    **
    **   Our Martian booleans and list terminator; empty string; not a nonu.
    */
#     define u2_yes   0
#     define u2_no    1
#     define u2_nul   0
#     define u2_blip  0

    /* Tools for Martian booleans.
    */
#     define u2_so(x)      (u2_yes == (x))
#     define u2_ne(x)      (u2_no == (x))
#     define u2_say(x)     ( (x) ? u2_yes : u2_no )
#     define u2_not(x)     ( (x == u2_yes) ? u2_no : u2_yes )
#     define u2_and(x, y)  ( (u2_so(x) && u2_so(y)) ? u2_yes : u2_no )
#     define u2_or(x, y)   ( (u2_so(x) || u2_so(y)) ? u2_yes : u2_no )

    /* Word axis macros.  For 31-bit axes only.
    */
      /* u2_ax_dep(): number of axis bits.
      */
#       define u2_ax_dep(a_w)   (c3_bits_word(a_w) - 1)

      /* u2_ax_cap(): root axis, 2 or 3.
      */
#       define u2_ax_cap(a_w)   (0x2 | (a_w >> (u2_ax_dep(a_w) - 1)))

      /* u2_ax_mas(): remainder after cap.
      */
#       define u2_ax_mas(a_w) \
          ( (a_w & ~(1 << u2_ax_dep(a_w))) | (1 << (u2_ax_dep(a_w) - 1)) )

      /* u2_ax_peg(): connect two axes.
      */
#       define u2_ax_peg(a_w, b_w) \
          ( (a_w << u2_ax_dep(b_w)) | (b_w &~ (1 << u2_ax_dep(b_w))) )

  /** Aliases - selective and syntactically unique.
  **/
#   define u2h(som)          u2_cx_h(som)
#   define u2t(som)          u2_cx_t(som)
#   define u2at(axe, som)    u2_cx_at(axe, som)

#   define u2nc(a, b)        u2_cn_cell(a, b)
#   define u2nt(a, b, c)     u2_cn_trel(a, b, c)
#   define u2nq(a, b, c, d)  u2_cn_qual(a, b, c, d)

#   define u2du(som)         (u2_cr_du(som))
#   define u2ud(som)         (u2_cr_ud(som))

#   define u2k(som)          u2_ca_gain(som)
#   define u2z(som)          u2_ca_lose(som)


  /** Tuning and configuration.
  **/
#   define u2_cc_fbox_no  28

#   undef U2_MEMORY_DEBUG
#   ifdef U2_MEMORY_DEBUG
#     define  u2_leak_on(x) (COD_w = x)
        extern  c3_w COD_w;
#     define  u2_leak_off  (COD_w = 0)
#   endif


  /** Data structures.
  **/
    /* u2_noun: tagged pointer.
    **
    **  If bit 31 is 0, a u2_noun is a direct 31-bit atom ("cat").
    **  If bit 31 is 1 and bit 30 0, an indirect atom ("pug").
    **  If bit 31 is 1 and bit 30 1, an indirect cell ("pom").
    **
    ** Bits 0-29 are a word offset against u2_Loom.
    */
      typedef c3_w    u2_noun;

    /* u2_none - out-of-band noun.
    */
#     define u2_none  (u2_noun)0xffffffff

    /* Informative typedefs.  Use if you like.
    */
      typedef u2_noun u2_atom;              //  must be atom
      typedef u2_noun u2_term;              //  @tas
      typedef u2_noun u2_mote;              //  @tas
      typedef u2_noun u2_cell;              //  must be cell
      typedef u2_noun u2_trel;              //  must be triple
      typedef u2_noun u2_qual;              //  must be quadruple
      typedef u2_noun u2_quin;              //  must be quintuple
      typedef u2_noun u2_bean;              //  loobean: 0 == u2_yes, 1 == u2_no
      typedef u2_noun u2_weak;              //  may be u2_none

    /* u2_atom, u2_cell: logical atom and cell structures.
    */
      typedef struct {
        c3_w mug_w;
      } u2_cs_noun;

      typedef struct {
        c3_w mug_w;
        c3_w len_w;
        c3_w buf_w[0];
      } u2_cs_atom;

      typedef struct _u2_loom_cell {
        c3_w    mug_w;
        u2_noun hed; 
        u2_noun tel;
      } u2_cs_cell;

    /* Inside a noun.
    */
#     define u2_co_is_cat(som)    (((som) >> 31) ? u2_no : u2_yes)
#     define u2_co_is_dog(som)    (((som) >> 31) ? u2_yes : u2_no)

#     define u2_co_is_pug(som)    (2 == (som >> 30))
#     define u2_co_is_pom(som)    (3 == (som >> 30))
#     define u2_co_to_off(som)    ((som) & 0x3fffffff)
#     define u2_co_to_ptr(som)    ((void *)(u2_co_into(u2_co_to_off(som))))
#     define u2_co_to_pug(off)    (off | 0x40000000)
#     define u2_co_to_pom(off)    (off | 0xc0000000)

#     define u2_co_is_atom(som)    u2_or(u2_co_is_cat(som), \
                                         u2_co_is_pug(som))
#     define u2_co_is_cell(som)    u2_co_is_pom(som)
#     define u2_co_de_twin(dog, dog_w)  ((dog & 0xc0000000) | u2_co_outa(dog_w))

#     define u2_co_h(som) \
        ( u2_so(u2_co_is_cell(som)) \
           ? ( ((u2_cs_cell *)u2_co_to_ptr(som))->hed )\
           : u2_cm_bail(c3__exit) )

#     define u2_co_t(som) \
        ( u2_so(u2_co_is_cell(som)) \
           ? ( ((u2_cs_cell *)u2_co_to_ptr(som))->tel )\
           : u2_cm_bail(c3__exit) )

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
          u2_noun dax;                      //  (map ,* (list chub))
        } jed;

        struct {                            //  namespace
          u2_noun fly;                      //  $+(* (unit))
        } ski;

        struct {                            //  need state
          u2_noun nyd;                      //  (list path)
        } nyd;

        struct {                            //  trace stack
          u2_noun tax;                      //  (list ,*)
        } bug;

        struct {                            //  profiling stack
          u2_noun don;                      //  (list ,*)
        } pro;

        struct {                            //  memoization
          u2_noun sav;                      //  (map (pair term noun) noun)
        } cax;
      } u2_cs_road;
      typedef u2_cs_road u2_road;


  /** Globals.
  **/
    /* u2_Loom: base of loom, as a word pointer.
    */
      c3_global c3_w* u2_Loom;
#       define u2L  u2_Loom

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
#     define  u2_co_is_north  ((u2R->cap_w > u2R->hat_w) ? u2_yes : u2_no)
#     define  u2_co_is_south  ((u2_yes == u2_co_is_north) ? u2_no : u2_yes)

#     define  u2_co_open      ( (u2_yes == u2_co_is_north) \
                                  ? (c3_w)(u2R->cap_w - u2R->hat_w) \
                                  : (c3_w)(u2R->hat_w - u2R->cap_w) )

#     define  u2_co_into(x) (u2_Loom + (x))
#     define  u2_co_outa(p) (((c3_w*)(void*)(p)) - u2_Loom)


  /** Functions.
  **/
    /** u2_cx_*: read, but bail with c3__exit on a crash.
    **/
#if 1
#     define u2_cx_h(som)  u2_co_h(som)
#     define u2_cx_t(som)  u2_co_t(som)
#else
      /* u2_cx_h (u2h): head.
      */
        u2_noun
        u2_cx_h(u2_noun som);

      /* u2_cx_t (u2t): tail.
      */
        u2_noun
        u2_cx_t(u2_noun som);
#endif
      /* u2_cx_at (u2at): fragment.
      */
        u2_noun
        u2_cx_at(u2_noun axe, u2_noun som);

      /* u2_cx_cell():
      **
      **   Divide `a` as a cell `[b c]`.
      */
        void
        u2_cx_cell(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c);

      /* u2_cx_trel():
      **
      **   Divide `a` as a trel `[b c d]`, or bail.
      */
        void
        u2_cx_trel(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c,
                   u2_noun* d);

      /* u2_cx_qual():
      **
      **   Divide `a` as a quadruple `[b c d e]`.
      */
        void
        u2_cx_qual(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c,
                   u2_noun* d,
                   u2_noun* e);

    /** u2_cr_*: read without ever crashing.
    **/
#if 1
#       define u2_cr_du(a)  u2_co_is_cell(a)
#       define u2_cr_ud(a)  u2_co_is_atom(a)
#else
      /* u2_cr_du(): u2_yes iff `a` is cell.
      */
        u2_bean
        u2_cr_du(u2_noun a);

      /* u2_cr_ud(): u2_no iff `a` is cell.
      */
        u2_bean
        u2_cr_ud(u2_noun a);
#endif

      /* u2_cr_at(): fragment `a` of `b`, or u2_none.
      */
        u2_weak
        u2_cr_at(u2_atom a,
                 u2_weak b);

      /* u2_cr_mean():
      **
      **   Attempt to deconstruct `a` by axis, noun pairs; 0 terminates.
      **   Axes must be sorted in tree order.
      */
        u2_bean
        u2_cr_mean(u2_noun a,
                   ...);

      /* u2_cr_mug():
      **
      **   Compute and/or recall the mug (31-bit hash) of (a).
      */
        c3_w
        u2_cr_mug(u2_noun a);

      /* u2_cr_mug_string():
      **
      **   Compute the mug of `a`, LSB first.
      */
        c3_w
        u2_cr_mug_string(const c3_c *a_c);

      /* u2_cr_mug_words():
      **
      **   Compute the mug of `buf`, `len`, LSW first.
      */
        c3_w
        u2_cr_mug_words(const c3_w *buf_w,
                        c3_w        len_w);

      /* u2_cr_mug_cell():
      **
      **   Compute the mug of `[a b]`.
      */
        c3_w
        u2_cr_mug_cell(u2_noun a,
                       u2_noun b);

      /* u2_cr_mug_trel():
      **
      **   Compute the mug of `[a b c]`.
      */
        c3_w
        u2_cr_mug_trel(u2_noun a,
                       u2_noun b,
                       u2_noun c);

      /* u2_cr_mug_qual():
      **
      **   Compute the mug of `[a b c d]`.
      */
        c3_w
        u2_cr_mug_qual(u2_noun a,
                       u2_noun b,
                       u2_noun c,
                       u2_noun d);

      /* u2_cr_mug_both():
      **
      **   Join two mugs.
      */
        c3_w
        u2_cr_mug_both(c3_w a_w,
                       c3_w b_w);

      /* u2_cr_fing():
      **
      **   Yes iff (a) and (b) are the same copy of the same noun.
      **   (Ie, by pointer equality - u2_cr_sing with false negatives.)
      */
        u2_bean
        u2_cr_fing(u2_noun a,
                   u2_noun b);

      /* u2_cr_fing_cell():
      **
      **   Yes iff `[p q]` and `b` are the same copy of the same noun.
      */
        u2_bean
        u2_cr_fing_cell(u2_noun p,
                        u2_noun q,
                        u2_noun b);

      /* u2_cr_fing_mixt():
      **
      **   Yes iff `[p q]` and `b` are the same copy of the same noun.
      */
        u2_bean
        u2_cr_fing_mixt(const c3_c* p_c,
                        u2_noun     q,
                        u2_noun     b);

      /* u2_cr_fing_trel():
      **
      **   Yes iff `[p q r]` and `b` are the same copy of the same noun.
      */
        u2_bean
        u2_cr_fing_trel(u2_noun p,
                        u2_noun q,
                        u2_noun r,
                        u2_noun b);

      /* u2_cr_fing_qual():
      **
      **   Yes iff `[p q r s]` and `b` are the same copy of the same noun.
      */
        u2_bean
        u2_cr_fing_qual(u2_noun p,
                        u2_noun q,
                        u2_noun r,
                        u2_noun s,
                        u2_noun b);

      /* u2_cr_sing():
      **
      **   Yes iff (a) and (b) are the same noun.
      */
        u2_bean
        u2_cr_sing(u2_noun a,
                   u2_noun b);

      /* u2_cr_sing_c():
      **
      **   Yes iff (b) is the same noun as the C string [a].
      */
        u2_bean
        u2_cr_sing_c(const c3_c* a_c,
                     u2_noun     b);

      /* u2_cr_sing_cell():
      **
      **   Yes iff `[p q]` and `b` are the same noun.
      */
        u2_bean
        u2_cr_sing_cell(u2_noun p,
                        u2_noun q,
                        u2_noun b);

      /* u2_cr_sing_mixt():
      **
      **   Yes iff `[p q]` and `b` are the same noun.
      */
        u2_bean
        u2_cr_sing_mixt(const c3_c* p_c,
                        u2_noun     q,
                        u2_noun     b);

      /* u2_cr_sing_trel():
      **
      **   Yes iff `[p q r]` and `b` are the same noun.
      */
        u2_bean
        u2_cr_sing_trel(u2_noun p,
                        u2_noun q,
                        u2_noun r,
                        u2_noun b);

      /* u2_cr_sing_qual():
      **
      **   Yes iff `[p q r s]` and `b` are the same noun.
      */
        u2_bean
        u2_cr_sing_qual(u2_noun p,
                        u2_noun q,
                        u2_noun r,
                        u2_noun s,
                        u2_noun b);

      /* u2_cr_nord():
      **
      **   Return 0, 1 or 2 if `a` is below, equal to, or above `b`.
      */
        u2_atom
        u2_cr_nord(u2_noun a,
                   u2_noun b);

      /* u2_cr_mold():
      **
      **   Divide `a` as a mold `[b.[p q] c]`.
      */
        u2_bean
        u2_cr_mold(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c);

      /* u2_cr_cell():
      **
      **   Divide `a` as a cell `[b c]`.
      */
        u2_bean
        u2_cr_cell(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c);

      /* u2_cr_trel():
      **
      **   Divide `a` as a trel `[b c]`.
      */
        u2_bean
        u2_cr_trel(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c,
                   u2_noun* d);

      /* u2_cr_qual():
      **
      **   Divide (a) as a qual (b c d e).
      */
        u2_bean
        u2_cr_qual(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c,
                   u2_noun* d,
                   u2_noun* e);

      /* u2_cr_p():
      **
      **   & [0] if [a] is of the form [b *c].
      */
        u2_bean
        u2_cr_p(u2_noun  a,
                u2_noun  b,
                u2_noun* c);

      /* u2_cr_pq():
      **
      **   & [0] if [a] is of the form [b *c d].
      */
        u2_bean
        u2_cr_pq(u2_noun  a,
                 u2_noun  b,
                 u2_noun* c,
                 u2_noun* d);

      /* u2_cr_pqr():
      **
      **   & [0] if [a] is of the form [b *c *d *e].
      */
        u2_bean
        u2_cr_pqr(u2_noun  a,
                  u2_noun  b,
                  u2_noun* c,
                  u2_noun* d,
                  u2_noun* e);

      /* u2_cr_pqrs():
      **
      **   & [0] if [a] is of the form [b *c *d *e *f].
      */
        u2_bean
        u2_cr_pqrs(u2_noun  a,
                   u2_noun  b,
                   u2_noun* c,
                   u2_noun* d,
                   u2_noun* e,
                   u2_noun* f);

      /* u2_cr_met():
      **
      **   Return the size of (b) in bits, rounded up to
      **   (1 << a_y).
      **
      **   For example, (a_y == 3) returns the size in bytes.
      */
        c3_w
        u2_cr_met(c3_y    a_y,
                  u2_atom b);

      /* u2_cr_bit():
      **
      **   Return bit (a_w) of (b).
      */
        c3_b
        u2_cr_bit(c3_w    a_w,
                  u2_atom b);

      /* u2_cr_byte():
      **
      **   Return byte (a_w) of (b).
      */
        c3_y
        u2_cr_byte(c3_w    a_w,
                   u2_atom b);

      /* u2_cr_bytes():
      **
      **   Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u2_cr_bytes(c3_w    a_w,
                    c3_w    b_w,
                    c3_y*   c_y,
                    u2_atom d);

      /* u2_cr_chop():
      **
      **   Into the bloq space of `met`, from position `fum` for a
      **   span of `wid`, to position `tou`, XOR from atom `src`
      **   into ray `dst`.
      */
        void
        u2_cr_chop(c3_g    met_g,
                   c3_w    fum_w,
                   c3_w    wid_w,
                   c3_w    tou_w,
                   c3_w*   dst_w,
                   u2_atom src);

      /* u2_cr_mp():
      **
      **   Copy (b) into (a_mp).
      */
        void
        u2_cr_mp(mpz_t   a_mp,
                 u2_atom b);

      /* u2_cr_word():
      **
      **   Return word (a_w) of (b).
      */
        c3_w
        u2_cr_word(c3_w    a_w,
                   u2_atom b);

      /* u2_cr_words():
      **
      **  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u2_cr_words(c3_w    a_w,
                    c3_w    b_w,
                    c3_w*   c_w,
                    u2_atom d);

      /* u2_cr_string(): `a`, a text atom, as malloced C string.
      */
        c3_c*
        u2_cr_string(u2_atom a);

      /* u2_cr_tape(): `a`, a list of bytes, as malloced C string.
      */
        c3_y*
        u2_cr_tape(u2_noun a);


    /** System management.
    **/
      /* u2_cm_boot(): make u2R and u2H from `len` words at `adr`.
      */
        void
        u2_cm_boot(c3_p adr_p, c3_w len_w);

      /* u2_cm_trap(): setjmp within road.
      */
#if 0
        u2_bean
        u2_cm_trap(void);
#else
#       define u2_cm_trap() (u2_noun)(setjmp(u2R->esc.buf))
#endif

      /* u2_cm_bail(): bail out.  Does not return.
      **
      **  Bail motes:
      **
      **    %exit               ::  semantic failure
      **    %evil               ::  bad crypto
      **    %intr               ::  interrupt
      **    %fail               ::  execution failure
      **    %foul               ::  assert failure
      **    %need               ::  network block
      **    %meme               ::  out of memory
      */ 
        c3_i
        u2_cm_bail(c3_m how_m);

      /* u2_cm_grab(): garbage-collect memory.  Asserts u2R == u2H.
      */
        void
        u2_cm_grab(void);

      /* u2_cm_check(): checkpoint memory to file.  Asserts u2R == u2H.
      */
        void
        u2_cm_check(void);

      /* u2_cm_fall(): return to parent road.
      */
        void
        u2_cm_fall(void);

      /* u2_cm_leap(): advance to inner road.
      */
        void
        u2_cm_leap(void);

      /* u2_cm_golf(): record cap length for u2_flog().
      */
        c3_w
        u2_cm_golf(void);

      /* u2_cm_flog(): pop the cap.
      **
      **    A common sequence for inner allocation is:
      **
      **    c3_w gof_w = u2_cm_golf();
      **    u2_cm_leap();
      **    //  allocate some inner stuff...
      **    u2_cm_fall();
      **    //  inner stuff is still valid, but on cap
      **    u2_cm_flog(gof_w);
      **
      ** u2_cm_flog(0) simply clears the cap.
      */
        void
        u2_cm_flog(c3_w gof_w);

      /* u2_cm_water(): produce high and low watermarks.  Asserts u2R == u2H.
      */
        void
        u2_cm_water(c3_w *low_w, c3_w *hig_w);


    /**  Allocation.
    **/
      /* Basic allocation.
      */
        /* u2_ca_walloc(): allocate storage measured in words.
        */
          void*
          u2_ca_walloc(c3_w len_w);

        /* u2_ca_malloc(): allocate storage measured in bytes.
        */
          void*
          u2_ca_malloc(c3_w len_w);

        /* u2_ca_free(): free storage.
        */
          void
          u2_ca_free(void* lag_v);


      /* Reference and arena control.
      */
        /* u2_ca_gain(): gain and/or copy juniors.
        */
          u2_weak
          u2_ca_gain(u2_weak som);

        /* u2_ca_lose(): lose a reference.
        */
          void
          u2_ca_lose(u2_weak som);

        /* u2_ca_use(): reference count.
        */
          c3_w
          u2_ca_use(u2_noun som);

        /* u2_ca_mark(): mark for gc, returning allocated words.
        */
          c3_w
          u2_ca_mark(u2_noun som);

        /* u2_ca_sweep(): sweep after gc, freeing, matching live count.
        */
          c3_w
          u2_ca_sweep(c3_w liv_w);


      /* Atoms from proto-atoms.
      */
        /* u2_ca_slab(): create a length-bounded proto-atom.
        */
          c3_w*
          u2_ca_slab(c3_w len_w);

        /* u2_ca_slaq(): u2_ca_slaq() with a defined blocksize.
        */
          c3_w*
          u2_ca_slaq(c3_g met_g, c3_w len_w);

        /* u2_ca_malt(): measure and finish a proto-atom.
        */
          u2_noun
          u2_ca_malt(c3_w* sal_w);

        /* u2_ca_moot(): finish a pre-measured proto-atom; dangerous.
        */
          u2_noun
          u2_ca_moot(c3_w* sal_w);

        /* u2_ca_mint(): finish a measured proto-atom.
        */
          u2_noun
          u2_ca_mint(c3_w* sal_w, c3_w len_w);


      /* General constructors.
      */
        /* u2_ci_words():
        **
        **   Copy [a] words from [b] into an atom.
        */
          u2_noun
          u2_ci_words(c3_w        a_w,
                      const c3_w* b_w);

        /* u2_ci_bytes():
        **
        **   Copy `a` bytes from `b` to an LSB first atom.
        */
          u2_noun
          u2_ci_bytes(c3_w        a_w,
                      const c3_y* b_y);

        /* u2_ci_mp():
        **
        **   Copy the GMP integer `a` into an atom, and clear it.
        */
          u2_noun
          u2_ci_mp(mpz_t a_mp);

        /* u2_ci_vint():
        **
        **   Create `a + 1`.
        */
          u2_noun
          u2_ci_vint(u2_noun a);

        /* u2_ci_cell():
        **
        **   Produce the cell `[a b]`.
        */
          u2_noun
          u2_ci_cell(u2_noun a, u2_noun b);

        /* u2_ci_string():
        **
        **   Produce an LSB-first atom from the C string `a`.
        */
          u2_noun
          u2_ci_string(const c3_c* a_c);

        /* u2_ci_molt():
        **
        **   Mutate `som` with a 0-terminated list of axis, noun pairs.
        **   Axes must be cats (31 bit).
        */
          u2_noun 
          u2_ci_molt(u2_noun som, ...);

        /* u2_ci_chubs():
        **
        **   Construct `a` double-words from `b`, LSD first, as an atom.
        */
          u2_atom
          u2_ci_chubs(c3_w        a_w,
                      const c3_d* b_d);

        /* u2_ci_tape(): from a C string, to a list of bytes.
        */
          u2_atom
          u2_ci_tape(const c3_c* txt_c);


    /**  Generic computation.
    **/
      /* u2_cn_nock_on(): produce .*(bus fol).
      */
        u2_noun
        u2_cn_nock_on(u2_noun bus, u2_noun fol);

      /* u2_cn_slam_on(): produce (gat sam).
      */
        u2_noun
        u2_cn_slam_on(u2_noun gat, u2_noun sam);

      /* u2_cn_nock_un(): produce .*(bus fol), as ++toon.
      */
        u2_noun
        u2_cn_nock_un(u2_noun bus, u2_noun fol);

      /* u2_cn_slam_un(): produce (gat sam), as ++toon.
      */
        u2_noun
        u2_cn_slam_un(u2_noun gat, u2_noun sam);

      /* u2_cn_nock_in(): produce .*(bus fol), as ++toon, in namespace.
      */
        u2_noun
        u2_cn_nock_in(u2_noun fly, u2_noun bus, u2_noun fol);

      /* u2_cn_slam_in(): produce (gat sam), as ++toon, in namespace.
      */
        u2_noun
        u2_cn_slam_in(u2_noun fly, u2_noun gat, u2_noun sam);


    /**  Memoization.
    ***
    ***  The memo cache is keyed by an arbitrary symbolic function
    ***  and a noun argument to that (logical) function.  Functions
    ***  are predefined by C-level callers, but 0 means nock.
    ***
    ***  The memo cache is within its road and dies when it falls.
    **/
      /* u2_ch_find*(): find in memo cache.
      */
        u2_weak u2_ch_find(u2_mote, u2_noun);
        u2_weak u2_ch_find_2(u2_mote, u2_noun, u2_noun);
        u2_weak u2_ch_find_3(u2_mote, u2_noun, u2_noun, u2_noun);
        u2_weak u2_ch_find_4(u2_mote, u2_noun, u2_noun, u2_noun, u2_noun);

      /* u2_ch_save*(): save in memo cache.
      */
        u2_weak u2_ch_save(u2_mote, u2_noun, u2_noun);
        u2_weak u2_ch_save_2(u2_mote, u2_noun, u2_noun, u2_noun);
        u2_weak u2_ch_save_3(u2_mote, u2_noun, u2_noun, u2_noun, u2_noun);
        u2_weak u2_ch_save_4
                  (u2_mote, u2_noun, u2_noun, u2_noun, u2_noun, u2_noun);

      /* u2_ch_uniq(): uniquify with memo cache.
      */
        u2_weak 
        u2_ch_uniq(u2_noun som);
