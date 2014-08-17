/* include/meme.h
**
** This file is in the public domain.
*/
  /** Subordinate includes.
  **/
    /** c3: the C layer.
    **/
#     include "c/portable.h"
#     include "c/tune.h"
#     include "c/types.h"
#     include "c/defs.h"
#     include "c/motes.h"
#     include "c/comd.h"


  /** Tuning and configuration.
  **/
#   define u2_me_fbox_no  28

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
      typedef c3_w u2_noun;

    /* u2_none - out-of-band noun.
    */
#     define u2_none  (u2_noun)0xffffffff

    /* u2_atom, u2_cell: logical atom and cell structures.
    */
      typedef struct {
        c3_w mug_w;
      } u2_me_noun;

      typedef struct {
        c3_w mug_w;
        c3_w len_w;
        c3_w buf_w[0];
      } u2_me_atom;

      typedef struct _u2_loom_cell {
        c3_w    mug_w;
        u2_noun hed; 
        u2_noun tel;
      } u2_me_cell;

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

    /* Inside a noun.
    */
#     define u2_me_is_cat(som)    (((som) >> 31) ? u2_no : u2_yes)
#     define u2_me_is_dog(som)    (((som) >> 31) ? u2_yes : u2_no)

#     define u2_me_is_pug(som)    (2 == (som >> 30))
#     define u2_me_is_pom(som)    (3 == (som >> 30))
#     define u2_me_to_off(som)    ((som) & 0x3fffffff)
#     define u2_me_to_ptr(som)    ((void *)(u2_me_into(u2_me_to_off(som))))
#     define u2_me_to_pug(off)    (off | 0x40000000)
#     define u2_me_to_pom(off)    (off | 0xc0000000)

#     define u2_me_is_atom(som)    u2_or(u2_me_is_cat(som), \
                                         u2_me_is_pug(som))
#     define u2_me_is_cell(som)    u2_me_is_pom(som)
#     define u2_me_de_twin(dog, dog_w)  ((dog & 0xc0000000) | u2_me_outa(dog_w))

#     define u2_h(som) \
        ( u2_so(u2_me_is_cell(som)) \
           ? ( ((u2_me_cell *)u2_me_to_ptr(som))->hed )\
           : u2_me_bail(c3__exit) )

#     define u2_t(som) \
        ( u2_so(u2_me_is_cell(som)) \
           ? ( ((u2_me_cell *)u2_me_to_ptr(som))->tel )\
           : u2_me_bail(c3__exit) )

    /* More typedefs.
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
      typedef u2_noun (*u2_gate)(u2_noun);  //  function pointer

    /*** Word axis macros.
    ****
    **** Use these on axes known to be in 31-bit range.
    ***/
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


    /* u2_me_box: classic allocation box.
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
      typedef struct _u2_me_box {
        c3_w   siz_w;                       // size of this box
        c3_w   use_w;                       // reference count; free if 0
#       ifdef U2_MEMORY_DEBUG
          c3_w   cod_w;                     // tracing code
#       endif
      } u2_me_box;

#     define u2_me_boxed(len_w)  (len_w + c3_wiseof(u2_me_box) + 1)
#     define u2_me_boxto(box_v)  ( (void *) \
                                   ( ((c3_w *)(void*)(box_v)) + \
                                     c3_wiseof(u2_me_box) ) )
#     define u2_me_botox(tox_v)  ( (struct _u2_me_box *) \
                                   (void *) \
                                   ( ((c3_w *)(void*)(tox_v)) - \
                                      c3_wiseof(u2_me_box)  ) )

    /* u2_me_fbox: free node in heap.  Sets minimum node size.
    **
    */
      typedef struct _u2_me_fbox {
        u2_me_box           box_u;
        struct _u2_me_fbox* pre_u;
        struct _u2_me_fbox* nex_u;
      } u2_me_fbox;

#     define u2_me_minimum   (c3_wiseof(u2_me_fbox))

    /* u2_me_road: contiguous allocation and execution context.
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
      typedef struct _u2_me_road {
        struct _u2_me_road* par_u;          //  parent road
        struct _u2_me_road* kid_u;          //  child road list
        struct _u2_me_road* nex_u;          //  sibling road

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
          u2_me_fbox* fre_u[u2_me_fbox_no]; //  heap by node size log
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
#     define  u2_me_is_north  ((u2R->cap_w > u2R->hat_w) ? u2_yes : u2_no)
#     define  u2_me_is_south  ((u2_yes == u2_me_is_north) ? u2_no : u2_yes)

#     define  u2_me_open      ( (u2_yes == u2_me_is_north) \
                                  ? (c3_w)(u2R->cap_w - u2R->hat_w) \
                                  : (c3_w)(u2R->hat_w - u2R->cap_w) )

#     define  u2_me_into(x) (u2_Loom + (x))
#     define  u2_me_outa(p) (((c3_w*)(void*)(p)) - u2_Loom)


  /** Functions.
  **/ 
    /** Reading nouns.
    **/
      /* u2_mr_du(): u2_yes iff `a` is cell.
      */
        u2_bean
        u2_mr_du(u2_noun a);

      /* u2_mr_ud(): u2_no iff `a` is cell.
      */
        u2_bean
        u2_mr_ud(u2_noun a);

      /* u2_mr_at(): fragment `a` of `b`, or none.
      */
        u2_weak
        u2_mr_at(u2_atom a,
                 u2_weak b);

      /* u2_mr_mean():
      **
      **   Attempt to deconstruct `a` by axis, noun pairs; 0 terminates.
      **   Axes must be sorted in tree order.
      */
        u2_bean
        u2_mr_mean(u2_noun a,
                   ...);

      /* u2_mr_mug():
      **
      **   Compute and/or recall the mug (31-bit hash) of (a).
      */
        c3_w
        u2_mr_mug(u2_noun a);

      /* u2_mr_mug_string():
      **
      **   Compute the mug of `a`, LSB first.
      */
        c3_w
        u2_mr_mug_string(const c3_c *a_c);

      /* u2_mr_mug_words():
      **
      **   Compute the mug of `buf`, `len`, LSW first.
      */
        c3_w
        u2_mr_mug_words(const c3_w *buf_w,
                        c3_w        len_w);

      /* u2_mr_mug_cell():
      **
      **   Compute the mug of `[a b]`.
      */
        c3_w
        u2_mr_mug_cell(u2_noun a,
                       u2_noun b);

      /* u2_mr_mug_trel():
      **
      **   Compute the mug of `[a b c]`.
      */
        c3_w
        u2_mr_mug_trel(u2_noun a,
                       u2_noun b,
                       u2_noun c);

      /* u2_mr_mug_qual():
      **
      **   Compute the mug of `[a b c d]`.
      */
        c3_w
        u2_mr_mug_qual(u2_noun a,
                       u2_noun b,
                       u2_noun c,
                       u2_noun d);

      /* u2_mr_mug_both():
      **
      **   Join two mugs.
      */
        c3_w
        u2_mr_mug_both(c3_w a_w,
                       c3_w b_w);


      /* u2_mr_fing():
      **
      **   Yes iff (a) and (b) are the same copy of the same noun.
      **   (Ie, by pointer equality - u2_mr_sing with false negatives.)
      */
        u2_bean
        u2_mr_fing(u2_noun a,
                   u2_noun b);

      /* u2_mr_fing_c():
      **
      **   Yes iff (b) is the same copy of the same noun as the C string [a].
      */
        u2_bean
        u2_mr_fing_c(const c3_c* a_c,
                     u2_noun     b);

      /* u2_mr_fing_cell():
      **
      **   Yes iff `[p q]` and `b` are the same copy of the same noun.
      */
        u2_bean
        u2_mr_fing_cell(u2_noun p,
                        u2_noun q,
                        u2_noun b);

      /* u2_mr_fing_mixt():
      **
      **   Yes iff `[p q]` and `b` are the same copy of the same noun.
      */
        u2_bean
        u2_mr_fing_mixt(const c3_c* p_c,
                        u2_noun     q,
                        u2_noun     b);

      /* u2_mr_fing_trel():
      **
      **   Yes iff `[p q r]` and `b` are the same copy of the same noun.
      */
        u2_bean
        u2_mr_fing_trel(u2_noun p,
                        u2_noun q,
                        u2_noun r,
                        u2_noun b);

      /* u2_mr_fing_qual():
      **
      **   Yes iff `[p q r s]` and `b` are the same copy of the same noun.
      */
        u2_bean
        u2_mr_fing_qual(u2_noun p,
                        u2_noun q,
                        u2_noun r,
                        u2_noun s,
                        u2_noun b);

      /* u2_mr_sing():
      **
      **   Yes iff (a) and (b) are the same noun.
      */
        u2_bean
        u2_mr_sing(u2_noun a,
                   u2_noun b);

      /* u2_mr_sing_c():
      **
      **   Yes iff (b) is the same noun as the C string [a].
      */
        u2_bean
        u2_mr_sing_c(const c3_c* a_c,
                     u2_noun     b);

      /* u2_mr_sing_cell():
      **
      **   Yes iff `[p q]` and `b` are the same noun.
      */
        u2_bean
        u2_mr_sing_cell(u2_noun p,
                        u2_noun q,
                        u2_noun b);

      /* u2_mr_sing_mixt():
      **
      **   Yes iff `[p q]` and `b` are the same noun.
      */
        u2_bean
        u2_mr_sing_mixt(const c3_c* p_c,
                        u2_noun     q,
                        u2_noun     b);

      /* u2_mr_sing_trel():
      **
      **   Yes iff `[p q r]` and `b` are the same noun.
      */
        u2_bean
        u2_mr_sing_trel(u2_noun p,
                        u2_noun q,
                        u2_noun r,
                        u2_noun b);

      /* u2_mr_sing_qual():
      **
      **   Yes iff `[p q r s]` and `b` are the same noun.
      */
        u2_bean
        u2_mr_sing_qual(u2_noun p,
                        u2_noun q,
                        u2_noun r,
                        u2_noun s,
                        u2_noun b);

      /* u2_mr_nord():
      **
      **   Return 0, 1 or 2 if `a` is below, equal to, or above `b`.
      */
        u2_atom
        u2_mr_nord(u2_noun a,
                   u2_noun b);

      /* u2_mr_mold():
      **
      **   Divide `a` as a mold `[b.[p q] c]`.
      */
        u2_bean
        u2_mr_mold(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c);

      /* u2_mr_cell():
      **
      **   Divide `a` as a cell `[b c]`.
      */
        u2_bean
        u2_mr_cell(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c);

      /* u2_mr_trel():
      **
      **   Divide `a` as a trel `[b c]`.
      */
        u2_bean
        u2_mr_trel(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c,
                   u2_noun* d);

      /* u2_mr_qual():
      **
      **   Divide (a) as a qual (b c d e).
      */
        u2_bean
        u2_mr_qual(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c,
                   u2_noun* d,
                   u2_noun* e);

      /* u2_mr_p():
      **
      **   & [0] if [a] is of the form [b *c].
      */
        u2_bean
        u2_mr_p(u2_noun  a,
                u2_noun  b,
                u2_noun* c);

      /* u2_mr_pq():
      **
      **   & [0] if [a] is of the form [b *c d].
      */
        u2_bean
        u2_mr_pq(u2_noun  a,
                 u2_noun  b,
                 u2_noun* c,
                 u2_noun* d);

      /* u2_mr_pqr():
      **
      **   & [0] if [a] is of the form [b *c *d *e].
      */
        u2_bean
        u2_mr_pqr(u2_noun  a,
                  u2_noun  b,
                  u2_noun* c,
                  u2_noun* d,
                  u2_noun* e);

      /* u2_mr_pqrs():
      **
      **   & [0] if [a] is of the form [b *c *d *e *f].
      */
        u2_bean
        u2_mr_pqrs(u2_noun  a,
                   u2_noun  b,
                   u2_noun* c,
                   u2_noun* d,
                   u2_noun* e,
                   u2_noun* f);

      /* u2_mr_met():
      **
      **   Return the size of (b) in bits, rounded up to
      **   (1 << a_y).
      **
      **   For example, (a_y == 3) returns the size in bytes.
      */
        c3_w
        u2_mr_met(c3_y    a_y,
                  u2_atom b);

      /* u2_mr_bit():
      **
      **   Return bit (a_w) of (b).
      */
        c3_b
        u2_mr_bit(c3_w    a_w,
                  u2_atom b);

      /* u2_mr_byte():
      **
      **   Return byte (a_w) of (b).
      */
        c3_y
        u2_mr_byte(c3_w    a_w,
                   u2_atom b);

      /* u2_mr_bytes():
      **
      **   Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u2_mr_bytes(c3_w    a_w,
                    c3_w    b_w,
                    c3_y*   c_y,
                    u2_atom d);

      /* u2_mr_chop():
      **
      **   Into the bloq space of `met`, from position `fum` for a
      **   span of `wid`, to position `tou`, XOR from atom `src`
      **   into ray `dst`.
      */
        void
        u2_mr_chop(c3_g    met_g,
                   c3_w    fum_w,
                   c3_w    wid_w,
                   c3_w    tou_w,
                   c3_w*   dst_w,
                   u2_atom src);

      /* u2_mr_mp():
      **
      **   Copy (b) into (a_mp).
      */
        void
        u2_mr_mp(mpz_t   a_mp,
                 u2_atom b);

      /* u2_mr_word():
      **
      **   Return word (a_w) of (b).
      */
        c3_w
        u2_mr_word(c3_w    a_w,
                   u2_atom b);

      /* u2_mr_words():
      **
      **  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u2_mr_words(c3_w    a_w,
                    c3_w    b_w,
                    c3_w*   c_w,
                    u2_atom d);

      /* u2_mr_string(): `a`, a text atom, as malloced C string.
      */
        c3_c*
        u2_mr_string(u2_atom a);

      /* u2_mr_tape(): `a`, a list of bytes, as malloced C string.
      */
        c3_y*
        u2_mr_tape(u2_noun a);


    /** System management.
    **/
      /* u2_me_boot(): make u2R and u2H from `len` words at `mem`.
      */
        void
        u2_me_boot(void* mem_v, c3_w len_w);

      /* u2_me_trap(): setjmp within road.
      */
#if 0
        u2_bean
        u2_me_trap(void);
#else
#       define u2_me_trap() (u2_noun)(setjmp(u2R->esc.buf))
#endif

      /* u2_me_bail(): bail out.  Does not return.
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
        u2_me_bail(c3_m how_m);

      /* u2_me_grab(): garbage-collect memory.  Asserts u2R == u2H.
      */
        void
        u2_me_grab(void);

      /* u2_me_check(): checkpoint memory to file.  Asserts u2R == u2H.
      */
        void
        u2_me_check(void);

      /* u2_me_fall(): return to parent road.
      */
        void
        u2_me_fall(void);

      /* u2_me_leap(): advance to inner road.
      */
        void
        u2_me_leap(void);

      /* u2_me_golf(): record cap length for u2_flog().
      */
        c3_w
        u2_me_golf(void);

      /* u2_me_flog(): pop the cap.
      **
      **    A common sequence for inner allocation is:
      **
      **    c3_w gof_w = u2_me_golf();
      **    u2_me_leap();
      **    //  allocate some inner stuff...
      **    u2_me_fall();
      **    //  inner stuff is still valid, but on cap
      **    u2_me_flog(gof_w);
      **
      ** u2_me_flog(0) simply clears the cap.
      */
        void
        u2_me_flog(c3_w gof_w);

      /* u2_me_water(): produce high and low watermarks.  Asserts u2R == u2H.
      */
        void
        u2_me_water(c3_w *low_w, c3_w *hig_w);


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
        /* u2_me_gain(): gain and/or copy juniors.
        */
          u2_weak
          u2_me_gain(u2_weak som);

        /* u2_me_lose(): lose a reference.
        */
          void
          u2_me_lose(u2_weak som);

        /* u2_me_use(): reference count.
        */
          c3_w
          u2_me_use(u2_noun som);


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

      /* General constructors.
      */
        /* u2_me_words():
        **
        **   Copy [a] words from [b] into an atom.
        */
          u2_noun
          u2_me_words(c3_w        a_w,
                      const c3_w* b_w);

        /* u2_me_bytes():
        **
        **   Copy `a` bytes from `b` to an LSB first atom.
        */
          u2_noun
          u2_me_bytes(c3_w        a_w,
                      const c3_y* b_y);

        /* u2_me_mp():
        **
        **   Copy the GMP integer `a` into an atom, and clear it.
        */
          u2_noun
          u2_me_mp(mpz_t a_mp);

        /* u2_me_vint():
        **
        **   Create `a + 1`.
        */
          u2_noun
          u2_me_vint(u2_noun a);

        /* u2_me_cons():
        **
        **   Produce the cell `[a b]`.
        */
          u2_noun
          u2_me_cons(u2_noun a, u2_noun b);

        /* u2_me_molt():
        **
        **   Mutate `som` with a 0-terminated list of axis, noun pairs.
        **   Axes must be cats (31 bit).
        */
          u2_noun 
          u2_me_molt(u2_noun som, ...);

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
