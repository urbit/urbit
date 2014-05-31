/* include/loom.h
**
** This file is in the public domain.
*/
  /** Global variables and definitions.
  **/
#   define Loom ((c3_w *)U2_OS_LoomBase)
#   define LoomBits U2_OS_LoomBits
#   define LoomSize (1 << LoomBits)
#   define HalfBits (U2_OS_LoomBits - 1)
#   define HalfSize (1 << HalfBits)
#   define HalfEnd  (HalfSize - 1)
#   define LoomPageWords      12            //  16K pages
#   define LoomAllPages       (1 << 16)     //  65536 pages
#   define LoomHalfPages      (1 << 15)     //  32768 pages
#   define LoomVersion        0xdeafbee

  /** Data structures.
  **/
    /* u2_life: page state.
    */
      typedef enum {
        u2_page_none = 0,                   //  page is not mapped
        u2_page_slow = 1,                   //  page not yet loaded (not used)
        u2_page_neat = 2,                   //  page is unmodified
        u2_page_tref = 3                    //  page is dirty
      } u2_page_life;

      typedef struct {
        u2_page_life lif_e: 2;              //  page state
        c3_w         mug_e: 30;             //  30-bit mug
      } u2_chit;

    /* u2_chef: memory control, per whole, on disk.
    */
      typedef struct _u2_chef {             //  control image
        c3_d ent_d;                         //  event number
        c3_w ven_w;                         //  version
        c3_w bot_w;                         //  offset in loom
        c3_w pgs_w;                         //  pages saved
      } u2_chef;

    /* u2_cheg: mapped segment.  always grows upward for now.
    */
      typedef struct _u2_cheg {
        c3_c*            nam_c;             //  name as a file
        struct _u2_cheg* nex_u;             //  linked in list
        c3_w             bot_w;             //  beginning page
        c3_w             len_w;             //  length in pages
        c3_w             pgs_w;             //  pages in used prefix
        c3_i             ctl_i;             //  control file
        c3_i             dat_i;             //  data file
      } u2_cheg;

#   ifdef U2_GLOBAL
      /* Memory control structures.
      */
        volatile u2_chit LoomChem[LoomAllPages];          //  memory

      /* Direct segment pointers.
      */
        u2_cheg LoomSegmentD = { "d" };
        u2_cheg LoomSegmentC = { "c", &LoomSegmentD };
        u2_cheg LoomSegmentB = { "b", &LoomSegmentC };
        u2_cheg LoomSegmentA = { "a", &LoomSegmentB };

      /* Critical-section bit, for signal handlers.
      */
        volatile uint32_t u2_Critical;

#   else
      extern uint32_t LoomFrame;
      extern volatile sig_atomic_t LoomStop;
      extern volatile sig_atomic_t LoomIntr;
      extern volatile uint32_t u2_Critical;

      extern volatile u2_chit LoomChem[LoomAllPages];
      extern u2_cheg LoomSegmentA, LoomSegmentB, LoomSegmentC, LoomSegmentD;
#   endif

#     define   LoomFrameMax  8192
#     define   LoomSink \
        (LoomFrame == LoomFrameMax ? (LoomStop = 1) : LoomFrame++)
#     define   LoomRise \
        (LoomFrame--)

// #   define LoomFold

  /** Data types.
  **/
    /** Bitfields.  Assumes HalfBits == 28 (the maximum).
    ***
    ***     u2_nit - word offset in loom
    ***       &&&&:28a
    ***         a: offset from loom
    ***
    ***     u2_ray - word offset on beam
    ***       &&&:1a:28b
    ***         a: beam (& = low = west, | = high = east)
    ***         b: offset on beam
    ***
    ***     u2_noun - noun
    ***       ?(u2_cat, u2_dog)
    ***       1a:31b
    ***         a: cat if 0, dog if 1
    ***
    ***     u2_weak - noun
    ***       ?(noun, u2_none)
    ***
    ***     u2_dog - indirect atom or cell
    ***       ?(u2_pom, u2_pug)
    ***       |:29a:1b:1c
    ***         a: ray to data
    ***         b: boxed bit
    ***
    ***     u2_cat - direct atom
    ***       &:31a
    ***         a: unsigned integer
    ***
    ***     u2_pom - indirect cell
    ***       |:29a:1b:&
    ***         a: ray to struct u2_cell
    ***         b: allocated bit (0 == fixed, 1 == allocated)
    ***
    ***     u2_pug - indirect atom
    ***       |:29a:1b:|
    ***         a: ray to struct u2_atom
    ***         b: allocated bit (0 == fixed, 1 == allocated)
    **/
      typedef c3_w u2_nit;
      typedef c3_w u2_ray;
      typedef c3_w u2_fly;
      typedef c3_w u2_dog;
      typedef c3_w u2_cat;
      typedef c3_w u2_pug;

    /** Structures.
    ***
    **/
      /* u2_atom, u2_cell: logical atom and cell structures.
      **
      ** Not referenced as such, since in ray space.
      */
        typedef struct _u2_loom_atom {
          c3_w mug_w;
          c3_w len_w;
          c3_w buf_w[0];
        } u2_loom_atom;

        typedef struct _u2_loom_cell {
          c3_w   mug_w;
          u2_ray hed_r;
          u2_ray tel_r;
        } u2_loom_cell;

  /** Basic macros.
  **/
    /** Bitfield unpacking.  See above.
    **/
#     define u2_ray_a(ray)     ( (ray) >> HalfBits )
#     define u2_ray_b(ray)     ( (ray) & ((1 << HalfBits) - 1) )

#     define u2_fly_a(fly)     ( (fly) >> 31 )

#     define u2_dog_a(dog)     ( ((dog) &~ (1 << 31)) >> 2 )
#     define u2_dog_b(dog)     ( 1 & ((dog) >> 1) )
#     define u2_dog_c(dog)     ( 1 & (dog) )

#     define u2_pom_a(pom)     u2_dog_a(pom)
#     define u2_pom_b(pom)     u2_dog_b(pom)
#     define u2_pug_a(pug)     u2_dog_a(pug)
#     define u2_pug_b(pug)     u2_dog_b(pug)

#     define u2_fly_is_cat(fly) ( !u2_fly_a(fly) )
#     define u2_fly_is_dog(fly) ( u2_fly_a(fly) )

#     define u2_dog_is_pom(dog)   ( !u2_dog_c(dog) )
#     define u2_dog_is_pug(dog)   ( u2_dog_c(dog) )

#     define u2_dog_is_all(dog) ( u2_dog_b(dog) )

#     define u2_fly_is_atom(a) \
        (u2_fly_is_cat(a) || u2_dog_is_pug(a))


    /** Bitfield packing.  See above.
    **/
#     define u2_ray_of(a, b)   ( ((a) << HalfBits) | (b) )

#     define u2_dog_of(a, b, c) \
        ( (1 << 31) | ((a) << 2) | ((b) << 1) | (c) )

#     define u2_pom_of(a, b)   u2_dog_of(a, b, 0)
#     define u2_pug_of(a, b)   u2_dog_of(a, b, 1)


    /** Cage reference and geometry.
    ***
    *** Formally, we are always folded.
    **/
#     define u2_ray_fnit(ray) \
        ( u2_ray_a(ray) ? (HalfEnd - u2_ray_b(ray)) : (ray) )
#   ifdef LoomFold
#     define u2_ray_nit(ray) u2_ray_fnit(ray)
#   else
#     define u2_ray_nit(ray) (ray)
#   endif

#     define u2_ray_open(tid, tat, das) \
        ( (0 == u2_ray_a(tid)) \
            ? ( (u2_ray_fnit(tid) + (das)) < u2_ray_fnit(tat) ) \
            : ( (u2_ray_fnit(tat) + (das)) < u2_ray_fnit(tid) ) )

#     define u2_ray_gap(tid, tat) \
        ( (HalfSize - (u2_ray_b(tid) + u2_ray_b(tat))) )

      /* last word of opposite is first word of other.  so, in a 4-word loom:
      **
      **  0    1    2    3
      **  |    |    |    |
      **  ##   ##   ##   ##
      **   |    |    |    |
      **   3    2    1    0
      */

#     define u2_ray_over(ray, siz_w) \
        u2_ray_of(!u2_ray_a(ray), (HalfSize - (u2_ray_b(ray) + (siz_w))))

#     define u2_at_nit(nit)    (Loom + nit)
#     define u2_at_ray(ray)    u2_at_nit(u2_ray_nit(ray))
#     define u2_nit_at(ptr_v)  (((c3_w *)(ptr_v)) - Loom)

#   ifdef LoomFold
#     define u2_at_cord(ray, siz) \
        (u2_ray_a(ray) ? (u2_at_ray(ray) - ((siz) - 1)) : u2_at_ray(ray))
#   else
#     define u2_at_cord(ray, siz) u2_at_ray(ray)
#   endif

#     define u2_aftr(ray, type, field) \
        ((ray) + \
         ( ((c3_w *)&((type *)0)->field) - \
           ((c3_w *)0) ) \
        )
#     define u2_fore(ray, type, field) \
        ((ray - 1) - \
         ( ((c3_w *)&((type *)0)->field) - \
           ((c3_w *)0) ) \
        )

#     define u2_at(ray, type, field) \
        u2_at_ray(u2_aftr(ray, type, field))
#     define u2_by(ray, type, field) \
        u2_at_ray(u2_fore(ray, type, field))

#     define u2_be(ray, type, item, field) \
        ( (item *)(void *)u2_at_ray(u2_aftr(ray, type, field)) )

#     define u2_ray_beam(ray)   u2_ray_a(ray)
#     define u2_ray_point(ray)  u2_ray_b(ray)

#     define u2_dog_ray(dog)    u2_dog_a(dog)

#     define u2_dog_beam(dog)   u2_ray_beam(u2_dog_ray(dog))


    /*** Noun structure access.
    ***/
#     define u2_at_dog_mug(a)   u2_at_ray(u2_dog_a(a))

#     define u2_at_pom_hed(a)   u2_at_ray((1 + u2_pom_a(a)))
#     define u2_at_pom_tel(a)   u2_at_ray((2 + u2_pom_a(a)))

#     define u2_at_pug_len(a)   u2_at_ray((1 + u2_pug_a(a)))
#     define u2_at_pug_buf(a, b) \
        u2_at_ray((2 + (b) + u2_pug_a(a)))

#     define u2_atom_word(a, b) \
        ( u2_fly_is_cat(a) ? (b ? 0 : (a)) : *u2_at_pug_buf(a, b) )

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

  /** Constants and macros.
  **/
    /* u2_none:
    **
    **   An exceptional value, comparable to NaN for floats.
    */
#     define u2_none u2_dog_of(u2_ray_of(0, 0), 0, 0)

    /* Small atoms.
    */
#     define _0   0
#     define _1   1
#     define _2   2
#     define _3   3
#     define _4   4
#     define _5   5
#     define _6   6
#     define _7   7
#     define _8   8
#     define _9   9
#     define _10  10
#     define _11  11
#     define _12  12
#     define _13  13
#     define _14  14
#     define _15  15

    /* Nock operators.
    */
#     define u2_nock_frag  0
#     define u2_nock_bone  1
#     define u2_nock_sail  2
#     define u2_nock_dust  3
#     define u2_nock_vint  4
#     define u2_nock_sing  5
#     define u2_nock_trol  6
#     define u2_nock_flac  7
#     define u2_nock_gant  8
#     define u2_nock_kick  9
#     define u2_nock_hint  10

#     define u2_nock_0  0
#     define u2_nock_1  1
#     define u2_nock_2  2
#     define u2_nock_3  3
#     define u2_nock_4  4
#     define u2_nock_5  5
#     define u2_nock_6  6
#     define u2_nock_7  7
#     define u2_nock_8  8
#     define u2_nock_9  9
#     define u2_nock_10  10


    /* New (Nock 5) conventional axes for gate call.
    */
#if 1
#     define u2_cv_pay      3       //  payload
#     define u2_cv_sam      6       //  sample
#       define u2_cv_sam_1  6
#       define u2_cv_sam_2  12
#       define u2_cv_sam_3  13
#       define u2_cv_sam_4  24
#       define u2_cv_sam_5  25
#       define u2_cv_sam_6  26
#       define u2_cv_sam_12 52
#       define u2_cv_sam_13 53
#       define u2_cv_sam_7  27
#     define u2_cv_con      7       //  context
#     define u2_cv_con_2    14      //  context
#     define u2_cv_con_3    15      //  context
#     define u2_cv_con_sam  30      //  sample in gate context
#     define u2_cv_noc      2       //  deprecated
#     define u2_cv_bat      2       //  battery
#endif

    /* u2_yes, u2_no, u2_nul;
    **
    **   Our Martian booleans and list terminator; empty string.
    */
#     define u2_yes   0
#     define u2_no    1
#     define u2_nul   0
#     define u2_blip  0

    /* Tools for Martian booleans.
    */
#     define u2_so(x)      (u2_yes == (x))
#     define u2_say(x)     ( (x) ? u2_yes : u2_no )
#     define u2_not(x)     ( (x == u2_yes) ? u2_no : u2_yes )
#     define u2_and(x, y)  ( (u2_so(x) && u2_so(y)) ? u2_yes : u2_no )
#     define u2_or(x, y)   ( (u2_so(x) || u2_so(y)) ? u2_yes : u2_no )

  /** Typedefs.
  ***
  *** Noun typedefs may reflect usage, structure, or both.  Any noun
  *** typedef is a noun - that is, it may not be (u2_none).
  **/
    /* u2_noun:
    **
    **   A fly which is not u2_none.
    */
      typedef u2_fly u2_noun;

    /* u2_weak:
    **
    **   A fly which may be (and must be tested for) u2_none.
    */
      typedef u2_fly u2_weak;

    /* u2_atom:
    **
    **   A noun which is an atom.
    */
      typedef u2_noun u2_atom;

    /* u2_cell:
    **
    **   A noun which is a cell.
    */
      typedef u2_noun u2_cell;

    /* u2_bean:
    **
    **   A Martian boolean (0 = yes, 1 = no).
    */
      typedef u2_noun u2_bean;

    /* u2_axis:
    **
    **   An atomic tree address.
    */
      typedef u2_noun u2_axis;

    /* u2_mote:
    **
    **   An atom < (1 << 31).
    */
      typedef u2_atom u2_mote;

    /* u2_term:
    **
    **   A Watt symbol.
    */
      typedef u2_noun u2_term;

    /* u2_list:
    **
    **   A zero-terminated, tailward list.
    */
      typedef u2_noun u2_list;

    /* u2_tool:
    **
    **   A valid or presumptive Nock formula.
    */
      typedef u2_noun u2_tool;

    /* u2_pool:
    **
    **   A set, stored as binary treap by mug.
    */
      typedef u2_noun u2_pool;

    /* u2_book:
    **
    **   An associative array, stored as binary treap by mug.
    */
      typedef u2_noun u2_book;

    /* u2_unit:
    **
    **   A conditional (maybe), `[0 n]` or `0`.
    */
      typedef u2_noun u2_unit;

    /** Functions.
    **/
      /** Lifecycle.
      **/
        /* u2_loom_boot():
        **
        **   Instantiate the loom as a new image.
        */
          void
          u2_loom_boot(void);

        /* u2_loom_clip():
        **
        **   Clip top and bottom.
        */
          void
          u2_loom_clip(c3_w bot_w, c3_w top_w);

        /* u2_loom_load():
        **
        **   Load the loom from a file.
        */
          u2_bean
          u2_loom_load(void);

        /* u2_loom_exit():
        **
        **   Quit out of the loom.
        */
          void
          u2_loom_exit(void);

        /* u2_loom_save():
        **
        **   Save the current loom at the current event number.
        */
          u2_bean
          u2_loom_save(c3_d ent_d);

      /** General.
      **/

        /* u2_dust():
        **
        **   Yes iff (a) is a cell.
        */
#if 0
          u2_bean
          u2_dust(u2_noun a)
#else
#         define u2_dust(a) \
            (u2_fly_is_atom(a) ? u2_no : u2_yes)
#endif

        /* u2_frag():
        **
        **   Return fragment (a) of (b), or u2_none if not applicable.
        */
          u2_weak
          u2_frag(u2_atom a,
                  u2_noun b);

        /* u2_mean():
        **
        **   Attempt to deconstruct `a` by axis, noun pairs; 0 terminates.
        **   Axes must be sorted in tree order.
        */
          u2_bean
          u2_mean(u2_noun a,
                  ...);

        /* u2_mug():
        **
        **   Compute and/or recall the mug (31-bit hash) of (a).
        */
          c3_w
          u2_mug(u2_noun a);

        /* u2_mug_string():
        **
        **   Compute the mug of `a`, LSB first.
        */
          c3_w
          u2_mug_string(const c3_c *a_c);

        /* u2_mug_words():
        **
        **   Compute the mug of `buf`, `len`, LSW first.
        */
          c3_w
          u2_mug_words(const c3_w *buf_w,
                       c3_w        len_w);

        /* u2_mug_cell():
        **
        **   Compute the mug of `[a b]`.
        */
          c3_w
          u2_mug_cell(u2_noun a,
                      u2_noun b);

        /* u2_mug_trel():
        **
        **   Compute the mug of `[a b c]`.
        */
          c3_w
          u2_mug_trel(u2_noun a,
                      u2_noun b,
                      u2_noun c);

        /* u2_mug_qual():
        **
        **   Compute the mug of `[a b c d]`.
        */
          c3_w
          u2_mug_qual(u2_noun a,
                      u2_noun b,
                      u2_noun c,
                      u2_noun d);

        /* u2_mug_both():
        **
        **   Join two mugs.
        */
          c3_w
          u2_mug_both(c3_w a_w,
                      c3_w b_w);

        /* u2_fing():
        **
        **   Yes iff (a) and (b) are the same copy of the same noun.
        **   (Ie, by pointer equality - u2_sing with false negatives.)
        */
          u2_bean
          u2_fing(u2_noun a,
                  u2_noun b);

        /* u2_fing_c():
        **
        **   Yes iff (b) is the same copy of the same noun as the C string [a].
        */
          u2_bean
          u2_fing_c(const c3_c* a_c,
                    u2_noun     b);

        /* u2_fing_cell():
        **
        **   Yes iff `[p q]` and `b` are the same copy of the same noun.
        */
          u2_bean
          u2_fing_cell(u2_noun p,
                       u2_noun q,
                       u2_noun b);

        /* u2_fing_mixt():
        **
        **   Yes iff `[p q]` and `b` are the same copy of the same noun.
        */
          u2_bean
          u2_fing_mixt(const c3_c* p_c,
                       u2_noun     q,
                       u2_noun     b);

        /* u2_fing_trel():
        **
        **   Yes iff `[p q r]` and `b` are the same copy of the same noun.
        */
          u2_bean
          u2_fing_trel(u2_noun p,
                       u2_noun q,
                       u2_noun r,
                       u2_noun b);

        /* u2_fing_qual():
        **
        **   Yes iff `[p q r s]` and `b` are the same copy of the same noun.
        */
          u2_bean
          u2_fing_qual(u2_noun p,
                       u2_noun q,
                       u2_noun r,
                       u2_noun s,
                       u2_noun b);

        /* u2_sing():
        **
        **   Yes iff (a) and (b) are the same noun.
        */
          u2_bean
          u2_sing(u2_noun a,
                  u2_noun b);

        /* u2_sing_c():
        **
        **   Yes iff (b) is the same noun as the C string [a].
        */
          u2_bean
          u2_sing_c(const c3_c* a_c,
                    u2_noun     b);

        /* u2_sing_cell():
        **
        **   Yes iff `[p q]` and `b` are the same noun.
        */
          u2_bean
          u2_sing_cell(u2_noun p,
                       u2_noun q,
                       u2_noun b);

        /* u2_sing_mixt():
        **
        **   Yes iff `[p q]` and `b` are the same noun.
        */
          u2_bean
          u2_sing_mixt(const c3_c* p_c,
                       u2_noun     q,
                       u2_noun     b);

        /* u2_sing_trel():
        **
        **   Yes iff `[p q r]` and `b` are the same noun.
        */
          u2_bean
          u2_sing_trel(u2_noun p,
                       u2_noun q,
                       u2_noun r,
                       u2_noun b);

        /* u2_sing_qual():
        **
        **   Yes iff `[p q r s]` and `b` are the same noun.
        */
          u2_bean
          u2_sing_qual(u2_noun p,
                       u2_noun q,
                       u2_noun r,
                       u2_noun s,
                       u2_noun b);

        /* u2_nord():
        **
        **   Return 0, 1 or 2 if `a` is below, equal to, or above `b`.
        */
          u2_atom
          u2_nord(u2_noun a,
                  u2_noun b);

        /* u2_stud():
        **
        **   Yes iff (a) is an atom.
        */
#if 0
          u2_bean
          u2_stud(u2_noun a)
#else
#         define u2_stud(a) \
            (u2_fly_is_atom(a) ? u2_yes : u2_no)
#endif

      /** Cell access.
      **/
        /* u2_as_bush():
        **
        **   Factor [a] as a bush [b.[p q] c].
        */
          u2_bean
          u2_as_bush(u2_noun  a,
                     u2_noun* b,
                     u2_noun* c);

        /* u2_as_cell():
        **
        **   Factor (a) as a cell (b c).
        */
          u2_bean
          u2_as_cell(u2_noun  a,
                     u2_noun* b,
                     u2_noun* c);

        /* u2_as_hext():
        **
        **   Factor (a) as a hext (b c d e f g)
        */
          u2_bean
          u2_as_hext(u2_noun  a,
                     u2_noun* b,
                     u2_noun* c,
                     u2_noun* d,
                     u2_noun* e,
                     u2_noun* f,
                     u2_noun* g);

        /* u2_as_p():
        **
        **   & [0] if [a] is of the form [b *c].
        */
          u2_bean
          u2_as_p(u2_noun  a,
                  u2_noun  b,
                  u2_noun* c);

        /* u2_as_pq():
        **
        **   & [0] if [a] is of the form [b *c d].
        */
          u2_bean
          u2_as_pq(u2_noun  a,
                   u2_noun  b,
                   u2_noun* c,
                   u2_noun* d);

        /* u2_as_pqr():
        **
        **   & [0] if [a] is of the form [b *c *d *e].
        */
          u2_bean
          u2_as_pqr(u2_noun  a,
                    u2_noun  b,
                    u2_noun* c,
                    u2_noun* d,
                    u2_noun* e);

        /* u2_as_pqrs():
        **
        **   & [0] if [a] is of the form [b *c *d *e *f].
        */
          u2_bean
          u2_as_pqrs(u2_noun  a,
                     u2_noun  b,
                     u2_noun* c,
                     u2_noun* d,
                     u2_noun* e,
                     u2_noun* f);

        /* u2_as_qual():
        **
        **   Factor (a) as a qual (b c d e).
        */
          u2_bean
          u2_as_qual(u2_noun  a,
                     u2_noun* b,
                     u2_noun* c,
                     u2_noun* d,
                     u2_noun* e);

        /* u2_as_quil():
        **
        **   Factor (a) as a quil (b c d e f).
        */
          u2_bean
          u2_as_quil(u2_noun  a,
                     u2_noun* b,
                     u2_noun* c,
                     u2_noun* d,
                     u2_noun* e,
                     u2_noun* f);

        /* u2_as_trel():
        **
        **   Factor (a) as a trel (b c d).
        */
          u2_bean
          u2_as_trel(u2_noun a,
                     u2_noun *b,
                     u2_noun *c,
                     u2_noun *d);

        /* u2_h(), u2_sh():
        **
        **   Return the head of `a`, unsafely or safely.
        */
#if 0
          u2_noun
          u2_h(u2_noun a);
#else
#         define u2_h(a)    (*u2_at_pom_hed(a))
#endif
          u2_noun
          u2_sh(u2_noun a);

        /* u2_t(), u2_st():
        **
        **   Return the tail of `a`, unsafely or safely.
        */
#if 0
          u2_noun
          u2_t(u2_noun a);
#else
#         define u2_t(a) (*u2_at_pom_tel(a))
#endif
          u2_noun
          u2_st(u2_noun a);

      /** Atom access.
      **/
        /* u2_met():
        **
        **   Return the size of (b) in bits, rounded up to
        **   (1 << a_y).
        **
        **   For example, (a_y == 3) returns the size in bytes.
        */
          c3_w
          u2_met(c3_y    a_y,
                 u2_atom b);

        /* u2_bit():
        **
        **   Return bit (a_w) of (b).
        */
          c3_b
          u2_bit(c3_w    a_w,
                 u2_atom b);

        /* u2_byte():
        **
        **   Return byte (a_w) of (b).
        */
          c3_y
          u2_byte(c3_w    a_w,
                  u2_atom b);

        /* u2_bytes():
        **
        **   Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
        */
          void
          u2_bytes(c3_w    a_w,
                   c3_w    b_w,
                   c3_y*   c_y,
                   u2_atom d);

        /* u2_chop():
        **
        **   Into the bloq space of `met`, from position `fum` for a
        **   span of `wid`, to position `tou`, XOR from atom `src`
        **   into ray `dst`.
        */
          void
          u2_chop(c3_g    met_g,
                  c3_w    fum_w,
                  c3_w    wid_w,
                  c3_w    tou_w,
                  u2_ray  dst_r,
                  u2_atom src);

        /* u2_mp():
        **
        **   Copy (b) into (a_mp).
        */
          void
          u2_mp(mpz_t   a_mp,
                u2_atom b);

        /* u2_word():
        **
        **   Return word (a_w) of (b).
        */
          c3_w
          u2_word(c3_w    a_w,
                  u2_atom b);

        /* u2_chub():
        **
        **   Return double-word (a_w) of (b).
        */
          c3_d
          u2_chub(c3_w    a_w,
                  u2_atom b);

        /* u2_words():
        **
        **  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
        */
          void
          u2_words(c3_w    a_w,
                   c3_w    b_w,
                   c3_w*   c_w,
                   u2_atom d);
