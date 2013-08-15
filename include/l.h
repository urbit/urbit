/* include/l.h
**
** This file is in the public domain.
**
** Prefixes:
**
**   u3_l   (loom)
**   u3_li  (loom internal)
**   u3_ln  (loom generate)
**   u3_lr  (loom read)
**   u3_lm  (loom management)
**
** Description:
**
**   The loom system, a linear noun allocator.
**
**   A "noun" is either an "atom" (an unsigned integer of any size),
**   or a "cell" (an ordered pair of any two nouns).
*/

  /** See Spec/zeno/2.txt for a discussion of the loom.
  **/

  /** Data types.
  **/
    /** Bitfields.
    ***
    ***     u3_nit - word offset in loom
    ***       &&&&:28a
    ***         a: offset from loom
    ***
    ***     u3_ray - word offset on beam
    ***       &&&:1a:28b
    ***         a: beam (& = low = west, | = high = east)
    ***         b: offset on beam
    ***
    ***     u3_rat - noun
    ***       ?(cat, dog)
    ***       1a:31b
    ***         a: cat if 0, dog if 1
    ***
    ***     u3_dog - indirect atom or cell
    ***       ?(pom, pug)
    ***       |:29a:1b:1c
    ***         a: ray to data
    ***         b: reserved for extension bit
    ***
    ***     u3_cat - direct atom
    ***       &:31a
    ***         a: unsigned integer
    ***
    ***     u3_pom - indirect cell
    ***       |:29a:1b:&
    ***         a: ray to struct u3_cell
    ***         b: reserved for extension bit
    ***
    ***     u3_pug - indirect atom
    ***       |:29a:1b:|
    ***         a: ray to struct u3_atom
    ***         b: reserved for extension bit
    **/
      typedef c3_w u3_nit;
      typedef c3_w u3_ray;
      typedef c3_w u3_rat;
      typedef c3_w u3_dog;
      typedef c3_w u3_cat;
      typedef c3_w u3_pug;


    /** Structures.
    ***
    ***   u3_loom - the loom
    ***   u3_cell - cell structure (in beam space - do not use)
    ***   u3_atom - atom structure (in beam space - do not use)
    **/
      /* u3_loom: the loom itself.
      */
        struct u3_loom {
          /* bat: east end of the loom.
          */
          u3_nit bat_nit;

          /* cap, hat, mat, rut: allocation pointers.
          */
          u3_ray  cap_ray;
          u3_ray  hat_ray;
          u3_ray  mat_ray;
          u3_ray  rut_ray;
          c3_w    way;
 
          /* cop: count of words copied.
          */
          c3_w   cop_w;
        };
        typedef struct u3_loom *u3_l;
        typedef void *u3_lv;

      /* u3_atom, u3_cell: atom and cell structures.
      */
        struct u3_atom {
          c3_w mug_w;
          c3_w len_w;
          c3_w buf_w[0];
        };

        struct u3_cell {
          c3_w   mug_w;
          u3_ray hed_ray;
          u3_ray tel_ray;
        };
  
      /* u3_mane: simple memory allocator.
      */
        struct u3_mane {
#if 0
          /* Free lists per word count, 2^30 down to 2^4.
          */
          u3_ray lib_ray[27];
#else
          /* Free list - singular.
          */
          u3_ray lib_ray;
#endif
          /* East and west control bars.  Nouns in the allocator
          ** can point to any noun between these rays.
          */
          u3_ray est_ray;
          u3_ray wst_ray;
        };
        struct u3_bloc {
          /* Bit 0: free/allocated.  Bits 1-31: length.
          */
          c3_w fer_w;

          /* Next bloc in this free list.
          */
          u3_ray nex_ray;

          /* Previous bloc in this free list.
          */
          u3_ray pre_ray;
        };

      /* u3_pair: internal name-value pair.
      */
        struct u3_pair {
          u3_rat nam;
          u3_rat val;
        };

      /* u3_sham:
      **
      **    16-way mug-powered hashtable.  If wun != u3_none,
      **    table is not allocated.
      */
        struct u3_sham {
          /* 16 subordinate values:
          **
          **  [nam val]:     name-value pair
          **  [u3_none ray]: subtable
          **  [u3_none 0]:   empty
          */
          struct u3_pair dol_ray[16];
        };
      
      /* u3_bask:
      **
      **    Memory basket.
      */ 
        struct u3_bask {
          /* Total words in basket.
          */
          c3_w siz_w;

          /* Memory allocator.
          */
          struct u3_mane m;

          /* Hash table.
          */
          struct u3_sham s;
        };

  /** Basic macros.
  **/
    /** Bitfield unpacking.  See above.
    **/
#     define u3_ray_a(ray)     ( (ray) >> 28 )
#     define u3_ray_b(ray)     ( (ray) & ((1 << 28) - 1) )

#     define u3_rat_a(rat)     ( (rat) >> 31 )

#     define u3_dog_a(dog)     ( ((dog) &~ (1 << 31)) >> 2 )
#     define u3_dog_b(dog)     ( 1 & ((dog) >> 1) )
#     define u3_dog_c(dog)     ( 1 & (dog) )

#     define u3_pom_a(pom)     u3_dog_a(pom)
#     define u3_pom_b(pom)     u3_dog_b(pom)
#     define u3_pug_a(pug)     u3_dog_a(pug)
#     define u3_pug_b(pug)     u3_dog_b(pug)

#     define u3_rat_is_cat(rat) ( !u3_rat_a(rat) )
#     define u3_rat_is_dog(rat) ( u3_rat_a(rat) )

#     define u3_dog_is_pom(dog) ( !u3_dog_c(dog) )
#     define u3_dog_is_pug(dog) ( u3_dog_c(dog) )

#     define u3_rat_is_atom(a) \
        (u3_rat_is_cat(a) || u3_dog_is_pug(a))
       

    /** Bitfield packing.  See above.
    **/
#     define u3_ray_of(a, b)   ( ((a) << 28) | (b) )

#     define u3_dog_of(a, b, c) \
        ( (1 << 31) | ((a) << 2) | ((b) << 1) | (c) )

#     define u3_pom_of(a, b)   u3_dog_of(a, b, 0)
#     define u3_pug_of(a, b)   u3_dog_of(a, b, 1)


    /** Cage reference and geometry.
    **/
#     define u3_ray_nit(l, ray) \
        ( u3_ray_a(ray) ? ((l)->bat_nit - u3_ray_b(ray)) : (ray) )

#     define u3_at_nit(l, nit)   ( ((c3_w *) (l)) + (nit) )
#     define u3_at_ray(l, ray)    u3_at_nit(l, u3_ray_nit(l, ray))

#     define u3_of(l, ray, type, field) \
        ((ray) + \
         ( ((c3_w *)&((type *)0)->field) - \
           ((c3_w *)0) ) \
        ) 
#     define u3_to(l, ray, type, field) \
        u3_at_ray(l, u3_of(l, ray, type, field))

#     define u3_ray_beam(ray)   u3_ray_a(ray)
#     define u3_ray_point(ray)  u3_ray_b(ray)

#     define u3_dog_ray(dog)    u3_dog_a(dog)

#     define u3_dog_beam(dog)   u3_ray_beam(u3_dog_ray(dog))

#     define u3_overflow(l, a) \
        ( ( (a) + \
            u3_ray_b((l)->hat_ray) + \
            u3_ray_b((l)->cap_ray) \
          ) >= \
          (l)->bat_nit \
        )

    /*** Noun structure access.
    ***/
#     define u3_at_dog_mug(l, a)   u3_at_ray(l, u3_dog_a(a))

#     define u3_at_pom_hed(l, a)   u3_at_ray(l, (1 + u3_pom_a(a)))
#     define u3_at_pom_tel(l, a)   u3_at_ray(l, (2 + u3_pom_a(a)))

#     define u3_at_pug_len(l, a)   u3_at_ray(l, (1 + u3_pug_a(a)))
#     define u3_at_pug_buf(l, a, b) \
        u3_at_ray(l, (2 + (b) + u3_pug_a(a)))


  /** Constants and macros.
  **/
    /* u3_none:
    **
    **   An exceptional value, comparable to NaN for floats.
    */
#     define u3_none u3_dog_of(u3_ray_of(0, 0), 0, 0)

    /* Distinguished numbers - don't laugh.
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
#     define u3_nock_frag  0
#     define u3_nock_bone  1
#     define u3_nock_sail  2
#     define u3_nock_dust  3
#     define u3_nock_vint  4
#     define u3_nock_sing  5
#     define u3_nock_trol  6
#     define u3_nock_flac  7
#     define u3_nock_gant  8
#     define u3_nock_mung  9
#     define u3_nock_germ  10
#     define u3_nock_hint  11
#     define u3_nock_coat  12

    /* u3_yes, u3_no, u3_nul;
    **
    **   Our Martian booleans and list terminator.
    */
#     define u3_yes 0
#     define u3_no  1
#     define u3_nul 0

    /* Tools for Martian booleans.
    */
#     define u3_so(x)      (u3_yes == (x))
#     define u3_say(x)     ( (x) ? u3_yes : u3_no )
#     define u3_and(x, y)  ( (u3_so(x) && u3_so(y)) ? u3_yes : u3_no )
#     define u3_or(x, y)   ( (u3_so(x) || u3_so(y)) ? u3_yes : u3_no )

#     define u3_nc(l, p, q)        u3_ln_cell(l, p, q)
#     define u3_nt(l, p, q, r)     u3_ln_trel(l, p, q, r)
#     define u3_nq(l, p, q, r, s)  u3_ln_qual(l, p, q, r, s)
#     define u3_nu(l, n)           u3_ln_cell(l, u3_nul, n)
#     define u3_nl(l, n)           u3_ln_cell(l, n, u3_nul)


  /** Typedefs.
  ***
  *** Noun typedefs may reflect usage, structure, or both.  Any noun
  *** typedef is a fox - that is, it may not be (u3_none).
  **/
    /* u3_fox:
    **
    **   A rat which is not u3_none.
    */
      typedef u3_rat u3_fox;

    /* u3_atom:
    **
    **   A fox which is an atom.
    */
      typedef u3_fox u3_atom;

    /* u3_cell:
    **
    **   A fox which is a cell.
    */
      typedef u3_fox u3_cell;

    /* u3_flag:
    **
    **   A Martian boolean (0 = yes, 1 = no).
    */
      typedef u3_fox u3_flag;

    /* u3_mote:
    **
    **   An ASCII string of 2-4 characters.
    */
      typedef u3_fox u3_mote;

    /* u3_list:
    **
    **   A zero-terminated, tailward list.
    */
      typedef u3_fox u3_list;


  /** Functions and hautes-macros.
  **/
    /** Lifecycle, management, and miscellaneous.
    **/
      /* u3_lm_new():
      **
      **   Create a loom, mallocing (1 << a_y) words of memory.  Return
      **   0 if malloc fails.
      **
      **   A loom can be freed with free() - there is no destructor.
      */
        u3_l
        u3_lm_new(c3_y a_y);

      /* u3_lm_alloc():
      **
      **   Allocate (a_w) words of C storage on the hat.
      */
        void *
        u3_lm_alloc(u3_lv lv,
                    c3_w  a_w);

      /* u3_lm_flap():
      **
      **   Reverse the beams forward, returning the old mat.
      */
        u3_ray
        u3_lm_flap(u3_lv lv);

      /* u3_lm_flop():
      **
      **   Reverse the beams backward, restoring the old mat.
      */
        void
        u3_lm_flop(u3_lv  lv,
                   u3_ray mat_ray);

      /* u3_lm_flog():
      **
      **   Release the can.
      */
        void
        u3_lm_flog(u3_lv lv);

      /* u3_lm_mug():
      **
      **   Compute and/or recall the mug (short hash) of (a).
      */
        c3_w
        u3_lm_mug(u3_lv  lv,
                  u3_fox a);

      /* u3_lm_order():
      **
      **   Produce u3_yes iff (a) is below (b) in mug order.
      **
      **   Assumes a and b are not equal.
      */
        u3_flag
        u3_lm_order(u3_lv  lv,
                    u3_fox a,
                    u3_fox b);

      /* u3_lm_nuke():
      **
      **   Zero all data between cap and hat.  For debugging.
      */
        void
        u3_lm_nuke(u3_lv lv);

      /* u3_lm_water():
      **
      **   Return west and east watermarks, respectively.
      */
        void
        u3_lm_water(u3_lv lv,
                    c3_w* maz_w,
                    c3_w* buc_w);

      /* u3_lm_open():
      **
      **   Yes iff [a] more words remain in the pad of [l].
      */
#if 0
        u3_flag
        u3_lm_open(u3_lv lv,
                   c3_w  a_w);
#else
#       define u3_lm_open(lv, a_w) \
          (u3_overflow((u3_l)(void *)(lv), a_w) ? u3_no : u3_yes)
#endif

      /* u3_lm_clear():
      **
      **   Yes iff [lef] does not point to any word >= [lid]
      **   and < [nut].
      */
        u3_flag
        u3_lm_clear(u3_lv  lv,
                    u3_fox lef,
                    u3_ray lid_ray,
                    u3_ray nut_ray);

      /* u3_lm_tamp():
      **
      **   Tamp, eliding the segment from [lid_ray] up to [nut_ray],
      **   preserving the root [lef].
      **
      **   Assumes u3_lm_clear() with the same arguments.
      */
        u3_fox
        u3_lm_tamp(u3_lv  lv,
                   u3_fox lef,
                   u3_ray lid_ray,
                   u3_ray nut_ray);

      /* u3_lm_water():
      **
      **   Return east and west watermarks, respectively.
      */
        void
        u3_lm_water(u3_lv lv,
                    c3_w* maz_w,
                    c3_w* buc_w);

    /** Generation.
    ***
    *** All generators return u3_none if out of memory.
    **/
      /* u3_ln_bytes():
      **
      **   Copy [a_w] bytes from [b_y].
      */
        u3_rat
        u3_ln_bytes(u3_lv       lv,
                    c3_w        a_w,
                    const c3_y* b_y);

      /* u3_ln_string():
      **
      **   u3_ln_bytes(l, strlen(a_c), (c3_y *)a_c);
      */
        u3_rat
        u3_ln_string(u3_lv       lv,
                     const c3_c* a_c);

      /* u3_ln_cell(): 
      **
      **   Produce the cell [a b].
      */
        u3_rat
        u3_ln_cell(u3_lv  lv,
                   u3_fox a,
                   u3_fox b);

      /* u3_ln_ice():
      **
      **   Produce [a], not referencing the cap.
      */
        u3_rat
        u3_ln_ice(u3_lv  lv,
                  u3_fox a);

      /* u3_ln_mp():
      **
      **   Copy the GMP integer (a_mp) into an atom on the hat of (l).
      */
        u3_rat
        u3_ln_mp(u3_lv lv,
                 mpz_t a_mp);

      /* u3_ln_nock():
      **
      **    Execute (nock lan sef) with a trivial interpreter.
      */
        u3_rat
        u3_ln_nock(u3_lv  lv,
                   u3_fox lan,
                   u3_fox sef);

      /* u3_ln_trel(): 
      **
      **   Produce the trel [a b c] on the hat of [l], or u3_none if
      **   this would overflow the loom.
      */
        u3_rat
        u3_ln_trel(u3_lv  lv,
                   u3_fox a,
                   u3_fox b,
                   u3_fox c);

      /* u3_ln_weld():
      **
      **   Weld (b) into a single atom, gluing on (1 << a_y) lines.
      **   Eg, a_y = 3 to concatenate bytes, 0 for bits, 5 for words.
      */
        u3_rat
        u3_ln_weld(u3_lv  lv,
                   c3_y   a_y,
                   u3_fox b);
        
      /* u3_ln_words():
      **
      **   Copy (a_w) words from (b_w) into an atom on the hat of (l).
      */
        u3_rat
        u3_ln_words(u3_lv       lv,
                    c3_w        a_w,
                    const c3_w* b_w);
      

    /** Reading.
    **/
      /* u3_lr_bin(): 
      **
      **   Return the size of (b) in bits, rounded up to
      **   (1 << a_y). 
      **
      **   For example, (a_y == 3) returns the size in bytes.
      */
        c3_w
        u3_lr_bin(u3_lv   lv,
                  c3_y    a_y,
                  u3_atom b);

      /* u3_lr_bit():
      **
      **   Return bit (a_w) of (b).
      */
        c3_b
        u3_lr_bit(u3_lv   lv,
                  c3_w    a_w,
                  u3_atom b);
       
      /* u3_lr_byte():
      **
      **   Return byte (a_w) of (b).
      */
        c3_y
        u3_lr_byte(u3_lv   lv,
                   c3_w    a_w,
                   u3_atom b);
                  
      /* u3_lr_bytes():
      **
      **  Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u3_lr_bytes(u3_lv   lv,
                    c3_w    a_w,
                    c3_w    b_w,
                    c3_y*   c_y,
                    u3_atom d);

      /* u3_lr_cell():
      **
      **   Factor (a) as a cell (b c).
      */
        u3_flag
        u3_lr_cell(u3_lv   lv,
                   u3_fox  a,
                   u3_rat* b,
                   u3_rat* c);

      /* u3_lr_sing():
      **
      **   Yes iff (a) and (b) are the same noun.
      */
        u3_flag
        u3_lr_sing(u3_lv  lv,
                   u3_fox a,
                   u3_fox b);

      /* u3_lr_sing_c():
      **
      **   Yes iff (b) is the same noun as the C string a_c.
      */
        u3_flag
        u3_lr_sing_c(u3_lv  lv,
                     c3_c*  a_c,
                     u3_fox b);

      /* u3_lr_fork():
      **
      **   Factor [a] as a fork [b.[p q] c].
      */
        u3_flag
        u3_lr_fork(u3_lv   lv,
                   u3_fox  a,
                   u3_rat* b,
                   u3_rat* c);

      /* u3_lr_ord():
      **
      **   0 if a < b, 1 if a == b, 2 if a > b
      */
        u3_atom
        u3_lr_ord(u3_lv  lv,
                  u3_fox a,
                  u3_fox b);

      /* u3_lr_p():
      **
      **   & [0] if [a] is of the form [b *c].
      */
        u3_flag
        u3_lr_p(u3_lv   lv,
                u3_fox  a,
                u3_fox  b,
                u3_fox* c);

      /* u3_lr_pq():
      **
      **   & [0] if [a] is of the form [b *c d].
      */
        u3_flag
        u3_lr_pq(u3_lv   lv,
                 u3_fox  a,
                 u3_fox  b,
                 u3_fox* c,
                 u3_fox* d);

      /* u3_lr_pqr():
      **
      **   & [0] if [a] is of the form [b *c *d *e].
      */
        u3_flag
        u3_lr_pqr(u3_lv   lv,
                  u3_fox  a,
                  u3_fox  b,
                  u3_fox* c,
                  u3_fox* d,
                  u3_fox* e);

      /* u3_lr_stud():
      **
      **   Yes iff (a) is an atom.
      */
#if 0
        u3_flag
        u3_lr_stud(u3_lv    lv,
                   u3_fox a)
#else 
#       define u3_lr_stud(lv, a) \
          (u3_rat_is_atom(a) ? u3_yes : u3_no)
#endif

      /* u3_lr_h():
      **
      **   Return the head of (a).
      */
        u3_rat
        u3_lr_h(u3_lv  lv, 
                u3_fox a);

      /* u3_lr_mp():
      **
      **   Copy (b) into (a_mp).
      */
        void
        u3_lr_mp(u3_lv   lv,
                 mpz_t   a_mp,
                 u3_atom b);

      /* u3_lr_qual():
      **
      **   Factor (a) as a qual (b c d e).
      */
        u3_flag
        u3_lr_qual(u3_lv   lv,
                   u3_fox  a,
                   u3_rat* b,
                   u3_rat* c,
                   u3_rat* d,
                   u3_rat* e);

      /* u3_lr_quil():
      **
      **   Factor (a) as a quil (b c d e f).
      */
        u3_flag
        u3_lr_quil(u3_lv   lv,
                   u3_fox  a,
                   u3_rat* b,
                   u3_rat* c,
                   u3_rat* d,
                   u3_rat* e,
                   u3_rat* f);

      /* u3_lr_hext():
      **
      **   Factor (a) as a hext (b c d e f g)
      */
        u3_flag
        u3_lr_hext(u3_lv   lv,
                   u3_fox  a,
                   u3_rat* b,
                   u3_rat* c,
                   u3_rat* d,
                   u3_rat* e,
                   u3_rat* f,
                   u3_rat* g);


      /* u3_lr_t():
      **
      **   Return the tail of (a).
      */
        u3_rat
        u3_lr_t(u3_lv    lv, 
                u3_fox a);

      /* u3_lr_dust():
      **
      **   Yes iff (a) is a cell.
      */
#if 0
        u3_flag
        u3_lr_dust(u3_lv    lv,
                   u3_fox a)
#else 
#       define u3_lr_dust(lv, a) \
          (u3_rat_is_atom(a) ? u3_no : u3_yes)
#endif

      /* u3_lr_trel():
      **
      **   Factor (a) as a trel (b c d).
      */
        u3_flag
        u3_lr_trel(u3_lv    lv,
                   u3_fox a,
                   u3_rat *b,
                   u3_rat *c,
                   u3_rat *d);

      /* u3_lr_twig():
      **
      **   Return twig (a) of (b).
      */
        u3_rat
        u3_lr_twig(u3_lv     lv,
                   u3_atom a,
                   u3_fox  b);

      /* u3_lr_word():
      **
      **   Return word (a_w) of (b).
      */
        c3_w
        u3_lr_word(u3_lv     lv,
                   c3_w      a_w,
                   u3_atom b);

      /* u3_lr_words():
      **
      **  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u3_lr_words(u3_lv   lv,
                    c3_w    a_w,
                    c3_w    b_w,
                    c3_w*   c_w,
                    u3_atom d);
