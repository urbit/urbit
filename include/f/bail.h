/* include/bail.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /** Structures - in loom space.
    **/
      /* u2_loom_kite: jump buffer.
      */
        typedef struct _u2_loom_kite {
          /* Parent kite.
          */
          u2_ray par_r;

          /* Interpreter trace at call point.
          */
          u2_noun tax;

          /* Profiling action stack.
          */
          u2_noun don;

          /* C escape buffer.
          */
          jmp_buf buf_f;
        } u2_loom_kite;

#       define  u2_kite_tax(kit_r)    *u2_at(kit_r, u2_loom_kite, tax)
#       define  u2_kite_don(kit_r)    *u2_at(kit_r, u2_loom_kite, don)
#       define  u2_kite_par_r(kit_r)  *u2_at(kit_r, u2_loom_kite, par_r)
#       define  u2_kite_buf_r(kit_r)  u2_aftr(kit_r, u2_loom_kite, buf_f)


  /** Functions.
  **/
    /** Fail-specific.
    **/
      /* u2_bl_bail(): bail out.
      **
      **  Bail codes:
      **
      **    c3__exit for normal exit with correct trace
      **    c3__fail for abnormal failure without assumptions
      **
      **  When in doubt, fail.
      **
      **  In both cases, a mark-and-sweep is necessary (and
      **  not currently performed) to clean up leaks.
      */
        u2_noun                                                   //  blocked
        u2_bl_bail(u2_wire wir_r,
                   c3_l    how_l);

      /* u2_bl_yes(): assure yes.
      */
#       define u2_bl_yes(wir_r, feg) \
          ( (u2_yes == (feg)) ? 0 : u2_bl_bail(wir_r, c3__fail) )

      /* u2_bl_good(): test for u2_none.
      */
        u2_noun
        u2_bl_good(u2_wire wir_r, u2_weak som);

      /* u2_bl_some(): test for zero ray.
      */
        u2_ray
        u2_bl_some(u2_wire wir_r, u2_ray ray_r);

      /* u2_bl_flat(): force to atom.
      */
        u2_atom
        u2_bl_flat(u2_wire wir_r, u2_noun som);


    /** General.  All functions bail out on error.
    **/
      /** Cell access.
      **/
        /* u2_bi_h():
        **
        **   Return the head of (a).
        */
          u2_noun
          u2_bi_h(u2_wire wir_r,
                  u2_noun a);

#         define u2_xh(wir_r, a) u2_bi_h(wir_r, a)
#         define u2_xt(wir_r, a) u2_bi_t(wir_r, a)

        /* u2_bi_t():
        **
        **   Return the tail of (a).
        */
          u2_noun
          u2_bi_t(u2_wire wir_r,
                  u2_noun a);


        /* u2_bi_frag():
        **
        **   Return fragment (a) of (b).
        */
          u2_noun
          u2_bi_frag(u2_wire wir_r,
                     u2_atom a,
                     u2_noun b);

        /* u2_bi_cell():
        **
        **   Factor `a` as a cell `[b c]`.
        */
          void
          u2_bi_cell(u2_wire  wir_r,
                     u2_noun  a,
                     u2_noun* b,
                     u2_noun* c);

        /* u2_bi_qual():
        **
        **   Factor `a` as a quadruple `[b c d e]`.
        */
          void
          u2_bi_qual(u2_wire  wir_r,
                     u2_noun  a,
                     u2_noun* b,
                     u2_noun* c,
                     u2_noun* d,
                     u2_noun* e);

        /* u2_bi_quil():
        **
        **   Factor `a` as a quintuple `[b c d e f]`, or bail.
        */
          void
          u2_bi_quil(u2_wire  wir_r,
                     u2_noun  a,
                     u2_noun* b,
                     u2_noun* c,
                     u2_noun* d,
                     u2_noun* e,
                     u2_noun* f);

        /* u2_bi_trel():
        **
        **   Factor `a` as a trel `[b c d]`, or bail.
        */
          void
          u2_bi_trel(u2_wire  wir_r,
                     u2_noun  a,
                     u2_noun* b,
                     u2_noun* c,
                     u2_noun* d);

      /** Tracing.
      **/
        /* u2_bl_push(): push on trace stack.
        */
          void
          u2_bl_push(u2_wire wir_r,
                     u2_noun mon);                                //  transfer

        /* u2_bl_mean(): push `[%mean roc]` on trace stack.
        */
          void
          u2_bl_mean(u2_wire wir_r,
                     u2_noun roc);                                //  transfer

        /* u2_bl_error(): simple string error.
        */
          u2_noun                                                 //  blocked
          u2_bl_error(u2_wire     wir_r,
                      const c3_c* err_c);                         //  retain

        /* u2_bl_drop(): drop from meaning stack.
        */
          void
          u2_bl_drop(u2_wire wir_r);

      /** Atom access.
      **/
        /* u2_bi_met():
        **
        **   Return the size of (b) in bits, rounded up to
        **   (1 << a_y).
        **
        **   For example, (a_y == 3) returns the size in bytes.
        */
          c3_w
          u2_bi_met(u2_wire wir_r,
                    c3_y    a_y,
                    u2_noun b);

        /* u2_bi_bit():
        **
        **   Return bit (a_w) of (b).
        */
          c3_b
          u2_bi_bit(u2_wire wir_r,
                    c3_w    a_w,
                    u2_noun b);

        /* u2_bi_byte():
        **
        **   Return byte (a_w) of (b).
        */
          c3_y
          u2_bi_byte(u2_wire wir_r,
                     c3_w    a_w,
                     u2_noun b);

        /* u2_bi_bytes():
        **
        **  Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
        */
          void
          u2_bi_bytes(u2_wire wir_r,
                      c3_w    a_w,
                      c3_w    b_w,
                      c3_y*   c_y,
                      u2_noun d);

        /* u2_bi_mp():
        **
        **   Copy (b) into (a_mp).
        */
          void
          u2_bi_mp(u2_wire wir_r,
                   mpz_t   a_mp,
                   u2_noun b);

        /* u2_bi_word():
        **
        **   Return word (a_w) of (b).
        */
          c3_w
          u2_bi_word(u2_wire wir_r,
                     c3_w    a_w,
                     u2_noun b);

        /* u2_bi_words():
        **
        **  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
        */
          void
          u2_bi_words(u2_wire wir_r,
                      c3_w    a_w,
                      c3_w    b_w,
                      c3_w*   c_w,
                      u2_noun d);

        /** Allocation.
        **/
          /* u2_bn_slab():
          **
          **   Create an atomic slab of `len` words.
          */
            u2_ray
            u2_bn_slab(u2_wire wir_r,
                       c3_w    len_w);

          /* u2_bn_slaq():
          **
          **   Create an atomic slab of `len` bloqs of size `met`.
          */
            u2_ray
            u2_bn_slaq(u2_wire wir_r,
                       c3_g    met_g,
                       c3_w    len_w);

        /** Noun construction.
        **/
          /* u2_bn_bytes():
          **
          **   Copy [a] bytes from [b].
          */
            u2_noun
            u2_bn_bytes(u2_wire      wir_r,
                        c3_w        a_w,
                        const c3_y* b_y);

          /* u2_bn_string():
          **
          **   u2_bn_bytes(wir_r, strlen(a_c), (c3_y *)a_c);
          */
            u2_noun
            u2_bn_string(u2_wire      wir_r,
                         const c3_c* a_c);

          /* u2_bn_tape():
          **
          **   Create an atomic string from a list of bytes.
          */
            u2_noun
            u2_bn_tape(u2_wire wir_r,
                       u2_list lit);

          /* u2_bn_cell():
          **
          **   Produce the cell [a b].
          */
            u2_noun
            u2_bn_cell(u2_wire wir_r,
                       u2_noun a,
                       u2_noun b);
#         define u2_bc(wir_r, a, b) u2_bn_cell(wir_r, a, b)
#         define u2_bo(wir_r, a)    u2_bn_cell(wir_r, a, _0)

          /* u2_bn_decimal():
          **
          **   On (wir_r), write (list), a list of digits, as a decimal.
          */
            u2_noun
            u2_bn_decimal(u2_wire wir_r,
                          u2_list lit);

          /* u2_bn_heximal():
          **
          **   On (wir_r), write (lit), a list of digits, as a hexadecimal.
          */
            u2_noun
            u2_bn_heximal(u2_wire wir_r,
                          u2_list lit);

          /* u2_bn_ice():
          **
          **   Produce `a`, not referencing the can.  Copy or gain reference.
          */
            u2_noun
            u2_bn_ice(u2_wire wir_r,
                      u2_weak a);
#         define u2_bx(wir_r, a)  u2_bn_ice(wir_r, a)

          /* u2_bn_list():
          **
          **   Generate a null-terminated list, with u2_none as terminator.
          */
            u2_noun
            u2_bn_list(u2_wire wir_r, ...);

          /* u2_bn_nock():
          **
          **   Nock or bail.
          */
            u2_noun                                                 //  transfer
            u2_bn_nock(u2_wire wir_r,
                       u2_noun bus,                                 //  retain
                       u2_noun fol);                                //  retain

          /* u2_bn_mp():
          **
          **   Copy the GMP integer [a] into an atom.  Free it.
          */
            u2_noun
            u2_bn_mp(u2_wire wir_r,
                     mpz_t  a_mp);

          /* u2_bn_qual():
          **
          **   Produce the quadruple [a b c d].
          */
            u2_noun
            u2_bn_qual(u2_wire wir_r,
                       u2_noun a,
                       u2_noun b,
                       u2_noun c,
                       u2_noun d);
#         define u2_bq(wir_r, a, b, c, d) u2_bn_qual(wir_r, a, b, c, d)

          /* u2_bn_quil():
          **
          **   Produce the quintuple [a b c d].
          */
            u2_noun
            u2_bn_quil(u2_wire wir_r,
                       u2_noun a,
                       u2_noun b,
                       u2_noun c,
                       u2_noun d,
                       u2_noun e);
#         define u2_bu(wir_r, a, b, c, d, e) u2_bn_quil(wir_r, a, b, c, d, e)

          /* u2_bn_trel():
          **
          **   Produce the triple [a b c].
          */
            u2_noun
            u2_bn_trel(u2_wire wir_r,
                       u2_noun a,
                       u2_noun b,
                       u2_noun c);
#         define u2_bt(wir_r, a, b, c) u2_bn_trel(wir_r, a, b, c)

          /* u2_bn_words():
          **
          **   Copy [a] words from [b] into an atom.
          */
            u2_noun
            u2_bn_words(u2_wire      wir_r,
                        c3_w        a_w,
                        const c3_w* b_w);

          /* u2_bn_molt():
          **
          **   Mutate `som` with a 0-terminated list of axis, noun pairs.
          **   Axes must be cats (31 bit).
          */
            u2_noun                                               //  transfer
            u2_bn_molt(u2_wire wir_r,
                       u2_noun som,                               //  retain
                       ...);                                      //  retain

          /* u2_bn_molf():
          **
          **   As u2_bn_molt(), with argument pointer.
          */
            u2_noun
            u2_bn_molf(u2_wire wir_r,
                       u2_noun som,
                       va_list vap);

          /* u2_bn_mang():
          **
          **   Kick a core, substituting axes with nouns.
          */
            u2_noun
            u2_bn_mang(u2_wire wir_r,
                       u2_noun cor,
                       ...);              // nouns

          /* u2_bn_mong():
          **
          **   Call by gate and sample (new convention).
          **   Caller retains `gat`, transfers `sam`.
          */
            u2_noun                                               //  produce
            u2_bn_mong(u2_wire wir_r,
                       u2_noun gat,                               //  retain
                       u2_noun sam);                              //  submit

          /* u2_bn_hook():
          **
          **   Execute hook from core.
          */
            u2_noun                                               //  transfer
            u2_bn_hook(u2_wire     wir_r,
                       u2_noun     cor,                           //  retain
                       const c3_c* tam_c);

          /* u2_bn_cook():
          **
          **   Reverse hook as molt.
          */
            u2_noun                                               //  transfer
            u2_bn_cook(u2_wire     wir_r,
                       u2_noun     cor,                           //  retain
                       const c3_c* tam_c,
                       u2_noun     som);                          //  transfer

          /* u2_bn_gart():
          **
          **   Call by core, hook, sample.
          */
            u2_noun
            u2_bn_gart(u2_wire     wir_r,
                       u2_noun     cor,
                       const c3_c* tam_c,
                       u2_noun     sam);

          /* u2_bn_gort():
          **
          **  Call by core, depth, hook, molt list.
          */
            u2_noun
            u2_bn_gort(u2_wire     wir_r,
                       u2_noun     cor,
                       const c3_c* tam_c,
                       ...);

          /* u2_bn_wait():
          **
          **  Produce the functional equivalent of `|.(~(tam cor sam))`.
          */
            u2_noun                                               //  produce
            u2_bn_wait(u2_wire     wir_r,
                       u2_noun     cor,                           //  retain
                       u2_noun     sam,                           //  retain
                       const c3_c* tam_c);                        //  retain
