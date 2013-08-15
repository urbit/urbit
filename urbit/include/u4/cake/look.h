/* include/cake/look.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /** Reading nouns.
    **/
      /* u4_n_atom():
      **
      **   Return 1 if (noun) is an atom.
      */
        u4_t
        u4_n_atom(u4_noun noun);

      /* u4_n_cell():
      **
      **   Return 1 if (noun) is a cell.
      */
        u4_t
        u4_n_cell(u4_noun noun);

      /* u4_n_trel():
      ** u4_n_qual():
      ** u4_n_quil():
      **
      **   Return 1 if (noun) is a trel, qual or quil.
      */
        u4_t u4_n_trel(u4_noun noun);
        u4_t u4_n_qual(u4_noun noun);
        u4_t u4_n_quil(u4_noun noun);

      /* u4_n_size():
      **
      **   Return number of subnouns in (a).
      */
        u4_xw
        u4_n_size(u4_noun a);

      /* u4_n_eq():
      **
      **   Return 1 if (a) and (b) are the same noun.
      */
        u4_t
        u4_n_eq(u4_noun a,
                u4_noun b);

      /* u4_n_eq_c():
      **
      **   Return 1 if (noun) equals the C string (cl).
      */
        u4_t
        u4_n_eq_c(u4_noun noun,
                  u4_cl   *cl);

      /* u4_n_zero():
      **
      **   Return 1 if (noun) is 0.
      */
        // u4_t
        // u4_n_zero(u4_noun noun);
#            define u4_n_zero(noun) \
               ( u4_n_eq(u4_noun_0, noun) )

      /* u4_n_true():
      **
      **   Return 1 if (noun) is 0, 0 if (noun) is 1.  Otherwise, trip.
      */
        u4_t
        u4_n_true(u4_noun noun);

      /* u4_n_snip_():
      **
      **   Return nock(b (0 a)), or u4_bull if there is no such.
      */
        u4_noun
        u4_n_snip_(u4_atom a,
                   u4_noun b);

      /* u4_n_nub():
      **
      **   Return the nub [31-bit nonzero insecure hash] of (noun).
      */
        u4_nub
        u4_n_nub(u4_noun noun);

      /* u4_n_nib():
      **
      **   Return the nub of the nub of (noun).
      */
        u4_nub
        u4_n_nib(u4_noun noun);


    /** Reading cells.
    **/
      /* u4_c_head():
      **
      **   Return the head of (cell).
      */
        u4_noun
        u4_c_head(u4_cell cell);

      /* u4_c_tail():
      **
      **   Return the tail of (cell).
      */
        u4_noun
        u4_c_tail(u4_cell cell);

#       define u4_ch(cell) u4_c_head(cell)
#       define u4_ct(cell) u4_c_tail(cell)

#       define u4_chh(cell) u4_ch(u4_ch(cell))
#       define u4_cht(cell) u4_ct(u4_ch(cell))
#       define u4_cth(cell) u4_ch(u4_ct(cell))
#       define u4_ctt(cell) u4_ct(u4_ct(cell))

#       define u4_chhh(cell) u4_ch(u4_ch(u4_ch(cell)))
#       define u4_chht(cell) u4_ct(u4_ch(u4_ch(cell)))
#       define u4_chth(cell) u4_ch(u4_ct(u4_ch(cell)))
#       define u4_chtt(cell) u4_ct(u4_ct(u4_ch(cell)))
#       define u4_cthh(cell) u4_ch(u4_ch(u4_ct(cell)))
#       define u4_ctht(cell) u4_ct(u4_ch(u4_ct(cell)))
#       define u4_ctth(cell) u4_ch(u4_ct(u4_ct(cell)))
#       define u4_cttt(cell) u4_ct(u4_ct(u4_ct(cell)))

#       define u4_c_2(cell)  u4_ch(cell)
#       define u4_c_3(cell)  u4_ct(cell)
#       define u4_c_4(cell)  u4_chh(cell)
#       define u4_c_5(cell)  u4_cht(cell)
#       define u4_c_6(cell)  u4_cth(cell)
#       define u4_c_7(cell)  u4_ctt(cell)

#       define u4_c_8(cell)  u4_chhh(cell)
#       define u4_c_9(cell)  u4_chht(cell)
#       define u4_c_10(cell) u4_chth(cell)
#       define u4_c_11(cell) u4_chtt(cell)
#       define u4_c_12(cell) u4_cthh(cell)
#       define u4_c_13(cell) u4_ctht(cell)
#       define u4_c_14(cell) u4_ctth(cell)
#       define u4_c_15(cell) u4_cttt(cell)

      /* u4_c_head_exit():
      **
      **   Return the head of (noun), or exit if (noun) is flat.
      */
        u4_noun
        u4_c_head_exit(u4_noun noun);

      /* u4_c_tail_exit():
      **
      **   Return the tail of (noun), or exit if (noun) is flat.
      */
        u4_noun
        u4_c_tail_exit(u4_noun noun);

#       define u4_chx(noun) u4_c_head_exit(noun)
#       define u4_ctx(noun) u4_c_tail_exit(noun)

      /* u4_c_cell(): 
      ** u4_c_trel():
      ** u4_c_qual():
      ** u4_c_quil():
      **
      **   Unpack (cell, trel, qual, quil), returning true if found.
      */
        u4_t u4_c_cell(u4_noun cell, u4_noun *a, u4_noun *b);
        u4_t u4_c_trel(u4_noun trel, u4_noun *a, u4_noun *b, u4_noun *c);
        u4_t u4_c_qual(u4_noun qual, u4_noun *a, u4_noun *b, u4_noun *c,
                                     u4_noun *d);
        u4_t u4_c_quil(u4_noun quil, u4_noun *a, u4_noun *b, u4_noun *c,
                                     u4_noun *d, u4_noun *e);

      /* u4_c_log():
      **
      **   Unpack a log of length (xw), ignoring the tail.
      */
        void
        u4_c_log(u4_noun log,
                 u4_xw   xw,
                 u4_noun *nouns);

      /* u4_c_tuple():
      **
      **   Unpack a tuple of (xw) nouns.
      */
        void
        u4_c_tuple(u4_noun tuple,
                   u4_xw   xw,
                   u4_noun *nouns);

    /** Bush form.
    **/
      /* u4_b_fork(): 
      **
      **   True iff, in (noun), (*p) and (*q) are a bush fork.
      */
        u4_t
        u4_b_fork(u4_noun noun,
                  u4_noun *p,
                  u4_noun *q);

      /* u4_b_p():
      **
      **   True iff (noun) is of the form (stem *p).
      */
        u4_t 
        u4_b_p(u4_noun noun,
               u4_atom stem,
               u4_noun *p);

      /* u4_b_pq():
      **
      **   True iff (noun) is of the form (stem *p *q).
      */
        u4_t 
        u4_b_pq(u4_noun noun,
                u4_atom stem,
                u4_noun *p,
                u4_noun *q);

      /* u4_b_pqr():
      **
      **   True iff (noun) is of the form (stem *p *q *r).
      */
        u4_t 
        u4_b_pqr(u4_noun noun,
                 u4_atom stem,
                 u4_noun *p,
                 u4_noun *q,
                 u4_noun *r);

      /* u4_b_pqrs():
      **
      **   True iff (noun) is of the form (stem *p *q *r *s).
      */
        u4_t 
        u4_b_pqrs(u4_noun noun,
                  u4_atom stem,
                  u4_noun *p,
                  u4_noun *q,
                  u4_noun *r, 
                  u4_noun *s);


    /** Reading atoms.
    **/
      /* u4_a_bin(): 
      **
      **   Return the size of (atom) in bits, rounded up to
      **   (1 << gt). 
      **
      **   For example, (gt = 3) returns the size in bytes.
      */
        u4_st
        u4_a_bin(u4_atom atom,
                 u4_gt   gt);

      /* u4_a_bit():
      **
      **   Return bit (pt) of (atom).
      */
        u4_t
        u4_a_bit(u4_atom atom,
                 u4_pt   pt);

      /* u4_a_byte():
      **
      **   Return byte (pb) of (atom).
      */
        u4_xb
        u4_a_byte(u4_atom atom,
                  u4_pb   pb);

      /* u4_a_word():
      **
      **   Return word (pw) of (atom).
      */
        u4_xw
        u4_a_word(u4_atom atom,
                  u4_pw   pw);

      /* u4_a_words():
      **
      **   Copy words from (atom) into (xw), starting at (pw)
      **   and continuing for (sw).
      **
      **   (xw) must be of length (sw).
      */
        void
        u4_a_words(u4_atom atom,
                   u4_xw   *xw,
                   u4_pw   pw,
                   u4_sw   sw);

      /* u4_a_bytes(): 
      **
      **   Copy bytes from (atom) into (xb), starting at (pb)
      **   and continuing for (sb).
      **
      **   (xb) must be of length (sb).
      */
        void
        u4_a_bytes(u4_atom atom,
                   u4_xb   *xb,
                   u4_pb   pb,
                   u4_sb   sb);

      /* u4_a_bits(): 
      **
      **   Xor bits from (atom) into (xw), starting at (hw) in
      **   (xw[0]), reading from (pt) for (st).
      **
      **   (xw) must be of length (u4_bblock(hw + st, 5)).
      */
        void
        u4_a_bits(u4_atom atom,
                  u4_xw   *xw,
                  u4_hw   hw,
                  u4_pt   pt,
                  u4_st   st);

      /* u4_a_gmp():
      **
      **   Copy (atom) into (mp).
      **
      **   The caller must free (mp) with mpz_clear, or equivalent.
      */
        void
        u4_a_gmp(u4_atom atom,
                 mpz_t   mp);

      /* u4_a_wbail():
      **
      **   Produce (atom) as a 32-bit word, or bail with (code).
      */
        u4_xw
        u4_a_wbail(u4_atom           atom,
                   enum u4_bail_code bail_code);

#         define u4_a_wtrip(atom) \
            u4_a_wbail(atom, u4_bail_trip)

#         define u4_a_wexit(atom) \
            u4_a_wbail(atom, u4_bail_exit)
