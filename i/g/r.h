/* include/g/r.h
**
** This file is in the public domain.
*/
    /** u3_cr_*: read without ever crashing.
    **/
#if 1
#       define u3_cr_du(a)  u3_ca_is_cell(a)
#       define u3_cr_ud(a)  u3_ca_is_atom(a)
#else
      /* u3_cr_du(): c3y iff `a` is cell.
      */
        u3_bean
        u3_cr_du(u3_noun a);

      /* u3_cr_ud(): c3n iff `a` is cell.
      */
        u3_bean
        u3_cr_ud(u3_noun a);
#endif

      /* u3_cr_at(): fragment `a` of `b`, or u3_none.
      */
        u3_weak
        u3_cr_at(u3_atom a,
                 u3_weak b);

      /* u3_cr_mean():
      **
      **   Attempt to deconstruct `a` by axis, noun pairs; 0 terminates.
      **   Axes must be sorted in tree order.
      */
        u3_bean
        u3_cr_mean(u3_noun a,
                   ...);

      /* u3_cr_mug():
      **
      **   Compute and/or recall the mug (31-bit hash) of (a).
      */
        c3_w
        u3_cr_mug(u3_noun a);

      /* u3_cr_mug_string():
      **
      **   Compute the mug of `a`, LSB first.
      */
        c3_w
        u3_cr_mug_string(const c3_c *a_c);

      /* u3_cr_mug_words():
      **
      **   Compute the mug of `buf`, `len`, LSW first.
      */
        c3_w
        u3_cr_mug_words(const c3_w *buf_w,
                        c3_w        len_w);

      /* u3_cr_mug_cell():
      **
      **   Compute the mug of `[a b]`.
      */
        c3_w
        u3_cr_mug_cell(u3_noun a,
                       u3_noun b);

      /* u3_cr_mug_trel():
      **
      **   Compute the mug of `[a b c]`.
      */
        c3_w
        u3_cr_mug_trel(u3_noun a,
                       u3_noun b,
                       u3_noun c);

      /* u3_cr_mug_qual():
      **
      **   Compute the mug of `[a b c d]`.
      */
        c3_w
        u3_cr_mug_qual(u3_noun a,
                       u3_noun b,
                       u3_noun c,
                       u3_noun d);

      /* u3_cr_mug_both():
      **
      **   Join two mugs.
      */
        c3_w
        u3_cr_mug_both(c3_w a_w,
                       c3_w b_w);

      /* u3_cr_fing():
      **
      **   Yes iff (a) and (b) are the same copy of the same noun.
      **   (Ie, by pointer equality - u3_cr_sing with false negatives.)
      */
        u3_bean
        u3_cr_fing(u3_noun a,
                   u3_noun b);

      /* u3_cr_fing_cell():
      **
      **   Yes iff `[p q]` and `b` are the same copy of the same noun.
      */
        u3_bean
        u3_cr_fing_cell(u3_noun p,
                        u3_noun q,
                        u3_noun b);

      /* u3_cr_fing_mixt():
      **
      **   Yes iff `[p q]` and `b` are the same copy of the same noun.
      */
        u3_bean
        u3_cr_fing_mixt(const c3_c* p_c,
                        u3_noun     q,
                        u3_noun     b);

      /* u3_cr_fing_trel():
      **
      **   Yes iff `[p q r]` and `b` are the same copy of the same noun.
      */
        u3_bean
        u3_cr_fing_trel(u3_noun p,
                        u3_noun q,
                        u3_noun r,
                        u3_noun b);

      /* u3_cr_fing_qual():
      **
      **   Yes iff `[p q r s]` and `b` are the same copy of the same noun.
      */
        u3_bean
        u3_cr_fing_qual(u3_noun p,
                        u3_noun q,
                        u3_noun r,
                        u3_noun s,
                        u3_noun b);

      /* u3_cr_sing():
      **
      **   Yes iff (a) and (b) are the same noun.
      */
        u3_bean
        u3_cr_sing(u3_noun a,
                   u3_noun b);

      /* u3_cr_sung(): yes iff (a) and (b) are the same noun, unifying equals.
      **
      **   Make sure you have no live, uncounted pointers to any noun
      **   within (a) or (b)!
      */
        u3_bean
        u3_cr_sung(u3_noun a,
                   u3_noun b);

      /* u3_cr_sing_c):
      **
      **   Yes iff (b) is the same noun as the C string [a].
      */
        u3_bean
        u3_cr_sing_c(const c3_c* a_c,
                     u3_noun     b);

      /* u3_cr_sing_cell():
      **
      **   Yes iff `[p q]` and `b` are the same noun.
      */
        u3_bean
        u3_cr_sing_cell(u3_noun p,
                        u3_noun q,
                        u3_noun b);

      /* u3_cr_sing_mixt():
      **
      **   Yes iff `[p q]` and `b` are the same noun.
      */
        u3_bean
        u3_cr_sing_mixt(const c3_c* p_c,
                        u3_noun     q,
                        u3_noun     b);

      /* u3_cr_sing_trel():
      **
      **   Yes iff `[p q r]` and `b` are the same noun.
      */
        u3_bean
        u3_cr_sing_trel(u3_noun p,
                        u3_noun q,
                        u3_noun r,
                        u3_noun b);

      /* u3_cr_sing_qual():
      **
      **   Yes iff `[p q r s]` and `b` are the same noun.
      */
        u3_bean
        u3_cr_sing_qual(u3_noun p,
                        u3_noun q,
                        u3_noun r,
                        u3_noun s,
                        u3_noun b);

      /* u3_cr_nord():
      **
      **   Return 0, 1 or 2 if `a` is below, equal to, or above `b`.
      */
        u3_atom
        u3_cr_nord(u3_noun a,
                   u3_noun b);

      /* u3_cr_mold():
      **
      **   Divide `a` as a mold `[b.[p q] c]`.
      */
        u3_bean
        u3_cr_mold(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c);

      /* u3_cr_cell():
      **
      **   Divide `a` as a cell `[b c]`.
      */
        u3_bean
        u3_cr_cell(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c);

      /* u3_cr_trel():
      **
      **   Divide `a` as a trel `[b c]`.
      */
        u3_bean
        u3_cr_trel(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c,
                   u3_noun* d);

      /* u3_cr_qual():
      **
      **   Divide (a) as a qual [b c d e f].
      */
        u3_bean
        u3_cr_qual(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c,
                   u3_noun* d,
                   u3_noun* e);

      /* u3_cr_quil():
      **
      **   Divide (a) as a quil [b c d e f].
      */
        u3_bean
        u3_cr_quil(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c,
                   u3_noun* d,
                   u3_noun* e,
                   u3_noun* f);

      /* u3_cr_p():
      **
      **   & [0] if [a] is of the form [b *c].
      */
        u3_bean
        u3_cr_p(u3_noun  a,
                u3_noun  b,
                u3_noun* c);

      /* u3_cr_bush():
      **
      **   Factor [a] as a bush [b.[p q] c].
      */
        u3_bean
        u3_cr_bush(u3_noun  a,
                   u3_noun* b,
                   u3_noun* c);

      /* u3_cr_pq():
      **
      **   & [0] if [a] is of the form [b *c d].
      */
        u3_bean
        u3_cr_pq(u3_noun  a,
                 u3_noun  b,
                 u3_noun* c,
                 u3_noun* d);

      /* u3_cr_pqr():
      **
      **   & [0] if [a] is of the form [b *c *d *e].
      */
        u3_bean
        u3_cr_pqr(u3_noun  a,
                  u3_noun  b,
                  u3_noun* c,
                  u3_noun* d,
                  u3_noun* e);

      /* u3_cr_pqrs():
      **
      **   & [0] if [a] is of the form [b *c *d *e *f].
      */
        u3_bean
        u3_cr_pqrs(u3_noun  a,
                   u3_noun  b,
                   u3_noun* c,
                   u3_noun* d,
                   u3_noun* e,
                   u3_noun* f);

      /* u3_cr_met():
      **
      **   Return the size of (b) in bits, rounded up to
      **   (1 << a_y).
      **
      **   For example, (a_y == 3) returns the size in bytes.
      */
        c3_w
        u3_cr_met(c3_y    a_y,
                  u3_atom b);

      /* u3_cr_bit():
      **
      **   Return bit (a_w) of (b).
      */
        c3_b
        u3_cr_bit(c3_w    a_w,
                  u3_atom b);

      /* u3_cr_byte():
      **
      **   Return byte (a_w) of (b).
      */
        c3_y
        u3_cr_byte(c3_w    a_w,
                   u3_atom b);

      /* u3_cr_bytes():
      **
      **   Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u3_cr_bytes(c3_w    a_w,
                    c3_w    b_w,
                    c3_y*   c_y,
                    u3_atom d);

      /* u3_cr_chop():
      **
      **   Into the bloq space of `met`, from position `fum` for a
      **   span of `wid`, to position `tou`, XOR from atom `src`
      **   into ray `dst`.
      */
        void
        u3_cr_chop(c3_g    met_g,
                   c3_w    fum_w,
                   c3_w    wid_w,
                   c3_w    tou_w,
                   c3_w*   dst_w,
                   u3_atom src);

      /* u3_cr_mp():
      **
      **   Copy (b) into (a_mp).
      */
        void
        u3_cr_mp(mpz_t   a_mp,
                 u3_atom b);

      /* u3_cr_word():
      **
      **   Return word (a_w) of (b).
      */
        c3_w
        u3_cr_word(c3_w    a_w,
                   u3_atom b);

      /* u3_cr_chub():
      **
      **   Return double-word (a_w) of (b).
      */
        c3_d
        u3_cr_chub(c3_w    a_w,
                   u3_atom b);

      /* u3_cr_words():
      **
      **  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u3_cr_words(c3_w    a_w,
                    c3_w    b_w,
                    c3_w*   c_w,
                    u3_atom d);

      /* u3_cr_string(): `a`, a text atom, as malloced C string.
      */
        c3_c*
        u3_cr_string(u3_atom a);

      /* u3_cr_tape(): `a`, a list of bytes, as malloced C string.
      */
        c3_y*
        u3_cr_tape(u3_noun a);
