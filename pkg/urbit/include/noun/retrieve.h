/* i/n/r.h
**
** This file is in the public domain.
*/
    /** u3r_*: read without ever crashing.
    **/
#if 1
#       define u3r_du(a)  u3a_is_cell(a)
#       define u3r_ud(a)  u3a_is_atom(a)
#else
      /* u3r_du(): c3y iff `a` is cell.
      */
        c3_o
        u3r_du(u3_noun a);

      /* u3r_ud(): c3n iff `a` is cell.
      */
        c3_o
        u3r_ud(u3_noun a);
#endif

      /* u3r_at(): fragment `a` of `b`, or u3_none.
      */
        u3_weak
        u3r_at(u3_atom a, u3_weak b);

      /* u3r_mean():
      **
      **   Attempt to deconstruct `a` by axis, noun pairs; 0 terminates.
      **   Axes must be sorted in tree order.
      */
        c3_o
        u3r_vmean(u3_noun a, va_list ap);
        c3_o
        u3r_mean(u3_noun a, ...);

      /* u3r_mug_bytes(): Compute the mug of `buf`, `len`, LSW first.
      */
        c3_w
        u3r_mug_bytes(const c3_y *buf_y,
                      c3_w        len_w);

      /* u3r_mug_chub(): Compute the mug of `num`, LSW first.
      */
        c3_w
        u3r_mug_chub(c3_d num_d);

      /* u3r_mug_string(): Compute the mug of `a`, LSB first.
      */
        c3_w
        u3r_mug_string(const c3_c *a_c);

      /* u3r_mug_words(): 31-bit nonzero MurmurHash3 on raw words.
      */
        c3_w
        u3r_mug_words(const c3_w* key_w, c3_w len_w);

      /* u3r_mug_both(): Join two mugs.
      */
        c3_w
        u3r_mug_both(c3_w lef_w, c3_w rit_w);

      /* u3r_mug_cell(): Compute the mug of the cell `[hed tel]`.
      */
        c3_w
        u3r_mug_cell(u3_noun hed,
                     u3_noun tel);

      /* u3r_mug_trel(): Compute the mug of `[a b c]`.
      */
        c3_w
        u3r_mug_trel(u3_noun a,
                     u3_noun b,
                     u3_noun c);

      /* u3r_mug_qual(): Compute the mug of `[a b c d]`.
      */
        c3_w
        u3r_mug_qual(u3_noun a,
                     u3_noun b,
                     u3_noun c,
                     u3_noun d);

      /* u3r_mug(): MurmurHash3 on a noun.
      */
        c3_w
        u3r_mug(u3_noun veb);

      /* u3r_fing():
      **
      **   Yes iff (a) and (b) are the same copy of the same noun.
      **   (Ie, by pointer equality - u3r_sing with false negatives.)
      */
        c3_o
        u3r_fing(u3_noun a,
                 u3_noun b);

      /* u3r_fing_cell():
      **
      **   Yes iff `[p q]` and `b` are the same copy of the same noun.
      */
        c3_o
        u3r_fing_cell(u3_noun p,
                      u3_noun q,
                      u3_noun b);

      /* u3r_fing_mixt():
      **
      **   Yes iff `[p q]` and `b` are the same copy of the same noun.
      */
        c3_o
        u3r_fing_mixt(const c3_c* p_c,
                      u3_noun     q,
                      u3_noun     b);

      /* u3r_fing_trel():
      **
      **   Yes iff `[p q r]` and `b` are the same copy of the same noun.
      */
        c3_o
        u3r_fing_trel(u3_noun p,
                      u3_noun q,
                      u3_noun r,
                      u3_noun b);

      /* u3r_fing_qual():
      **
      **   Yes iff `[p q r s]` and `b` are the same copy of the same noun.
      */
        c3_o
        u3r_fing_qual(u3_noun p,
                      u3_noun q,
                      u3_noun r,
                      u3_noun s,
                      u3_noun b);

      /* u3r_sing():
      **
      **   Yes iff (a) and (b) are the same noun.
      */
        c3_o
        u3r_sing(u3_noun a, u3_noun b);

      /* u3rz_sing(): transferring u3r_sing
      */
        c3_o
        u3rz_sing(u3_noun a, u3_noun b);

      /* u3r_sung(): yes iff (a) and (b) are the same noun, unifying equals.
      **
      **   Make sure you have no live, uncounted pointers to any noun
      **   within (a) or (b)!
      */
        c3_o
        u3r_sung(u3_noun a, u3_noun b);

      /* u3r_sing_c):
      **
      **   Yes iff (b) is the same noun as the C string [a].
      */
        c3_o
        u3r_sing_c(const c3_c* a_c,
                   u3_noun     b);

      /* u3r_sing_cell():
      **
      **   Yes iff `[p q]` and `b` are the same noun.
      */
        c3_o
        u3r_sing_cell(u3_noun p,
                      u3_noun q,
                      u3_noun b);

      /* u3r_sing_mixt():
      **
      **   Yes iff `[p q]` and `b` are the same noun.
      */
        c3_o
        u3r_sing_mixt(const c3_c* p_c,
                      u3_noun     q,
                      u3_noun     b);

      /* u3r_sing_trel():
      **
      **   Yes iff `[p q r]` and `b` are the same noun.
      */
        c3_o
        u3r_sing_trel(u3_noun p,
                      u3_noun q,
                      u3_noun r,
                      u3_noun b);

      /* u3r_sing_qual():
      **
      **   Yes iff `[p q r s]` and `b` are the same noun.
      */
        c3_o
        u3r_sing_qual(u3_noun p,
                      u3_noun q,
                      u3_noun r,
                      u3_noun s,
                      u3_noun b);

      /* u3r_nord():
      **
      **   Return 0, 1 or 2 if `a` is below, equal to, or above `b`.
      */
        u3_atom
        u3r_nord(u3_noun a,
                 u3_noun b);

      /* u3r_mold():
      **
      **   Divide `a` as a mold `[b.[p q] c]`.
      */
        c3_o
        u3r_mold(u3_noun  a,
                 u3_noun* b,
                 u3_noun* c);

      /* u3r_cell():
      **
      **   Divide `a` as a cell `[b c]`.
      */
        c3_o
        u3r_cell(u3_noun  a,
                 u3_noun* b,
                 u3_noun* c);

      /* u3r_trel():
      **
      **   Divide `a` as a trel `[b c d]`.
      */
        c3_o
        u3r_trel(u3_noun  a,
                 u3_noun* b,
                 u3_noun* c,
                 u3_noun* d);

      /* u3r_qual():
      **
      **   Divide (a) as a qual [b c d e].
      */
        c3_o
        u3r_qual(u3_noun  a,
                 u3_noun* b,
                 u3_noun* c,
                 u3_noun* d,
                 u3_noun* e);

      /* u3r_quil():
      **
      **   Divide (a) as a quil [b c d e f].
      */
        c3_o
        u3r_quil(u3_noun  a,
                 u3_noun* b,
                 u3_noun* c,
                 u3_noun* d,
                 u3_noun* e,
                 u3_noun* f);

      /* u3r_hext():
      **
      **   Divide (a) as a hext [b c d e f g].
      */
        c3_o
        u3r_hext(u3_noun  a,
                 u3_noun* b,
                 u3_noun* c,
                 u3_noun* d,
                 u3_noun* e,
                 u3_noun* f,
                 u3_noun* g);

      /* u3r_p():
      **
      **   & [0] if [a] is of the form [b *c].
      */
        c3_o
        u3r_p(u3_noun  a,
              u3_noun  b,
              u3_noun* c);

      /* u3r_bush():
      **
      **   Factor [a] as a bush [b.[p q] c].
      */
        c3_o
        u3r_bush(u3_noun  a,
                 u3_noun* b,
                 u3_noun* c);

      /* u3r_pq():
      **
      **   & [0] if [a] is of the form [b *c d].
      */
        c3_o
        u3r_pq(u3_noun  a,
               u3_noun  b,
               u3_noun* c,
               u3_noun* d);

      /* u3r_pqr():
      **
      **   & [0] if [a] is of the form [b *c *d *e].
      */
        c3_o
        u3r_pqr(u3_noun  a,
                u3_noun  b,
                u3_noun* c,
                u3_noun* d,
                u3_noun* e);

      /* u3r_pqrs():
      **
      **   & [0] if [a] is of the form [b *c *d *e *f].
      */
        c3_o
        u3r_pqrs(u3_noun  a,
                 u3_noun  b,
                 u3_noun* c,
                 u3_noun* d,
                 u3_noun* e,
                 u3_noun* f);

      /* u3r_met():
      **
      **   Return the size of (b) in bits, rounded up to
      **   (1 << a_y).
      **
      **   For example, (a_y == 3) returns the size in bytes.
      */
        c3_w
        u3r_met(c3_y    a_y,
                u3_atom b);

      /* u3r_bit():
      **
      **   Return bit (a_w) of (b).
      */
        c3_b
        u3r_bit(c3_w    a_w,
                u3_atom b);

      /* u3r_byte():
      **
      **   Return byte (a_w) of (b).
      */
        c3_y
        u3r_byte(c3_w    a_w,
                 u3_atom b);

      /* u3r_bytes():
      **
      **   Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u3r_bytes(c3_w    a_w,
                  c3_w    b_w,
                  c3_y*   c_y,
                  u3_atom d);

      /* u3r_chop():
      **
      **   Into the bloq space of `met`, from position `fum` for a
      **   span of `wid`, to position `tou`, XOR from atom `src`
      **   into ray `dst`.
      */
        void
        u3r_chop(c3_g    met_g,
                 c3_w    fum_w,
                 c3_w    wid_w,
                 c3_w    tou_w,
                 c3_w*   dst_w,
                 u3_atom src);

      /* u3r_mp():
      **
      **   Copy (b) into (a_mp).
      */
        void
        u3r_mp(mpz_t   a_mp,
               u3_atom b);

      /* u3r_word():
      **
      **   Return word (a_w) of (b).
      */
        c3_w
        u3r_word(c3_w    a_w,
                 u3_atom b);

      /* u3r_chub():
      **
      **   Return double-word (a_w) of (b).
      */
        c3_d
        u3r_chub(c3_w    a_w,
                 u3_atom b);

      /* u3r_words():
      **
      **  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u3r_words(c3_w    a_w,
                  c3_w    b_w,
                  c3_w*   c_w,
                  u3_atom d);

      /* u3r_chubs():
      **
      **  Copy double-words (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u3r_chubs(c3_w    a_w,
                  c3_w    b_w,
                  c3_d*   c_d,
                  u3_atom d);

      /* u3r_string(): `a`, a text atom, as malloced C string.
      */
        c3_c*
        u3r_string(u3_atom a);

      /* u3r_tape(): `a`, a list of bytes, as malloced C string.
      */
        c3_y*
        u3r_tape(u3_noun a);
