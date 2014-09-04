/* include/g/i.h
**
** This file is in the public domain.
*/
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

        /* u2_ci_trel():
        **
        **   Produce the triple `[a b c]`.
        */
          u2_noun
          u2_ci_trel(u2_noun a, u2_noun b, u2_noun c);

        /* u2_ci_qual():
        **
        **   Produce the cell `[a b c d]`.
        */
          u2_noun
          u2_ci_qual(u2_noun a, u2_noun b, u2_noun c, u2_noun d);

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


