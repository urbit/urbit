/* include/g/i.h
**
** This file is in the public domain.
*/
      /* General constructors.
      */
        /* u3_ci_words():
        **
        **   Copy [a] words from [b] into an atom.
        */
          u3_noun
          u3_ci_words(c3_w        a_w,
                      const c3_w* b_w);

        /* u3_ci_bytes():
        **
        **   Copy `a` bytes from `b` to an LSB first atom.
        */
          u3_noun
          u3_ci_bytes(c3_w        a_w,
                      const c3_y* b_y);

        /* u3_ci_mp():
        **
        **   Copy the GMP integer `a` into an atom, and clear it.
        */
          u3_noun
          u3_ci_mp(mpz_t a_mp);

        /* u3_ci_vint():
        **
        **   Create `a + 1`.
        */
          u3_noun
          u3_ci_vint(u3_noun a);

        /* u3_ci_cell():
        **
        **   Produce the cell `[a b]`.
        */
          u3_noun
          u3_ci_cell(u3_noun a, u3_noun b);

        /* u3_ci_trel():
        **
        **   Produce the triple `[a b c]`.
        */
          u3_noun
          u3_ci_trel(u3_noun a, u3_noun b, u3_noun c);

        /* u3_ci_qual():
        **
        **   Produce the cell `[a b c d]`.
        */
          u3_noun
          u3_ci_qual(u3_noun a, u3_noun b, u3_noun c, u3_noun d);

        /* u3_ci_string():
        **
        **   Produce an LSB-first atom from the C string `a`.
        */
          u3_noun
          u3_ci_string(const c3_c* a_c);

        /* u3_ci_molt():
        **
        **   Mutate `som` with a 0-terminated list of axis, noun pairs.
        **   Axes must be cats (31 bit).
        */
          u3_noun 
          u3_ci_molt(u3_noun som, ...);

        /* u3_ci_chubs():
        **
        **   Construct `a` double-words from `b`, LSD first, as an atom.
        */
          u3_atom
          u3_ci_chubs(c3_w        a_w,
                      const c3_d* b_d);

        /* u3_ci_tape(): from a C string, to a list of bytes.
        */
          u3_atom
          u3_ci_tape(const c3_c* txt_c);


