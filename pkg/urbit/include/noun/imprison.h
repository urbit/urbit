/* include/n/i.h
**
** This file is in the public domain.
*/
      /* General constructors.
      */
        /* u3i_words():
        **
        **   Copy [a] words from [b] into an atom.
        */
          u3_noun
          u3i_words(c3_w        a_w,
                    const c3_w* b_w);

        /* u3i_bytes():
        **
        **   Copy `a` bytes from `b` to an LSB first atom.
        */
          u3_noun
          u3i_bytes(c3_w        a_w,
                    const c3_y* b_y);

        /* u3i_mp():
        **
        **   Copy the GMP integer `a` into an atom, and clear it.
        */
          u3_noun
          u3i_mp(mpz_t a_mp);

        /* u3i_vint():
        **
        **   Create `a + 1`.
        */
          u3_noun
          u3i_vint(u3_noun a);

        /* u3i_cell():
        **
        **   Produce the cell `[a b]`.
        */
          u3_noun
          u3i_cell(u3_noun a, u3_noun b);

        /* u3i_trel():
        **
        **   Produce the triple `[a b c]`.
        */
          u3_noun
          u3i_trel(u3_noun a, u3_noun b, u3_noun c);

        /* u3i_qual():
        **
        **   Produce the cell `[a b c d]`.
        */
          u3_noun
          u3i_qual(u3_noun a, u3_noun b, u3_noun c, u3_noun d);

        /* u3i_edit():
        **
        **   Mutate `big` at axis `axe` with new value `som`
        **   `axe` is RETAINED.
        */
          u3_noun
          u3i_edit(u3_noun big, u3_noun axe, u3_noun som);

        /* u3i_string():
        **
        **   Produce an LSB-first atom from the C string `a`.
        */
          u3_noun
          u3i_string(const c3_c* a_c);

        /* u3i_molt():
        **
        **   Mutate `som` with a 0-terminated list of axis, noun pairs.
        **   Axes must be cats (31 bit).
        */
          u3_noun
          u3i_molt(u3_noun som, ...);

        /* u3i_chubs():
        **
        **   Construct `a` double-words from `b`, LSD first, as an atom.
        */
          u3_atom
          u3i_chubs(c3_w        a_w,
                    const c3_d* b_d);

        /* u3i_tape(): from a C string, to a list of bytes.
        */
          u3_atom
          u3i_tape(const c3_c* txt_c);


