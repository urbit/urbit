/* include/noun/imprison.h
**
** This file is in the public domain.
*/

  /**  Structures.
  **/
    /* u3i_slab: atom builder.
    */
      typedef struct _u3i_slab {
        struct {                              //  internals
          u3a_atom* _vat_u;                   //  heap atom (nullable)
          c3_w      _sat_w;                   //  static storage
        } _;                                  //
        union {                               //
          c3_y*      buf_y;                   //  bytes
          c3_w*      buf_w;                   //  words
        };                                    //
        c3_w         len_w;                   //  word length
      } u3i_slab;

      /* staged atom-building api
      */
        /* u3i_slab_init(): configure bloq-length slab, zero-initialize.
        */
          void
          u3i_slab_init(u3i_slab* sab_u, c3_g met_g, c3_d len_d);

        /* u3i_slab_bare(): configure bloq-length slab, uninitialized.
        */
          void
          u3i_slab_bare(u3i_slab* sab_u, c3_g met_g, c3_d len_d);

        /* u3i_slab_from(): configure bloq-length slab, initialize with [a].
        */
          void
          u3i_slab_from(u3i_slab* sab_u, u3_atom a, c3_g met_g, c3_d len_d);

        /* u3i_slab_grow(): resize slab, zero-initializing new space.
        */
          void
          u3i_slab_grow(u3i_slab* sab_u, c3_g met_g, c3_d len_d);

        /* u3i_slab_free(): dispose memory backing slab.
        */
          void
          u3i_slab_free(u3i_slab* sab_u);

        /* u3i_slab_mint(): produce atom from slab, trimming.
        */
          u3_atom
          u3i_slab_mint(u3i_slab* sab_u);

        /* u3i_slab_moot(): produce atom from slab, no trimming.
        */
          u3_atom
          u3i_slab_moot(u3i_slab* sab_u);

        /* u3i_slab_mint_bytes(): produce atom from byte-slab, trimming.
        ** XX assumes little-endian, implement swap to support big-endian
        */
#  define u3i_slab_mint_bytes u3i_slab_mint

        /* u3i_slab_moot_bytes(): produce atom from byte-slab, no trimming.
        ** XX assumes little-endian, implement swap to support big-endian
        */
#  define u3i_slab_moot_bytes u3i_slab_moot

      /* General constructors.
      */
        /* u3i_word(): construct u3_atom from c3_w.
        */
          u3_atom
          u3i_word(c3_w dat_w);

        /* u3i_chub(): construct u3_atom from c3_d.
        */
          u3_atom
          u3i_chub(c3_d dat_d);

        /* u3i_bytes(): Copy [a] bytes from [b] to an LSB first atom.
        */
          u3_atom
          u3i_bytes(c3_w        a_w,
                    const c3_y* b_y);

        /* u3i_words(): Copy [a] words from [b] into an atom.
        */
          u3_atom
          u3i_words(c3_w        a_w,
                    const c3_w* b_w);

        /* u3i_chubs(): Copy [a] chubs from [b] into an atom.
        */
          u3_atom
          u3i_chubs(c3_w        a_w,
                    const c3_d* b_d);

        /* u3i_mp(): Copy the GMP integer [a] into an atom, and clear it.
        */
          u3_atom
          u3i_mp(mpz_t a_mp);

        /* u3i_vint(): increment [a].
        */
          u3_atom
          u3i_vint(u3_noun a);

        /* u3i_cell(): Produce the cell `[a b]`.
        */
          u3_noun
          u3i_cell(u3_noun a, u3_noun b);

        /* u3i_defcons(): allocate cell for deferred construction.
        **            NB: [hed] and [tel] pointers MUST be filled.
        */
          u3_cell
          u3i_defcons(u3_noun** hed, u3_noun** tel);

        /* u3i_trel(): Produce the triple `[a b c]`.
        */
          u3_noun
          u3i_trel(u3_noun a, u3_noun b, u3_noun c);

        /* u3i_qual(): Produce the cell `[a b c d]`.
        */
          u3_noun
          u3i_qual(u3_noun a, u3_noun b, u3_noun c, u3_noun d);

        /* u3i_string(): Produce an LSB-first atom from the C string [a].
        */
          u3_noun
          u3i_string(const c3_c* a_c);

        /* u3i_tape(): from a C string, to a list of bytes.
        */
          u3_atom
          u3i_tape(const c3_c* txt_c);

        /* u3i_list(): list from `u3_none`-terminated varargs.
        */
          u3_noun
          u3i_list(u3_weak som, ...);

        /* u3i_edit():
        **
        **   Mutate `big` at axis `axe` with new value `som`
        **   `axe` is RETAINED.
        */
          u3_noun
          u3i_edit(u3_noun big, u3_noun axe, u3_noun som);

        /* u3i_molt():
        **
        **   Mutate `som` with a 0-terminated list of axis, noun pairs.
        **   Axes must be cats (31 bit).
        */
          u3_noun
          u3i_molt(u3_noun som, ...);
