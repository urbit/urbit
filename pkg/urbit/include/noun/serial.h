/* i/n/serial.h
**
*/
    /*  opaque handles
    */
      /* u3_cue_xeno: handle for cue-ing with an off-loom dictionary.
      */
        typedef struct _u3_cue_xeno u3_cue_xeno;

    /*  Noun serialization. All noun arguments RETAINED.
    */

      /* u3s_jam_fib(): jam without atom allocation.
      **
      **   returns atom-suitable words, and *bit_w will have
      **   the length (in bits). return should be freed with u3a_wfree().
      */
        c3_w
        u3s_jam_fib(u3i_slab* sab_u, u3_noun a);

      /* u3s_jam_xeno(): jam with off-loom buffer (re-)allocation.
      */
        c3_d
        u3s_jam_xeno(u3_noun a, c3_d* len_d, c3_y** byt_y);

      /* u3s_cue(): cue [a]
      */
        u3_noun
        u3s_cue(u3_atom a);

      /* u3s_cue_xeno_init_with(): initialize a cue_xeno handle as specified.
      */
        u3_cue_xeno*
        u3s_cue_xeno_init_with(c3_d pre_d, c3_d siz_d);

      /* u3s_cue_xeno_init(): initialize a cue_xeno handle.
      */
        u3_cue_xeno*
        u3s_cue_xeno_init(void);

      /* u3s_cue_xeno_init(): cue on-loom, with off-loom dictionary in handle.
      */
        u3_weak
        u3s_cue_xeno_with(u3_cue_xeno* sil_u,
                          c3_d         len_d,
                          const c3_y*  byt_y);

      /* u3s_cue_xeno_init(): dispose cue_xeno handle.
      */
        void
        u3s_cue_xeno_done(u3_cue_xeno* sil_u);

      /* u3s_cue_xeno(): cue on-loom, with off-loom dictionary.
      */
        u3_weak
        u3s_cue_xeno(c3_d        len_d,
                     const c3_y* byt_y);

      /* u3s_cue_bytes(): cue bytes onto the loom.
      */
        u3_noun
        u3s_cue_bytes(c3_d len_d, const c3_y* byt_y);

      /* u3s_cue_atom(): cue atom.
      */
        u3_noun
        u3s_cue_atom(u3_atom a);
