/* i/n/serial.h
**
*/
    /*  forward declarations
    */
      /* ur_dict32_s: off-loom 32-bit dictionary.
      */
        struct ur_dict32_s;

    /*  Noun serialization. All noun arguments RETAINED.
    */

      /* u3s_jam_fib(): jam without atom allocation.
      **
      **   returns atom-suitable words, and *bit_w will have
      **   the length (in bits). return should be freed with u3a_wfree().
      */
        c3_w*
        u3s_jam_fib(u3_noun a, c3_w* bit_w);

      /* u3s_jam_xeno(): jam with off-loom buffer (re-)allocation.
      */
        c3_d
        u3s_jam_xeno(u3_noun a, c3_d* len_d, c3_y** byt_y);

      /* u3s_cue(): cue [a]
      */
        u3_noun
        u3s_cue(u3_atom a);

      /* u3s_cue_xeno_unsafe(): cue onto the loom, all bookkeeping off-loom.
      */
        c3_o
        u3s_cue_xeno_unsafe(struct ur_dict32_s* dic_u,
                            c3_d         len_d,
                            const c3_y*  byt_y,
                            u3_noun*       out);

      /* u3s_cue_xeno(): cue onto the loom, bookkeeping off the loom.
      */
        c3_o
        u3s_cue_xeno(c3_d len_d, const c3_y* byt_y, u3_noun* out);

      /* u3s_cue_bytes(): cue bytes onto the loom.
      */
        u3_noun
        u3s_cue_bytes(c3_d len_d, const c3_y* byt_y);

      /* u3s_cue_atom(): cue atom.
      */
        u3_noun
        u3s_cue_atom(u3_atom a);
