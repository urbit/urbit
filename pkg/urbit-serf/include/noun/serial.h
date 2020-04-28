/* i/n/serial.h
**
*/
    /*  Noun serialization. All noun arguments RETAINED.
    */

      /* u3s_jam_fib(): jam without atom allocation.
      **
      **   returns atom-suitable words, and *bit_w will have
      **   the length (in bits). return should be freed with u3a_wfree().
      */
        c3_w*
        u3s_jam_fib(u3_noun a, c3_w* bit_w);

      /* u3s_jam_met(): measure a noun for jam, calculating backrefs
      */
        c3_d
        u3s_jam_met(u3_noun a, u3p(u3h_root)* bak_p);

      /* u3s_jam_buf(): jam [a] into [buf_w], without allocation
      **
      **   using backrefs in [bak_p], as computed by u3s_jam_met
      **   can only encode up to c3_w bits
      */
        void
        u3s_jam_buf(u3_noun a, u3p(u3h_root) bak_p, c3_w* buf_w);

      /* u3s_jam_file(): jam [a] into a file, overwriting
      */
        c3_o
        u3s_jam_file(u3_noun a, c3_c* pas_c);

      /* u3s_cue(): cue [a]
      */
        u3_noun
        u3s_cue(u3_atom a);
