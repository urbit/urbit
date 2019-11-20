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
