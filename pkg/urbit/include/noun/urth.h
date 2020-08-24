/* include/noun/urth.h
*/
    /**  Functions.
    **/
      /* u3u_uniq(): hash-cons roots off-loom, reallocate on loom.
      */
        void
        u3u_uniq(void);

      /* u3u_cram(): globably deduplicate memory, and write a rock to disk.
      */
        c3_o
        u3u_cram(c3_c* dir_c, c3_d eve_d);
