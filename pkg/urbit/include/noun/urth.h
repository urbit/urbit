/* include/noun/urth.h
*/
    /**  Functions.
    **/
      /* u3u_meld(): globally deduplicate memory.
      */
        void
        u3u_meld(void);

      /* u3u_cram(): globably deduplicate memory, and write a rock to disk.
      */
        c3_o
        u3u_cram(c3_c* dir_c, c3_d eve_d);
      /* u3u_uncram(): restore persistent state from a rock.
      */
        c3_o
        u3u_uncram(c3_c* dir_c, c3_d eve_d);

      /* u3u_mmap_read(): open and mmap the file at [pat_c] for reading.
      */
        c3_o
        u3u_mmap_read(c3_c* cap_c, c3_c* pat_c, c3_d* out_d, c3_y** out_y);

      /* u3u_mmap(): open/create file-backed mmap at [pat_c] for read/write.
      */
        c3_o
        u3u_mmap(c3_c* cap_c, c3_c* pat_c, c3_d len_d, c3_y** out_y);

      /* u3u_mmap_save(): sync file-backed mmap.
      */
        c3_o
        u3u_mmap_save(c3_c* cap_c, c3_c* pat_c, c3_d len_d, c3_y* byt_y);

      /* u3u_munmap(): unmap the region at [byt_y].
      */
        c3_o
        u3u_munmap(c3_d len_d, c3_y* byt_y);
