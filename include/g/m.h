/* include/g/m.h
**
** This file is in the public domain.
*/
    /** System management.
    **/
      /* u2_cm_boot(): make u2R and u2H from `len` words at `adr`.
      */
        void
        u2_cm_boot(c3_p adr_p, c3_w len_w);

      /* u2_cm_trap(): setjmp within road.
      */
#if 0
        u2_bean
        u2_cm_trap(void);
#else
#       define u2_cm_trap() (u2_noun)(setjmp(u2R->esc.buf))
#endif

      /* u2_cm_bail(): bail out.  Does not return.
      **
      **  Bail motes:
      **
      **    %exit               ::  semantic failure
      **    %evil               ::  bad crypto
      **    %intr               ::  interrupt
      **    %fail               ::  execution failure
      **    %foul               ::  assert failure
      **    %need               ::  network block
      **    %meme               ::  out of memory
      */ 
        c3_i
        u2_cm_bail(c3_m how_m);

      /* u2_cm_dump(): dump the current road to stderr.
      */
        void
        u2_cm_dump(void);

      /* u2_cm_error(): bail out with %exit, ct_pushing error.
      */
        c3_i
        u2_cm_error(c3_c* str_c);

      /* u2_cm_grab(): garbage-collect memory.  Asserts u2R == u2H.
      */
        void
        u2_cm_grab(void);

      /* u2_cm_check(): checkpoint memory to file.  Asserts u2R == u2H.
      */
        void
        u2_cm_check(void);

      /* u2_cm_fall(): return to parent road.
      */
        void
        u2_cm_fall(void);

      /* u2_cm_leap(): advance to inner road.
      */
        void
        u2_cm_leap(void);

      /* u2_cm_golf(): record cap length for u2_flog().
      */
        c3_w
        u2_cm_golf(void);

      /* u2_cm_flog(): pop the cap.
      **
      **    A common sequence for inner allocation is:
      **
      **    c3_w gof_w = u2_cm_golf();
      **    u2_cm_leap();
      **    //  allocate some inner stuff...
      **    u2_cm_fall();
      **    //  inner stuff is still valid, but on cap
      **    u2_cm_flog(gof_w);
      **
      ** u2_cm_flog(0) simply clears the cap.
      */
        void
        u2_cm_flog(c3_w gof_w);

      /* u2_cm_water(): produce high and low watermarks.  Asserts u2R == u2H.
      */
        void
        u2_cm_water(c3_w *low_w, c3_w *hig_w);



