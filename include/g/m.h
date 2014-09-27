/* include/g/m.h
**
** This file is in the public domain.
*/
    /** System management.
    **/
      /* u3_cm_boot(): set up top-level road.
      */
        void
        u3_cm_boot(c3_o nuu_o);

      /* u3_cm_trap(): setjmp within road.
      */
#if 0
        u3_bean
        u3_cm_trap(void);
#else
#       define u3_cm_trap() (u3_noun)(setjmp(u3R->esc.buf))
#endif

      /* u3_cm_bail(): bail out.  Does not return.
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
        u3_cm_bail(c3_m how_m);

      /* u3_cm_dump(): dump the current road to stderr.
      */
        void
        u3_cm_dump(void);

      /* u3_cm_file(): load file, as atom, or bail.
      */
        u3_noun
        u3_cm_file(c3_c* pas_c);

      /* u3_cm_clear(): clear all allocated data in road.
      */
        void
        u3_cm_clear(void);

      /* u3_cm_error(): bail out with %exit, ct_pushing error.
      */
        c3_i
        u3_cm_error(c3_c* str_c);

      /* u3_cm_grab(): garbage-collect memory.  Asserts u3R == u3H.
      */
        void
        u3_cm_grab(void);

      /* u3_cm_check(): checkpoint memory to file.  Asserts u3R == u3H.
      */
        void
        u3_cm_check(void);

      /* u3_cm_fall(): return to parent road.
      */
        void
        u3_cm_fall(void);

      /* u3_cm_leap(): advance to inner road.
      */
        void
        u3_cm_leap(void);

      /* u3_cm_wash(): wash all lazy mugs.  RETAIN.
      */
        void
        u3_cm_wash(u3_noun som);

      /* u3_cm_golf(): record cap length for u3_flog().
      */
        c3_w
        u3_cm_golf(void);

      /* u3_cm_flog(): pop the cap.
      **
      **    A common sequence for inner allocation is:
      **
      **    c3_w gof_w = u3_cm_golf();
      **    u3_cm_leap();
      **    //  allocate some inner stuff...
      **    u3_cm_fall();
      **    //  inner stuff is still valid, but on cap
      **    u3_cm_flog(gof_w);
      **
      ** u3_cm_flog(0) simply clears the cap.
      */
        void
        u3_cm_flog(c3_w gof_w);

      /* u3_cm_soft(): system soft wrapper.  unifies unix and nock errors.
      **
      **  Produces [%$ result] or [%error (list tank)].
      */
        u3_noun
        u3_cm_soft(c3_w sec_w, u3_funk fun_f, u3_noun arg);

      /* u3_cm_water(): produce high and low watermarks.  Asserts u3R == u3H.
      */
        void
        u3_cm_water(c3_w *low_w, c3_w *hig_w);

      /* u3_cm_pretty(): dumb prettyprint to string.  RETAIN.
      */
        c3_c* 
        u3_cm_pretty(u3_noun som);

      /* u3_cm_p(): dumb print with caption.  RETAIN.
      */
        void
        u3_cm_p(const c3_c* cap_c, u3_noun som);

      /* u3_cm_tape(): dump a tape to stdout.
      */
        void
        u3_cm_tape(u3_noun tep);

      /* u3_cm_wall(): dump a wall to stdout.
      */
        void
        u3_cm_wall(u3_noun wol);
