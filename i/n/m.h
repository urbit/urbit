/* i/n/m.h
**
** This file is in the public domain.
*/
    /** System management.
    **/
      /* u3m_boot(): set up top-level road.
      */
        void
        u3m_boot(c3_o nuu_o, c3_o bug_o);

      /* u3m_trap(): setjmp within road.
      */
#if 0
        c3_o
        u3m_trap(void);
#else
#       define u3m_trap() (u3_noun)(setjmp(u3R->esc.buf))
#endif

      /* u3m_signal(): treat a nock-level exception as a signal interrupt.
      */
        void
        u3m_signal(u3_noun sig_l);

      /* u3m_bail(): bail out.  Does not return.
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
        u3m_bail(c3_m how_m);

      /* u3m_dump(): dump the current road to stderr.
      */
        void
        u3m_dump(void);

      /* u3m_file(): load file, as atom, or bail.
      */
        u3_noun
        u3m_file(c3_c* pas_c);

      /* u3m_clear(): clear all allocated data in road.
      */
        void
        u3m_clear(void);

      /* u3m_mark(): mark all nouns in the road.
      */
        void
        u3m_mark(void);

      /* u3m_error(): bail out with %exit, ct_pushing error.
      */
        c3_i
        u3m_error(c3_c* str_c);

      /* u3m_check(): checkpoint memory to file.  Asserts u3R == u3H.
      */
        void
        u3m_check(void);

      /* u3m_fall(): return to parent road.
      */
        void
        u3m_fall(void);

      /* u3m_hate(): new, integrated leap mechanism (enter).
      */
        void
        u3m_hate(c3_w pad_w);

      /* u3m_love(): return product from leap.
      */
        u3_noun
        u3m_love(u3_noun pro);

      /* u3m_leap(): in u3R, create a new road within the existing one.
      */
        void
        u3m_leap(c3_w pad_w);

      /* u3m_golf(): record cap length for u3_flog().
      */
        c3_w
        u3m_golf(void);

      /* u3m_flog(): pop the cap.
      **
      **    A common sequence for inner allocation is:
      **
      **    c3_w gof_w = u3m_golf();
      **    u3m_leap();
      **    //  allocate some inner stuff...
      **    u3m_fall();
      **    //  inner stuff is still valid, but on cap
      **    u3m_flog(gof_w);
      **
      ** u3m_flog(0) simply clears the cap.
      */
        void
        u3m_flog(c3_w gof_w);

      /* u3m_soft(): system soft wrapper.  unifies unix and nock errors.
      **
      **  Produces [%$ result] or [%error (list tank)].
      */
        u3_noun
        u3m_soft(c3_w sec_w, u3_funk fun_f, u3_noun arg);

      /* u3m_soft_top(): top-level safety wrapper.
      */
        u3_noun 
        u3m_soft_top(c3_w    sec_w,                     //  timer seconds
                     c3_w    pad_w,                     //  base memory pad
                     u3_funk fun_f,
                     u3_noun arg);

      /* u3m_soft_slam: top-level call.
      */
        u3_noun 
        u3m_soft_slam(u3_noun gat, u3_noun sam);

      /* u3m_soft_sure(): top-level call assumed correct.
      */
        u3_noun 
        u3m_soft_sure(u3_funk fun_f, u3_noun arg);

      /* u3m_soft_run(): descend into virtualization context.
      */
        u3_noun 
        u3m_soft_run(u3_noun fly,
                       u3_funq fun_f,
                       u3_noun aga,
                       u3_noun agb);

      /* u3m_soft_esc(): namespace lookup to (unit ,*).
      */
        u3_noun
        u3m_soft_esc(u3_noun sam);

      /* u3m_water(): produce high and low watermarks.  Asserts u3R == u3H.
      */
        void
        u3m_water(c3_w *low_w, c3_w *hig_w);

      /* u3m_pretty(): dumb prettyprint to string.  RETAIN.
      */
        c3_c* 
        u3m_pretty(u3_noun som);

      /* u3m_p(): dumb print with caption.  RETAIN.
      */
        void
        u3m_p(const c3_c* cap_c, u3_noun som);

      /* u3m_tape(): dump a tape to stdout.
      */
        void
        u3m_tape(u3_noun tep);

      /* u3m_wall(): dump a wall to stdout.
      */
        void
        u3m_wall(u3_noun wol);
