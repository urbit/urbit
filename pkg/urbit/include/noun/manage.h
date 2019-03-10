/* i/n/m.h
**
** This file is in the public domain.
*/
    /** System management.
    **/
      /* u3m_boot(): start the u3 system.
      */
        void
        u3m_boot(c3_o nuu_o, c3_o bug_o, c3_c* dir_c, c3_c *pil_c, c3_c *url_c, c3_c *arv_c);

      /* u3m_boot_new(): start the u3 system (new).  return next event,
      ** starting from 1.
      */
        c3_d
        u3m_boot_new(c3_c* dir_c);

      /* u3m_boot_pier(): start without checkpointing.
      */
        c3_d
        u3m_boot_pier(void);

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
      **    %time               ::  timed out
      **    %oops               ::  assertion failure
      */
        c3_i
        u3m_bail(c3_m how_m) __attribute__((noreturn));

      /* u3m_init(): start the environment.
      */
        void
        u3m_init();

      /* u3m_pave(): instantiate or activate image.
      */
        void
        u3m_pave(c3_o nuu_o, c3_o bug_o);

      /* u3m_file(): load file, as atom, or bail.
      */
        u3_noun
        u3m_file(c3_c* pas_c);

      /* u3m_error(): bail out with %exit, ct_pushing error.
      */
        c3_i
        u3m_error(c3_c* str_c);

      /* u3m_hate(): new, integrated leap mechanism (enter).
      */
        void
        u3m_hate(c3_w pad_w);

      /* u3m_love(): return product from leap.
      */
        u3_noun
        u3m_love(u3_noun pro);

      /* u3m_soft(): system soft wrapper.  unifies unix and nock errors.
      **
      **  Produces [%$ result] or [%error (list tank)].
      */
        u3_noun
        u3m_soft(c3_w sec_w, u3_funk fun_f, u3_noun arg);

      /* u3m_soft_slam: top-level call.
      */
        u3_noun
        u3m_soft_slam(u3_noun gat, u3_noun sam);

      /* u3m_soft_nock: top-level nock.
      */
        u3_noun
        u3m_soft_nock(u3_noun bus, u3_noun fol);

      /* u3m_soft_sure(): top-level call assumed correct.
      */
        u3_noun
        u3m_soft_sure(u3_funk fun_f, u3_noun arg);

      /* u3m_soft_run(): descend into virtualization context.
      */
        u3_noun
        u3m_soft_run(u3_noun gul,
                     u3_funq fun_f,
                     u3_noun aga,
                     u3_noun agb);

      /* u3m_soft_esc(): namespace lookup to (unit ,*).
      */
        u3_noun
        u3m_soft_esc(u3_noun ref, u3_noun sam);

      /* u3m_mark(): mark all nouns in the road.
      */
        c3_w
        u3m_mark(FILE* fil_u);

      /* u3m_grab(): garbage-collect the world, plus extra roots.
      */
        void
        u3m_grab(u3_noun som, ...);   // terminate with u3_none

      /* u3m_water(): produce high and low watermarks.  Asserts u3R == u3H.
      */
        void
        u3m_water(c3_w *low_w, c3_w *hig_w);

      /* u3m_pretty(): dumb prettyprint to string.  RETAIN.
      */
        c3_c*
        u3m_pretty(u3_noun som);

      /* u3m_pretty_path(): prettyprint a path to string.  RETAIN.
      */
        c3_c*
        u3m_pretty_path(u3_noun som);

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

      /* u3m_reclaim: clear persistent caches to reclaim memory
      */
        void
        u3m_reclaim(void);
