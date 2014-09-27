/* include/g/t.h
**
** This file is in the public domain.
*/
    /** Tracing.
    **/
      /* u3_ct_push(): push on trace stack.
      */
        void
        u3_ct_push(u3_noun mon);

      /* u3_ct_mean(): push `[%mean roc]` on trace stack.
      */
        void
        u3_ct_mean(u3_noun roc);

      /* u3_ct_drop(): drop from meaning stack.
      */
        void
        u3_ct_drop(void);

      /* u3_ct_slog(): print directly.
      */
        void
        u3_ct_slog(u3_noun hod);

      /* u3_ct_heck(): profile point.
      */
        void
        u3_ct_heck(u3_atom cog);

      /* u3_ct_samp(): sample.
      */
        void
        u3_ct_samp(void);

      /* u3_ct_come(): push on profile stack.
      */
        void
        u3_ct_come(u3_atom cog);

      /* u3_ct_flee(): pop off profile stack.
      */
        void
        u3_ct_flee(void);

      /* u3_ct_damp(): print and clear profile data.
      */
        void
        u3_ct_damp(void);

      /* u3_ct_boff(): turn profile sampling off.
      */
        void
        u3_ct_boff(void);

      /* u3_ct_boot(): turn sampling on.
      */
        void
        u3_ct_boot(void);
