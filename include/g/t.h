/* include/g/t.h
**
** This file is in the public domain.
*/
    /** Tracing.
    **/
      /* u2_ct_push(): push on trace stack.
      */
        void
        u2_ct_push(u2_noun mon);

      /* u2_ct_mean(): push `[%mean roc]` on trace stack.
      */
        void
        u2_ct_mean(u2_noun roc);

      /* u2_ct_drop(): drop from meaning stack.
      */
        void
        u2_ct_drop(void);

      /* u2_ct_slog(): print directly.
      */
        void
        u2_ct_slog(u2_noun hod);

