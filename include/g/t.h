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

