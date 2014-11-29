/* i/n/t.h
**
** This file is in the public domain.
*/
  /**  Functions.
  **/
    /* u3t_push(): push on trace stack.
    */
      void
      u3t_push(u3_noun mon);

    /* u3t_mean(): push `[%mean roc]` on trace stack.
    */
      void
      u3t_mean(u3_noun roc);

    /* u3t_drop(): drop from meaning stack.
    */
      void
      u3t_drop(void);

    /* u3t_slog(): print directly.
    */
      void
      u3t_slog(u3_noun hod);

    /* u3t_heck(): profile point.
    */
      void
      u3t_heck(u3_atom cog);

    /* u3t_samp(): sample.
    */
      void
      u3t_samp(void);

    /* u3t_come(): push on profile stack; return yes if active push.  RETAIN.
    */
      c3_o
      u3t_come(u3_noun bat);

    /* u3t_flee(): pop off profile stack.
    */
      void
      u3t_flee(void);

    /* u3t_damp(): print and clear profile data.
    */
      void
      u3t_damp(void);

    /* u3t_boff(): turn profile sampling off.
    */
      void
      u3t_boff(void);

    /* u3t_boot(): turn sampling on.
    */
      void
      u3t_boot(void);
