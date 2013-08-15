/* include/fake/log.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /* u4_log_at():
    **
    **   Produce the nth cow in (log), or bull.
    */
      _(u4_nopt, cow)
      u4_log_at(u4_log  log,
                u4_atom n);

    /* u4_log_flip(): 
    **
    **   On (lane), produce the inverse of (log).
    */
      _(u4_log, log_pro)
      u4_log_flip(u4_lane lane,
                  u4_log  log);

    /* u4_log_len():
    **
    **   Produce the length of (log).
    */
      _(u4_xw, xw_len)
      u4_log_len(u4_log log);

    /* u4_log_cat():
    **
    **   Concatenate (log_a) and (log_b).
    */
      u4_log
      u4_log_cat(u4_lane lane,
                 u4_log  log_a,
                 u4_log  log_b);

    /* u4_log_tupl():
    **
    **   Convert (log) to a tuple.
    */
      _(u4_noun, pro)
      u4_log_tupl(u4_lane lane,
                  u4_log  log);
