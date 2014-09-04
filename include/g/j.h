/* include/g/j.h
**
** This file is in the public domain.
*/
    /**  Jets.
    **/
      /* u2_cj_boot(): initialize jet system.
      */
        void
        u2_cj_boot(void);

      /* u2_cj_hook():
      **
      **   Execute hook from core. 
      */
        u2_noun
        u2_cj_hook(u2_noun     cor,
                   const c3_c* tam_c);

      /* u2_cj_find(): battery to driver number, or 0.
      **
      ** `bat` is RETAINED by the caller.
      */
        c3_l
        u2_cj_find(u2_noun bat);

      /* u2_cj_kick(): try to kick by jet.  If no kick, produce u2_none.
      **
      ** `axe` is RETAINED by the caller; `cor` is RETAINED iff there 
      ** is no kick, TRANSFERRED if one.
      */
        u2_weak
        u2_cj_kick(u2_noun cor,
                   u2_noun axe);

      /* u2_cj_kink(): kick either by jet or by nock.
      */
        u2_noun
        u2_cj_kink(u2_noun cor,
                   u2_noun axe);
        
      /* u2_cj_mine(): register core for jets.
      */
        u2_noun
        u2_cj_mine(u2_noun clu,
                   u2_noun cor);

