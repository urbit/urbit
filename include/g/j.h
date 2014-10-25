/* include/g/j.h
**
** This file is in the public domain.
*/
    /**  Jets.
    **/
      /* u3_cj_boot(): initialize jet system.
      */
        void
        u3_cj_boot(void);

      /* u3_cj_clear(): clear jet table to re-register.
      */
        void
        u3_cj_clear(void);

      /* u3_cj_hook():
      **
      **   Execute hook from core. 
      */
        u3_noun
        u3_cj_hook(u3_noun     cor,
                   const c3_c* tam_c);

      /* u3_cj_soft():
      **
      **   Execute hook from core, without jet.
      */
        u3_noun
        u3_cj_soft(u3_noun     cor,
                   const c3_c* tam_c);

      /* u3_cj_find(): battery to driver number, or 0.
      **
      ** `bat` is RETAINED by the caller.
      */
        c3_l
        u3_cj_find(u3_noun bat);

      /* u3_cj_kick(): try to kick by jet.  If no kick, produce u3_none.
      **
      ** `axe` is RETAINED by the caller; `cor` is RETAINED iff there 
      ** is no kick, TRANSFERRED if one.
      */
        u3_weak
        u3_cj_kick(u3_noun cor,
                   u3_noun axe);

      /* u3_cj_kicq(): new kick.
      **
      ** `axe` is RETAINED by the caller; `cor` is RETAINED iff there 
      ** is no kick, TRANSFERRED if one.
      */
        u3_weak
        u3_cj_kicq(u3_noun cor, u3_noun axe);

      /* u3_cj_kink(): kick either by jet or by nock.
      */
        u3_noun
        u3_cj_kink(u3_noun cor,
                   u3_noun axe);
 
      /* u3_cj_mine(): register core for jets.
      */
        void
        u3_cj_mine(u3_noun clu,
                   u3_noun cor);
