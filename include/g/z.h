/* include/g/z.h
**
** This file is in the public domain.
*/
  /**  Memoization.
  ***
  ***  The memo cache is keyed by an arbitrary symbolic function
  ***  and a noun argument to that (logical) function.  Functions
  ***  are predefined by C-level callers, but 0 means nock.
  ***
  ***  The memo cache is within its road and dies when it falls.
  ***
  ***  Memo functions RETAIN keys and transfer values.
  **/
    /* u2_cz_find*(): find in memo cache.
    */
      u2_weak u2_cz_find(u2_mote, u2_noun);
      u2_weak u2_cz_find_2(u2_mote, u2_noun, u2_noun);
      u2_weak u2_cz_find_3(u2_mote, u2_noun, u2_noun, u2_noun);
      u2_weak u2_cz_find_4(u2_mote, u2_noun, u2_noun, u2_noun, u2_noun);

    /* u2_cz_save*(): save in memo cache.
    */
      u2_noun u2_cz_save(u2_mote, u2_noun, u2_noun);
      u2_noun u2_cz_save_2(u2_mote, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cz_save_3(u2_mote, u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cz_save_4
                (u2_mote, u2_noun, u2_noun, u2_noun, u2_noun, u2_noun);

    /* u2_cz_uniq(): uniquify with memo cache.
    */
      u2_noun 
      u2_cz_uniq(u2_noun som);

