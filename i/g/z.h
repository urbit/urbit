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
    /* u3z_find*(): find in memo cache.
    */
      u3_weak u3z_find(u3_mote, u3_noun);
      u3_weak u3z_find_2(u3_mote, u3_noun, u3_noun);
      u3_weak u3z_find_3(u3_mote, u3_noun, u3_noun, u3_noun);
      u3_weak u3z_find_4(u3_mote, u3_noun, u3_noun, u3_noun, u3_noun);

    /* u3z_save*(): save in memo cache.
    */
      u3_noun u3z_save(u3_mote, u3_noun, u3_noun);
      u3_noun u3z_save_2(u3_mote, u3_noun, u3_noun, u3_noun);
      u3_noun u3z_save_3(u3_mote, u3_noun, u3_noun, u3_noun, u3_noun);
      u3_noun u3z_save_4
                (u3_mote, u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);

    /* u3z_uniq(): uniquify with memo cache.
    */
      u3_noun 
      u3z_uniq(u3_noun som);

