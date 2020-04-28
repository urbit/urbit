/* i/n/z.h
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
    /* u3z_key*(): construct a memo cache-key.  Arguments retained.
    */
      u3_noun u3z_key(c3_m, u3_noun);
      u3_noun u3z_key_2(c3_m, u3_noun, u3_noun);
      u3_noun u3z_key_3(c3_m, u3_noun, u3_noun, u3_noun);
      u3_noun u3z_key_4(c3_m, u3_noun, u3_noun, u3_noun, u3_noun);

    /* u3z_find*(): find in memo cache. Arguments retained
    */
      u3_weak u3z_find(u3_noun key);
      u3_weak u3z_find_m(c3_m fun_m, u3_noun one);

    /* u3z_save(): save in memo cache. TRANSFER key; RETAIN val;
    */
      u3_noun u3z_save(u3_noun key, u3_noun val);

    /* u3z_save_m(): save in memo cache. Arguments retained
    */
      u3_noun u3z_save_m(c3_m fun_m, u3_noun one, u3_noun val);

    /* u3z_uniq(): uniquify with memo cache.
    */
      u3_noun
      u3z_uniq(u3_noun som);
