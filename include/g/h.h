/* include/g/h.h
**
** This file is in the public domain.
*/

    /**  Functions.
    ***
    ***  Needs: delete and merge functions; clock reclamation function.
    **/

      /* u2_ch_new(): create hashtable.
      */
        u2_ch_root* 
        u2_ch_new(void);

      /* u2_ch_put(): insert in hashtable.
      **
      ** `key` is RETAINED; `val` is transferred.
      */
        void
        u2_ch_put(u2_ch_root* har_u, u2_noun key, u2_noun val);

      /* u2_ch_get(): read from hashtable.
      **
      ** `key` is RETAINED.
      */
        u2_weak
        u2_ch_get(u2_ch_root* har_u, u2_noun key);

      /* u2_ch_free(): free hashtable.
      */
        void
        u2_ch_free(u2_ch_root* har_u);

