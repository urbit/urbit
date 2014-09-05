/* include/g/h.h
**
** This file is in the public domain.
*/

    /**  Functions.
    ***
    ***  Needs: delete and merge functions; clock reclamation function.
    **/

      /* u3_ch_new(): create hashtable.
      */
        u3_ch_root* 
        u3_ch_new(void);

      /* u3_ch_put(): insert in hashtable.
      **
      ** `key` is RETAINED; `val` is transferred.
      */
        void
        u3_ch_put(u3_ch_root* har_u, u3_noun key, u3_noun val);

      /* u3_ch_get(): read from hashtable.
      **
      ** `key` is RETAINED.
      */
        u3_weak
        u3_ch_get(u3_ch_root* har_u, u3_noun key);

      /* u3_ch_free(): free hashtable.
      */
        void
        u3_ch_free(u3_ch_root* har_u);

