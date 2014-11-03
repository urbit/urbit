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

      /* u3_ch_gut(): read from hashtable, unifying key nouns.
      **
      ** `key` is RETAINED.
      */
        u3_weak
        u3_ch_gut(u3_ch_root* har_u, u3_noun key);

      /* u3_ch_free(): free hashtable.
      */
        void
        u3_ch_free(u3_ch_root* har_u);

      /* u3_ch_mark(): mark hashtable for gc.
      */
        void
        u3_ch_mark(u3_ch_root* har_u);

      /* u3_ch_walk(): traverse hashtable with key, value fn; RETAINS.
      */
        void
        u3_ch_walk(u3_ch_root* har_u, void (*fun_f)(u3_noun));
