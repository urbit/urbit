/* include/g/a.h
**
** This file is in the public domain.
*/

    /**  Allocation.
    **/
      /* Basic allocation.
      */
        /* u2_ca_walloc(): allocate storage measured in words.
        */
          void*
          u2_ca_walloc(c3_w len_w);

        /* u2_ca_malloc(): allocate storage measured in bytes.
        */
          void*
          u2_ca_malloc(c3_w len_w);

        /* u2_ca_free(): free storage.
        */
          void
          u2_ca_free(void* lag_v);

        /* u2_ca_wealloc(): word realloc.
        */
          void*
          u2_ca_wealloc(void* lag_v, c3_w len_w);

        /* u2_ca_realloc(): byte realloc.
        */
          void*
          u2_ca_realloc(void* lag_v, c3_w len_w);


      /* Reference and arena control.
      */
        /* u2_ca_gain(): gain and/or copy juniors.
        */
          u2_weak
          u2_ca_gain(u2_weak som);

        /* u2_ca_lose(): lose a reference.
        */
          void
          u2_ca_lose(u2_weak som);

        /* u2_ca_use(): reference count.
        */
          c3_w
          u2_ca_use(u2_noun som);

        /* u2_ca_mark(): mark for gc, returning allocated words.
        */
          c3_w
          u2_ca_mark(u2_noun som);

        /* u2_ca_sweep(): sweep after gc, freeing, matching live count.
        */
          c3_w
          u2_ca_sweep(c3_w liv_w);


      /* Atoms from proto-atoms.
      */
        /* u2_ca_slab(): create a length-bounded proto-atom.
        */
          c3_w*
          u2_ca_slab(c3_w len_w);

        /* u2_ca_slaq(): u2_ca_slaq() with a defined blocksize.
        */
          c3_w*
          u2_ca_slaq(c3_g met_g, c3_w len_w);

        /* u2_ca_malt(): measure and finish a proto-atom.
        */
          u2_noun
          u2_ca_malt(c3_w* sal_w);

        /* u2_ca_moot(): finish a pre-measured proto-atom; dangerous.
        */
          u2_noun
          u2_ca_moot(c3_w* sal_w);

        /* u2_ca_mint(): finish a measured proto-atom.
        */
          u2_noun
          u2_ca_mint(c3_w* sal_w, c3_w len_w);


