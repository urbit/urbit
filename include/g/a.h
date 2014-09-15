/* include/g/a.h
**
** This file is in the public domain.
*/

    /**  Allocation.
    **/
      /* Basic allocation.
      */
        /* u3_ca_walloc(): allocate storage measured in words.
        */
          void*
          u3_ca_walloc(c3_w len_w);

        /* u3_ca_malloc(): allocate storage measured in bytes.
        */
          void*
          u3_ca_malloc(c3_w len_w);

        /* u3_ca_free(): free storage.
        */
          void
          u3_ca_free(void* lag_v);

        /* u3_ca_wealloc(): word realloc.
        */
          void*
          u3_ca_wealloc(void* lag_v, c3_w len_w);

        /* u3_ca_realloc(): byte realloc.
        */
          void*
          u3_ca_realloc(void* lag_v, c3_w len_w);


      /* Reference and arena control.
      */
        /* u3_ca_gain(): gain and/or copy juniors.
        */
          u3_weak
          u3_ca_gain(u3_weak som);

        /* u3_ca_lose(): lose a reference.
        */
          void
          u3_ca_lose(u3_weak som);

        /* u3_ca_use(): reference count.
        */
          c3_w
          u3_ca_use(u3_noun som);

        /* u3_ca_mark(): mark for gc, returning allocated words.
        */
          c3_w
          u3_ca_mark(u3_noun som);

        /* u3_ca_sweep(): sweep after gc, freeing, matching live count.
        */
          c3_w
          u3_ca_sweep(c3_w liv_w);

        /* u3_ca_sane(): check allocator sanity.
        */
          void
          u3_ca_sane(void);

      /* Atoms from proto-atoms.
      */
        /* u3_ca_slab(): create a length-bounded proto-atom.
        */
          c3_w*
          u3_ca_slab(c3_w len_w);

        /* u3_ca_slaq(): u3_ca_slaq() with a defined blocksize.
        */
          c3_w*
          u3_ca_slaq(c3_g met_g, c3_w len_w);

        /* u3_ca_malt(): measure and finish a proto-atom.
        */
          u3_noun
          u3_ca_malt(c3_w* sal_w);

        /* u3_ca_moot(): finish a pre-measured proto-atom; dangerous.
        */
          u3_noun
          u3_ca_moot(c3_w* sal_w);

        /* u3_ca_mint(): finish a measured proto-atom.
        */
          u3_noun
          u3_ca_mint(c3_w* sal_w, c3_w len_w);


