/* j/c/muk.c
**
*/
#include "all.h"
#include <murmur3.h>

/* functions
*/
  u3_noun
  u3qc_muk(u3_atom seed,
           u3_atom len,
           u3_atom key)
  {
    c3_assert( u3r_met(5, seed) <= 1 );
    c3_w seed_w = u3a_get_cat31(seed);;

    c3_assert( u3r_met(0, len) <= 31 );
    c3_w len_w  = u3a_get_cat31(len);


    //  can't u3a_calloc 0 bytes
    //
    c3_assert( u3r_met(3, key) <= len_w );
    c3_y *key_y;
    if ( 0 == len_w ) {
      key_y = 0;
    }
    else {
      key_y = u3a_calloc(sizeof(c3_y), len_w);
    }

    //  Efficiency: unnecessary copy.
    //
    //    We could calculate the hash directly against
    //    the atom internals, a la u3r_mug
    //
    u3r_bytes(0, len_w, key_y, key);

    c3_w out_w;
    MurmurHash3_x86_32(key_y, len_w, seed_w, &out_w);

    u3a_free(key_y);
    return u3i_words(1, &out_w);
  }

  u3_noun
  u3wc_muk(u3_noun cor)
  {
    u3_noun seed, len, key;

    if ( !_(u3r_mean(cor, u3x_sam_2, &seed,
                               u3x_sam_6, &len,
                               u3x_sam_7, &key, 0)) ||
         !_(u3ud(seed)) ||
         !_(u3ud(len))  ||
         !_(u3ud(key))  )
    {
      return u3m_bail(c3__exit);
    }

    return u3qc_muk(seed, len, key);
  }
