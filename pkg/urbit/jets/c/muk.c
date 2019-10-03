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
  c3_w key_w = u3r_met(3, key);

  //  XX revisit
  //
  //    the first two conditions are explicitly asserted in +muk;
  //    the third is implicit in the pad subtraction.
  //
  //    the first assertion is semantically required for murmur32,
  //    the latter two are particular to our hoon implementation.
  //
  //    +muk (via +mug) is routinely "called" on samples that
  //    violate these assertions.
  //    are all three necessary for the rest of +muk to be correct?
  //
  if ( (u3r_met(5, seed) >  1) ||
       (u3r_met(0, len)  > 31) ||
       (key_w > len) )
  {
    return u3m_bail(c3__exit);
  }
  else {
    c3_w seed_w = u3r_word(0, seed);
    c3_w  len_w = u3r_word(0, len);
    c3_o  loc_o = c3n;
    c3_y* key_y = 0;
    c3_w  out_w;

    //  if we're hashing more bytes than we have, allocate and copy
    //  to ensure trailing null bytes
    //
    if ( len_w > key_w ) {
      loc_o = c3y;
      key_y = u3a_calloc(sizeof(c3_y), len_w);
      u3r_bytes(0, len_w, key_y, key);
    }
    else if ( len_w > 0 ) {
      key_y = ( c3y == u3a_is_cat(key) )
              ? (c3_y*)&key
              : (c3_y*)((u3a_atom*)u3a_to_ptr(key))->buf_w;
    }

    MurmurHash3_x86_32(key_y, len_w, seed_w, &out_w);

    if ( c3y == loc_o ) {
      u3a_free(key_y);
    }

    return u3i_words(1, &out_w);
  }
}

u3_noun
u3wc_muk(u3_noun cor)
{
  u3_noun seed, len, key;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &seed,
                             u3x_sam_6, &len,
                             u3x_sam_7, &key, 0)) ||
       (c3n == u3ud(seed)) ||
       (c3n == u3ud(len))  ||
       (c3n == u3ud(key))  )
  {
    return u3m_bail(c3__exit);
  }
  else {
    return u3qc_muk(seed, len, key);
  }
}
