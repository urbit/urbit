/* j/5/shar.c
**
*/
#include "all.h"
#include <urcrypt.h>

  static u3_atom
  _cqee_shar(u3_atom pub, u3_atom sek)
  {
    c3_y pub_y[32], sek_y[32];

    if ( 0 != u3r_bytes_fit(32, pub_y, pub) ) {
      // pub is not size checked in the hoon
      return u3_none;
    }
    else if ( 0 != u3r_bytes_fit(32, sek_y, sek) ) {
      // sek explicitly bails through suck
      return u3m_bail(c3__exit);
    }
    else {
      c3_y shr_y[32];
      urcrypt_ed_shar(pub_y, sek_y, shr_y);
      return u3i_bytes(32, shr_y);
    }
  }

  u3_noun
  u3wee_shar(u3_noun cor)
  {
    u3_noun pub, sek;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &pub, u3x_sam_3, &sek, 0)) ||
         (c3n == u3ud(pub)) ||
         (c3n == u3ud(sek)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("shar", _cqee_shar(pub, sek));
    }
  }
