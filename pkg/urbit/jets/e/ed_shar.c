/* j/5/shar.c
**
*/
#include "all.h"

#include <ed25519.h>

  u3_noun
  u3qee_shar(u3_atom pub, u3_atom sek)
  {
    c3_y pub_y[32], sek_y[32], self_y[32], exp_y[64], shr_y[32];
    c3_w met_pub_w, met_sek_w;

    met_pub_w = u3r_met(3, pub);
    met_sek_w = u3r_met(3, sek);

    if ( (met_pub_w > 32) || (met_sek_w > 32) ) {
      return u3m_bail(c3__exit);
    }

    u3r_bytes(0, 32, pub_y, pub);
    u3r_bytes(0, 32, sek_y, sek);

    memset(self_y, 0, 32);
    memset(exp_y, 0, 64);
    memset(shr_y, 0, 32);

    ed25519_create_keypair(self_y, exp_y, sek_y);
    ed25519_key_exchange(shr_y, pub_y, exp_y);

    return u3i_bytes(32, shr_y);
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
      return u3qee_shar(pub, sek);
    }
  }
