/* gen164/5/ed_puck.c
**
*/
#include "all.h"


#include <ed25519.h>

/* functions
*/
  u3_noun
  u3wee_puck(u3_noun cor)
  {
    c3_y pub_y[32];
    c3_y sec_y[64];
    c3_y sed_y[32];
    c3_w met_w;
    u3_noun a = u3r_at(u3x_sam, cor);

    if ( (u3_none == a) || (c3n == u3ud(a)) ) {
      return u3m_bail(c3__exit);
    }

    met_w = u3r_met(3, a);
    if ( met_w > 32 ) {
      return u3m_bail(c3__exit);
    }

    memset(sed_y, 0, 32);
    u3r_bytes(0, met_w, sed_y, a);
    ed25519_create_keypair(pub_y, sec_y, sed_y);
    return u3i_bytes(32, pub_y);
  }
