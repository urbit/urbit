/* gen164/5/ed_sign.c
**
*/
#include "all.h"


#include <ed25519.h>

/* functions
*/
  static u3_noun
  _cqee_sign(u3_noun a, 
             u3_noun b)
  {
    c3_y sig_y[64];
    c3_y sed_y[32];
    c3_y pub_y[64];
    c3_y sec_y[64];

    c3_w mesm_w = u3r_met(3, a);
    c3_w mess_w = u3r_met(3, b);

    c3_y* mes_y = 0;

    memset(sig_y, 0, 64);
    memset(sed_y, 0, 32);
    memset(pub_y, 0, 64);
    memset(sec_y, 0, 64);

    mes_y = malloc(mesm_w);

    u3r_bytes(0, mesm_w, mes_y, a);
    u3r_bytes(0, mess_w, sed_y, b);

    ed25519_create_keypair(pub_y, sec_y, sed_y);
    ed25519_sign(sig_y, mes_y, mesm_w, pub_y, sec_y);
    free(mes_y);
    return u3i_bytes(64, sig_y);
  }

  u3_noun
  u3wee_sign(u3_noun cor)
  {
    u3_noun a, b;
    if ( c3n == u3r_mean(cor,
                         u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return _cqee_sign(a, b);
    }
  }
