/* gen164/5/ed_veri.c
**
*/
#include "all.h"


#include <ed25519.h>

/* functions
*/
  static u3_noun
  _cqee_veri(u3_noun s,
             u3_noun m,
             u3_noun pk)
  {
    c3_y  sig_y[64];
    c3_y  pub_y[32];
    c3_w  ret;
    c3_y* mes_y;

    c3_w mesm_w = u3r_met(3, m);

    memset(sig_y, 0, 64);
    memset(pub_y, 0, 32);

    mes_y = c3_malloc(mesm_w);

    u3r_bytes(0, 64, sig_y, s);
    u3r_bytes(0, 32, pub_y, pk);
    u3r_bytes(0, mesm_w, mes_y, m);

    ret = ed25519_verify(sig_y, mes_y, mesm_w, pub_y) == 1 ? c3y : c3n;
    free(mes_y);
    return ret;
  }

  u3_noun
  u3wee_veri(u3_noun cor)
  {
    u3_noun a, b, c;
    if ( c3n == u3r_mean(cor,
                         u3x_sam_2, &a, u3x_sam_6, &b,
                         u3x_sam_7, &c, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return _cqee_veri(a, b, c);
    }
  }
