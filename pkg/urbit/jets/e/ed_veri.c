/* gen164/5/ed_veri.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* functions
*/
  static u3_atom
  _cqee_veri(u3_noun s,
             u3_noun m,
             u3_noun pk)
  {
    c3_w set_w, pek_w;

    if ( ((set_w = u3r_met(3, s)) > 64) ||
         ((pek_w = u3r_met(3, pk)) > 32) ) {
      // hoon checks sizes, but weirdly and without crashes
      return u3_none;
    }
    else {
      c3_y  sig_y[64], pub_y[32];
      c3_w  met_w = u3r_met(3, m);
      c3_y* mes_y = u3a_malloc(met_w);
      c3_o  ret_o;

      memset(sig_y, 0, 64);
      memset(pub_y, 0, 32);
      u3r_bytes(0, 64, sig_y, s);
      u3r_bytes(0, 32, pub_y, pk);
      u3r_bytes(0, met_w, mes_y, m);

      ret_o = urcrypt_ed_veri(mes_y, met_w, pub_y, sig_y) ? c3y : c3n;
      u3a_free(mes_y);
      return ret_o;
    }
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
