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
    c3_y  sig_y[64], pub_y[32];

    if ( (0 != u3r_bytes_fit(64, sig_y, s)) ||
         (0 != u3r_bytes_fit(32, pub_y, pk)) ) {
      // hoon checks sizes, but weirdly and without crashes
      return u3_none;
    }
    else {
      c3_w  met_w;
      c3_y* mes_y = u3r_bytes_all(&met_w, m);
      c3_t  val_t = urcrypt_ed_veri(mes_y, met_w, pub_y, sig_y);
      u3a_free(mes_y);

      return val_t ? c3y : c3n;
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
      return u3l_punt("veri", _cqee_veri(a, b, c));
    }
  }
