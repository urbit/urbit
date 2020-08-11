/* gen164/5/ed_sign.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* functions
*/
  static u3_atom
  _cqee_sign(u3_noun a,
             u3_noun b)
  {
    c3_y sed_y[32];

    if ( 0 != u3r_bytes_fit(32, sed_y, b) ) {
      // hoon calls suck, which calls puck, which crashes
      return u3m_bail(c3__exit);
    }
    else {
      c3_y  sig_y[64];
      c3_w  met_w;
      c3_y* mes_y = u3r_bytes_all(&met_w, a);

      urcrypt_ed_sign(mes_y, met_w, sed_y, sig_y);
      u3a_free(mes_y);

      return u3i_bytes(64, sig_y);
    }
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
