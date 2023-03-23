/* gen164/5/ed_scalarmult_base.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* functions
*/
  static u3_atom
  _cqee_scalarmult_base(u3_atom a)
  {
    c3_y a_y[32];

    if ( 0 != u3r_bytes_fit(32, a_y, a) ) {
      return u3_none;
    }
    else {
      c3_y out_y[32];
      urcrypt_ed_scalarmult_base(a_y, out_y);
      return u3i_bytes(32, out_y);
    }
  }

  u3_noun
  u3wee_scalarmult_base(u3_noun cor)
  {
    u3_noun a = u3r_at(u3x_sam, cor);

    if ( (u3_none == a) || (c3n == u3ud(a)) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3l_punt("scalarmult-base", _cqee_scalarmult_base(a));
    }
  }
