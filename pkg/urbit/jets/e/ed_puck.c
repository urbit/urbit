/* gen164/5/ed_puck.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* functions
*/
  static u3_atom
  _cqee_puck(u3_atom sed)
  {
    c3_y sed_y[32], pub_y[32];
    c3_w met_w;

    if ( 0 == u3r_unpack(32, sed_y, sed) ) {
      urcrypt_ed_puck(sed_y, pub_y);
      return u3i_bytes(32, pub_y);
    }
    else {
      // hoon explicitly crashes on mis-size
      return u3m_bail(c3__exit);
    }
  }

  u3_noun
  u3wee_puck(u3_noun cor)
  {
    u3_noun a = u3r_at(u3x_sam, cor);

    if ( (u3_none == a) || (c3n == u3ud(a)) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return _cqee_puck(a);
    }
  }
