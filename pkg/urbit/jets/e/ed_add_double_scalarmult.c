/* gen164/5/ed_add_double_scalarmult.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* functions
*/
  static u3_atom
  _cqee_add_double_scalarmult(u3_atom a,
                             u3_atom b,
                             u3_atom c,
                             u3_atom d)
  {
    c3_y a_y[32], b_y[32], c_y[32], d_y[32], out_y[32];

    if ( (0 != u3r_bytes_fit(32, a_y, a)) ||
         (0 != u3r_bytes_fit(32, b_y, b)) ||
         (0 != u3r_bytes_fit(32, c_y, c)) ||
         (0 != u3r_bytes_fit(32, d_y, d)) ||
         (0 != urcrypt_ed_add_double_scalarmult(a_y, b_y, c_y, d_y, out_y)) ) {
      return u3_none;
    }
    else {
      return u3i_bytes(32, out_y);
    }
  }

  u3_noun
  u3wee_add_double_scalarmult(u3_noun cor)
  {
    u3_noun a, b, c, d;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a,
                               u3x_sam_6, &b,
                               u3x_sam_14, &c,
                               u3x_sam_15, &d, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) ||
         (c3n == u3ud(c)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3l_punt("add-double-scalarmult",
                      _cqee_add_double_scalarmult(a, b, c, d));
    }
  }
