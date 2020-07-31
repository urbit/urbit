/* gen164/5/ed_double_scalarmult.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* functions
*/
  static u3_atom
  _cqee_add_scalarmult_scalarmult_base(u3_atom a,
                                      u3_atom b,
                                      u3_atom c)
  {
    c3_w ate_w, bet_w, get_w;

    if ( ((ate_w = u3r_met(3, a)) > 32) ||
         ((bet_w = u3r_met(3, b)) > 32) ||
         ((get_w = u3r_met(3, c)) > 32) ) {
      return u3_none;
    }
    else {
      c3_y a_y[32], b_y[32], c_y[32], out_y[32];

      memset(a_y, 0, 32);
      memset(b_y, 0, 32);
      memset(c_y, 0, 32);
      u3r_bytes(0, ate_w, a_y, a);
      u3r_bytes(0, bet_w, b_y, b);
      u3r_bytes(0, get_w, c_y, c);

      return
        ( 0 == urcrypt_ed_add_scalarmult_scalarmult_base(a_y, b_y, c_y, out_y) )
        ? u3i_bytes(32, out_y)
        : u3_none;
    }
  }

  u3_noun
  u3wee_add_scalarmult_scalarmult_base(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a,
                                u3x_sam_6, &b,
                                u3x_sam_7, &c, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) ||
         (c3n == u3ud(c)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return _cqee_add_scalarmult_scalarmult_base(a, b, c);
    }
  }
