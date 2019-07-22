/* gen164/5/ed_scalarmult_base.c
**
*/
#include "all.h"

#include <ed25519.h>
#include <ge.h>

/* functions
*/
  u3_noun
  u3wee_scalarmult_base(u3_noun cor)
  {
    u3_noun scalar = u3r_at(u3x_sam, cor);

    if ( (u3_none == scalar) || (c3n == u3ud(scalar)) ) {
      return u3m_bail(c3__exit);
    }

    c3_w met_w = u3r_met(3, scalar);
    if ( met_w > 32 ) {
      return u3m_bail(c3__fail);
    }

    c3_y scalar_y[32];
    memset(scalar_y, 0, 32);
    u3r_bytes(0, met_w, scalar_y, scalar);

    ge_p3 R;
    ge_scalarmult_base(&R, scalar_y);

    c3_y output_y[32];
    ge_p3_tobytes(output_y, &R);

    return u3i_bytes(32, output_y);
  }
