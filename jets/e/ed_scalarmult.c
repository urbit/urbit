/* gen164/5/ed_scalarmult.c
**
*/
#include "all.h"

#include <ed25519.h>

#include "ge_additions.h"

/* functions
*/
  u3_noun
  u3qc_scalarmult(u3_atom a,
                  u3_atom b)
  {
    c3_y met_w;

    met_w = u3r_met(3, a);
    if (met_w > 32) {
      return u3m_bail(c3__exit);
    }
    c3_y a_y[32];
    memset(a_y, 0, 32);
    u3r_bytes(0, met_w, a_y, a);

    met_w = u3r_met(3, b);
    if (met_w > 32) {
      return u3m_bail(c3__exit);
    }
    c3_y b_y[32];
    memset(b_y, 0, 32);
    u3r_bytes(0, met_w, b_y, b);

    ge_p3 B;
    if (ge_frombytes_negate_vartime(&B, b_y) != 0) {
      return u3m_bail(c3__exit);
    }

    // Undo the negation from above. See add_scalar.c in the ed25519 distro.
    fe_neg(B.X, B.X);
    fe_neg(B.T, B.T);

    ge_p3 result;
    ge_scalarmult(&result, a_y, &B);

    c3_y output_y[32];
    ge_p3_tobytes(output_y, &result);

    return u3i_bytes(32, output_y);
  }

  u3_noun
  u3wee_scalarmult(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a,
                               u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_scalarmult(a, b);
    }
  }
