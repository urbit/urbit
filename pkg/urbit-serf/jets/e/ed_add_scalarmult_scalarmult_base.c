/* gen164/5/ed_double_scalarmult.c
**
*/
#include "all.h"


#include <ed25519.h>
#include <ge.h>

/* functions
*/

  u3_noun
  u3qc_add_scalarmult_scalarmult_base(u3_atom a,
                                      u3_atom a_point,
                                      u3_atom b)
  {
    c3_y met_w;

    met_w = u3r_met(3, a);
    if (met_w > 32) {
      return u3m_bail(c3__fail);
    }
    c3_y a_y[32];
    memset(a_y, 0, 32);
    u3r_bytes(0, met_w, a_y, a);

    met_w = u3r_met(3, a_point);
    if (met_w > 32) {
      return u3m_bail(c3__fail);
    }
    c3_y a_point_y[32];
    memset(a_point_y, 0, 32);
    u3r_bytes(0, met_w, a_point_y, a_point);

    met_w = u3r_met(3, b);
    if (met_w > 32) {
      return u3m_bail(c3__fail);
    }
    c3_y b_y[32];
    memset(b_y, 0, 32);
    u3r_bytes(0, met_w, b_y, b);

    ge_p3 A;
    if (ge_frombytes_negate_vartime(&A, a_point_y) != 0) {
      return u3m_bail(c3__exit);
    }

    // Undo the negation from above. See add_scalar.c in the ed25519 distro.
    fe_neg(A.X, A.X);
    fe_neg(A.T, A.T);

    ge_p2 r;
    ge_double_scalarmult_vartime(&r, a_y, &A, b_y);

    c3_y output_y[32];
    ge_tobytes(output_y, &r);

    return u3i_bytes(32, output_y);
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
      return u3qc_add_scalarmult_scalarmult_base(a, b, c);
    }
  }
