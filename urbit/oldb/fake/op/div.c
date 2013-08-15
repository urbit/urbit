/* fake/op/div.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_op_div():
**
**   Produce (b / a), or bull if (a) is 0.
*/
u4_nopt
u4_op_div(u4_lane lane,
          u4_atom a,
          u4_atom b)
{
  if ( u4_n_zero(a) ) {
    return u4_bull;
  }
  else {
    mpz_t mp_a, mp_b;

    u4_a_gmp(a, mp_a);
    u4_a_gmp(b, mp_b);

    mpz_tdiv_q(mp_b, mp_b, mp_a);
    mpz_clear(mp_a);

    return u4_k_atom_gmp(lane, mp_b);
  }
}
