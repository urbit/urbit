/* fake/op/mod.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_op_mod():
**
**   Produce (b % a).
*/
u4_atom
u4_op_mod(u4_lane lane,
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

    mpz_tdiv_r(mp_b, mp_b, mp_a);
    mpz_clear(mp_a);

    return u4_k_atom_gmp(lane, mp_b);
  }
}
