/* fake/op/sub.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_op_sub():
**
**   Produce (b - a), or bull if (a > b).
*/
u4_nopt
u4_op_sub(u4_lane lane,
          u4_atom a,
          u4_atom b)
{
  mpz_t mp_a, mp_b;

  u4_a_gmp(a, mp_a);
  u4_a_gmp(b, mp_b);

  if ( mpz_cmp(mp_b, mp_a) < 0 ) {
    mpz_clear(mp_a);
    mpz_clear(mp_b);

    return u4_bull;
  }
  else {
    mpz_sub(mp_b, mp_b, mp_a);
    mpz_clear(mp_a);

    return u4_k_atom_gmp(lane, mp_b);
  }
}
