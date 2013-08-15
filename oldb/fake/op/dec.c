/* fake/op/dec.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_op_dec():
**
**   Produce (atom - 1), or bull if (atom) is 0.
*/
u4_nopt
u4_op_dec(u4_lane lane,
          u4_atom atom)
{
  if ( u4_n_zero(atom) ) {
    return u4_bull;
  }
  else {
    mpz_t mp;

    u4_a_gmp(atom, mp);
    mpz_sub_ui(mp, mp, 1);

    return u4_k_atom_gmp(lane, mp);
  }
}

