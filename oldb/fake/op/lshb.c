/* fake/op/lshb.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_op_lshb():
**
**   Produce (c >> (b << a)).
*/
u4_atom
u4_op_lshb(u4_lane lane,
           u4_atom a,
           u4_atom b,
           u4_atom c)
{
  u4_xw xw_a = u4_a_wbail(a, u4_bail_tank);
  u4_xw xw_b = u4_a_wbail(b, u4_bail_tank);
  mpz_t mp;

  if ( xw_b != ((xw_b << xw_a) >> xw_a) ) {
    return u4_tank;
  }
  else {
    u4_a_gmp(c, mp);
    mpz_mul_2exp(mp, mp, (xw_b << xw_a));

    return u4_k_atom_gmp(lane, mp);
  }
}
