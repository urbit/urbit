/* fake/op/end.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_op_end():
**
**   Produce (b &~ (1 << a)).
*/
u4_atom
u4_op_end(u4_lane lane,
          u4_atom a, 
          u4_atom b)
{
  u4_st st_count = u4_a_wbail(a, u4_bail_tank);
  mpz_t mp;

  u4_a_gmp(b, mp);
  mpz_tdiv_r_2exp(mp, mp, st_count);

  return u4_k_atom_gmp(lane, mp);
}
