/* fake/op/inc.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_op_inc():
**
**   Produce (atom + 1).
*/
u4_atom
u4_op_inc(u4_lane lane,
          u4_atom atom)
{
  mpz_t mp;

  u4_a_gmp(atom, mp);
  mpz_add_ui(mp, mp, 1);

  return u4_k_atom_gmp(lane, mp);
}
