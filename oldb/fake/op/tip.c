/* fake/op/tip.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_op_tip():
**
**   Produce the root of (twig) - 2 or 3; o4 bull if (twig) is 1.
*/
u4_nopt
u4_op_tip(u4_twig twig)
{
  if ( u4_n_eq(twig, u4_noun_1) ) {
    return u4_bull;
  }
  else {
    u4_st st = u4_a_bin(twig, 0);

    if ( u4_a_bit(twig, (st - 2)) == 0 ) {
      return u4_cod_in(2);
    } else {
      return u4_cod_in(3);
    }
  }
}
