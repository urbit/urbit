/* fake/op/log.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_op_log():
**
**   Produce the lowest m_log such that (1 << m_log) > m.
*/
u4_atom
u4_op_log(u4_lane lane,
          u4_atom atom)
{
  u4_st st = u4_a_bin(atom, 0);

  return u4_k_atom_xw(lane, st);
}
