/* mill/trap.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_trap(): save trap.
*/
void
_mill_trap(u4_milr m,
           const u4_cl *cl_msg)
{
  u4_lane lane = m->lane;

  if ( !cl_msg ) {
    m->meb = u4_ct(m->meb);
  }
  else {
    u4_atom hal  = u4_k_atom_c(lane, cl_msg);
    u4_atom nof  = m->nix;
    u4_atom jup  = m->zud;

    m->meb = u4_k_cell(lane, u4_k_trel(lane, hal, nof, jup), m->meb);
  }
}
