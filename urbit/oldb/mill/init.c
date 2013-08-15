/* mill/init.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_mill_init(): 
**
**   Initialize (m) with (lane).
*/
void
u4_mill_init(u4_milr m,
             u4_lane lane)
{
  m->lane = lane;
  m->prf = 0;

  m->fan = u4_noun_0;
  m->pox = u4_noun_0;
  m->rux = u4_noun_0;
  m->zud = u4_noun_0;
  m->nix = u4_noun_0;

  m->meb = u4_noun_0;
  m->dam = u4_noun_0;
  m->niq = u4_noun_0;
  m->zor = u4_noun_0;
  m->vus = u4_noun_0;
  m->rep = u4_noun_0;
  m->pon = u4_noun_0;
  m->fin = u4_noun_0;

  m->ply = u4_noun_0;
  m->bak = u4_noun_0;
}
