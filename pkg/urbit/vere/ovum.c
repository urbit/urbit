//! @file ovum.c

#include "vere/ovum.h"

#include "c/defs.h"
#include "c/motes.h"
#include "c/portable.h"
#include "c/types.h"
#include "noun/aliases.h"
#include "noun/allocate.h"
#include "noun/xtract.h"

u3_ovum*
u3_ovum_init(c3_w     mil_w,
             u3_noun    tar,
             u3_noun    wir,
             u3_noun    cad)
{
  u3_ovum* egg_u = c3_malloc(sizeof(*egg_u));
  egg_u->car_u = 0;
  egg_u->try_w = 0;
  egg_u->ptr_v = 0;
  egg_u->mil_w = mil_w;
  egg_u->tar   = tar;
  egg_u->wir   = wir;
  egg_u->cad   = cad;

  egg_u->pre_u = egg_u->nex_u = 0;

  egg_u->cb_u.news_f = 0;
  egg_u->cb_u.bail_f = 0;

  //  spinner defaults
  //
  egg_u->pin_u.lab   = u3k(u3h(wir));
  egg_u->pin_u.del_o = c3y;

  return egg_u;
}

void
u3_ovum_free(u3_ovum *egg_u)
{
  u3z(egg_u->pin_u.lab);
  u3z(egg_u->tar);
  u3z(egg_u->wir);
  u3z(egg_u->cad);

  c3_free(egg_u);
}
