/* mill/hook.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_hook(): hook from gene.
*/
u4_axis
_mill_hook(u4_milr m,
           u4_type nit,
           u4_gene col)
{
  u4_form muv = _mill_make(m, nit, col);
  u4_axis p_muv;

  if ( u4_b_p(muv, u4_noun_0, &p_muv) ) {
    return p_muv;
  }
  else {
    // u4_burp(m->lane, "hook/a", _mill_dung(m, col));

    return _mill_fail(m, "hook obstruction");
  }
}
