/* mill/durf.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_durf(): type to prep.
*/
u4_prep
_mill_durf(u4_milr m,
           u4_form rid)
{
  return u4_pump_prep(m->lane, rid);
}
