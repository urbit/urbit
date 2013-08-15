/* mill/dung.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_dung(): gene to prep.
*/
u4_prep
_mill_dung(u4_milr m,
           u4_gene lub)
{
  return u4_pump_prep(m->lane, lub);
}
