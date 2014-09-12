/* g/t.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u3_ct_push(): push on trace stack.
*/
void
u3_ct_push(u3_noun mon)
{
  u3R->bug.tax = u3nc(mon, u3R->bug.tax);
}

/* u3_ct_mean(): push `[%mean roc]` on trace stack.
*/
void
u3_ct_mean(u3_noun roc)
{
  u3R->bug.tax = u3nc(u3nc(c3__mean, roc), u3R->bug.tax);
}

/* u3_ct_drop(): drop from meaning stack.
*/
void
u3_ct_drop(void)
{
  c3_assert(u3_so(u3du(u3R->bug.tax)));
  {
    u3_noun tax = u3R->bug.tax;

    u3R->bug.tax = u3k(u3t(tax));
    u3z(tax);
  }
}

/* u3_ct_slog(): print directly.
*/
void
u3_ct_slog(u3_noun hod)
{
  u3_cm_p("&", hod);
  u3z(hod);
}
