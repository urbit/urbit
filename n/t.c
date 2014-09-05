/* n/t.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u2_ct_push(): push on trace stack.
*/
void
u2_ct_push(u2_noun mon)
{
  u2R->bug.tax = u2nc(mon, u2R->bug.tax);
}

/* u2_ct_mean(): push `[%mean roc]` on trace stack.
*/
void
u2_ct_mean(u2_noun roc)
{
  u2R->bug.tax = u2nc(u2nc(c3__mean, roc), u2R->bug.tax);
}

/* u2_ct_drop(): drop from meaning stack.
*/
void
u2_ct_drop(void)
{
  c3_assert(u2_so(u2du(u2R->bug.tax)));
  {
    u2_noun tax = u2R->bug.tax;

    u2R->bug.tax = u2k(u2t(tax));
    u2z(tax);
  }
}

/* u2_ct_slog(): print directly.
*/
void
u2_ct_slog(u2_noun hod)
{
  u2z(hod);
}
