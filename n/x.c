/* n/x.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u2_cx_good(): test for u2_none.
*/
u2_noun
u2_cx_good(u2_weak som)
{
  if ( u2_none == som ) {
    return u2_cm_bail(c3__exit);
  }
  else return som;
}

/* u2_cx_at (u2at): fragment.
*/
u2_noun
u2_cx_at(u2_noun axe, u2_noun som)
{
  u2_weak pro = u2_cr_at(axe, som);

  if ( u2_none == pro ) {
    return u2_cm_bail(c3__exit);
  } else return pro;
}

/* u2_cx_cell():
**
**   Divide `a` as a cell `[b c]`.
*/
void
u2_cx_cell(u2_noun  a,
           u2_noun* b,
           u2_noun* c)
{
  if ( u2_no == u2_cr_cell(a, b, c) ) {
    u2_cm_bail(c3__exit);
  }
}

/* u2_cx_trel():
**
**   Divide `a` as a trel `[b c d]`, or bail.
*/
void
u2_cx_trel(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d)
{
  if ( u2_no == u2_cr_trel(a, b, c, d) ) {
    u2_cm_bail(c3__exit);
  }
}

/* u2_cx_qual():
**
**   Divide `a` as a quadruple `[b c d e]`.
*/
void
u2_cx_qual(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e)
{
  if ( u2_no == u2_cr_qual(a, b, c, d, e) ) {
    u2_cm_bail(c3__exit);
  }
}
