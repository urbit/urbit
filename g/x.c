/* g/x.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u3_cx_good(): test for u3_none.
*/
u3_noun
u3_cx_good(u3_weak som)
{
  if ( u3_none == som ) {
    return u3_cm_bail(c3__exit);
  }
  else return som;
}

/* u3_cx_at (u3at): fragment.
*/
u3_noun
u3_cx_at(u3_noun axe, u3_noun som)
{
  u3_weak pro = u3_cr_at(axe, som);

  if ( u3_none == pro ) {
    return u3_cm_bail(c3__exit);
  } else return pro;
}

/* u3_cx_cell():
**
**   Divide `a` as a cell `[b c]`.
*/
void
u3_cx_cell(u3_noun  a,
           u3_noun* b,
           u3_noun* c)
{
  if ( c3n == u3_cr_cell(a, b, c) ) {
    u3_cm_bail(c3__exit);
  }
}

/* u3_cx_trel():
**
**   Divide `a` as a trel `[b c d]`, or bail.
*/
void
u3_cx_trel(u3_noun  a,
           u3_noun* b,
           u3_noun* c,
           u3_noun* d)
{
  if ( c3n == u3_cr_trel(a, b, c, d) ) {
    u3_cm_bail(c3__exit);
  }
}

/* u3_cx_qual():
**
**   Divide `a` as a quadruple `[b c d e]`.
*/
void
u3_cx_qual(u3_noun  a,
           u3_noun* b,
           u3_noun* c,
           u3_noun* d,
           u3_noun* e)
{
  if ( c3n == u3_cr_qual(a, b, c, d, e) ) {
    u3_cm_bail(c3__exit);
  }
}

/* u3_cx_quil():
**
**   Divide `a` as a quintuple `[b c d e f]`.
*/
void
u3_cx_quil(u3_noun  a,
           u3_noun* b,
           u3_noun* c,
           u3_noun* d,
           u3_noun* e,
           u3_noun* f)
{
  if ( c3n == u3_cr_quil(a, b, c, d, e, f) ) {
    u3_cm_bail(c3__exit);
  }
}

