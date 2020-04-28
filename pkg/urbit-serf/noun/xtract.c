/* g/x.c
**
*/
#include "all.h"

/* u3x_good(): test for u3_none.
*/
u3_noun
u3x_good(u3_weak som)
{
  return ( u3_none == som ) ? u3m_bail(c3__exit) : som;
}

/* u3x_at (u3at): fragment.
*/
u3_noun
u3x_at(u3_noun axe, u3_noun som)
{
  return u3x_good(u3r_at(axe, som));
}

/* u3x_mean():
**
**   Attempt to deconstruct `a` by axis, noun pairs; 0 terminates.
**   Axes must be sorted in tree order.
*/
void
u3x_mean(u3_noun som, ...)
{
  c3_o    ret_o;
  va_list ap;

  va_start(ap, som);
  ret_o = u3r_vmean(som, ap);
  va_end(ap);

  if ( c3n == ret_o ) {
    u3m_bail(c3__exit);
  }
}

/* u3x_cell():
**
**   Divide `a` as a cell `[b c]`.
*/
void
u3x_cell(u3_noun  a,
         u3_noun* b,
         u3_noun* c)
{
  if ( c3n == u3r_cell(a, b, c) ) {
    u3m_bail(c3__exit);
  }
}

/* u3x_trel():
**
**   Divide `a` as a trel `[b c d]`, or bail.
*/
void
u3x_trel(u3_noun  a,
         u3_noun* b,
         u3_noun* c,
         u3_noun* d)
{
  if ( c3n == u3r_trel(a, b, c, d) ) {
    u3m_bail(c3__exit);
  }
}

/* u3x_qual():
**
**   Divide `a` as a quadruple `[b c d e]`.
*/
void
u3x_qual(u3_noun  a,
         u3_noun* b,
         u3_noun* c,
         u3_noun* d,
         u3_noun* e)
{
  if ( c3n == u3r_qual(a, b, c, d, e) ) {
    u3m_bail(c3__exit);
  }
}

/* u3x_quil():
**
**   Divide `a` as a quintuple `[b c d e f]`.
*/
void
u3x_quil(u3_noun  a,
         u3_noun* b,
         u3_noun* c,
         u3_noun* d,
         u3_noun* e,
         u3_noun* f)
{
  if ( c3n == u3r_quil(a, b, c, d, e, f) ) {
    u3m_bail(c3__exit);
  }
}

/* u3x_hext():
**
**   Divide `a` as a hextuple `[b c d e f g]`.
*/
void
u3x_hext(u3_noun  a,
         u3_noun* b,
         u3_noun* c,
         u3_noun* d,
         u3_noun* e,
         u3_noun* f,
         u3_noun* g)
{
  if ( c3n == u3r_hext(a, b, c, d, e, f, g) ) {
    u3m_bail(c3__exit);
  }
}

