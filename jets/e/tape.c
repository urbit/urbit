/* j/3/tape.c
**
*/
#include "all.h"


/* functions
*/
  static u3_noun
  _norm(u3_noun a)
  {
    if ( c3n == u3du(a) ) {
      return u3_nul;
    } else {
      return u3nc(((c3y == u3du(u3h(a))) ? u3_nul : u3k(u3h(a))),
                  _norm(u3t(a)));
    }
  }

  static u3_noun
  _good(u3_noun a)
  {
    while ( 1 ) {
      if ( u3_nul == a ) {
        return c3y;
      }
      if ( c3n == u3ud(u3h(a)) ) {
        return c3n;
      }
      a = u3t(a);
    }
  }

  u3_noun
  u3qe_tape(u3_noun a)
  {
    if ( c3y == _good(a) ) {
      return u3k(a);
    } else {
      return _norm(a);
    }
  }
  u3_noun
  u3we_tape(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qe_tape(a);
    }
  }
