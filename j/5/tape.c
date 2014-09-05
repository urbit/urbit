/* j/3/tape.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  static u3_noun
  _norm(
        u3_noun a)
  {
    if ( u3_no == u3du(a) ) {
      return u3_nul;
    } else {
      return u3nc(((u3_yes == u3du(u3h(a))) ? u3_nul : u3k(u3h(a))),
                  _norm(u3t(a)));
    }
  }

  static u3_bean
  _good(
        u3_noun a)
  {
    while ( 1 ) {
      if ( u3_nul == a ) {
        return u3_yes;
      }
      if ( u3_no == u3ud(u3h(a)) ) {
        return u3_no;
      }
      a = u3t(a);
    }
  }

  u3_noun
  u3_cqe_tape(u3_noun a)
  {
    if ( u3_yes == _good(a) ) {
      return u3k(a);
    } else {
      return _norm(a);
    }
  }
  u3_noun
  u3_cwe_tape(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3_cr_at(u3_cv_sam, cor))) ) {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqe_tape(a);
    }
  }
