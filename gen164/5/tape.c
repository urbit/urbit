/* j/3/tape.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  static u2_noun
  _norm(
        u2_noun a)
  {
    if ( u2_no == u2du(a) ) {
      return u2_nul;
    } else {
      return u2nc(((u2_yes == u2du(u2h(a))) ? u2_nul : u2k(u2h(a))),
                  _norm(u2t(a)));
    }
  }

  static u2_bean
  _good(
        u2_noun a)
  {
    while ( 1 ) {
      if ( u2_nul == a ) {
        return u2_yes;
      }
      if ( u2_no == u2ud(u2h(a)) ) {
        return u2_no;
      }
      a = u2t(a);
    }
  }

  u2_noun
  u2_cqe_tape(u2_noun a)
  {
    if ( u2_yes == _good(a) ) {
      return u2k(a);
    } else {
      return _norm(a);
    }
  }
  u2_noun
  u2_cwe_tape(u2_noun cor)
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqe_tape(a);
    }
  }
