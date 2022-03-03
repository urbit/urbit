//! @file defs.h
//! Module-independent u3 macros.

#ifndef U3_DEFS_H
#define U3_DEFS_H

#include "c/all.h"
#include "noun/manage.h"

  /**  Constants.
  **/
    /* u3_nul: 0, hoon ~.
    */
#     define u3_nul   0

    /* u3_blip: 0, hoon %$.
    */
#     define u3_blip  0


  /**  Macros.
  **/
    /* u3_assure(): loobean assert, bailing with %fail.
    */
#     define u3_assure(x)  if ( !_(x) ) { u3m_bail(c3__fail); }

    /* u3_assert(): loobean assert, bailing with %exit.
    */
#     define u3_assent(x)  if ( !_(x) ) { u3m_bail(c3__exit); }

#endif /* ifndef U3_DEFS_H */
