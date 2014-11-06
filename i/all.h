/* include/all.h
**
** This file is in the public domain.
*/
  /** c3: C environment.
  **/
#   include "c/portable.h"  //  C and OS portability
#   include "c/types.h"     //  c3 types
#   include "c/defs.h"      //  c3 macros
#   include "c/motes.h"     //  c3 constants

  /** u3: noun environment.
  **/
#   include "n/u.h"         //  general u3 

#   include "n/a.h"         //  u3a: allocation
#   include "n/e.h"         //  u3e: persistence
#   include "n/h.h"         //  u3h: hashtables
#   include "n/i.h"         //  u3i: noun construction
#   include "n/j.h"         //  u3j: jet control
#   include "n/m.h"         //  u3m: master state
#   include "n/n.h"         //  u3n: nock execution
#   include "n/r.h"         //  u3r: noun access (error returns)
#   include "n/t.h"         //  u3t: profiling / tracing
#   include "n/x.h"         //  u3x: noun access (error crashes) 
#   include "n/v.h"         //  u3v: arvo kernel
#   include "n/z.h"         //  u3z: memoization

#   include "j/k.h"         //  u3k: jets (transfer, args)
#   include "j/q.h"         //  u3q: jets (retain, args)
#   include "j/w.h"         //  u3w: jets (retain, core)
