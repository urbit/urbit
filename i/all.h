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
#   include "g/u.h"         //  general u3 

#   include "g/a.h"         //  u3a: allocation
#   include "g/e.h"         //  u3e: persistence
#   include "g/h.h"         //  u3h: hashtables
#   include "g/i.h"         //  u3i: noun construction
#   include "g/j.h"         //  u3j: jet control
#   include "g/m.h"         //  u3m: master state
#   include "g/n.h"         //  u3n: nock execution
#   include "g/r.h"         //  u3r: noun access (error returns)
#   include "g/t.h"         //  u3t: profiling / tracing
#   include "g/x.h"         //  u3x: noun access (error crashes) 
#   include "g/v.h"         //  u3v: arvo kernel
#   include "g/z.h"         //  u3z: memoization

#   include "j/k.h"         //  u3k: jets (transfer, args)
#   include "j/q.h"         //  u3q: jets (retain, args)
#   include "j/w.h"         //  u3w: jets (retain, core)
