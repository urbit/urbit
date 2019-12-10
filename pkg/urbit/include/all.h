/* include/all.h
**
** This file is in the public domain.
*/
#   include "config.h"
  /** c3: C environment.
  **/
#   include "c/portable.h"  //  C and OS portability
#   include "c/types.h"     //  c3 types
#   include "c/defs.h"      //  c3 macros
#   include "c/motes.h"     //  c3 constants

  /** u3: noun environment.
  **/
#   include "noun/aliases.h"   //  general u3

#   include "noun/allocate.h"  //  u3a: allocation
#   include "noun/events.h"    //  u3e: persistence
#   include "noun/hashtable.h" //  u3h: hashtables
#   include "noun/imprison.h"  //  u3i: noun construction
#   include "noun/jets.h"      //  u3j: jet control
#   include "noun/log.h"       //  u3l: logging
#   include "noun/manage.h"    //  u3m: master state
#   include "noun/nock.h"      //  u3n: nock execution
#   include "noun/options.h"   //  u3o: config options
#   include "noun/retrieve.h"  //  u3r: noun access (error returns)
#   include "noun/serial.h"    //  u3s: serialization
#   include "noun/trace.h"     //  u3t: profiling / tracing
#   include "noun/xtract.h"    //  u3x: noun access (error crashes)
#   include "noun/vortex.h"    //  u3v: arvo kernel
#   include "noun/zave.h"      //  u3z: memoization

#   include "jets/k.h"         //  u3k: jets (transfer, args)
#   include "jets/q.h"         //  u3q: jets (retain, args)
#   include "jets/w.h"         //  u3w: jets (retain, core)
