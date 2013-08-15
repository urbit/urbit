/* include/all.h
**
** This file is in the public domain.
*/
  /** Must be compiled on gcc with C99 support.
  **/
#    ifndef __GNUC__
#      error "port me"
#    endif
#    define _GNU_SOURCE


  /** External, OS-independent library dependencies.
  **/
    /* The GMP (GNU arbitrary-precision arithmetic) library.
    ** (Tested with version 4.0.1.)
    */
#      include <gmp.h>


  /** Global variable control.
  **/
#   ifdef U4_GLOBALS
#     define U4_GLOBAL
#   else
#     define U4_GLOBAL extern
#   endif

  /** Transition.
  **/
#     define u4_true      1
#     define u4_false     0

  /** Compilation flags.
  **/
    /* Turn on diagnostic stats.
    */
#     define U4_DIAG_NOCK 1


  /** Internal includes.
  **/
    /* C support libraries (nonportable).
    */
#     include "u4/lib/lib.h"
  
    /* Memory and storage.
    */
#     include "u4/cake/cake.h"

    /* Nock computation.
    */
#     include "u4/nock/nock.h"

    /* Fake noun functions.
    */
#     include "u4/fake/fake.h"

    /* Mill - more fake.
    */
#if 0
#     include "u4/mill/mill.h"
#endif
    /* Bootstrap compiler.
    */
#     include "u4/watt/plow.h"

    /* Fake parser.
    */
#     include "u4/watt/parse.h"

    /* Filesystem I/O.
    */
#     include "u4/disk/disk.h"
