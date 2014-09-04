/* include/all.h
**
** This file is in the public domain.
*/
  /**  Prefix definitions:
  ***
  ***  u2_ca_: fundamental allocators.
  ***  u2_cc_: constants.
  ***  u2_ch_: HAMT hash tables.
  ***  u2_ci_: noun constructors
  ***  u2_cj_: jets.
  ***  u2_ck*: direct jet calls (modern C convention)
  ***  u2_cm_: system management etc.
  ***  u2_cn_: nock interpreter.
  ***  u2_co_: fundamental macros.
  ***  u2_cq*: direct jet calls (archaic C convention)
  ***  u2_cr_: read functions which never bail out.
  ***  u2_cs_: structures and definitions.
  ***  u2_ct_: tracing.
  ***  u2_cw_: direct jet calls (core noun convention)
  ***  u2_cx_: read functions which do bail out.
  ***  u2_cz_: memoization.
  ***
  ***  u2_cr_, u2_cx_ functions use retain conventions; the caller
  ***  retains ownership of passed-in nouns, the callee preserves 
  ***  ownership of returned nouns.
  ***
  ***  Unless documented otherwise, all other functions use transfer 
  ***  conventions; the caller logically releases passed-in nouns, 
  ***  the callee logically releases returned nouns.
  ***
  ***  In general, exceptions to the transfer convention all occur
  ***  when we're using a noun as a key.
  **/

    /** c: the c3 layer, C portability and definitions.
    **/
#     include "c/portable.h"
#     include "c/tune.h"
#     include "c/types.h"
#     include "c/defs.h"
#     include "c/motes.h"
#     include "c/comd.h"

    /** f: the u2 layer, definitions and data structures.
    **/
#     include "f/meze.h"
#     include "f/tune.h"
#     include "f/noun.h"
#     include "f/hash.h"
#     include "f/jets.h"
#     include "f/road.h"
#     include "f/glob.h"

    /** g: the u2 layer, functions.
    **/
#     include "g/a.h"
#     include "g/h.h"
#     include "g/i.h"
#     include "g/j.h"
#     include "g/m.h"
#     include "g/n.h"
#     include "g/r.h"
#     include "g/t.h"
#     include "g/x.h"
#     include "g/z.h"

    /** j: the u2 layer, jets.
    **/
#     include "j/k.h"
#     include "j/w.h"
#     include "j/q.h"
