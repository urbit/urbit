/* include/all.h
**
** This file is in the public domain.
*/
  /**  Prefix definitions:
  ***
  ***  u3_ca_: fundamental allocators.
  ***  u3_cc_: constants.
  ***  u3_ce_: checkpointing.
  ***  u3_ch_: HAMT hash tables.
  ***  u3_ci_: noun constructors
  ***  u3_cj_: jets.
  ***  u3_ck*: direct jet calls (modern C convention)
  ***  u3_cm_: system management etc.
  ***  u3_cn_: nock interpreter.
  ***  u3_co_: fundamental macros.
  ***  u3_cq*: direct jet calls (archaic C convention)
  ***  u3_cr_: read functions which never bail out.
  ***  u3_cs_: structures and definitions.
  ***  u3_ct_: tracing.
  ***  u3_cw_: direct jet calls (core noun convention)
  ***  u3_cx_: read functions which do bail out.
  ***  u3_cv_: arvo specific structures.
  ***  u3_cz_: memoization.
  ***
  ***  u3_cr_, u3_cx_ functions use retain conventions; the caller
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

    /** n: the u3 layer, definitions and data structures.
    **/
#     include "n/tune.h"
#     include "n/noun.h"
#     include "n/hash.h"
#     include "n/road.h"
#     include "n/jets.h"
#     include "n/meze.h"
#     include "n/arvo.h"
#     include "n/glob.h"

    /** g: the u3 layer, functions.
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
#     include "g/v.h"
#     include "g/z.h"

    /** j: the u3 layer, jets.
    **/
#     include "j/k.h"
#     include "j/w.h"
#     include "j/q.h"
