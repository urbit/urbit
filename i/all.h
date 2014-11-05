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
  ***  u3_cr_, u3_cx_, u3_cz_ functions use retain conventions; the caller
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
#     include "c/types.h"
#     include "c/defs.h"
#     include "c/motes.h"

    /** miscellaneous definitions and data structures.
    **/
    /* u3_yes, u3_no, u3_nul;
    **
    **   Our Martian booleans and list terminator; empty string; not a noun.
    */
#     define u3_yes   0
#     define u3_no    1
#     define u3_nul   0
#     define u3_blip  0

    /* Tools for Martian booleans.
    */
#     define u3_so(x)      (u3_yes == (x))
#     define u3_ne(x)      (u3_no == (x))
#     define u3_say(x)     ( (x) ? u3_yes : u3_no )
#     define u3_not(x)     ( (x == u3_yes) ? u3_no : u3_yes )
#     define u3_and(x, y)  ( (u3_so(x) && u3_so(y)) ? u3_yes : u3_no )
#     define u3_or(x, y)   ( (u3_so(x) || u3_so(y)) ? u3_yes : u3_no )

#     define u3_assure(x)  if ( u3_ne(x) ) { u3_cm_bail(c3__fail); }
#     define u3_assent(x)  if ( u3_ne(x) ) { u3_cm_bail(c3__exit); }


  /** Aliases - selective and syntactically unique.
  **/
#   define u3h(som)          u3_cx_h(som)
#   define u3t(som)          u3_cx_t(som)
#   define u3at(axe, som)    u3_cx_at(axe, som)

#   define u3nc(a, b)        u3_ci_cell(a, b)
#   define u3nt(a, b, c)     u3_ci_trel(a, b, c)
#   define u3nq(a, b, c, d)  u3_ci_qual(a, b, c, d)

#   define u3du(som)         (u3_cr_du(som))
#   define u3ud(som)         (u3_cr_ud(som))

#   define u3k(som)          u3_ca_gain(som)
#   define u3z(som)          u3_ca_lose(som)

  /** Arvo macros.
  **/
#   define  u3_do(txt_c, arg)         u3_cv_do(txt_c, arg)
#   define  u3_dc(txt_c, a, b)        u3_cv_do(txt_c, u3nc(a, b))
#   define  u3_dt(txt_c, a, b, c)     u3_cv_do(txt_c, u3nt(a, b, c))
#   define  u3_dq(txt_c, a, b, c, d)  u3_cv_do(txt_c, u3nt(a, b, c, d))

    /** g: the u3 layer, functions.
    **/
#     include "g/a.h"
#     include "g/e.h"
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
