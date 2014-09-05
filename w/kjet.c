/* f/meme.c
**
** This file is in the public domain.
*/
#include "f/meme.h"

  /* u2_cka_sub(): a + b.
  */
  /* u2_cka_gth(): a + b.
  */

  /* u2_cka_mul(): a * b.
  */
  /* u2_cka_lte(): a * b.
  */

  /* u2_ckb_lent(): length of list `a`.
  */
  /* u2_ckb_flop(): reverse list `a`.
  */
  /* u2_ckb_weld(): concatenate lists `a` before `b`.
  */
  /* u2_ckc_lsh(): left shift.
  */
  /* u2_ckc_rsh(): right shift.
  */
  /* u2_ckdb_get(): map get for key `b` in map `a` with u2_none.
  */
  u2_weak
  u2_ckdb_get(u2_noun a, u2_noun b)
  {
    u2_noun c = u2_cqdb_get(a, b);

    u2z(a); u2z(b);
    if ( u2_no == u2_cr_du(c) ) {
      u2z(c);
      return u2_none;
    } else {
      u2_noun pro = u2k(u2t(c));

      u2z(c);
      return pro;
    }
  }

  /* u2_ckdb_got(): map get for key `b` in map `a` with fail.
  */
  /* u2_ckdb_put(): map put for key `b`, value `c` in map `a`.
  */
  /* u2_ckdb_gas(): list to map.
  */
  /* u2_ckdi_gas(): list to map.
  */

  /* u2_ckdb_has(): test for presence.
  */
  /* u2_ckdi_has(): test for presence.
  */
  /* u2_ckdi_tap(): map/set convert to list.  (solves by_tap also.)
  */

  /* u2_cke_cue(): expand saved pill.
  */

  /* u2_cke_jam(): pack noun as atom.
  */
  /* u2_cke_trip(): atom to tape.
  */
