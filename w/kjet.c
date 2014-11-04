/* f/meme.c
**
** This file is in the public domain.
*/
#include "f/meme.h"

  /* u3_cka_sub(): a + b.
  */
  /* u3_cka_gth(): a + b.
  */

  /* u3_cka_mul(): a * b.
  */
  /* u3_cka_lte(): a * b.
  */

  /* u3_ckb_lent(): length of list `a`.
  */
  /* u3_ckb_flop(): reverse list `a`.
  */
  /* u3_ckb_weld(): concatenate lists `a` before `b`.
  */
  /* u3_ckc_lsh(): left shift.
  */
  /* u3_ckc_rsh(): right shift.
  */
  /* u3_ckdb_get(): map get for key `b` in map `a` with u3_none.
  */
  u3_weak
  u3_ckdb_get(u3_noun a, u3_noun b)
  {
    u3_noun c = u3_cqdb_get(a, b);

    u3z(a); u3z(b);
    if ( u3_no == u3_cr_du(c) ) {
      u3z(c);
      return u3_none;
    } else {
      u3_noun pro = u3k(u3t(c));

      u3z(c);
      return pro;
    }
  }

  /* u3_ckdb_got(): map get for key `b` in map `a` with fail.
  */
  /* u3_ckdb_put(): map put for key `b`, value `c` in map `a`.
  */
  /* u3_ckdb_gas(): list to map.
  */
  /* u3_ckdi_gas(): list to map.
  */

  /* u3_ckdb_has(): test for presence.
  */
  /* u3_ckdi_has(): test for presence.
  */
  /* u3_ckdi_tap(): map/set convert to list.  (solves by_tap also.)
  */

  /* u3_cke_cue(): expand saved pill.
  */

  /* u3_cke_jam(): pack noun as atom.
  */
  /* u3_cke_trip(): atom to tape.
  */
