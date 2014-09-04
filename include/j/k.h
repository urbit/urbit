/* include/f/kjet.h
**
** This file is in the public domain.
*/
  /** Tier 1.
  **/
    u2_noun u2_cka_add(u2_noun a, u2_noun b); 
    u2_noun u2_cka_sub(u2_noun a, u2_noun b);
    u2_noun u2_cka_mul(u2_noun a, u2_noun b);
    u2_noun u2_cka_gth(u2_noun a, u2_noun b);
    u2_bean u2_cka_lte(u2_noun a, u2_noun b);

  /** Tier 2.
  **/
    u2_noun u2_ckb_lent(u2_noun a);
    u2_noun u2_ckb_weld(u2_noun a, u2_noun b);
    u2_noun u2_ckb_flop(u2_noun a);

/* u2_ckc: tier 3 functions
*/
  /* u2_ckc_lsh(): left shift.
  */
    u2_noun
    u2_ckc_lsh(u2_noun a, u2_noun b, u2_noun c);

  /* u2_ckc_rsh(): right shift.
  */
    u2_noun
    u2_ckc_rsh(u2_noun a, u2_noun b, u2_noun c);

/* u2_ckd: tier 4 functions
*/
  /* u2_ckdb_get(): map get for key `b` in map `a` with u2_none.
  */
    u2_weak
    u2_ckdb_get(u2_noun a, u2_noun b);

  /* u2_ckdb_got(): map get for key `b` in map `a` with bail.
  */
    u2_noun
    u2_ckdb_got(u2_noun a, u2_noun b);

  /* u2_ckdb_put(): map put for key `b`, value `c` in map `a`.
  */
    u2_weak
    u2_ckdb_put(u2_noun a, u2_noun b, u2_noun c);

  /* u2_ckdb_has(): test for get.
  */
    u2_bean
    u2_ckdb_has(u2_noun a, u2_noun b);

  /* u2_ckdb_gas(): list to map.
  */
    u2_noun
    u2_ckdb_gas(u2_noun a, u2_noun b);

  /* u2_ckdi_gas(): list to map.
  */
    u2_noun
    u2_ckdi_gas(u2_noun a, u2_noun b);

  /* u2_ckdi_has(): test for presence.
  */
    u2_bean
    u2_ckdi_has(u2_noun a, u2_noun b);

  /* u2_ckdi_tap(): map/set convert to list.  (solves by_tap also.)
  */
    u2_noun
    u2_ckdi_tap(u2_noun a, u2_noun b);

#define u2_ckd_by_tap(a, b) u2_ckdi_tap(a, b)

/* u2_cke: tier 5 functions
*/
  /* u2_cke_cue(): expand saved pill.
  */
    u2_noun
    u2_cke_cue(u2_atom a);

  /* u2_cke_jam(): pack noun as atom.
  */
    u2_atom
    u2_cke_jam(u2_noun a);

  /* u2_cke_trip: atom to tape.
  */
    u2_noun
    u2_cke_trip(u2_noun a);

