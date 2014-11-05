/* include/f/kjet.h
**
** This file is in the public domain.
*/
  /** Tier 1.
  **/
    u3_noun u3_cka_add(u3_noun a, u3_noun b); 
    u3_noun u3_cka_sub(u3_noun a, u3_noun b);
    u3_noun u3_cka_mul(u3_noun a, u3_noun b);
    u3_noun u3_cka_gth(u3_noun a, u3_noun b);
    u3_bean u3_cka_lte(u3_noun a, u3_noun b);

  /** Tier 2.
  **/
    u3_noun u3_ckb_lent(u3_noun a);
    u3_noun u3_ckb_weld(u3_noun a, u3_noun b);
    u3_noun u3_ckb_flop(u3_noun a);

/* u3_ckc: tier 3 functions
*/
  /* u3_ckc_lsh(): left shift.
  */
    u3_noun
    u3_ckc_lsh(u3_noun a, u3_noun b, u3_noun c);

  /* u3_ckc_rsh(): right shift.
  */
    u3_noun
    u3_ckc_rsh(u3_noun a, u3_noun b, u3_noun c);

/* u3_ckd: tier 4 functions
*/
  /* u3_ckdb_get(): map get for key `b` in map `a` with u3_none.
  */
    u3_weak
    u3_ckdb_get(u3_noun a, u3_noun b);

  /* u3_ckdb_got(): map get for key `b` in map `a` with bail.
  */
    u3_noun
    u3_ckdb_got(u3_noun a, u3_noun b);

  /* u3_ckdb_put(): map put for key `b`, value `c` in map `a`.
  */
    u3_weak
    u3_ckdb_put(u3_noun a, u3_noun b, u3_noun c);

  /* u3_ckdb_has(): test for get.
  */
    u3_bean
    u3_ckdb_has(u3_noun a, u3_noun b);

  /* u3_ckdb_gas(): list to map.
  */
    u3_noun
    u3_ckdb_gas(u3_noun a, u3_noun b);

  /* u3_ckdi_gas(): list to map.
  */
    u3_noun
    u3_ckdi_gas(u3_noun a, u3_noun b);

  /* u3_ckdi_has(): test for presence.
  */
    u3_bean
    u3_ckdi_has(u3_noun a, u3_noun b);

  /* u3_ckdi_tap(): map/set convert to list.  (solves by_tap also.)
  */
    u3_noun
    u3_ckdi_tap(u3_noun a, u3_noun b);

  /* u3_ckdi_put(): put in set.
  */
  u3_weak
  u3_ckdi_put(u3_noun a, u3_noun b);

#   define u3_ckdb_tap(a, b) u3_ckdi_tap(a, b)

/* u3_cke: tier 5 functions
*/
  /* u3_cke_cue(): expand saved pill.
  */
    u3_noun
    u3_cke_cue(u3_atom a);

  /* u3_cke_jam(): pack noun as atom.
  */
    u3_atom
    u3_cke_jam(u3_noun a);

  /* u3_cke_trip: atom to tape.
  */
    u3_noun
    u3_cke_trip(u3_noun a);

