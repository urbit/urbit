/* include/f/kjet.h
**
** This file is in the public domain.
*/
  /** Tier 1.
  **/
    u3_noun u3ka_add(u3_noun a, u3_noun b); 
    u3_noun u3ka_sub(u3_noun a, u3_noun b);
    u3_noun u3ka_mul(u3_noun a, u3_noun b);
    u3_noun u3ka_gth(u3_noun a, u3_noun b);
    u3_noun u3ka_lte(u3_noun a, u3_noun b);

  /** Tier 2.
  **/
    u3_noun u3kb_lent(u3_noun a);
    u3_noun u3kb_weld(u3_noun a, u3_noun b);
    u3_noun u3kb_flop(u3_noun a);

/* u3kc: tier 3 functions
*/
  /* u3kc_lsh(): left shift.
  */
    u3_noun
    u3kc_lsh(u3_noun a, u3_noun b, u3_noun c);

  /* u3kc_rsh(): right shift.
  */
    u3_noun
    u3kc_rsh(u3_noun a, u3_noun b, u3_noun c);

/* u3kd: tier 4 functions
*/
  /* u3kdb_get(): map get for key `b` in map `a` with u3_none.
  */
    u3_weak
    u3kdb_get(u3_noun a, u3_noun b);

  /* u3kdb_got(): map get for key `b` in map `a` with bail.
  */
    u3_noun
    u3kdb_got(u3_noun a, u3_noun b);

  /* u3kdb_put(): map put for key `b`, value `c` in map `a`.
  */
    u3_weak
    u3kdb_put(u3_noun a, u3_noun b, u3_noun c);

  /* u3kdb_has(): test for get.
  */
    u3_noun
    u3kdb_has(u3_noun a, u3_noun b);

  /* u3kdb_gas(): list to map.
  */
    u3_noun
    u3kdb_gas(u3_noun a, u3_noun b);

  /* u3kdi_gas(): list to map.
  */
    u3_noun
    u3kdi_gas(u3_noun a, u3_noun b);

  /* u3kdi_has(): test for presence.
  */
    u3_noun
    u3kdi_has(u3_noun a, u3_noun b);

  /* u3kdi_tap(): map/set convert to list.  (solves by_tap also.)
  */
    u3_noun
    u3kdi_tap(u3_noun a, u3_noun b);

  /* u3kdi_put(): put in set.
  */
    u3_weak
    u3kdi_put(u3_noun a, u3_noun b);

#   define u3kdb_tap(a, b) u3kdi_tap(a, b)

/* u3ke: tier 5 functions
*/
  /* u3ke_cue(): expand saved pill.
  */
    u3_noun
    u3ke_cue(u3_atom a);

  /* u3ke_jam(): pack noun as atom.
  */
    u3_atom
    u3ke_jam(u3_noun a);

  /* u3ke_trip(): atom to tape.
  */
    u3_noun
    u3ke_trip(u3_noun a);

  /* u3kf_frog(): build %fork span.
  */
    u3_noun
    u3kf_frog(u3_noun yed);
