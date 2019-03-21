/* include/f/kjet.h
**
** This file is in the public domain.
*/
  /** Tier 1.
  **/
    u3_noun u3la_add(u3_noun a, u3_noun b);
    u3_noun u3la_sub(u3_noun a, u3_noun b);
    u3_noun u3la_mul(u3_noun a, u3_noun b);
    u3_noun u3la_gth(u3_noun a, u3_noun b);
    u3_noun u3la_lte(u3_noun a, u3_noun b);

  /** Tier 2.
  **/
    u3_noun u3lb_lent(u3_noun a);
    u3_noun u3lb_weld(u3_noun a, u3_noun b);
    u3_noun u3lb_flop(u3_noun a);

/* u3lc: tier 3 functions
*/
  /* u3lc_lsh(): left shift.
  */
    u3_noun
    u3lc_lsh(u3_noun a, u3_noun b, u3_noun c);

  /* u3lc_rsh(): right shift.
  */
    u3_noun
    u3lc_rsh(u3_noun a, u3_noun b, u3_noun c);

/* u3ld: tier 4 functions
*/
  /* u3ldb_get(): map get for key `b` in map `a` with u3_none.
  */
    u3_weak
    u3ldb_get(u3_noun a, u3_noun b);

  /* u3ldb_got(): map get for key `b` in map `a` with bail.
  */
    u3_noun
    u3ldb_got(u3_noun a, u3_noun b);

  /* u3ldb_put(): map put for key `b`, value `c` in map `a`.
  */
    u3_weak
    u3ldb_put(u3_noun a, u3_noun b, u3_noun c);

  /* u3ldb_has(): test for get.
  */
    u3_noun
    u3ldb_has(u3_noun a, u3_noun b);

  /* u3ldb_gas(): list to map.
  */
    u3_noun
    u3ldb_gas(u3_noun a, u3_noun b);

  /* u3ldi_gas(): list to map.
  */
    u3_noun
    u3ldi_gas(u3_noun a, u3_noun b);

  /* u3ldi_has(): test for presence.
  */
    u3_noun
    u3ldi_has(u3_noun a, u3_noun b);

  /* u3ldi_tap(): map/set convert to list.  (solves by_tap also.)
  */
    u3_noun
    u3ldi_tap(u3_noun a);

  /* u3ldi_put(): put in set.
  */
    u3_weak
    u3ldi_put(u3_noun a, u3_noun b);

  /* u3ldi_uni(): set union.
  */
    u3_noun
    u3ldi_uni(u3_noun a, u3_noun b);

#   define u3ldb_tap(a) u3ldi_tap(a)

/* u3le: tier 5 functions
*/
  /* u3le_cue(): expand saved pill.
  */
    u3_noun
    u3le_cue(u3_atom a);

  /* u3le_jam(): pack noun as atom.
  */
    u3_atom
    u3le_jam(u3_noun a);

  /* u3le_trip(): atom to tape.
  */
    u3_noun
    u3le_trip(u3_noun a);

  /* u3lf_fork(): build %fork span.
  */
    u3_noun
    u3lf_fork(u3_noun yed);

  /* u3kz_fork(): build %fork span.
  */
    u3_noun
    u3kz_fork(u3_noun yed);

  /* u3lfu_repo():
  */
    u3_noun
    u3lfu_repo(u3_noun, u3_noun);
