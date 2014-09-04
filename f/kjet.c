/* f/meme.c
**
** This file is in the public domain.
*/
#include "f/meme.h"

/* u2_cka_add(): a + b.
*/
u2_noun
u2_cka_add(u2_noun a, u2_noun b)
{
  u2_noun c = u2_cqa_add(a, b);

  u2z(a); u2z(b);
  return c;
}

/* u2_cka_sub(): a + b.
*/
u2_noun
u2_cka_sub(u2_noun a, u2_noun b)
{
  u2_noun c = u2_cqa_sub(a, b);

  u2z(a); u2z(b);
  return c;
}

/* u2_cka_gth(): a + b.
*/
u2_noun
u2_cka_gth(u2_noun a, u2_noun b)
{
  u2_noun c = u2_cqa_gth(a, b);

  u2z(a); u2z(b);
  return c;
}

/* u2_cka_mul(): a * b.
*/
u2_noun
u2_cka_mul(u2_noun a, u2_noun b)
{
  u2_noun c = u2_cqa_mul(a, b);

  u2z(a); u2z(b);
  return c;
}

/* u2_cka_lte(): a * b.
*/
u2_noun
u2_cka_lte(u2_noun a, u2_noun b)
{
  u2_noun c = u2_cqa_lte(a, b);

  u2z(a); u2z(b);
  return c;
}

/* u2_ckb_lent(): length of list `a`.
*/
u2_noun
u2_ckb_lent(u2_noun a)
{
  u2_noun b = u2_cqb_lent(a);

  u2z(a);
  return b;
}

/* u2_ckb_flop(): reverse list `a`.
*/
u2_noun
u2_ckb_flop(u2_noun a)
{
  u2_noun b = u2_cqb_flop(a);

  u2z(a);
  return b;
}

/* u2_ckb_weld(): concatenate lists `a` before `b`.
*/
u2_noun
u2_ckb_weld(u2_noun a, u2_noun b)
{
  u2_noun c = u2_cqb_weld(a, b);

  u2z(a); u2z(b);
  return c;
}

/* u2_ckc_lsh(): left shift.
*/
u2_noun
u2_ckc_lsh(u2_noun a, u2_noun b, u2_noun c)
{
  u2_noun d = u2_cqc_lsh(a, b, c);

  u2z(a); u2z(b); u2z(c);
  return d;
}

/* u2_ckc_rsh(): right shift.
*/
u2_noun
u2_ckc_rsh(u2_noun a, u2_noun b, u2_noun c)
{
  u2_noun d = u2_cqc_rsh(a, b, c);

  u2z(a); u2z(b); u2z(c);
  return d;
}

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
u2_noun
u2_ckdb_got(u2_noun a, u2_noun b)
{
  u2_weak c = u2_ckdb_get(a, b);

  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  }
  else return c;
}

/* u2_ckdb_put(): map put for key `b`, value `c` in map `a`.
*/
u2_weak
u2_ckdb_put(u2_noun a, u2_noun b, u2_noun c)
{
  // Bizarre asymmetry in old jets.
  //
  // (Mysterious comment in old glue code.)
  //
  u2_noun pro = u2_cqdb_put(a, b, c);

  u2z(a); u2z(b); u2z(c);
  return pro;
}

/* u2_ckdb_gas(): list to map.
*/
u2_noun
u2_ckdb_gas(u2_noun a, u2_noun b)
{
  u2_weak c = u2_cqdb_gas(a, b);

  u2z(a); u2z(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  }
  else return c;
}

/* u2_ckdi_gas(): list to map.
*/
u2_noun
u2_ckdi_gas(u2_noun a, u2_noun b)
{
  u2_weak c = u2_cqdi_gas(a, b);

  u2z(a); u2z(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  }
  else return c;
}

/* u2_ckdb_has(): test for presence.
*/
u2_bean
u2_ckdb_has(u2_noun a, u2_noun b)
{
  u2_weak c = u2_cqdb_has(a, b);

  u2z(a); u2z(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  }
  else return c;
}

/* u2_ckdi_has(): test for presence.
*/
u2_bean
u2_ckdi_has(u2_noun a, u2_noun b)
{
  u2_weak c = u2_cqdi_has(a, b);

  u2z(a); u2z(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  }
  else return c;
}

/* u2_ckdi_tap(): map/set convert to list.  (solves by_tap also.)
*/
u2_noun
u2_ckdi_tap(u2_noun a, u2_noun b)
{
  u2_weak c = u2_cqdi_tap(a, b);

  u2z(a); u2z(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  }
  else return c;
}

/* u2_cke_cue(): expand saved pill.
*/

u2_noun
u2_cke_cue(u2_atom a)
{
  u2_noun b = u2_cqe_cue(a);

  u2z(a);
  return b;
}

/* u2_cke_jam(): pack noun as atom.
*/
u2_atom
u2_cke_jam(u2_noun a)
{
  u2_atom b = u2_cqe_jam(a);

  u2z(a);
  return b;
}

/* u2_cke_trip(): atom to tape.
*/
u2_atom
u2_cke_trip(u2_noun a)
{
  u2_atom b = u2_cqe_trip(a);

  u2z(a);
  return b;
}
