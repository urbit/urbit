/* fake/pump/prep.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* Return true iff (atom) is an ASCII string of (xb) or more bytes,
** using no characters besides a-z and -.
*/
static u4_t
_test_term(u4_lane lane,
           u4_atom atom,
           u4_xb   xb)
{
  u4_sb sb = u4_a_bin(atom, 3);

  if ( sb >= xb) {
    u4_xb *xb = alloca(sb);
    u4_pb i;

    u4_a_bytes(atom, xb, 0, sb);

    for ( i=0; i < sb; i++ ) {
      if ( ((xb[i] < 'a') || (xb[i] > 'z')) && (xb[i] != '-') ) {
        return 0;
      }
    }
    return 1;
  }
  else return 0;
}

/* u4_prep_textual():
**
**   Prep with a text bias; fall back to decimal.
*/
u4_prep
u4_prep_textual(u4_lane lane,
                u4_atom atom)
{
  if ( _test_term(lane, atom, 1) ) {
    return atom;
  }
  else return u4_prep_decimal(lane, atom);
}

/* u4_prep_decimal():
**
**   Prep a decimal value.
*/
u4_prep
u4_prep_decimal(u4_lane lane,
                u4_atom atom)
{
  u4_ca *ca;
  mpz_t mp;
  u4_noun text;

  u4_a_gmp(atom, mp);
  ca = mpz_get_str(0, 10, mp);

  text = u4_k_atom_c(lane, ca);

  free(ca);
  mpz_clear(mp);

  return text;
}

/* u4_prep_heximal():
**
**   Prep a hexadecimal value, with 0x.
*/
u4_prep
u4_prep_heximal(u4_lane lane,
                u4_atom atom)
{
  u4_sy sy_n = u4_a_bin(atom, 2);
  u4_sb sb_hex = (sy_n ? sy_n : 1) + 2;
  u4_ca *ca = malloc(sb_hex + 1);
  mpz_t mp;
  u4_noun text;

  u4_a_gmp(atom, mp);

  ca[0] = '0';
  ca[1] = 'x';
  mpz_get_str(ca + 2, 16, mp);

  text = u4_k_atom_c(lane, ca);
  free(ca);
  mpz_clear(mp);

  return text;
}

/* u4_prep_hexinal():
**
**   Prep a heximal value, without 0x.
*/
u4_prep
u4_prep_hexinal(u4_lane lane,
                u4_atom atom)
{
  u4_sy sy_n = u4_a_bin(atom, 2);
  u4_sb sb_hex = (sy_n ? sy_n : 1);
  u4_ca *ca = malloc(sb_hex + 1);
  mpz_t mp;
  u4_noun text;

  u4_a_gmp(atom, mp);

  mpz_get_str(ca, 16, mp);

  text = u4_k_atom_c(lane, ca);
  free(ca);
  mpz_clear(mp);

  return text;
}

/* Convert (atom) to string text - by guessing.  Use only as last resort.
*/
static u4_noun
_prep_atom(u4_lane lane, u4_atom atom)
{
  u4_sb sb;

  switch ( (sb = u4_a_bin(atom, 3)) ) {
    case 0: return u4_cod_in('0');
    case 1: return u4_prep_decimal(lane, atom);

    default: {
      if ( _test_term(lane, atom, 2) ) {
        return u4_k_safe(lane, atom);
      }
      else {
        if ( sb > 2 ) {
          return u4_prep_heximal(lane, atom);
        }
        else return u4_prep_decimal(lane, atom);
      }
    }
  }
}

/* Convert (cell) to a list of preps.
*/
static u4_noun
_prep_tuple(u4_lane lane, u4_cell cell)
{
  u4_noun head = u4_ch(cell);
  u4_noun tail = u4_ct(cell);

  if ( u4_n_cell(tail) ) {
    return u4_k_cell(lane, u4_pump_prep(lane, head),
                           _prep_tuple(lane, tail));
  }
  else {
    u4_prep prep_head = u4_pump_prep(lane, head);
    u4_prep prep_tail = u4_pump_prep(lane, tail);
 
    return u4_k_trel(lane, prep_head, prep_tail, u4_noun_0);
  }
}

/* Convert (cell) to a prep.
*/
static u4_noun
_prep_cell(u4_lane lane, 
           u4_cell cell)
{
  return u4_k_quil
    (lane, u4_atom_nail,
           u4_cod_in('['),
           u4_k_cell(lane, u4_atom_glue, _prep_tuple(lane, cell)),
           u4_cod_in(']'),
           u4_noun_0);
}

/* u4_pump_prep():
**
**   Convert (noun) to a prep, which is
**
**      (text)
**    | (.glue *prep)
**    | (.nail *prep)
*/
u4_prep
u4_pump_prep(u4_lane lane,
             u4_noun noun)
{
  if ( u4_n_atom(noun) ) {
    return _prep_atom(lane, noun);
  } else {
    return _prep_cell(lane, noun);
  }
}

/* u4_prep_close():
**
**   Prep a list of preps, in (xb_a, xb_b).
*/
u4_prep
u4_prep_close(u4_lane lane,
              u4_xb   xb_a,
              u4_xb   xb_b,
              u4_log  gah)
{
  return u4_k_quil
    (lane, u4_atom_nail,
           u4_cod_in(xb_a),
           u4_k_cell(lane, u4_atom_glue, gah),
           u4_cod_in(xb_b),
           u4_noun_0);
}
