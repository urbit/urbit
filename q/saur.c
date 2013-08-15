/* j/saur.c
**
** This file is in the public domain.
*/
#include "all.h"

/** Warning: this file contains old code which does not comply
*** with current coding conventions.
**/

/* u2_fj_op_add(): 
**
**   Produce the sum of (a) and (b).
*/
u2_atom
u2_fj_op_add(u2_ray  wir_r,
             u2_atom a,
             u2_atom b)
{
  mpz_t mp_a, mp_b;

  u2_mp(mp_a, a);
  u2_mp(mp_b, b);

  mpz_add(mp_a, mp_a, mp_b);
  mpz_clear(mp_b);

  return u2_bn_mp(wir_r, mp_a);
}

/* u2_fj_op_con():
**
**   Produce (a | b).
*/
u2_atom
u2_fj_op_con(u2_ray  wir_r,
             u2_atom a,
             u2_atom b)
{
  mpz_t mp_a, mp_b;

  u2_mp(mp_a, a);
  u2_mp(mp_b, b);

  mpz_ior(mp_a, mp_a, mp_b);
  mpz_clear(mp_b);

  return u2_bn_mp(wir_r, mp_a);
}

/* u2_fj_op_dec():
**
**   Produce (atom - 1), or bull if (atom) is 0.
*/
u2_weak
u2_fj_op_dec(u2_ray  wir_r,
             u2_atom atom)
{
  if ( (_0 == atom) ) {
    return u2_none;
  }
  else {
    mpz_t mp;

    u2_mp(mp, atom);
    mpz_sub_ui(mp, mp, 1);

    return u2_bn_mp(wir_r, mp);
  }
}

/* u2_fj_op_div():
**
**   Produce (b / a), or bull if (a) is 0.
*/
u2_weak
u2_fj_op_div(u2_ray  wir_r,
             u2_atom a,
             u2_atom b)
{
  if ( (_0 == a) ) {
    return u2_none;
  }
  else {
    mpz_t mp_a, mp_b;

    u2_mp(mp_a, a);
    u2_mp(mp_b, b);

    mpz_tdiv_q(mp_b, mp_b, mp_a);
    mpz_clear(mp_a);

    return u2_bn_mp(wir_r, mp_b);
  }
}

/* u2_fj_op_glu():
**
**   Concatenate atomic strings `a` and `b`.
*/
u2_atom
u2_fj_op_glu(u2_ray  wir_r,
             u2_atom a,
             u2_atom b)
{
  c3_w sb_a = u2_met(3, a);
  c3_w sb_b = u2_met(3, b);
  c3_w sb_cat = (sb_a + sb_b);
  {
    c3_y *xb_buf = alloca(sb_cat);

    u2_bytes(0, sb_a, xb_buf, a);
    u2_bytes(0, sb_b, xb_buf + sb_a, b);

    return u2_bn_bytes(wir_r, sb_cat, xb_buf);
  }
}

/* u2_fj_op_inc():
**
**   Produce (atom + 1).
*/
u2_atom
u2_fj_op_inc(u2_ray  wir_r,
             u2_atom atom)
{
  mpz_t mp;

  u2_mp(mp, atom);
  mpz_add_ui(mp, mp, 1);

  return u2_bn_mp(wir_r, mp);
}

/* u2_fj_op_log():
**
**   Produce the lowest m_log such that (1 << m_log) > m.
*/
u2_atom
u2_fj_op_log(u2_ray  wir_r,
             u2_atom atom)
{
  return u2_met(0, atom);
}
  
/* u2_fj_op_lsh():
**
**   Produce (b << a).
*/
u2_atom
u2_fj_op_lsh(u2_ray  wir_r,
             u2_atom a,
             u2_atom b)
{
  c3_w xw_count = a;
  mpz_t mp;

  u2_mp(mp, b);
  mpz_mul_2exp(mp, mp, xw_count);

  return u2_bn_mp(wir_r, mp);
}

/* u2_fj_op_peg():
**
**   Concatenate (twig_a) above (twig_b).
*/
u2_atom
u2_fj_op_peg(u2_ray  wir_r,
          u2_atom twig_a,
          u2_atom twig_b)
{
  u2_atom c, d, e, f, g;

  c = u2_fj_op_log(wir_r, twig_b);
  d = u2_fj_op_dec(wir_r, c);
  e = u2_fj_op_lsh(wir_r, d, _1);
  f = u2_fj_op_sub(wir_r, e, twig_b);
  g = u2_fj_op_lsh(wir_r, d, twig_a);

  return u2_fj_op_add(wir_r, f, g);
}

/* u2_fj_op_rsh():
**
**   Produce (b >> a).
*/
u2_atom
u2_fj_op_rsh(u2_ray  wir_r,
             u2_atom a,
             u2_atom b)
{
  c3_w xw_count = a;
  mpz_t mp;

  u2_mp(mp, b);
  mpz_tdiv_q_2exp(mp, mp, xw_count);

  return u2_bn_mp(wir_r, mp);
}

/* u2_fj_op_sub():
**
**   Produce (b - a), or bull if (a > b).
*/
u2_weak
u2_fj_op_sub(u2_ray  wir_r,
             u2_atom a,
             u2_atom b)
{
  mpz_t mp_a, mp_b;

  u2_mp(mp_a, a);
  u2_mp(mp_b, b);

  if ( mpz_cmp(mp_b, mp_a) < 0 ) {
    mpz_clear(mp_a);
    mpz_clear(mp_b);

    return u2_none;
  }
  else {
    mpz_sub(mp_b, mp_b, mp_a);
    mpz_clear(mp_a);

    return u2_bn_mp(wir_r, mp_b);
  }
}

/* u2_fj_op_tip():
**
**   Produce the root of (twig) - 2 or 3; o4 bull if (twig) is 1.
*/
u2_weak
u2_fj_op_tip(u2_atom twig)
{
  if ( u2_yes == u2_sing(twig, _1) ) {
    return u2_none;
  }
  else {
    c3_w st = u2_met(0, twig);

    if ( u2_bit((st - 2), twig) == 0 ) {
      return _2;
    } else {
      return _3;
    }
  }
}

/* u2_fj_op_tap():
**
**   Produce (twig) with the root bit removed, or bull if (twig) is 1.
*/
u2_weak
u2_fj_op_tap(u2_ray  wir_r,
             u2_atom twig)
{
  if ( u2_yes == u2_sing(twig, _1) ) {
    return u2_none;
  }
  else {
    u2_atom pro;

    /* Ugly.
    */
    {
      u2_atom a = u2_fj_op_log(wir_r, twig);
      u2_atom b = u2_fj_op_dec(wir_r, a);
      u2_atom c = u2_fj_op_lsh(wir_r, b, _1);
      u2_atom d = u2_fj_op_sub(wir_r, c, twig);
      u2_atom e = u2_fj_op_rsh(wir_r, _1, c);
      
      pro = u2_fj_op_con(wir_r, e, d);
    }
    return pro;
  }
}
