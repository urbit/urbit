/* j/3/ob.c
**
*/
#include "all.h"

// Functions written in reverse order from the core because earlier functions
// depend on later functions.

// We have to both suport passing u3qc_ob_eff around and passing a prepared
// gate which usually, but not necessarily points to it. So we have a
// trampoline which slams a gate, but have the same signature as the normal eff
// hash function.
u3_noun
_do_slam_gate(u3_atom j, u3_atom r, void* datum)
{
  return u3j_gate_slam((u3j_site*)datum, u3nc(j, r));
}

u3_noun
u3qc_ob_eff(u3_atom j, u3_atom r, void* unused)
{
  c3_d found = 0;
  switch (j) {
    case 0:
      found = 0xb76d5eed;
      break;
    case 1:
      found = 0xee281300;
      break;
    case 2:
      found = 0x85bcae01;
      break;
    case 3:
      found = 0x4b387af7;
      break;
    default:
      // the internal snag would have failed.
      return u3m_bail(c3__exit);
  }

  return u3qc_muk(u3i_chubs(1, &found), 2, r);
}

u3_noun
u3wc_ob_eff(u3_noun cor)
{
  u3_noun j, r;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &j,
                             u3x_sam_3, &r, 0)) ||
       (c3n == u3ud(j)) ||
       (c3n == u3ud(r)) )
  {
    fprintf(stderr, "u3wc_ob_eff failed to unpack\r\n");
    return u3m_bail(c3__exit);
  }
  else {
    return u3qc_ob_eff(j, r, 0);
  }
}

u3_noun
u3qc_ob_fen(u3_atom r,
            u3_atom a,
            u3_atom b,
            u3_atom(*prf_fun)(u3_atom j, u3_atom r, void* datum_u),
            void* datum_u,
            u3_atom m)
{
  u3_atom j = r;

  u3_atom ahh;
  u3_atom ale;
  if (c3y == u3r_sing(u3qa_mod(r, 2), 0)) {
    ahh = u3qa_mod(m, a);
    ale = u3qa_div(m, a);
  } else {
    ahh = u3qa_div(m, a);
    ale = u3qa_mod(m, a);
  }

  u3_atom ell;
  u3_atom arr;
  if (c3y == u3r_sing(ale, a)) {
    ell = ahh;
    arr = ale;
  } else {
    ell = ale;
    arr = ahh;
  }

  while (1) {
    if (c3y == u3qa_lth(j, 1)) {
      return u3qa_add(u3qa_mul(arr, a), ell);
    }

    u3_atom f = (*prf_fun)(u3qa_sub(j, 1), ell, datum_u);

    u3_atom tmp;
    if (c3y == u3r_sing(u3qa_mod(j, 2), 0)) {
      tmp = u3qa_mod(u3qa_sub(u3qa_add(arr, b), u3qa_mod(f, b)), b);
    } else {
      tmp = u3qa_mod(u3qa_sub(u3qa_add(arr, a), u3qa_mod(f, a)), a);
    }

    j = u3qa_sub(j, 1);
    arr = ell;
    ell = tmp;
  }
}

u3_noun
u3wc_ob_fen(u3_noun cor)
{
  u3_noun r, a, b, prf, m;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &r,
                             u3x_sam_6, &a,
                             u3x_sam_14, &b,
                             u3x_sam_30, &prf,
                             u3x_sam_31, &m, 0)) ||
       (c3n == u3ud(r)) ||
       (c3n == u3ud(a)) ||
       (c3n == u3ud(b)) ||
       (c3n == u3ud(m)) )
  {
    fprintf(stderr, "u3wc_ob_fen failed to unpack\r\n");
    return u3m_bail(c3__exit);
  }
  else {
    u3_noun pro;
    u3j_site sit_u;

    u3j_gate_prep(&sit_u, u3k(prf));
    pro = u3qc_ob_fen(r, a, b, _do_slam_gate, &sit_u, m);
    u3j_gate_lose(&sit_u);
    return pro;
  }
}


u3_noun
u3qc_ob_fe(u3_atom r,
            u3_atom a,
            u3_atom b,
            u3_atom(*prf_fun)(u3_atom j, u3_atom r, void* datum_u),
            void* datum_u,
            u3_atom m)
{
  u3_atom j = 1;
  u3_atom ell = u3qa_mod(m, a);
  u3_atom arr = u3qa_div(m, a);

  while (1) {
    if (c3y == u3qa_gth(j, r)) {
      if (c3n == u3r_sing(u3qa_mod(r, 2), 0)) {
        return u3qa_add(u3qa_mul(arr, a), ell);
      }

      if (c3y == u3r_sing(arr, a)) {
        return u3qa_add(u3qa_mul(arr, a), ell);
      }

      return u3qa_add(u3qa_mul(ell, a), arr);
    }

    //    u3_atom f = u3j_gate_slam(prf_u, u3nc(u3qa_sub(j, 1), arr));
    u3_atom f = (*prf_fun)(u3qa_sub(j, 1), arr, datum_u);

    u3_atom tmp;
    if (c3n == u3r_sing(u3qa_mod(j, 2), 0)) {
      tmp = u3qa_mod(u3qa_add(f, ell), a);
    } else {
      tmp = u3qa_mod(u3qa_add(f, ell), b);
    }

    j = u3qa_add(j, 1);
    ell = arr;
    arr = tmp;
  }
}

u3_noun
u3wc_ob_fe(u3_noun cor)
{
  u3_noun r, a, b, prf, m;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &r,
                             u3x_sam_6, &a,
                             u3x_sam_14, &b,
                             u3x_sam_30, &prf,
                             u3x_sam_31, &m, 0)) ||
       (c3n == u3ud(r)) ||
       (c3n == u3ud(a)) ||
       (c3n == u3ud(b)) ||
       (c3n == u3ud(m)) )
  {
    fprintf(stderr, "u3wc_ob_fe failed to unpack\r\n");
    return u3m_bail(c3__exit);
  }
  else {
    u3_noun pro;
    u3j_site sit_u;

    u3j_gate_prep(&sit_u, u3k(prf));
    pro = u3qc_ob_fe(r, a, b, _do_slam_gate, &sit_u, m);
    u3j_gate_lose(&sit_u);
    return pro;
  }
}


u3_noun
u3qc_ob_feen(u3_atom r,
             u3_atom a,
             u3_atom b,
             u3_atom k,
             u3_atom(*prf_fun)(u3_atom j, u3_atom r, void* datum_u),
             void* datum_u,
             u3_atom m)
{
  u3_atom c = u3qc_ob_fen(r, a, b, prf_fun, datum_u, m);
  if (c3y == u3qa_lth(c, k)) {
    return c;
  }

  return u3qc_ob_fen(r, a, b, prf_fun, datum_u, c);
}

u3_noun
u3wc_ob_feen(u3_noun cor)
{
  u3_noun r, a, b, k, prf, m;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &r,
                             u3x_sam_6, &a,
                             u3x_sam_14, &b,
                             u3x_sam_30, &k,
                             u3x_sam_62, &prf,
                             u3x_sam_63, &m, 0)) ||
       (c3n == u3ud(r)) ||
       (c3n == u3ud(a)) ||
       (c3n == u3ud(b)) ||
       (c3n == u3ud(k)) ||
       (c3n == u3ud(m)) )
  {
    fprintf(stderr, "u3wc_ob_fe failed to unpack\r\n");
    return u3m_bail(c3__exit);
  }
  else {
    u3_noun pro;
    u3j_site sit_u;

    u3j_gate_prep(&sit_u, u3k(prf));
    pro = u3qc_ob_feen(r, a, b, k, _do_slam_gate, &sit_u, m);
    u3j_gate_lose(&sit_u);
    return pro;
  }
}


u3_noun
u3qc_ob_fee(u3_atom r,
             u3_atom a,
             u3_atom b,
             u3_atom k,
             u3_atom(*prf_fun)(u3_atom j, u3_atom r, void* datum_u),
             void* datum_u,
             u3_atom m)
{
  u3_atom c = u3qc_ob_fe(r, a, b, prf_fun, datum_u, m);
  if (c3y == u3qa_lth(c, k)) {
    return c;
  }

  return u3qc_ob_fe(r, a, b, prf_fun, datum_u, c);
}

u3_noun
u3wc_ob_fee(u3_noun cor)
{
  u3_noun r, a, b, k, prf, m;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &r,
                             u3x_sam_6, &a,
                             u3x_sam_14, &b,
                             u3x_sam_30, &k,
                             u3x_sam_62, &prf,
                             u3x_sam_63, &m, 0)) ||
       (c3n == u3ud(r)) ||
       (c3n == u3ud(a)) ||
       (c3n == u3ud(b)) ||
       (c3n == u3ud(k)) ||
       (c3n == u3ud(m)) )
  {
    return u3m_bail(c3__exit);
  }
  else {
    u3_noun pro;
    u3j_site sit_u;

    u3j_gate_prep(&sit_u, u3k(prf));
    pro = u3qc_ob_fee(r, a, b, k, _do_slam_gate, &sit_u, m);
    u3j_gate_lose(&sit_u);
    return pro;
  }
}

u3_noun
u3qc_ob_tail(u3_atom m)
{
  return u3qc_ob_feen(4, 0xffff, 0x10000, u3qa_mul(0xffff, 0x10000),
                      u3qc_ob_eff, (void*)0, m);
}

u3_noun
u3wc_ob_tail(u3_noun cor)
{
  u3_noun m;

  if ( (c3n == u3r_mean(cor, u3x_sam, &m, 0)) ||
       (c3n == u3ud(m)) )
  {
    return u3m_bail(c3__exit);
  }
  else {
    return u3qc_ob_tail(m);
  }
}

u3_noun
u3qc_ob_feis(u3_atom m)
{
  return u3qc_ob_fee(4, 0xffff, 0x10000, u3qa_mul(0xffff, 0x10000),
                      u3qc_ob_eff, (void*)0, m);
}

u3_noun
u3wc_ob_feis(u3_noun cor)
{
  u3_noun m;

  if ( (c3n == u3r_mean(cor, u3x_sam, &m, 0)) ||
       (c3n == u3ud(m)) )
  {
    return u3m_bail(c3__exit);
  }
  else {
    return u3qc_ob_feis(m);
  }
}
