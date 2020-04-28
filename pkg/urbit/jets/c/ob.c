/* j/3/ob.c
**
*/
#include "all.h"

u3_noun
u3qc_ob_fen(u3_atom r,
            u3_atom a,
            u3_atom b,
            u3j_site* prf_u,
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

    u3_atom f = u3j_gate_slam(prf_u, u3nc(u3qa_sub(j, 1), ell));

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
    pro = u3qc_ob_fen(r, a, b, &sit_u, m);
    u3j_gate_lose(&sit_u);
    return pro;
  }
}


u3_noun
u3qc_ob_eff(u3_atom j, u3_atom r)
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
    return u3qc_ob_eff(j, r);
  }
}

