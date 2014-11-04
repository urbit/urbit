/* j/5/jam.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  static u3_noun
  _jam_in(u3_ch_root* har_u, u3_atom, u3_atom, u3_noun);

  static u3_noun
  _jam_in_pair(u3_ch_root* har_u,
               u3_atom h_a,
               u3_atom t_a,
               u3_atom b,
               u3_noun l)
  {
    u3_noun w = u3nc(u3nc(2, 1), u3k(l));
    u3_noun x = u3_cqa_add(2, b);
    u3_noun d = _jam_in(har_u, h_a, x, w);
    u3_noun p_d, q_d, r_d;
    u3_noun r;

    u3_cr_trel(d, &p_d, &q_d, &r_d);
    {
      u3_noun y = u3_cqa_add(x, p_d);
      u3_noun e = _jam_in(har_u, t_a, y, q_d);
      u3_noun p_e, q_e, r_e;

      u3_cr_trel(e, &p_e, &q_e, &r_e);
      {
        u3_noun z = u3_cqa_add(p_d, p_e);

        r = u3nt(u3_cqa_add(2, z), u3k(q_e), 0);

        u3z(z);
      }
      u3z(e);
      u3z(y);
    }
    u3z(d);
    u3z(x);
    u3z(w);

    return r;
  }

  static u3_noun
  _jam_in_flat(u3_ch_root* har_u,
               u3_atom a,
               u3_noun l)
  {
    u3_noun d = u3_cqe_mat(a);
    u3_noun x = u3_cqa_add(1, u3h(d));
    u3_noun y = u3nt
      (u3k(x), u3nc(u3nc(x, u3_cqc_lsh(0, 1, u3t(d))), u3k(l)), 0);

    u3z(d);

    return y;
  }

  static u3_noun
  _jam_in_ptr(u3_ch_root* har_u,
              u3_atom u_c,
              u3_noun l)
  {
    u3_noun d = u3_cqe_mat(u_c);
    u3_atom x = u3_cqc_lsh(0, 2, u3t(d));
    u3_atom y = u3_cqa_add(2, u3h(d));
    u3_noun z = u3nt
      (u3k(y), u3nc(u3nc(y, u3_cqc_mix(3, x)), u3k(l)), 0);

    u3z(d);
    u3z(x);

    return z;
  }

  static u3_noun
  _jam_in(u3_ch_root* har_u,
          u3_noun a,
          u3_atom b,
          u3_noun l)
  {
    u3_noun c = u3_ch_get(har_u, a);
    u3_noun x;

    if ( u3_none == c ) {
        u3_ch_put(har_u, a, u3k(b));

      if ( u3_yes == u3ud(a) ) {
        x = _jam_in_flat(har_u, a, l);
      } else {
        x = _jam_in_pair(har_u, u3h(a), u3t(a), b, l);
      }
    }
    else {
      if ( u3_yes == u3ud(a) && u3_cr_met(0, a) <= u3_cr_met(0, c) ) {
        x = _jam_in_flat(har_u, a, l);
      }
      else {
        x = _jam_in_ptr(har_u, c, l);
      }
    }
    return x;
  }

  u3_noun
  u3_cqe_jam(u3_atom a)
  {
    u3_ch_root* har_u = u3_ch_new();

    u3_noun x = _jam_in(har_u, a, 0, u3_nul);
    u3_noun q = u3_cqb_flop(u3h(u3t(x)));
    u3_noun r = u3_cqc_can(0, q);

    u3z(x);
    u3z(q);
    u3_ch_free(har_u);
    return r;
  }
  u3_noun
  u3_cwe_jam(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3_cr_at(u3_cv_sam, cor))) ) {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqe_jam(a);
    }
  }
  u3_atom
  u3_cke_jam(u3_noun a)
  {
    u3_atom b = u3_cqe_jam(a);

    u3z(a);
    return b;
  }

