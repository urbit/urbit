/* j/5/jam.c
**
*/
#include "all.h"


/* functions
*/
  static u3_noun
  _jam_in(u3p(u3h_root) har_p, u3_atom, u3_atom, u3_noun);

  static u3_noun
  _jam_in_pair(u3p(u3h_root) har_p,
               u3_atom       h_a,
               u3_atom       t_a,
               u3_atom       b,
               u3_noun       l)
  {
    u3_noun w = u3nc(u3nc(2, 1), u3k(l));
    u3_noun x = u3qa_add(2, b);
    u3_noun d = _jam_in(har_p, h_a, x, w);
    u3_noun p_d, q_d, r_d;
    u3_noun r;

    u3r_trel(d, &p_d, &q_d, &r_d);
    {
      u3_noun y = u3qa_add(x, p_d);
      u3_noun e = _jam_in(har_p, t_a, y, q_d);
      u3_noun p_e, q_e, r_e;

      u3r_trel(e, &p_e, &q_e, &r_e);
      {
        u3_noun z = u3qa_add(p_d, p_e);

        r = u3nt(u3qa_add(2, z), u3k(q_e), 0);

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
  _jam_in_flat(u3p(u3h_root) har_p,
               u3_atom       a,
               u3_noun       l)
  {
    u3_noun d = u3qe_mat(a);
    u3_noun x = u3qa_add(1, u3h(d));
    u3_noun y = u3nt
      (u3k(x), u3nc(u3nc(x, u3qc_lsh(0, 1, u3t(d))), u3k(l)), 0);

    u3z(d);

    return y;
  }

  static u3_noun
  _jam_in_ptr(u3p(u3h_root) har_p,
              u3_atom       u_c,
              u3_noun       l)
  {
    u3_noun d = u3qe_mat(u_c);
    u3_atom x = u3qc_lsh(0, 2, u3t(d));
    u3_atom y = u3qa_add(2, u3h(d));
    u3_noun z = u3nt
      (u3k(y), u3nc(u3nc(y, u3qc_mix(3, x)), u3k(l)), 0);

    u3z(d);
    u3z(x);

    return z;
  }

  static u3_noun
  _jam_in(u3p(u3h_root) har_p,
          u3_noun       a,
          u3_atom       b,
          u3_noun       l)
  {
    u3_noun c = u3h_get(har_p, a);
    u3_noun x;

    if ( u3_none == c ) {
        u3h_put(har_p, a, u3k(b));

      if ( c3y == u3ud(a) ) {
        x = _jam_in_flat(har_p, a, l);
      } else {
        x = _jam_in_pair(har_p, u3h(a), u3t(a), b, l);
      }
    }
    else {
      if ( c3y == u3ud(a) && u3r_met(0, a) <= u3r_met(0, c) ) {
        x = _jam_in_flat(har_p, a, l);
      }
      else {
        x = _jam_in_ptr(har_p, c, l);
      }
    }
    return x;
  }

  u3_noun
  u3qe_jam(u3_atom a)
  {
    u3p(u3h_root) har_p = u3h_new();

    u3_noun x = _jam_in(har_p, a, 0, u3_nul);
    u3_noun q = u3qb_flop(u3h(u3t(x)));
    u3_noun r = u3qc_can(0, q);

    u3z(x);
    u3z(q);
    u3h_free(har_p);
    return r;
  }
  u3_noun
  u3we_jam(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qe_jam(a);
    }
  }
  u3_atom
  u3ke_jam(u3_noun a)
  {
    u3_atom b = u3qe_jam(a);

    u3z(a);
    return b;
  }

