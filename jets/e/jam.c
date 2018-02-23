/* j/5/jam.c
**
*/
#include "all.h"

/* functions
*/

  static u3_noun
  _jam_pair(u3_noun x, u3_noun d, u3_noun e)
  {
    u3_noun r, p_d, q_d, r_d;
    u3x_trel(d, &p_d, &q_d, &r_d);
    {
      u3_noun y = u3qa_add(x, p_d);
      u3_noun p_e, q_e, r_e;

      u3x_trel(e, &p_e, &q_e, &r_e);
      {
        u3_noun z = u3qa_add(p_d, p_e);

        r = u3nt(u3qa_add(2, z), u3k(q_e), 0);

        u3z(z);
      }
      u3z(y);
    }
    u3z(x);
    u3z(d);
    u3z(e);
    return r;
  }

  static u3_noun
  _jam_flat(u3_atom a, u3_noun l)
  {
    u3_noun d = u3qe_mat(a);
    u3_noun x = u3qa_add(1, u3h(d));
    u3_noun y = u3nt
      (u3k(x), u3nc(u3nc(x, u3qc_lsh(0, 1, u3t(d))), u3k(l)), 0);

    u3z(d);

    return y;
  }

  static u3_noun
  _jam_ptr(u3_atom u_c, u3_noun l)
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

  typedef struct {
    u3_noun  a;
    u3_noun  b;
    u3_noun  l;
    u3_noun* r;
    u3_noun hed;
    u3_noun tel;
  } jamframe;

  static inline void
  _jam_push(u3_noun a, u3_noun b, u3_noun l, u3_noun *r)
  {
    jamframe* fam = u3a_push(sizeof(jamframe));
    fam->a   = a;
    fam->b   = b;
    fam->l   = l;
    fam->r   = r;
    fam->hed = u3_none;
    fam->tel = u3_none;
  }

  static inline void
  _jam_pop()
  {
    u3a_pop(sizeof(jamframe));
  }

  static inline jamframe*
  _jam_peek()
  {
    return (jamframe*) u3a_peek(sizeof(jamframe));
  }

  u3_noun
  u3qe_jam(u3_atom a)
  {
    u3p(u3h_root) har_p = u3h_new();
    u3p(jamframe) empty = u3R->cap_p;
    jamframe* fam;
    u3_noun out, c, x, q, r;

    _jam_push(a, 0, u3_nul, &out);
    while ( empty != u3R->cap_p ) {
      fam  = _jam_peek();
      if ( u3_none != fam->tel ) {
        u3_noun z = u3qa_add(2, fam->b);
        x = _jam_pair(z, fam->hed, fam->tel);
      }
      else if ( u3_none != fam->hed ) {
        u3_noun p_d, q_d, r_d;
        u3x_trel(fam->hed, &p_d, &q_d, &r_d);
        {
          u3_noun z = u3qa_add(2, fam->b);
          u3_noun y = u3qa_add(z, p_d);
          _jam_push(u3t(fam->a), y, q_d, &(fam->tel));
          u3z(z);
          continue;
        }
      }
      else {
        a = fam->a;
        c = u3h_get(har_p, a);
        if ( u3_none != c ) {
          if ( (c3y == u3ud(a)) && u3r_met(0, a) <= u3r_met(0, c) ) {
            x = _jam_flat(a, fam->l);
          }
          else {
            x = _jam_ptr(c, fam->l);
          }
        }
        else {
          u3h_put(har_p, a, u3k(fam->b));
          if ( c3y == u3ud(a) ) {
            x = _jam_flat(a, fam->l);
          }
          else {
            u3_noun z = u3qa_add(2, fam->b);
            u3_noun w = u3nc(u3nc(2, 1), u3k(fam->l));
            _jam_push(u3h(a), z, w, &(fam->hed));
            continue;
          }
        }
      }
      *(fam->r) = x;
      u3z(fam->b);
      _jam_pop();
    }

    q = u3qb_flop(u3h(u3t(out)));
    r = u3qc_can(0, q);
    u3z(out);
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

