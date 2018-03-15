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
    u3z(l);

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
    u3z(l);

    return z;
  }

  #define JAM_NONE 0
  #define JAM_HEAD 1
  #define JAM_TAIL 2

  typedef struct {
    c3_y    sat_y;
    u3_noun nun;
    u3_noun len;
    u3_noun lis;
    u3_noun hed;
  } jamframe;

  static inline jamframe*
  _jam_push(c3_ys mov, c3_ys off)
  {
    u3R->cap_p += mov;
    return u3to(jamframe, u3R->cap_p + off);
  }

  static inline jamframe*
  _jam_pop(c3_ys mov, c3_ys off)
  {
    u3R->cap_p -= mov;
    return u3to(jamframe, u3R->cap_p + off);
  }

  static u3_noun
  _jam_cap(u3_atom a)
  {
    u3p(jamframe) empty = u3R->cap_p;
    u3p(u3h_root) har_p = u3h_new();
    c3_o          nor_o = u3a_is_north(u3R);
    c3_y          wis_y = c3_wiseof(jamframe);
    c3_ys         mov   = ( c3y == nor_o ? -wis_y : wis_y );
    c3_ys         off   = ( c3y == nor_o ? 0 : -wis_y );
    jamframe*     fam   = _jam_push(mov, off);
    jamframe*     don   = u3to(jamframe, empty + off);

    fam->sat_y = JAM_NONE;
    fam->nun   = a;
    fam->len   = 0;
    fam->lis   = u3_nul;

    u3_noun q, r = u3_none;

    while ( don != fam ) {
      switch ( fam->sat_y ) {
        case JAM_NONE: {
          u3_noun nun = fam->nun;
          u3_noun len = fam->len;
          u3_noun lis = fam->lis;
          u3_weak got = u3h_get(har_p, nun);

          if ( u3_none == got ) {
            u3h_put(har_p, nun, u3k(len));
            if ( c3n == u3du(nun) ) {
              r   = _jam_flat(nun, lis);
              fam = _jam_pop(mov, off);
              u3z(len);
            }
            else {
              fam->sat_y  = JAM_HEAD;
              fam         = _jam_push(mov, off);
              fam->sat_y  = JAM_NONE;
              fam->nun    = u3h(nun);
              fam->len    = u3qa_add(2, len);
              fam->lis    = u3nc(u3nc(2, 1), lis);
            }
          }
          else {
            if ( c3y == u3ud(nun) && (u3r_met(0, nun) <= u3r_met(0, got)) ) {
              r = _jam_flat(nun, lis);
            }
            else {
              r = _jam_ptr(got, lis);
            }
            fam = _jam_pop(mov, off);
            u3z(len);
          }
          break;
        }
        case JAM_HEAD: {
          u3_noun p_r, q_r, r_r;
          u3x_trel(r, &p_r, &q_r, &r_r);
          u3_noun nun = fam->nun;
          fam->sat_y  = JAM_TAIL;
          fam->hed    = r;
          u3_noun z   = u3qa_add(2, fam->len);
          fam         = _jam_push(mov, off);
          fam->sat_y  = JAM_NONE;
          fam->nun    = u3t(nun);
          fam->len    = u3qa_add(z, p_r);
          fam->lis    = u3k(q_r);
          u3z(z);
          break;
        }
        case JAM_TAIL: {
          u3_noun len = fam->len;
          r   = _jam_pair(u3qa_add(2, len), fam->hed, r);
          fam = _jam_pop(mov, off);
          u3z(len);
          break;
        }
        default:
          c3_assert(0);
          return u3_none;
      }
    }

    q = u3qb_flop(u3h(u3t(r)));
    u3z(r);
    r = u3qc_can(0, q);
    u3z(q);
    u3h_free(har_p);
    return r;
  }

  u3_noun
  u3qe_jam(u3_atom a)
  {
    return _jam_cap(a);
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

