/* j/4/by_apt.c
**
*/
#include "all.h"

// l and r are keys, p.n.a-s.
static
c3_o _by_apt(u3_noun a, u3_noun l, u3_noun r)
{
  if (a == 0) {
    return c3y;
  }

  u3_noun n_a, l_a, r_a;
  u3_noun p_n_a, q_n_a;
  u3x_trel(a, &n_a, &l_a, &r_a);
  u3x_cell(n_a, &p_n_a, &q_n_a);

  if ( u3_nul != l ) {
    u3_noun u_l;
    u3x_cell(l, 0, &u_l);
    if (c3n == u3qc_gor(p_n_a, u_l)) {
      return c3n;
    }
    if (c3y == u3r_sing(p_n_a, u_l)) {
      return c3n;
    }
  }

  if ( u3_nul != r ) {
    u3_noun u_r;
    u3x_cell(r, 0, &u_r);
    if (c3n == u3qc_gor(u_r, p_n_a)) {
      return c3n;
    }
    if (c3y == u3r_sing(u_r, p_n_a)) {
      return c3n;
    }
  }

  if ( u3_nul != l_a ) {
    u3_noun n_l_a, l_l_a, r_l_a;
    u3_noun p_n_l_a;
    u3x_trel(l_a, &n_l_a, &l_l_a, &r_l_a);
    u3x_cell(n_l_a, &p_n_l_a, 0);

    if (c3n == u3qc_mor(p_n_a, p_n_l_a)) {
      return c3n;
    }
    if (c3y == u3r_sing(p_n_a, p_n_l_a)) {
      return c3n;
    }
    u3_noun nu_l = u3nc(0, p_n_a);
    if (c3n == _by_apt(l_a, nu_l, r)) {
      u3z(nu_l);
      return c3n;
    }
    u3z(nu_l);
  }

  if ( u3_nul != r_a ) {
    u3_noun n_r_a, l_r_a, r_r_a;
    u3_noun p_n_r_a;
    u3x_trel(r_a, &n_r_a, &l_r_a, &r_r_a);
    u3x_cell(n_r_a, &p_n_r_a, 0);

    if (c3n == u3qc_mor(p_n_a, p_n_r_a)) {
      return c3n;
    }
    if (c3y == u3r_sing(p_n_a, p_n_r_a)) {
      return c3n;
    }
    u3_noun nu_r = u3nc(0, p_n_a);
    if (c3n == _by_apt(r_a, l, nu_r)) {
      u3z(nu_r);
      return c3n;
    }
    u3z(nu_r);
  }

  return c3y;
}

// TODO: +apt:in and +apt:by are disabled since it breaks ames in some way?
u3_noun
u3wdb_apt(u3_noun cor)
{
  u3_noun a;
  u3x_mean(cor, u3x_con_sam, &a, 0);

  c3_y pro = _by_apt(a, 0, 0);
  return pro;
}
