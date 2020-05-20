/* j/4/in_apt.c
**
*/
#include "all.h"

static
c3_o _in_apt(u3_noun a, u3_noun l, u3_noun r)
{
  if (a == 0) {
    return c3y;
  }

  u3_noun n_a, l_a, r_a;
  u3x_trel(a, &n_a, &l_a, &r_a);

  if ( u3_nul != l ) {
    u3_noun u_l;
    u3x_cell(l, 0, &u_l);
    if (c3n == u3qc_gor(n_a, u_l)) {
      return c3n;
    }
  }

  if ( u3_nul != r ) {
    u3_noun u_r;
    u3r_cell(r, 0, &u_r);
    if (c3n == u3qc_gor(u_r, n_a)) {
      return c3n;
    }
  }

  if ( u3_nul != l_a ) {
    u3_noun n_l_a, l_l_a, r_l_a;
    u3x_trel(l_a, &n_l_a, &l_l_a, &r_l_a);
    if (c3n == u3qc_mor(n_a, n_l_a)) {
      return c3n;
    }
    u3_noun nu_l = u3nc(0, n_a);
    if (c3n == _in_apt(l_a, nu_l, r)) {
      u3z(nu_l);
      return c3n;
    }
    u3z(nu_l);
  }

  if ( u3_nul != r_a ) {
    u3_noun n_r_a, l_r_a, r_r_a;
    u3x_trel(r_a, &n_r_a, &l_r_a, &r_r_a);
    if (c3n == u3qc_mor(n_a, n_r_a)) {
      return c3n;
    }
    u3_noun nu_r = u3nc(0, n_a);
    if (c3n == _in_apt(r_a, l, nu_r)) {
      u3z(nu_r);
      return c3n;
    }
    u3z(nu_r);
  }

  return c3y;
}

// TODO: +apt:in and +apt:by are disabled since it breaks ames in some way?
u3_noun
u3wdi_apt(u3_noun cor)
{
  u3_noun a;
  u3x_mean(cor, u3x_con_sam, &a, 0);
  c3_y pro = _in_apt(a, 0, 0);
  return pro;
}
