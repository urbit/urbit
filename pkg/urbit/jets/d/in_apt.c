/* j/4/in_apt.c
**
*/
#include "all.h"

static
c3_y _in_apt(u3_noun a, u3_noun l, u3_noun r)
{
  if (a == 0) {
    return c3y;
  }

  u3_noun n_a, l_a, r_a;
  u3x_trel(a, &n_a, &l_a, &r_a);

  u3_noun u_l;
  if (c3y == u3r_cell(l, 0, &u_l)) {
    if (!_(u3qc_gor(n_a, u_l))) {
      return c3n;
    }
  }

  u3_noun u_r;
  if (c3y == u3r_cell(r, 0, &u_r)) {
    if (!_(u3qc_gor(u_r, n_a))) {
      return c3n;
    }
  }

  u3_noun n_l_a, l_l_a, r_l_a;
  if (c3y == u3r_trel(l_a, &n_l_a, &l_l_a, &r_l_a)) {
    if (!_(u3qc_mor(n_a, n_l_a))) {
      return c3n;
    }
    u3_noun nu_l = u3nc(0, n_a);
    if (!_(_in_apt(l_a, nu_l, 0))) {
      u3z(nu_l);
      return c3n;
    }
    u3z(nu_l);
  }

  u3_noun n_r_a, l_r_a, r_r_a;
  if (c3y == u3r_trel(r_a, &n_r_a, &l_r_a, &r_r_a)) {
    if (!_(u3qc_mor(n_a, n_r_a))) {
      return c3n;
    }
    u3_noun nu_r = u3nc(0, n_a);
    if (!_(_in_apt(r_a, 0, nu_r))) {
      u3z(nu_r);
      return c3n;
    }
    u3z(nu_r);
  }

  return c3y;
}

u3_noun
u3wdi_apt(u3_noun cor)
{
  u3_noun a;
  u3x_mean(cor, u3x_con_sam, &a, 0);
  c3_y pro = _in_apt(a, 0, 0);
  return pro;
}
