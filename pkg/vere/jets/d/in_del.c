/* j/4/in_del.c
**
*/
#include "all.h"

/* functions
*/
static u3_noun
_rebalance(u3_noun a)
{
  u3_noun l_a, n_a, r_a;
  u3x_trel(a, &n_a, &l_a, &r_a);

  if ( u3_nul == l_a) {
    return u3k(r_a);
  }
  else if ( u3_nul == r_a) {
    return u3k(l_a);
  }
  else {
    u3_noun n_l_a, l_l_a, r_l_a;
    u3_noun n_r_a, l_r_a, r_r_a;
    u3x_trel(l_a, &n_l_a, &l_l_a, &r_l_a);
    u3x_trel(r_a, &n_r_a, &l_r_a, &r_r_a);

    if ( c3y == u3qc_mor(n_l_a, n_r_a) ) {
      u3_noun new_right = u3nt(u3k(n_a),
                               u3k(r_l_a),
                               u3k(r_a));

      u3_noun ret = u3nt(u3k(n_l_a),
                         u3k(l_l_a),
                         _rebalance(new_right));
      u3z(new_right);

      return ret;
    }
    else {
      u3_noun new_left = u3nt(u3k(n_a),
                              u3k(l_a),
                              u3k(l_r_a));

      u3_noun ret = u3nt(u3k(n_r_a),
                         _rebalance(new_left),
                         u3k(r_r_a));
      u3z(new_left);

      return ret;
    }
  }
}

u3_noun
u3qdi_del(u3_noun a,
          u3_noun b)
{
  if ( u3_nul == a ) {
    return u3_nul;
  }
  else {
    u3_noun l_a, n_a, r_a;
    u3x_trel(a, &n_a, &l_a, &r_a);

    if ( c3y == u3r_sing(n_a, b) ) {
      return _rebalance(a);
    }
    else {
      if ( c3y == u3qc_gor(b, n_a) ) {
        return u3nt(u3k(n_a),
                    u3qdi_del(l_a, b),
                    u3k(r_a));
      }
      else {
        return u3nt(u3k(n_a),
                    u3k(l_a),
                    u3qdi_del(r_a, b));
      }
    }
  }
}

u3_noun
u3wdi_del(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdi_del(a, b);
}
