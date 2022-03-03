/* jets/d/by_dif.c
**
*/
#include "noun/all.h"

/* internal functions
*/

/* RETAIN
*/
static u3_noun
_b_dif_join(u3_noun d,
            u3_noun e)
{
  if ( u3_nul == d ) {
    return u3k(e);
  }
  else if ( u3_nul == e ) {
    return u3k(d);
  }
  else {
    u3_noun n_d, lr_d;
    u3_noun n_e, lr_e;
    u3x_cell(d, &n_d, &lr_d);
    u3x_cell(e, &n_e, &lr_e);

    if ( c3y == u3qc_mor(u3h(n_d), u3h(n_e)) ) {
      u3_noun l_d, r_d;
      u3x_cell(lr_d, &l_d, &r_d);

      return u3nt(u3k(n_d),
                  u3k(l_d),
                  _b_dif_join(r_d, e));
    }
    else {
      u3_noun l_e, r_e;
      u3x_cell(lr_e, &l_e, &r_e);

      return u3nt(u3k(n_e),
                  _b_dif_join(d, l_e),
                  u3k(r_e));
    }
  }
}

/* functions
*/
u3_noun
u3wdb_dif(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdb_dif(a, b);
}

u3_noun
u3qdb_dif(u3_noun a,
          u3_noun b)
{
  if ( u3_nul == b ) {
    return u3k(a);
  }
  else {
    u3_noun n_b, l_b, r_b;
    u3_noun c, l_c, r_c;
    u3x_trel(b, &n_b, &l_b, &r_b);

    c = u3qdb_bif(a, n_b);
    u3x_cell(c, &l_c, &r_c);

    u3_noun d = u3qdb_dif(l_c, l_b);
    u3_noun e = u3qdb_dif(r_c, r_b);

    u3z(c);

    u3_noun pro = _b_dif_join(d, e);
    u3z(d);
    u3z(e);

    return pro;
  }
}
