/* j/4/in_int.c
**
*/
#include "all.h"

u3_noun
u3qdi_int(u3_noun a, u3_noun b)
{
  if (  (u3_nul == a)
     || (u3_nul == b) )
  {
    return u3_nul;
  }
  else {
    u3_noun n_a, l_a, r_a;
    u3_noun n_b, l_b, r_b;

    {
      u3_noun lr_a, lr_b;
      u3x_cell(a, &n_a, &lr_a);
      u3x_cell(b, &n_b, &lr_b);

      if ( c3y == u3qc_mor(n_a, n_b) ) {
        u3_noun c;
        c = a;    a    = b;    b    = c;
        c = n_a;  n_a  = n_b;  n_b  = c;
        c = lr_a; lr_a = lr_b; lr_b = c;
      }

      u3x_cell(lr_a, &l_a, &r_a);
      u3x_cell(lr_b, &l_b, &r_b);
    }

    if ( c3y == u3r_sing(n_b, n_a) ) {
      return u3nt(u3k(n_a),
                  u3qdi_int(l_a, l_b),
                  u3qdi_int(r_a, r_b));
    }
    else if ( c3y == u3qc_gor(n_b, n_a) ) {
      u3_noun new_l_b = u3nt(u3k(n_b), u3k(l_b), u3_nul);
      u3_noun   new_a = u3qdi_int(l_a, new_l_b);

      u3z(new_l_b);
      return u3kdi_uni(new_a, u3qdi_int(a, r_b));
    }
    else {
      u3_noun new_r_b = u3nt(u3k(n_b), u3_nul, u3k(r_b));
      u3_noun   new_a = u3qdi_int(r_a, new_r_b);

      u3z(new_r_b);
      return u3kdi_uni(new_a, u3qdi_int(a, l_b));
    }
  }
}

u3_noun
u3wdi_int(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdi_int(a, b);
}
