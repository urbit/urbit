/* j/4/orm_get.c
**
*/
#include "all.h"

/* functions
*/
static u3_noun
_get_orm(u3_noun cmp,
         u3_noun a,
         u3_noun key)
{
  if ( u3_nul == a ) {
    return u3_nul;
  }
  else {
    u3_noun n_a, l_a, r_a;
    u3_noun pn_a, qn_a;
    u3x_trel(a, &n_a, &l_a, &r_a);
    u3x_cell(n_a, &pn_a, &qn_a);

    if(c3y == u3r_sing(pn_a, key)) {
      return u3nc(u3_nul, u3k(qn_a));
    }
    return (c3y == u3x_good(u3n_slam_on(u3k(cmp), u3nc(u3k(pn_a), u3k(key))))) 
      ?  _get_orm(cmp, l_a, key)
      :  _get_orm(cmp, r_a, key);

  }
}

u3_noun
u3wdb_get_orm(u3_noun cor)
{

  u3_noun cmp, a, key, sam;
  u3x_mean(cor, u3x_sam, &sam, u3x_con_sam, &cmp);
  u3x_cell(sam, &a, &key);
  return _get_orm(cmp, a, key);
}
