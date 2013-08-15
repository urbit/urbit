/* j/list.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u2_fj_list_cat(): concatenate list.
*/
u2_list
u2_fj_list_cat(u2_ray  wir_r,
               u2_list lit,
               u2_list lus)
{
  if ( u2_nul == lit ) {
    return lus;
  } else return u2_bc(wir_r, u2_h(lit), u2_fj_list_cat(wir_r, u2_t(lit), lus));
}

/* u2_fj_list_flip(): invert list.
*/
u2_list
u2_fj_list_flip(u2_ray  wir_r,
                u2_list lit)
{
  u2_list lum = _0;

  while ( 1 ) {
    if ( u2_nul == lit ) {
      return lum;
    } else {
      lum = u2_bc(wir_r, u2_h(lit), lum);
      lit = u2_t(lit);
    }
  }
}

/* u2_fj_list_len(): length of list.
*/
c3_w
u2_fj_list_len(u2_ray  wir_r,
               u2_list lit)
{
  c3_w len_w = 0;

  while ( 1 ) {
    if ( u2_nul == lit ) {
      return len_w;
    } else {
      len_w++;
      lit = u2_t(lit);
    }
  }
}
