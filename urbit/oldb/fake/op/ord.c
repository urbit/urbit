/* fake/op/ord.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_op_ord():
**
**   Produce 0 if (a < b), 1 if (a > b), bull if (a == b).
*/
u4_nopt
u4_op_ord(u4_noun a,
          u4_noun b)
{
  /* Efficiency: could be considerably improved.
  */
  if ( u4_n_cell(a) ) {
    if ( u4_n_cell(b) ) {
      u4_noun a_h = u4_ch(a);
      u4_noun b_h = u4_ch(b);

      if ( u4_n_eq(a_h, b_h) ) {
        return u4_op_ord(u4_ct(a), u4_ct(b));
      }
      else {
        return u4_op_ord(a_h, b_h);
      }
    }
    else {
      return u4_noun_1;
    }
  }
  else {
    if ( u4_n_cell(b) ) {
      return u4_noun_0;
    }
    else {
      u4_sw sw_a = u4_a_bin(a, 5);
      u4_sw sw_b = u4_a_bin(b, 5);

      if ( sw_a != sw_b ) {
        return ( (sw_a < sw_b) ? u4_noun_0 : u4_noun_1 );
      }
      else {
        u4_sw sw = sw_a;
        u4_pw i;

        for ( i=0; i < sw; i++ ) {
          u4_pw pw = (sw - (i + 1));
          u4_xw xw_a, xw_b;

          xw_a = u4_a_word(a, pw);
          xw_b = u4_a_word(b, pw);

          if ( xw_a != xw_b ) {
            return ( (xw_a < xw_b) ? u4_noun_0 : u4_noun_1 );
            break;
          }
        }
        return u4_bull;
      }
    }
  }
}
