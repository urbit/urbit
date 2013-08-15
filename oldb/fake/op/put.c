/* fake/op/put.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

static u4_nopt
_put(u4_lane lane, 
     u4_twig twig, 
     u4_noun a, 
     u4_noun b, 
     u4_pt pt)
{
  if ( pt == 0 ) {
    return u4_k_safe(lane, a);
  }
  else {
    pt -= 1;
    {
      if ( u4_n_atom(b) ) {
        return u4_bull;
      }
      else {
        u4_noun head = u4_ch(b);
        u4_noun tail = u4_ct(b);

        if ( 0 == u4_a_bit(twig, pt) ) {
          u4_noun put_head = _put(lane, twig, a, head, pt);
          
          if ( u4_bull == put_head ) {
            return u4_bull;
          }
          else return u4_k_cell(lane, put_head, tail);
        }
        else {
          u4_noun put_tail = _put(lane, twig, a, tail, pt);

          if ( u4_bull == put_tail ) {
            return u4_bull;
          }
          else return u4_k_cell(lane, head, put_tail);
        }
      }
    }
  }
}

/* u4_op_put():
**
**   Insert (a) at (twig) in (b).  Return bull if there is no
**   such twig.
*/
u4_nopt
u4_op_put(u4_lane lane,
          u4_twig twig,
          u4_noun a,
          u4_noun b)
{
  u4_st st = u4_a_bin(twig, 0);

  if ( !st ) {
    return u4_trip;
  }
  else {
    u4_pt pt = (st - 1);

    return _put(lane, twig, a, b, pt);
  }
}
