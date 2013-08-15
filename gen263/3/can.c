/* j/3/can.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mbc(Pit, can)(u2_wire wir_r, 
                   u2_atom a,                                     //  retain
                   u2_noun b)                                     //  retain
  {
    if ( !u2_fly_is_cat(a) || (a >= 32) ) {
      return u2_none;
    }
    else {
      c3_g   a_g = a;
      c3_w   tot_w = 0;
      u2_ray sal_r;
     
      /* Measure and validate the slab required.
      */
      {
        u2_noun cab = b;

        while ( 1 ) {
          u2_noun i_cab, pi_cab, qi_cab;

          if ( _0 == cab ) {
            break;
          }
          if ( (u2_no == u2_dust(cab)) ||
               (u2_no == u2_dust(i_cab = u2_h(cab))) ||
               !(u2_fly_is_cat(pi_cab = u2_h(i_cab))) ||
               u2_no == u2_stud(qi_cab = u2_t(i_cab)) )
          {
            return u2_bl_bail(wir_r, c3__fail);
          }
          else if ( (tot_w + pi_cab) < tot_w ) {
            return u2_bl_bail(wir_r, c3__fail);
          }
          tot_w += pi_cab;
          cab = u2_t(cab); 
        }
        if ( 0 == tot_w ) {
          return _0;
        }
        if ( 0 == (sal_r = u2_rl_slaq(wir_r, a_g, tot_w)) ) {
          return u2_none;
        }
      }

      /* Chop the list atoms in.
      */
      {
        u2_noun cab = b;
        c3_w    pos_w = 0;

        while ( _0 != cab ) {
          u2_noun i_cab = u2_h(cab);
          u2_atom pi_cab = u2_h(i_cab);
          u2_atom qi_cab = u2_t(i_cab);

          u2_chop(a_g, 0, pi_cab, pos_w, sal_r, qi_cab);
          pos_w += pi_cab;
          cab = u2_t(cab);
        }
      }
      return u2_rl_malt(wir_r, sal_r);
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pit, can)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, 8, &a, 9, &b, 0)) ||
         (u2_no == u2_stud(a)) ) 
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mbc(Pit, can)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, can)[] = {
    { ".3", c3__hevy, j2_mb(Pit, can), Tier3_test, u2_none, u2_none },
    { }
  };
