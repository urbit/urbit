/* j/3/hor.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pit, hor)(u2_wire wir_r, 
                   u2_noun a,                                     //  retain
                   u2_noun b)                                     //  retain
  {
    if ( u2_yes == u2_stud(a) ) {
      if ( u2_yes == u2_stud(b) ) {
        return j2_mbc(Pit, gor)(wir_r, a, b);
      } else {
        return u2_yes;
      }
    } else {
      if ( u2_yes == u2_stud(b) ) {
        return u2_no;
      }
      else {
        u2_noun h_a = u2_h(a);
        u2_noun h_b = u2_h(b);

        if ( u2_yes == u2_sing(h_a, h_b) ) {
          return j2_mbc(Pit, gor)(wir_r, u2_t(a), u2_t(b));
        } else {
          return j2_mbc(Pit, gor)(wir_r, h_a, h_b);
        }
      }
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pit, hor)(u2_wire wir_r, 
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, 8, &a, 9, &b, 0)) ) {
      return u2_none;
    } else {
      return j2_mbc(Pit, hor)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pit, hor)[] = {
    { ".3", c3__lite, j2_mb(Pit, hor), Tier3, u2_none, u2_none },
    { }
  };
