/* j/2/scag.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt2, scag)(
                    u2_atom a,                                    //  retain
                    u2_noun b)                                    //  retain
  {
    if ( u2_ne(u2_co_is_cat(a)) ) {
      return u2_cm_bail(c3__fail);
    }
    else {
      u2_noun acc;
      c3_w i_w = a;

      if ( !i_w )
	return u2_nul;

      while ( i_w ) {
        if ( u2_no == u2du(b) ) {
          return u2_nul;
        }
	acc = u2_ci_cell( u2h(b), acc );
	b = u2t(b);
	i_w--;
      }

      return u2_ckb_flop(acc);
    }
  }
  u2_noun                                                         // transfer
  j2_mb(Pt2, scag)(
                   u2_noun cor)                                   // retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ||
         (u2_no == u2ud(a)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return j2_mbc(Pt2, scag)(a, b);
    }
  }

/* structures
*/
  /* u2_ho_jet */
  /* j2_mbj(Pt2, scag)[] = { */
  /*   { ".2", c3__lite, j2_mb(Pt2, scag), u2_jet_dead, u2_none, u2_none }, */
  /*   { } */
  /* }; */

  u2_ho_jet
  j2_mbj(Pt2, scag)[] = {
    { ".2", c3__lite, j2_mb(Pt2, scag), u2_jet_dead, u2_none, u2_none },
    { }
  };
