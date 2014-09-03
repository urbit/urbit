/* j/6/cell.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  u2_cqf_cell(
                    u2_noun hed,                                  //  retain
                    u2_noun tal)                                  //  retain
  {
    if ( (c3__void == hed) || (c3__void == tal) ) {
      return c3__void;
    } else {
      return u2nt(c3__cell, u2k(hed), u2k(tal));
    }
  }
  u2_noun                                                         //  transfer
  u2_cwf_cell(
                   u2_noun cor)                                   //  retain
  {
    u2_noun hed, tal;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &hed, u2_cv_sam_3, &tal, 0) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqf_cell(hed, tal);
    }
  }
