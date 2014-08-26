/* j/3/mur.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_weak                                                         //  transfer
  j2_mbc(Pt3, mur)(u2_wire wir_r,
                   u2_atom key,                                   //  retain
                   u2_atom syd)                                   //  retain
  {
    c3_w len_w = u2_cr_met(5, key);
    c3_w syd_w = u2_cr_word(0, syd);
    {
      c3_w* key_w = alloca(4 * len_w);
      c3_w  goc_w;

      u2_cr_words(0, len_w, key_w, key);
      goc_w = u2_mur_words(key_w, len_w, syd_w);

      fprintf(stderr, "goc_w %x\r\n", goc_w);
      return u2_ci_words(1, &goc_w);
    }
  }
  u2_weak                                                         //  transfer
  j2_mb(Pt3, mur)(u2_wire wir_r,
                  u2_noun cor)                                    //  retain
  {
    u2_noun a, b;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0)) ||
         (u2_no == u2_stud(a)) ||
         (u2_no == u2_stud(b)) )
    {
      return u2_bl_bail(wir_r, c3__exit);
    } else {
      return j2_mbc(Pt3, mur)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt3, mur)[] = {
    { ".2", c3__lite, j2_mb(Pt3, mur), 
      u2_jet_test | u2_jet_live, u2_none, u2_none },
    { }
  };
