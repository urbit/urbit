/* mill/x/quiz.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_p_quiz()::
*/
u4_mold
_mill_p_quiz(u4_milr m, 
             u4_gene tes,
             u4_gene bif, 
             u4_gene hob, 
             u4_mold tip)
{
  u4_loaf ruf  = _mill_test(m, tes, tip);
  u4_nock fal  = u4_ct(ruf);

  if ( u4_n_eq(u4_noun_1, u4_ch(fal)) ) {
    if ( u4_n_eq(u4_noun_0, u4_ct(fal)) ) {
      return _mill_play(m, bif, u4_ch(ruf));
    } else {
      return _mill_play(m, hob, tip);
    }
  }
  else {
    u4_mold tuv = _mill_play(m, bif, u4_ch(ruf));
    u4_mold miz = _mill_play(m, hob, tip);

    return _mill_eith(m, tuv, miz);
  }
}

/* _mill_b_quiz()::
*/
u4_nock
_mill_b_quiz(u4_milr m, 
             u4_gene tes,
             u4_gene bif, 
             u4_gene hob, 
             u4_mold tip)
{
  u4_loaf ruf  = _mill_test(m, tes, tip);
  u4_nock fal  = u4_ct(ruf);

  if ( u4_n_eq(u4_noun_1, u4_ch(fal)) ) {
    if ( u4_n_eq(u4_noun_0, u4_ct(fal)) ) {
      return _mill_bake(m, bif, u4_ch(ruf));
    } else {
      return _mill_bake(m, hob, tip);
    }
  }
  else {
    u4_nock piz = _mill_bake(m, bif, u4_ch(ruf));
    u4_nock duf = _mill_bake(m, hob, tip);

    return u4_k_qual(m->lane, u4_noun_2, fal, piz, duf);
  }
}

/* _mill_m_quiz()::
*/
u4_loaf
_mill_m_quiz(u4_milr m, 
             u4_gene tes,
             u4_gene bif, 
             u4_gene hob, 
             u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_loaf ruf  = _mill_test(m, tes, tip);
  u4_nock fal  = u4_ct(ruf);

  if ( u4_n_eq(u4_noun_1, u4_ch(fal)) ) {
    if ( u4_n_eq(u4_noun_0, u4_ct(fal)) ) {
      return _mill_make(m, bif, u4_ch(ruf));
    } else {
      return _mill_make(m, hob, tip);
    }
  }
  else {
    u4_loaf hig  = _mill_make(m, bif, u4_ch(ruf));
    u4_loaf goz  = _mill_make(m, hob, tip);

    return u4_k_cell
      (lane, 
       _mill_eith(m, u4_ch(hig), u4_ch(goz)),
       u4_k_qual(m->lane, u4_noun_2, fal, u4_ct(hig), u4_ct(goz)));
  }
}
