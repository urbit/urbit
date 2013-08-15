/* mill/x/link.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_p_link()::
*/
u4_mold
_mill_p_link(u4_milr m, 
             u4_gene del,
             u4_gene zim, 
             u4_mold tip)
{
  u4_mold jes  = _mill_play(m, del, tip);
  u4_mold dux  = _mill_play(m, zim, jes);

  return dux;
}

/* _mill_b_link()::
*/
u4_nock
_mill_b_link(u4_milr m, 
             u4_gene del,
             u4_gene zim, 
             u4_mold tip)
{
  u4_loaf fod = _mill_make(m, del, tip);
  u4_nock pum = _mill_bake(m, zim, u4_ch(fod));

  return _mill_comp(m, u4_ct(fod), pum);
}

/* _mill_m_link()::
*/
u4_loaf
_mill_m_link(u4_milr m, 
             u4_gene del,
             u4_gene zim, 
             u4_mold tip)
{
  u4_lane lane  = m->lane;
  u4_loaf fod = _mill_make(m, del, tip);
  u4_loaf saq = _mill_make(m, zim, u4_ch(fod));

  return u4_k_cell
    (lane, u4_ch(saq),
           _mill_comp(m, u4_ct(fod), u4_ct(saq)));
}
