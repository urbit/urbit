/* mill/x/dbug.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_p_dbug()::
*/
u4_mold
_mill_p_dbug(u4_milr m, 
             u4_gene lyq,
             u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_mold zat;

  m->rux = u4_op_inc(lane, m->rux);
  zat = _mill_play(m, lyq, tip);
  m->rux = u4_op_dec(lane, m->rux);

  return zat;
}

/* _mill_b_dbug()::
*/
u4_nock
_mill_b_dbug(u4_milr m, 
             u4_gene lyq,
             u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_nock gof;

  m->rux = u4_op_inc(lane, m->rux);
  gof = _mill_bake(m, lyq, tip);
  m->rux = u4_op_dec(lane, m->rux);

  return gof;
}

/* _mill_m_dbug()::
*/
u4_loaf
_mill_m_dbug(u4_milr m, 
             u4_gene lyq,
             u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_loaf fod;

  m->rux = u4_op_inc(lane, m->rux);
  fod = _mill_make(m, lyq, tip);
  m->rux = u4_op_dec(lane, m->rux);

  return fod;
}
