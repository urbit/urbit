/* mill/x/sure.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_p_sure()::
*/
u4_mold
_mill_p_sure(u4_milr m, 
             u4_gene fes,
             u4_gene gav, 
             u4_gene vep, 
             u4_mold tip)
{
  return _mill_play(m, vep, tip);
}

/* _mill_b_sure()::
*/
u4_nock
_mill_b_sure(u4_milr m, 
             u4_gene fes,
             u4_gene gav, 
             u4_gene vep, 
             u4_mold tip)
{
  u4_mold nud = _mill_play(m, fes, tip);
  u4_mold gor = _mill_play(m, gav, tip);

#if 0
  if ( !u4_n_zero(m->rux) ) {
    u4_burp(m->lane, "sure: b: gor", _mill_dump(m, gor));
    u4_burp(m->lane, "sure: b: nud", _mill_dump(m, nud));
  }
#endif

  if ( !_mill_nest(m, gor, nud) ) {
    return _mill_fail(m, "mold mismatch");
  }
  else {
    return _mill_bake(m, vep, tip);
  }
}

/* _mill_m_sure()::
*/
u4_loaf
_mill_m_sure(u4_milr m, 
             u4_gene fes,
             u4_gene gav, 
             u4_gene vep, 
             u4_mold tip)
{
  u4_mold nud = _mill_play(m, fes, tip);
  u4_mold gor = _mill_play(m, gav, tip);

#if 0
  if ( !u4_n_zero(m->rux) ) {
    u4_burp(m->lane, "sure: m: gor", _mill_dump(m, gor));
    u4_burp(m->lane, "sure: m: nud", _mill_dump(m, nud));
  }
#endif

  if ( !_mill_nest(m, gor, nud) ) {
    return _mill_fail(m, "mold mismatch");
  }
  else {
    return _mill_make(m, vep, tip);
  }
}
