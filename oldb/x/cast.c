/* mill/x/cast.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_p_cast()::
*/
u4_mold
_mill_p_cast(u4_milr m, 
             u4_gene fes,
             u4_gene rum, 
             u4_mold tip)
{
  u4_mold maf  = _mill_play(m, fes, tip);

  return maf;
}

/* _mill_b_cast()::
*/
u4_loaf
_mill_b_cast(u4_milr m, 
             u4_gene fes,
             u4_gene rum, 
             u4_mold tip)
{
  // u4_lane lane = m->lane;
  u4_mold maf  = _mill_play(m, fes, tip);
  u4_loaf kib  = _mill_make(m, rum, tip);

#if 0
  if ( !u4_n_zero(m->rux) ) {
    u4_burp(m->lane, "cast: b: kib", _mill_dump(m, u4_ch(kib)));
    u4_burp(m->lane, "cast: b: maf", _mill_dump(m, maf));
  }
#endif

  if ( _mill_nest(m, u4_ch(kib), maf) ) {
    return u4_ct(kib);
  }
  else {
    // u4_burp(lane, "cast: maf", _mill_dump(m, maf));
    // u4_burp(lane, "cast: kib", _mill_dump(m, u4_ch(kib)));
    // return _mill_fail(m, "cast failure - not every kib is a maf");

    return _mill_fail(m, "cast failure");
  }
}

/* _mill_m_cast()::
*/
u4_loaf
_mill_m_cast(u4_milr m, 
             u4_gene fes,
             u4_gene rum, 
             u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_mold maf  = _mill_play(m, fes, tip);
  u4_loaf kib  = _mill_make(m, rum, tip);

#if 0
  if ( !u4_n_zero(m->rux) ) {
    u4_burp(m->lane, "cast: m: kib", _mill_dump(m, u4_ch(kib)));
    u4_burp(m->lane, "cast: m: maf", _mill_dump(m, maf));
  }
#endif

  if ( _mill_nest(m, u4_ch(kib), maf) ) {
    return u4_k_cell(lane, maf, u4_ct(kib));
  }
  else {
    // u4_burp(lane, "cast: maf", _mill_dump(m, maf));
    // u4_burp(lane, "cast: kib", _mill_dump(m, u4_ch(kib)));
    // return _mill_fail(m, "cast failure - not every kib is a maf");

    return _mill_fail(m, "cast failure");
  }
}
