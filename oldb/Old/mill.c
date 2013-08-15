/* mill/mill.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_mill(): 
**
**   Convert (type gene) to (type form).
*/
u4_loaf
u4_mill(u4_lane lane,
        u4_lump nes,
        u4_lump zyl)
{
  struct _u4_mill milr;
  u4_type cav;
  u4_form bos;

  milr.lane = lane;
  milr.fan = u4_noun_0;
  milr.rux = u4_noun_0;

  milr.zud = u4_noun_0;
  milr.nix = u4_noun_0;
  milr.meb = u4_noun_0;

  milr.duf = u4_noun_0;
  milr.dam = u4_noun_0;
  milr.niq = u4_noun_0;
  milr.vus = u4_noun_0;
  milr.rof = u4_noun_0;
  milr.tyx = u4_noun_0;

  nes = _mill_type(&milr, nes);
  zyl = _mill_gene(&milr, zyl);

  if ( !_mill_safe(&milr, nes, zyl) ) {
    printf("mill: unsafe\n");
    return u4_trip;
  }
  // else printf("-\n");

  // printf("mill: make...\n");
  bos = _mill_make(&milr, nes, zyl);

  // printf("mill: play...\n");
  cav = _mill_play(&milr, nes, zyl);

  // u4_burp(lane, "mill: type", _mill_dump(&milr, cav));

  return u4_k_cell(lane, cav, bos);
}
