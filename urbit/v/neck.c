/* v/neck.c
**
**  This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <dirent.h>
#include <stdint.h>
#include <ev.h>

#include "all.h"
#include "f/coal.h"
#include "v/vere.h"

/* _neck_root(): tool from boot.
*/
static u2_noun
_neck_root(const c3_c *txt_c, u2_noun ken)
{
  c3_c *ful_c = alloca(7 + strlen(txt_c));

  strcpy(ful_c, "=>  !%  ");
  strcat(ful_c, txt_c);

  return u2_cn_nock(0, u2_cn_nock(u2_ci_string(ful_c), u2k(ken)));
}

/* _neck_ream(): ream with toy.
*/
static u2_noun
_neck_ream(u2_reck* rec_u, u2_noun txt)
{
  return u2_cn_mung(u2k(rec_u->toy.ream), txt);
}

/* _neck_rain(): rain with toy.
*/
static u2_noun
_neck_rain(u2_reck* rec_u, u2_noun bon, u2_noun txt)
{
  return u2_cn_mung(u2k(rec_u->toy.rain), u2nc(bon, txt));
}

/* _neck_slam(): slam with toy.
*/
static u2_noun
_neck_slam(u2_reck* rec_u, u2_noun gat, u2_noun sam)
{
  return u2_cn_mung(u2k(rec_u->toy.slam), u2nc(gat, sam));
}

/* _neck_slap(): slap with toy.
*/
static u2_noun
_neck_slap(u2_reck* rec_u, u2_noun vax, u2_noun gen)
{
  return u2_cn_mung(u2k(rec_u->toy.slap), u2nc(vax, gen));
}

/* _neck_slop(): slop with toy.
*/
static u2_noun
_neck_slop(u2_reck* rec_u, u2_noun hed, u2_noun tal)
{
  return u2_cn_mung(u2k(rec_u->toy.slop), u2nc(hed, tal));
}

/* _neck_gate(): nock gate from vase and text.
*/
static u2_noun
_neck_gate(u2_reck* rec_u, u2_noun vax, const c3_c* txt_c)
{
  u2_noun gen = _neck_ream(rec_u, u2_ci_string(txt_c));
  u2_noun vag = _neck_slap(rec_u, vax, gen);
  u2_noun gat = u2k(u2t(vag));

  u2z(vag); 
  return gat;
}

/* _neck_hard(): function against vase, producing noun.
*/
static u2_noun
_neck_hard(u2_reck* rec_u, u2_noun vax, const c3_c* txt_c, u2_noun sam)
{
  return u2_cn_mung(_neck_gate(rec_u, vax, txt_c), sam);
}

/* _neck_coin_da(): print atom as date.
*/
static u2_noun
_neck_coin_da(u2_reck* rec_u, u2_noun ato)
{
  return _neck_hard
    (rec_u, u2k(rec_u->syd), "|=([a=@] ~(rent co ~ %da a))", ato);
}

/* _neck_soft(): function against vase, producing vase.
*/
static u2_noun
_neck_soft(u2_reck* rec_u, u2_noun vax, const c3_c* txt_c, u2_noun sam)
{
  u2_noun gen = _neck_ream(rec_u, u2_ci_string(txt_c));
  u2_noun gat = _neck_slap(rec_u, vax, gen);

  return _neck_slam(rec_u, gat, sam);
}

/* _neck_spat(): noun path from c, kind of a hack.
*/
static u2_noun 
_neck_spat(c3_c* pax_c)
{
  if ( !*pax_c ) {
    return u2_nul;
  } else {
    c3_c* ash_c = strchr(pax_c, '/');

    if ( !ash_c ) {
      return u2nc(u2_ci_string(pax_c), u2_nul);
    } 
    else {
      u2_noun pan;

      *ash_c = 0;
      pan = u2_ci_string(pax_c);
      *ash_c = '/';
      
      return u2nc(pan, _neck_spat(ash_c + 1));
    }
  }
}

/* _neck_load(): layer file on vase -> vase.
*/
static u2_noun
_neck_load(u2_reck* rec_u, u2_noun vax, c3_c* pax_c)
{
  u2_noun txt = u2_walk_load(pax_c);
  u2_noun gen = _neck_rain
    (rec_u, _neck_spat(pax_c + strlen(u2_Local) + 1), txt);

  return _neck_slap(rec_u, vax, gen);
}

/* _neck_load_temp(): _neck_load() for old fs structure.
*/
static u2_noun
_neck_load_temp(u2_reck* rec_u, u2_noun vax, c3_w kno_w, c3_c* pax_c)
{
  c3_c ful_c[2048];
 
  sprintf(ful_c, "%s/sys/%d/%s", u2_Local, kno_w, pax_c);
  return _neck_load(rec_u, vax, ful_c);
}

/* _neck_time_set(): set the neck time.
*/
static void
_neck_time_set(u2_reck* rec_u)
{
  struct timeval tim_tv;

  gettimeofday(&tim_tv, 0);
  u2z(rec_u->now);
  rec_u->now = u2_time_in_tv(&tim_tv);

  u2z(rec_u->wen);
  rec_u->wen = _neck_coin_da(rec_u, u2k(rec_u->now));
}

