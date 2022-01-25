/* vere/ward.c
*/
#include "all.h"
#include "vere/vere.h"

//  ward: lifecycle management for common structures
//
//    should contain anything allocated in multiple modules,
//    or allocated in one and freed in another
//

/* u3_dent_init(): initialize file record.
*/
u3_dent*
u3_dent_init(const c3_c* nam_c)
{
  u3_dent *det_u = c3_malloc(sizeof(*det_u));
  det_u->nex_u   = 0;
  det_u->nam_c   = c3_malloc(1 + strlen(nam_c));
  strcpy(det_u->nam_c, nam_c);

  return det_u;
}

/* u3_dent_free(): dispose file record.
*/
void
u3_dent_free(u3_dent *det_u)
{
  c3_free(det_u->nam_c);
  c3_free(det_u);
}

/* u3_dire_init(): initialize directory record.
*/
u3_dire*
u3_dire_init(const c3_c* pax_c)
{
  u3_dire *dir_u = c3_malloc(sizeof *dir_u);
  dir_u->all_u = 0;
  dir_u->pax_c = c3_malloc(1 + strlen(pax_c));
  strcpy(dir_u->pax_c, pax_c);

  return dir_u;
}

/* u3_dire_free(): dispose directory record.
*/
void
u3_dire_free(u3_dire *dir_u)
{
  {
    u3_dent *det_u = dir_u->all_u;
    u3_dent *nex_u;

    while ( det_u ) {
      nex_u = det_u->nex_u;
      u3_dent_free(det_u);
      det_u = nex_u;
    }
  }

  c3_free(dir_u->pax_c);
  c3_free(dir_u);
}

/* u3_pico_init(): initialize a scry request struct
*/
u3_pico*
u3_pico_init()
{
  u3_pico* pic_u = c3_calloc(sizeof(*pic_u));
  return pic_u;
}

/* u3_pico_free(): dispose a scry request struct
*/
void
u3_pico_free(u3_pico* pic_u)
{
  u3z(pic_u->gan);

  switch ( pic_u->typ_e ) {
    default: c3_assert(0);

    case u3_pico_full: {
      u3z(pic_u->ful);
    } break;

    case u3_pico_once: {
      u3z(pic_u->las_u.des);
      u3z(pic_u->las_u.pax);
    } break;
  }

  c3_free(pic_u);
}

/* u3_mcut_char(): measure/cut character.
*/
c3_w
u3_mcut_char(c3_c* buf_c, c3_w len_w, c3_c chr_c)
{
  if ( buf_c ) {
    buf_c[len_w] = chr_c;
  }
  return len_w + 1;
}

/* u3_mcut_cord(): measure/cut cord.
*/
c3_w
u3_mcut_cord(c3_c* buf_c, c3_w len_w, u3_noun san)
{
  c3_w ten_w = u3r_met(3, san);

  if ( buf_c ) {
    u3r_bytes(0, ten_w, (c3_y *)(buf_c + len_w), san);
  }
  u3z(san);
  return (len_w + ten_w);
}

/* u3_mcut_path(): measure/cut cord list.
*/
c3_w
u3_mcut_path(c3_c* buf_c, c3_w len_w, c3_c sep_c, u3_noun pax)
{
  u3_noun axp = pax;

  while ( u3_nul != axp ) {
    u3_noun h_axp = u3h(axp);

    len_w = u3_mcut_cord(buf_c, len_w, u3k(h_axp));
    axp = u3t(axp);

    if ( u3_nul != axp ) {
      len_w = u3_mcut_char(buf_c, len_w, sep_c);
    }
  }
  u3z(pax);
  return len_w;
}

/* u3_mcut_host(): measure/cut host.
*/
c3_w
u3_mcut_host(c3_c* buf_c, c3_w len_w, u3_noun hot)
{
  len_w = u3_mcut_path(buf_c, len_w, '.', u3kb_flop(u3k(hot)));
  u3z(hot);
  return len_w;
}
