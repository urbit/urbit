/* vere/ward.c
*/
#include "all.h"
#include "vere/vere.h"

//  ward: lifecycle management for common structures
//
//    should contain anything allocated in multiple modules,
//    or allocated in one and freed in another
//

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
