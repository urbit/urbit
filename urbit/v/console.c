/* v/console.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <stdint.h>
#include <ev.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "v/vere.h"

#define NEW9

/* u2_ve_dump_columns(): return screen column width from OS.
*/
c3_l
u2_ve_dump_columns(void)
{
  c3_s buf_s[4];
  
  ioctl(0, TIOCGWINSZ, &buf_s);

  return buf_s[1] - 1;
}

/* u2_ve_dump_tape(): print a tape of text.
*/
void
u2_ve_dump_tape(u2_noun tep)
{
  u2_noun tap = tep;

  while ( u2_nul != tap ) {
    c3_c car_c;

    if ( u2h(tap) >= 127 ) {
      car_c = '?';
    } else car_c = u2h(tap);

    putchar(car_c);
    tap = u2t(tap);
  }
  u2z(tep);
}

/* u2_ve_dump_wall(): print a wall of text.
*/
void
u2_ve_dump_wall(u2_noun wol)
{
  u2_noun wal = wol;

  u2_term_io_hija();
  while ( u2_nul != wal ) {
    u2_ve_dump_tape(u2_ct(u2h(wal)));
    putchar(13);
    putchar(10);

    wal = u2t(wal);
  }
  u2_term_io_loja(0);
  u2z(wol);
}

/* u2_ve_tank(): print a tank at `tab`.
*/
void
u2_ve_tank(c3_l tab_l, u2_noun tac)
{
  c3_l    col_l = u2_ve_dump_columns();
  u2_noun wol;

  {
    wol = u2_dc("wash", u2nc(tab_l, col_l), u2k(tac));

    u2_ve_dump_wall(wol);
  }
}

/* u2_ve_geto(): ghetto-print failed trace stack.  Will not fail!
*/
void
u2_ve_geto(u2_noun hoe)
{
#if 1
  u2z(hoe);
#else
  u2_noun doe = hoe;

  while ( 1 ) {
    if ( u2_no == u2du(doe) ) {
      u2z(hoe); return;
    } else {
      u2_noun i_doe = u2h(doe);
      u2_noun t_doe = u2t(doe);

      if ( u2_yes == u2du(i_doe) ) {
        u2_err(u2_Wire, "h", u2h(i_doe));

        if ( c3__mean != u2h(i_doe) ) {
          u2_err(u2_Wire, "t", u2t(i_doe));
        }
      } 
      doe = t_doe;
    }
  }
#endif
}

/* u2_ve_sway(): print trace stack.
*/
void
u2_ve_sway(c3_l tab_l, u2_noun tax)
{
  u2_noun rax = tax;

  while ( u2_yes == u2_cr_du(rax) ) {
    u2_noun h_rax = u2h(rax);
    u2_noun t_rax = u2t(rax);
    u2_noun hoe;

    if ( 0 != (hoe = u2_cm_trap()) ) {
      c3_c *hho_c = u2_cr_string(u2h(hoe));
      fprintf(stderr, "  !!-%s-!!\n", hho_c);

      free(hho_c);
      u2_ve_geto(u2k(u2t(hoe)));
      u2z(hoe);
      // c3_assert(0);
    } 
    else {
      c3_l    col_l = u2_ve_dump_columns();
      u2_noun tac;
      u2_noun wol;
#if 0
      u2_err(u2_Wire, "hh_rax", u2h(h_rax));
      if ( c3__mean != u2h(h_rax) ) {
        u2_err(u2_Wire, "th_rax", u2t(h_rax));
      }
#endif
      if ( u2_Host.kno_w > 209 ) {
        tac = u2_ve_hard("pitt", "swan", u2_ct(h_rax));
        wol = u2_ve_hard("pitt", "wash", u2nc(u2nc(tab_l, col_l), tac));
      }
      else {
        if ( u2_Host.kno_w > 203 ) {
          tac = u2_ve_hard("born", "swan", u2_ct(h_rax));
          wol = u2_ve_hard("born", "wash", u2nc(u2nc(tab_l, col_l), tac));
        }
        else {
          tac = u2_ve_hard("vane", "swan", u2_ct(h_rax));
          wol = u2_ve_hard("vane", "wash", u2nc(u2nc(tab_l, col_l), tac));
        }
      }
      u2_ve_dump_wall(wol);
      u2_cm_done();
    }
    rax = t_rax;
  }
  u2z(tax);
}

/* u2_ve_wine(): analyze and express error result.
*/
void
u2_ve_wine(u2_noun how)
{
  if ( u2_no == u2_cr_du(how) ) {
    switch ( how ) {
      case c3__exit: fprintf(stderr, "<<exit>>:\n"); break;
      case c3__fail: fprintf(stderr, "<<fail>>:\n"); break;
      case c3__intr: fprintf(stderr, "<<intr>>:\n"); break;
      default:       fprintf(stderr, "<<--->>\n"); c3_assert(0);
    }
  } else {
    u2_ve_wine(u2k(u2h(how)));  // ??
  }
  u2z(how);
}

/* u2_ve_zuse_load(): fuel to vase.
*/
u2_noun
u2_ve_zuse_load(u2_noun ful)
{
  u2_noun p_ful, q_ful;
  c3_c*   ext_c;
  u2_noun dat;

  u2_cx_cell(ful, &p_ful, &q_ful);

  switch ( p_ful ) {
    default: return u2_cm_bail(c3__fail);
    case c3__atom: ext_c = "atom"; break;
    case c3__pill: ext_c = "pill"; break;
  }

  /* Load raw data.
  */
  {
    u2_noun hat = u2_ve_slap(u2_ve_seed(), u2k(q_ful));
    u2_noun tah = u2k(u2t(hat));

    u2z(hat);
    u2_cm_push(u2nc(c3__pray, u2k(tah)));
    {
      dat = u2_ve_file(ext_c, tah);

      if ( u2_none == dat ) {
        return u2_cm_bail(c3__fail);
      }
    }
    u2_cm_drop();
  }

  /* Assemble as a vase.
  */
  {
    if ( c3__atom == p_ful ) {
      return u2nc(u2nc(c3__atom, u2_blip), dat);
    }
    else {
      return u2nc(c3__noun, u2_cke_cue(dat));
    }
  }
}

#if 0
/* u2_ve_zuse_pile(): load the packet log.
*/
u2_noun
u2_ve_zuse_pile(void)
{
  u2_noun 
}
#endif

/* u2_ve_zuse_save(): vent to output.
*/
void
u2_ve_zuse_save(u2_noun ven, u2_noun out)
{
  u2_noun p_ven, q_ven;
  c3_c*   ext_c;
  u2_noun dat;

  u2_cx_cell(ven, &p_ven, &q_ven);

  switch ( p_ven ) {
    default: u2_cm_bail(c3__fail);
    case c3__atom: ext_c = "atom"; break;
    case c3__pill: ext_c = "pill"; break;
  }

  /* Convert to data form.
  */
  {
    if ( c3__atom == p_ven ) {
      out = u2_ve_slan(out, "^-(@ .)");
      dat = u2k(u2t(out));
    } 
    else {
      dat = u2_cke_jam(u2k(u2t(out)));
    }
  }

  /* Write.
  */
  {
    u2_noun hat = u2_ve_slap(u2_ve_seed(), u2k(q_ven));
    u2_noun tah = u2k(u2t(hat));

    u2z(hat);
    if ( u2_no == u2_ve_save(ext_c, tah, dat) ) {
      u2_cm_bail(c3__fail);
    }
  }
  u2z(ven);
  u2z(out);
}

/* u2_ve_zuse_deed(): execute a zuse deed.
*/
void
u2_ve_zuse_deed(u2_noun ded)
{
  u2_noun p_ded, q_ded, r_ded;
  u2_noun gen, out;

  u2_cx_trel(ded, &p_ded, &q_ded, &r_ded);
  gen = u2k(p_ded);

  if ( u2_nul == q_ded ) {
    // inn = u2nc(u2nc(c3__atom, 'n'), 0);
    out = u2_ve_slap(u2_ve_seed(), gen);
  }
  else {
    u2_noun inn, fun;

    inn = u2_ve_zuse_load(u2k(u2t(q_ded)));
    fun = u2_ve_slap(u2_ve_seed(), gen);
    out = u2_ve_slam(fun, inn);
  }

  if ( u2_nul == r_ded ) {
    u2_ve_tank(2, u2_ve_skol(u2k(u2h(out))));
    u2_ve_tank(0, u2_ve_sell(u2k(out)));

    u2z(out);
  }
  else {
    u2_ve_zuse_save(u2k(u2t(r_ded)), out);
  }
  u2z(ded);
}

/* u2_ve_zuse_boom_see(): print to screen.
*/
void
u2_ve_zuse_boom_see(u2_noun tub, u2_noun tac)
{
  if ( !u2_fly_is_cat(tub) ) {
    u2_cm_bail(c3__fail);
  } else {
    u2_ve_tank(tub, tac);
  }
}

/* u2_ve_zuse_boom_dat(): save to disk.
*/
void
u2_ve_zuse_boom_dat(u2_noun tah, u2_noun mod, u2_noun som)
{
  u2_err(u2_Wire, "dat: tah", tah);
  u2_err(u2_Wire, "dat: mod", mod);

  u2z(tah);
}

/* u2_ve_zuse_boom(): produce zuse outputs.
*/
void
u2_ve_zuse_boom(u2_noun out)
{
  u2_noun tou = out;

  while ( 1 ) {
    if ( u2_nul == tou ) {
      u2z(out); 
      return;
    } else {
      u2_noun i_tou = u2h(tou);
      u2_noun t_tou = u2t(tou);
      u2_noun bum = i_tou;
      u2_noun p_bum, q_bum, r_bum;

      switch ( u2h(bum) ) {
        case c3__hit: {
          u2_as_trel(u2t(bum), &p_bum, &q_bum, &r_bum);
          u2_ve_zuse_boom_dat(u2k(p_bum), u2k(q_bum), u2k(r_bum));

          break;
        }
        case c3__see: {
          u2_as_cell(u2t(bum), &p_bum, &q_bum);

          u2_ve_zuse_boom_see(u2k(p_bum), u2k(q_bum));
          break;
        }
      }
      tou = t_tou;
    }
  }
}

/* u2_ve_zuse_loon_load(): preload a zuse line.
*/
u2_noun 
u2_ve_zuse_loon_load(u2_noun lun)
{
  return lun;
}

/* u2_ve_zuse_loon(): execute a zuse line.
*/
void
u2_ve_zuse_loon(u2_noun lun)
{
  lun = u2_ve_zuse_loon_load(lun);
  {
    u2_noun res = u2_ve_hard("born", "live", lun);
    u2_noun h_res = u2_h(res);
    u2_noun t_res = u2_t(res);

    // u2_err(u2_Wire, "boom", h_res); 
    u2_ve_zuse_boom(u2k(h_res));
    u2_ve_step("born", u2k(t_res));

    u2z(res);
  }
}

/* u2_ve_zuse_line(): execute a command with the zuse shell, protected.
*/
void
u2_ve_zuse_line(u2_noun lin)
{
  u2_hevn_be(u2_pryr, god) = u2_ve_zeus;
  {
    u2_noun ded;

    if ( u2_Host.kno_w > 209 ) {
      ded = u2_ve_hard("zuse", "scan", lin);

      u2_ve_zuse_deed(ded);
    } else {
#ifdef NEW9
      if ( u2_Host.kno_w > 208 ) {
        ded = u2_ve_hard("born", "slan", lin);
      } else {
        ded = u2_ve_hard("born", "scan", lin);
      }
      u2_ve_zuse_loon(ded);
#else
      ded = u2_ve_hard("born", "scan", lin);
      u2_ve_zuse_deed(ded);
#endif
    }
  }
  u2_hevn_be(u2_pryr, god) = 0;
}

static u2_noun
_http_in(u2_hreq* req_u)
{
  u2_noun met, hed, que, url, txt;

  switch ( req_u->met_e ) {
    default: return 0;

    case u2_hmet_get: met = c3__get;
    case u2_hmet_put: met = c3__put;
    case u2_hmet_post: met = c3__pos;
    case u2_hmet_delete: met = c3__del;
  }

  hed = u2_nul;
  {
    u2_hhed* hed_u;

    for ( hed_u = req_u->hed_u; hed_u; hed_u = hed_u->nex_u ) {
      hed = u2nc(u2nc(u2_ci_string(hed_u->nam_c), 
                      u2_ci_string(hed_u->val_c)),
                 hed);
    }
  }

  que = u2_nul;   //  XX fixme w/http query parser

  url = u2_ci_string(req_u->url_c);

  txt = u2_nul;   //  XX actually load body if any

  return u2nq(met, hed, que, u2nc(url, txt));
}

/* u2_ve_http_sync(): simple synchronous http.
*/
u2_hrep*
u2_ve_http_sync(u2_hreq* req_u)
{
  if ( u2_Host.kno_w > 205 ) {
    return 0;
  } else {
    u2_noun raw = _http_in(req_u);

    if ( 0 == raw ) {
      return 0;
    }
    else {
      return 0;
    }
  }
}

/* u2_ve_sync(): filesystem sync, unprotected.
*/
void
u2_ve_sync(void)
{
  u2_noun hoe;

  u2_cm_trip();
  if ( 0 != (hoe = u2_cm_trap()) ) {
    u2_cm_purge();
    u2_ve_grab(hoe, 0);

    u2_ve_wine(u2k(u2h(hoe)));
    u2_ve_sway(2, u2_ckb_flop(u2k(u2t(hoe))));
    u2z(hoe);
  } 
  else {
    u2_reck_sync(u2_Host.arv_u);
  
    u2_cm_done();
  
    u2_cm_purge();
    if ( (u2_yes == u2_Flag_Garbage) || (u2_no == u2_wire_lan(u2_Wire)) ) {
      u2_ve_grab(0);
    }
  }
  u2_cm_chin();
}

