/* v/term.c
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
#include <sys/socket.h>
#include <netinet/in.h>
#include <ev.h>
#include <errno.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "v/vere.h"

struct termios Oldattr;
struct termios Newattr;

static void _lo_term(struct ev_loop *lup_u, struct ev_io* wax_u, c3_i rev_i)
  { u2_lo_call(u2_Host.arv_u, lup_u, wax_u, c3__term, rev_i); }

void 
u2_term_io_init(u2_reck* rec_u)
{
  u2_utty* uty_u = malloc(sizeof(u2_utty));
  c3_i     fid_i = 1;                         //  stdin/stdout

  //  Initialize event processing.
  //
  {
    ev_io_init(&uty_u->wax_u, _lo_term, fid_i, 0);
  }

  //  Configure horrible stateful terminfo api.
  //
  {
    if ( 0 != setupterm(0, 2, 0) ) {
      c3_assert(!"init-setupterm");
    }
  }

  //  Load terminfo strings.
  //
  {
    c3_w len_w;

#   define _utfo(way, nam) \
    { \
      uty_u->ufo_u.way.nam##_y = (const c3_y *) tigetstr(#nam); \
      c3_assert(uty_u->ufo_u.way.nam##_y); \
    }

    uty_u->ufo_u.inn.max_w = 0;

    _utfo(inn, kcuu1);
    _utfo(inn, kcud1);
    _utfo(inn, kcub1);
    _utfo(inn, kcuf1);

    _utfo(out, clear);
    _utfo(out, el);
    _utfo(out, el1);
    _utfo(out, ed);
    _utfo(out, bel);
    _utfo(out, cub1);
    _utfo(out, cuf1);
    _utfo(out, cuu1);
    _utfo(out, cud1);
    _utfo(out, cub);
    _utfo(out, cuf);

    //  Terminfo chronically reports the wrong sequence for arrow
    //  keys on xterms.  Drastic fix for ridiculous unacceptable bug.
    //  Yes, we could fix this with smkx/rmkx, but this is retarded as well.
    {
      uty_u->ufo_u.inn.kcuu1_y = (const c3_y*)"\033[A";
      uty_u->ufo_u.inn.kcud1_y = (const c3_y*)"\033[B";
      uty_u->ufo_u.inn.kcuf1_y = (const c3_y*)"\033[C";
      uty_u->ufo_u.inn.kcub1_y = (const c3_y*)"\033[D";
    }

    uty_u->ufo_u.inn.max_w = 0;
    if ( (len_w = strlen((c3_c*)uty_u->ufo_u.inn.kcuu1_y)) > 
          uty_u->ufo_u.inn.max_w ) 
    {
      uty_u->ufo_u.inn.max_w = len_w;
    }
    if ( (len_w = strlen((c3_c*)uty_u->ufo_u.inn.kcud1_y)) > 
          uty_u->ufo_u.inn.max_w ) 
    {
      uty_u->ufo_u.inn.max_w = len_w;
    }
    if ( (len_w = strlen((c3_c*)uty_u->ufo_u.inn.kcub1_y)) > 
          uty_u->ufo_u.inn.max_w ) 
    {
      uty_u->ufo_u.inn.max_w = len_w;
    }
    if ( (len_w = strlen((c3_c*)uty_u->ufo_u.inn.kcuf1_y)) > 
          uty_u->ufo_u.inn.max_w ) 
    {
      uty_u->ufo_u.inn.max_w = len_w;
    }
  }

  //  Load old terminal state to restore.
  //
  {
    if ( 0 != tcgetattr(fid_i, &uty_u->bak_u) ) {
      c3_assert(!"init-tcgetattr");
    }
    if ( -1 == fcntl(fid_i, F_GETFL, &uty_u->cug_i) ) {
      c3_assert(!"init-fcntl");
    }
    uty_u->cug_i &= ~O_NONBLOCK;                // could fix?
    uty_u->nob_i = uty_u->cug_i | O_NONBLOCK;   // O_NDELAY on older unix
  }

  //  Construct raw termios configuration.
  //
  {
    uty_u->raw_u = uty_u->bak_u;

    uty_u->raw_u.c_lflag &= ~(ECHO | ECHONL | ICANON | IEXTEN);
    uty_u->raw_u.c_iflag &= ~(ICRNL | INPCK | ISTRIP);
    uty_u->raw_u.c_cflag &= ~(CSIZE | PARENB);
    uty_u->raw_u.c_cflag |= CS8;
    uty_u->raw_u.c_oflag &= ~(OPOST);
    uty_u->raw_u.c_cc[VMIN] = 0;
    uty_u->raw_u.c_cc[VTIME] = 0;
  } 

  //  Initialize mirror and accumulator state.
  //
  {
    uty_u->tat_u.mir.lin_w = 0;
    uty_u->tat_u.mir.len_w = 0;
    uty_u->tat_u.mir.cus_w = 0;

    uty_u->tat_u.esc.ape = u2_no;
    uty_u->tat_u.esc.bra = u2_no;

    uty_u->tat_u.fut.len_w = 0;
    uty_u->tat_u.fut.wid_w = 0;
  }

  //  This is terminal 1, with an empty queue, linked in host.
  //
  {
    uty_u->tid_l = 1;
    uty_u->out_u = 0;
    uty_u->tou_u = 0;

    uty_u->nex_u = u2_Host.uty_u;
    u2_Host.uty_u = uty_u;
    u2_Host.tem_u = uty_u;
  }

  //  Start raw input.
  //
  {
    if ( 0 != tcsetattr(uty_u->wax_u.fd, TCSADRAIN, &uty_u->raw_u) ) {
      c3_assert(!"init-tcsetattr");
    }
    if ( -1 == fcntl(uty_u->wax_u.fd, F_SETFL, uty_u->nob_i) ) {
      c3_assert(!"init-fcntl");
    }
  }
}

/* u2_term_io_exit(): clean up terminal.
*/
void 
u2_term_io_exit(u2_reck* rec_u)
{
  u2_utty* uty_u;

  for ( uty_u = u2_Host.uty_u; uty_u; uty_u = uty_u->nex_u ) {
    if ( 0 != tcsetattr(uty_u->wax_u.fd, TCSADRAIN, &uty_u->bak_u) ) {
      c3_assert(!"exit-tcsetattr");
    }
    if ( -1 == fcntl(uty_u->wax_u.fd, F_SETFL, uty_u->cug_i) ) {
      c3_assert(!"exit-fcntl");
    }
    write(uty_u->wax_u.fd, "\r\n", 2);
  }
}

void
u2_term_io_poll(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_utty* uty_u;

  for ( uty_u = u2_Host.uty_u; uty_u; uty_u = uty_u->nex_u ) {
    c3_i ver_i = 0;

    ver_i |= EV_READ;   //  no constraint on reading right now
    if ( uty_u->out_u ) {
      ver_i |= EV_WRITE;
    }
    ev_io_set(&uty_u->wax_u, uty_u->wax_u.fd, ver_i);
  }
}

void
u2_term_io_spin(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_utty* uty_u;

  for ( uty_u = u2_Host.uty_u; uty_u; uty_u = uty_u->nex_u ) {
    ev_io_start(lup_u, &uty_u->wax_u);
  }
}

void
u2_term_io_stop(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_utty* uty_u;

  for ( uty_u = u2_Host.uty_u; uty_u; uty_u = uty_u->nex_u ) {
    ev_io_stop(lup_u, &uty_u->wax_u);
  }
}

/* _term_it_clip(): remove sent bytes from buffer.
**
** XX unify with ubuf in http.c
*/
static void
_term_it_clip(u2_ubuf* buf_u, c3_i siz_i)
{
  if ( siz_i ) {
    c3_assert(siz_i < buf_u->len_w);
    {
      c3_w res_w = (buf_u->len_w - siz_i);

      memmove(buf_u->hun_y, (buf_u->hun_y + siz_i), res_w);
      buf_u->len_w = res_w;
    }
  }
}

/* _term_it_buf(): create a data buffer.
*/
static u2_ubuf*
_term_it_buf(c3_w len_w, const c3_y* hun_y)
{
  u2_ubuf* buf_u = malloc(len_w + sizeof(*buf_u));

  buf_u->len_w = len_w;
  memcpy(buf_u->hun_y, hun_y, len_w);
  
  buf_u->nex_u = 0;
  return buf_u;
}

/* _term_it_queue_buf(): queue buffer, transferring pointer.
*/
static void
_term_it_queue_buf(u2_utty* uty_u,
                   u2_ubuf* buf_u)
{
  if ( !uty_u->tou_u ) {
    uty_u->out_u = uty_u->tou_u = buf_u;
  }
  else {
    uty_u->tou_u->nex_u = buf_u;
    uty_u->tou_u = buf_u;
  }
}

/* _term_it_queue_bytes(): queue bytes, retaining pointer.
*/
static void
_term_it_queue_bytes(u2_utty*    uty_u,
                     c3_w        len_w,
                     const c3_y* hun_y)
{
  _term_it_queue_buf(uty_u, _term_it_buf(len_w, hun_y));
}

/* _term_it_queue_txt(): queue null-terminated string, retaining pointer.
*/
static void
_term_it_queue_txt(u2_utty*    uty_u,
                   const c3_y* hun_y)
{
  _term_it_queue_bytes(uty_u, strlen((const c3_c*)hun_y), hun_y);
}

/* _term_it_queue_str(): queue null-terminated string, retaining pointer.
*/
static void
_term_it_queue_str(u2_utty*    uty_u,
                   const c3_c* str_c)
{
  _term_it_queue_txt(uty_u, (const c3_y*) str_c);
}

#if 0
/* _term_it_queue_strnum(): queue string with terminal parameter, retaining.
*/
static void
_term_it_queue_strnum(u2_utty* uty_u, const c3_c* str_c, c3_w num_w)
{
  c3_c buf_c[16];

  sprintf(buf_c, "#%ud", num_w);   //  XX slow
  _term_it_queue_str(uty_u, str_c);
  _term_it_queue_str(uty_u, buf_c);
}
#endif

/* _term_it_show_wide(): show wide text, retaining.
*/
static void
_term_it_show_wide(u2_reck* rec_u, u2_utty* uty_u, c3_w len_w, c3_w* txt_w)
{
  u2_noun wad   = u2_ci_words(len_w, txt_w);
  u2_noun txt   = u2_do("tuft", wad);
  c3_c*   txt_c = u2_cr_string(txt);

  _term_it_queue_str(uty_u, txt_c);
  free(txt_c);
  u2z(txt);

  uty_u->tat_u.mir.cus_w += len_w;
}

/* _term_it_show_clear(): clear to the beginning of the current line.
*/
static void
_term_it_show_clear(u2_utty* uty_u)
{
  u2_utat* tat_u = &uty_u->tat_u; 

  if ( tat_u->siz.col_l ) {
    c3_w     ful_w = tat_u->mir.cus_w / tat_u->siz.col_l;

    while ( ful_w-- ) {
      _term_it_queue_txt(uty_u, uty_u->ufo_u.out.cuu1_y);
    }
    _term_it_queue_str(uty_u, "\r");
    _term_it_queue_txt(uty_u, uty_u->ufo_u.out.ed_y);

    tat_u->mir.len_w = 0;
    tat_u->mir.cus_w = 0;
  }
}

/* _term_it_show_blank(): blank the screen.
*/
static void
_term_it_show_blank(u2_utty* uty_u)
{
  _term_it_queue_txt(uty_u, uty_u->ufo_u.out.clear_y);
  uty_u->tat_u.mir.cus_w = 0;
}

/* _term_it_show_cursor(): set current line, transferring pointer.
*/
static void
_term_it_show_cursor(u2_utty* uty_u, c3_w cur_w)
{
  if ( cur_w < uty_u->tat_u.mir.cus_w ) {
    c3_w dif_w = (uty_u->tat_u.mir.cus_w - cur_w);

    while ( dif_w-- ) {
      _term_it_queue_txt(uty_u, uty_u->ufo_u.out.cub1_y);
    }
  }
  else if ( cur_w > uty_u->tat_u.mir.cus_w ) {
    c3_w dif_w = (cur_w - uty_u->tat_u.mir.cus_w);

    while ( dif_w-- ) {
      _term_it_queue_txt(uty_u, uty_u->ufo_u.out.cuf1_y);
    }
  }
  uty_u->tat_u.mir.cus_w = cur_w;
}

/* _term_it_show_line(): set current line, transferring pointer.
*/
static void
_term_it_show_line(u2_reck* rec_u, u2_utty* uty_u, c3_w len_w, c3_w* lin_w)
{
  _term_it_show_clear(uty_u);
  _term_it_show_wide(rec_u, uty_u, len_w, lin_w);

  if ( lin_w != uty_u->tat_u.mir.lin_w ) {
    if ( uty_u->tat_u.mir.lin_w ) {
      free(uty_u->tat_u.mir.lin_w);
    }
    uty_u->tat_u.mir.lin_w = lin_w;
  }
  uty_u->tat_u.mir.len_w = len_w;
}

/* _term_it_refresh_line(): refresh current line.
*/
static void
_term_it_refresh_line(u2_reck* rec_u, u2_utty* uty_u)
{
  _term_it_show_clear(uty_u);
  _term_it_show_wide
    (rec_u, uty_u, uty_u->tat_u.mir.len_w, uty_u->tat_u.mir.lin_w);
}

/* _term_it_show_more(): new current line.
*/
static void
_term_it_show_more(u2_utty* uty_u)
{
  _term_it_queue_str(uty_u, "\r\n");
  uty_u->tat_u.mir.cus_w = 0;
}

/* _term_it_path(): path for console file.
*/
static c3_c*
_term_it_path(u2_reck* rec_u, u2_bean fyl, u2_noun pax)
{
  c3_w len_w;
  c3_c *pas_c;

  //  measure
  //
  len_w = strlen(u2_Host.cpu_c);
  {
    u2_noun wiz = pax;

    while ( u2_nul != wiz ) {
      len_w += (1 + u2_cr_met(3, u2h(wiz)));
      wiz = u2t(wiz);
    }
  }

  //  cut
  //
  pas_c = malloc(len_w + 1);
  strcpy(pas_c, u2_Host.cpu_c);
  {
    u2_noun wiz   = pax;
    c3_c*   waq_c = (pas_c + strlen(pas_c));

    while ( u2_nul != wiz ) {
      c3_w tis_w = u2_cr_met(3, u2h(wiz));

      if ( (u2_yes == fyl) && (u2_nul == u2t(wiz)) ) {
        *waq_c++ = '.';
      } else *waq_c++ = '/';

      u2_cr_bytes(0, tis_w, (c3_y*)waq_c, u2h(wiz));
      waq_c += tis_w;

      wiz = u2t(wiz);
    }
    *waq_c = 0;
  }
  u2z(pax);
  return pas_c;
}

/* _term_it_save(): save file by path.
*/
static void
_term_it_save(u2_reck* rec_u, u2_noun pax, u2_noun pad)
{
  c3_c* pax_c;

  pax = u2nc(c3_s3('p','u','t'), pax);
  pax_c = _term_it_path(rec_u, u2_yes, pax);

  u2_walk_save(pax_c, 0, pad);
  free(pax_c);
}

/* _term_io_belt(): send belt.
*/
static void
_term_io_belt(u2_reck* rec_u,
              u2_utty* uty_u,
              u2_noun  blb)
{
  u2_noun tid = u2_dc("scot", c3__ud, uty_u->tid_l);
  u2_noun pax = u2nq(c3__gold, c3__term, tid, u2_nul);

  u2_reck_plan(rec_u, pax, u2nc(c3__belt, blb));
}

/* _term_io_suck_char(): process a single character.
*/
static void
_term_io_suck_char(u2_reck* rec_u,
                   u2_utty* uty_u,
                   c3_y     cay_y)
{ 
  u2_utat* tat_u = &uty_u->tat_u;

  // uL(fprintf(uH, "suck-char %x\n", cay_y));

  if ( u2_yes == tat_u->esc.ape ) {
    if ( u2_yes == tat_u->esc.bra ) {
      switch ( cay_y ) {
        default: {
          _term_it_queue_txt(uty_u, uty_u->ufo_u.out.bel_y);
          break;
        }
        case 'A': _term_io_belt(rec_u, uty_u, u2nc(c3__aro, 'u')); break;
        case 'B': _term_io_belt(rec_u, uty_u, u2nc(c3__aro, 'd')); break;
        case 'C': _term_io_belt(rec_u, uty_u, u2nc(c3__aro, 'r')); break;
        case 'D': _term_io_belt(rec_u, uty_u, u2nc(c3__aro, 'l')); break;
      }
      tat_u->esc.ape = tat_u->esc.bra = u2_no;
    }
    else {
      if ( (cay_y >= 'a') && (cay_y <= 'z') ) {
        tat_u->esc.ape = u2_no;
        _term_io_belt(rec_u, uty_u, u2nc(c3__met, cay_y));
      }
      else if ( ('[' == cay_y) || ('O' == cay_y) ) {
        tat_u->esc.bra = u2_yes;
      }
      else {
        tat_u->esc.ape = u2_no;
       
        _term_it_queue_txt(uty_u, uty_u->ufo_u.out.bel_y);
      }
    }
  }
  else if ( 0 != tat_u->fut.wid_w ) {
    tat_u->fut.syb_y[tat_u->fut.len_w++] = cay_y;

    if ( tat_u->fut.len_w == tat_u->fut.wid_w ) {
      u2_noun huv = u2_ci_bytes(tat_u->fut.wid_w, tat_u->fut.syb_y);
      u2_noun wug;

      // uL(fprintf(uH, "muck-utf8 len %d\n", tat_u->fut.len_w));
      // uL(fprintf(uH, "muck-utf8 %x\n", huv));
      wug = u2_do("turf", huv);
      // uL(fprintf(uH, "muck-utf32 %x\n", tat_u->fut.len_w));

      tat_u->fut.len_w = tat_u->fut.wid_w = 0;
      _term_io_belt(rec_u, uty_u, u2nt(c3__txt, wug, u2_nul));
    }
  }
  else {
    if ( (cay_y >= 32) && (cay_y < 127) ) {
      _term_io_belt(rec_u, uty_u, u2nt(c3__txt, cay_y, u2_nul));
    }
    else if ( 0 == cay_y ) {
      _term_it_queue_txt(uty_u, uty_u->ufo_u.out.bel_y);
    }
    else if ( 13 == cay_y ) {
      _term_io_belt(rec_u, uty_u, u2nc(c3__ret, u2_nul));
    }
    else if ( cay_y <= 26 ) {
      _term_io_belt(rec_u, uty_u, u2nc(c3__ctl, ('a' + (cay_y - 1))));
    }
    else if ( 27 == cay_y ) {
      tat_u->esc.ape = u2_yes;
    }
    else if ( 127 == cay_y ) {
      _term_io_belt(rec_u, uty_u, u2nc(c3__bac, u2_nul));
    }
    else if ( cay_y >= 128 ) {
      tat_u->fut.len_w = 1;
      tat_u->fut.syb_y[0] = cay_y;

      if ( cay_y <= 224 ) {
        tat_u->fut.wid_w = 2;
      } else if ( cay_y <= 240 ) {
        tat_u->fut.wid_w = 3;
      } else tat_u->fut.wid_w = 4;
    }
  }
}

/* u2_term_io_suck(): read terminal bytes.
*/
void
u2_term_io_suck(u2_reck*      rec_u,
                struct ev_io* wax_u)
{
  u2_utty* uty_u = (u2_utty*)(void*)wax_u;

  {
    while ( 1 ) {
      c3_y buf_y[4096];
      c3_i siz_i, i;

      if ( (siz_i = read(uty_u->wax_u.fd, buf_y, 4096)) < 0) {
        if ( EAGAIN == errno ) {
          break;
        } else {
          c3_assert(!"term: read");
        }
      }
      for ( i=0; i < siz_i; i++ ) {
        _term_io_suck_char(rec_u, uty_u, buf_y[i]);
      }
      if ( 4096 != siz_i ) {
        break;
      }
    }
  }
}

/* u2_term_io_fuck(): write terminal bytes.
*/
void
u2_term_io_fuck(u2_reck*      rec_u,
                struct ev_io* wax_u)
{
  u2_utty* uty_u = (u2_utty*)(void*)wax_u;

  {
    while ( uty_u->out_u ) {
      u2_ubuf* out_u = uty_u->out_u;
      c3_i     siz_i;

      if ( (siz_i = write(uty_u->wax_u.fd, 
                          uty_u->out_u->hun_y, 
                          uty_u->out_u->len_w)) < 0 ) {
        if ( EAGAIN == errno ) {
          break;
        } else {
          c3_assert(!"term: write");
        }
      }
      if ( siz_i < out_u->len_w ) {
        _term_it_clip(out_u, siz_i);
        break;
      }
      else {
        uty_u->out_u = uty_u->out_u->nex_u;
        if ( 0 == uty_u->out_u ) {
          c3_assert(out_u == uty_u->tou_u);
          uty_u->tou_u = 0;
        }
        free(out_u);
      }
    }
  }
}

/* _term_main(): return main or console terminal.
*/
static u2_utty*
_term_main()
{
  u2_utty* uty_u;

  for ( uty_u = u2_Host.uty_u; uty_u; uty_u = uty_u->nex_u ) {
    if ( uty_u->wax_u.fd <= 2 ) {
      return uty_u;
    }
  }
  return u2_Host.uty_u;
}

/* _term_ef_get(): terminal by id.
*/
static u2_utty*
_term_ef_get(u2_reck* rec_u,
             c3_l     tid_l)
{
  if ( 0 != tid_l ) {
    u2_utty* uty_u;

    for ( uty_u = u2_Host.uty_u; uty_u; uty_u = uty_u->nex_u ) {
      if ( tid_l == uty_u->tid_l ) {
        return uty_u;
      }
    }
  }
  return _term_main();
}

/* _term_ef_blew(): return window size [columns rows].
*/
u2_noun
_term_ef_blew(u2_reck* rec_u, c3_l tid_l)
{
  struct winsize siz_u;
  u2_utty*       uty_u = _term_ef_get(rec_u, tid_l);
  c3_l           col_l, row_l;

  if ( 0 == ioctl(uty_u->wax_u.fd, TIOCGWINSZ, &siz_u) ) {
    col_l = siz_u.ws_col;
    row_l = siz_u.ws_row;
  } else {
    col_l = 80;
    row_l = 24;
  }
  uty_u->tat_u.siz.col_l = col_l;
  uty_u->tat_u.siz.row_l = row_l;
  
  return u2nc(col_l, row_l);
}

#if 0
/* u2_term_ef_boil(): initial effects for loaded servers.
*/
void
u2_term_ef_boil(u2_reck* rec_u,
                c3_l     old_l,
                c3_l     ono_l)
{
  if ( ono_l ) {
    u2_noun oan = u2_dc("scot", c3__uv, old_l);
    u2_noun tid_l;

    for ( tid_l = 1; tid_l <= ono_l; tid_l++ ) {
      u2_noun tin = u2_dc("scot", c3__ud, tid_l);
      u2_noun pax = u2nc(c3__gold, u2nq(c3__term, u2k(oan), tin, u2_nul));
      u2_noun hud = u2nc(c3__wipe, u2_nul);
 
      u2_reck_plan(rec_u, pax, hud);
    }
    u2z(oan);
  }

  {
    u2_noun pax = u2nc(c3__gold, u2nq(c3__term, u2k(rec_u->sen), '1', u2_nul));

    u2_reck_plan(rec_u, u2k(pax), u2nc(c3__init, u2k(u2h(rec_u->own))));
    u2_reck_plan(rec_u, u2k(pax), u2nc(c3__blew, _term_ef_blew(rec_u, 1)));
    u2_reck_plan(rec_u, u2k(pax), u2nc(c3__hail, u2_nul));

    u2z(pax);
  }
}
#else

/* u2_term_ef_winc(): window change.  Just console right now.
*/
void
u2_term_ef_winc(u2_reck* rec_u) 
{
  u2_noun pax = u2nq(c3__gold, c3__term, '1', u2_nul);

  u2_reck_plan(rec_u, u2k(pax), u2nc(c3__blew, _term_ef_blew(rec_u, 1)));
}

/* u2_term_ef_ctlc(): send ^C on console.
*/
void
u2_term_ef_ctlc(u2_reck* rec_u) 
{
  u2_noun pax = u2nq(c3__gold, c3__term, '1', u2_nul);

  u2_reck_plan(rec_u, u2k(pax), u2nt(c3__belt, c3__ctl, 'c'));
}

/* u2_term_ef_boil(): initial effects for loaded servers.
*/
void
u2_term_ef_boil(u2_reck* rec_u,
                c3_l     ono_l)
{
  if ( ono_l ) {
    u2_noun tid_l;

    for ( tid_l = 2; tid_l <= ono_l; tid_l++ ) {
      u2_noun tin = u2_dc("scot", c3__ud, tid_l);
      u2_noun pax = u2nq(c3__gold, c3__term, tin, u2_nul);
      u2_noun hud = u2nc(c3__wipe, u2_nul);
 
      u2_reck_plan(rec_u, pax, hud);
    }
  }

  {
    u2_noun pax = u2nq(c3__gold, c3__term, '1', u2_nul);

    //  u2_reck_plan(rec_u, u2k(pax), u2nc(c3__init, u2k(u2h(rec_u->own))));
    u2_reck_plan(rec_u, u2k(pax), u2nc(c3__blew, _term_ef_blew(rec_u, 1)));
    u2_reck_plan(rec_u, u2k(pax), u2nc(c3__hail, u2_nul));

    u2z(pax);
  }
}
#endif

#if 0
/* u2_term_ef_bake(): initial effects for new terminal.
*/
void
u2_term_ef_bake(u2_reck* rec_u,
                u2_noun  fav)
{
  u2_noun pax = u2nc(c3__gold, u2nq(c3__term, u2k(rec_u->sen), '1', u2_nul));

  u2_reck_plan(rec_u, u2k(pax), u2nc(c3__boot, fav));
  u2_reck_plan(rec_u, u2k(pax), u2nc(c3__blew, _term_ef_blew(rec_u, 1)));
  u2_reck_plan(rec_u, u2k(pax), u2nc(c3__hail, u2_nul));

  u2z(pax);
}
#else
/* u2_term_ef_bake(): initial effects for new terminal.
*/
void
u2_term_ef_bake(u2_reck* rec_u,
                u2_noun  fav)
{
  u2_noun pax = u2nq(c3__gold, c3__term, '1', u2_nul);

  u2_reck_plan(rec_u, u2k(pax), u2nc(c3__boot, fav));
  u2_reck_plan(rec_u, u2k(pax), u2nc(c3__blew, _term_ef_blew(rec_u, 1)));
  u2_reck_plan(rec_u, u2k(pax), u2nc(c3__hail, u2_nul));

  u2z(pax);
}
#endif

/* _term_ef_blit(): send blit to terminal.
*/
static void
_term_ef_blit(u2_reck* rec_u,
              u2_utty* uty_u,
              u2_noun  blt)
{
  switch ( u2h(blt) ) {
    default: break;
    case c3__bel: {
      _term_it_queue_txt(uty_u, uty_u->ufo_u.out.bel_y);
    } break; 

    case c3__clr: {
      _term_it_show_blank(uty_u);
      _term_it_refresh_line(rec_u, uty_u);
    } break;

    case c3__hop: {
      _term_it_show_cursor(uty_u, u2t(blt)); 
    } break;

    case c3__lin: {
      u2_noun lin = u2t(blt);
      c3_w    len_w = u2_ckb_lent(u2k(lin));
      c3_w*   lin_w = malloc(4 * len_w);
       
      {
        c3_w i_w;

        for ( i_w = 0; u2_nul != lin; i_w++, lin = u2t(lin) ) {
          lin_w[i_w] = u2_cr_word(0, u2h(lin));
        }
      }
      _term_it_show_line(rec_u, uty_u, len_w, lin_w);
    } break;

    case c3__mor: {
      _term_it_show_more(uty_u);
    } break;

    case c3__sav: {
      _term_it_save(rec_u, u2k(u2h(u2t(blt))), u2k(u2t(u2t(blt)))); 
    } break;
  }
  u2z(blt);
  return;
}

/* u2_term_ef_blit(): send %blit list to specific terminal.
*/
void
u2_term_ef_blit(u2_reck* rec_u,
                c3_l     tid_l,
                u2_noun  bls)
{
  u2_utty* uty_u = _term_ef_get(rec_u, tid_l);

  if ( 0 == uty_u ) {
    // uL(fprintf(uH, "no terminal %d\n", tid_l));
    // uL(fprintf(uH, "uty_u %p\n", u2_Host.uty_u));

    u2z(bls); return;
  }

  {
    u2_noun bis = bls;

    while ( u2_yes == u2du(bis) ) {
      _term_ef_blit(rec_u, uty_u, u2k(u2h(bis)));
      bis = u2t(bis);
    }
    u2z(bls);
  }
} 

/* u2_term_io_hija(): hijack console for fprintf, returning FILE*.
*/
FILE*
u2_term_io_hija(void)
{
  u2_utty* uty_u = _term_main();

  if ( uty_u ) {
    if ( uty_u->wax_u.fd > 2 ) {
      //  We *should* in fact, produce some kind of fake FILE* for
      //  non-console terminals.  If we use this interface enough...
      //
      c3_assert(0);    
    }
    else {
      if ( 0 != tcsetattr(1, TCSADRAIN, &uty_u->bak_u) ) {
        c3_assert(!"hija-tcsetattr");
      }
      if ( -1 == fcntl(1, F_SETFL, uty_u->cug_i) ) {
        c3_assert(!"hija-fcntl");
      }
      if ( 0 != tcsetattr(0, TCSADRAIN, &uty_u->bak_u) ) {
        c3_assert(!"hija-tcsetattr");
      }
      if ( -1 == fcntl(0, F_SETFL, uty_u->cug_i) ) {
        c3_assert(!"hija-fcntl");
      }
      write(uty_u->wax_u.fd, "\r", 1);
      write(uty_u->wax_u.fd, uty_u->ufo_u.out.el_y,
                             strlen((c3_c*) uty_u->ufo_u.out.el_y));
      return stdout;
    }
  }
  else return stdout;
}

/* u2_term_io_loja(): release console from fprintf.
*/
void
u2_term_io_loja(int x)
{
  u2_utty* uty_u = _term_main();

  if ( uty_u ) {
    if ( uty_u->wax_u.fd > 2 ) {
      //  We *should* in fact, produce some kind of fake FILE* for
      //  non-console terminals.  If we use this interface enough...
      //
      c3_assert(0);    
    }
    else {
      if ( 0 != tcsetattr(1, TCSADRAIN, &uty_u->raw_u) ) {
        c3_assert(!"loja-tcsetattr");
      }
      if ( -1 == fcntl(1, F_SETFL, uty_u->nob_i) ) {
        c3_assert(!"loja-fcntl");
      }
      if ( 0 != tcsetattr(0, TCSADRAIN, &uty_u->raw_u) ) {
        c3_assert(!"loja-tcsetattr");
      }
      if ( -1 == fcntl(0, F_SETFL, uty_u->nob_i) ) {
        c3_assert(!"loja-fcntl");
      }
      _term_it_refresh_line(u2_Host.arv_u, uty_u);
    }
  }
}
