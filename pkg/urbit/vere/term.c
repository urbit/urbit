/* vere/term.c
**
*/
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>
#include <ncurses/curses.h>
#include <termios.h>
#include <ncurses/term.h>

#include "all.h"
#include "vere/vere.h"

static u3_utty* _term_main();
static void     _term_read_cb(uv_stream_t*    tcp_u,
                              ssize_t         siz_i,
                              const uv_buf_t* buf_u);
static c3_i     _term_tcsetattr(int, int, const struct termios *);

/* _write(): wraps write(), asserting length
*/
static void
_write(c3_i fid_i, const void* buf_v, size_t len)
{
  if ( len != write(fid_i, buf_v, len) ){
    u3l_log("write failed\r\n");
    c3_assert(0);
  }
}

/* _term_msc_out_host(): unix microseconds from current host time.
*/
static c3_d
_term_msc_out_host()
{
  struct timeval tim_tv;
  gettimeofday(&tim_tv, 0);
  return 1000000ULL * tim_tv.tv_sec + tim_tv.tv_usec;
}

/* _term_alloc(): libuv buffer allocator.
*/
static void
_term_alloc(uv_handle_t* had_u,
            size_t len_i,
            uv_buf_t* buf
            )
{
  //  this read can range from a single byte to a paste buffer
  //  123 bytes has been chosen because its not a power of 2
  //  this is probably still broken
  //
  void* ptr_v = c3_malloc(123);
  *buf = uv_buf_init(ptr_v, 123);
}

/* u3_term_io_init(): initialize terminal.
*/
void
u3_term_io_init()
{
  u3_utty* uty_u = c3_calloc(sizeof(u3_utty));

  if ( c3y == u3_Host.ops_u.tem ) {
    uty_u->fid_i = 1;

    uv_pipe_init(u3L, &(uty_u->pop_u), 0);
    uv_pipe_open(&(uty_u->pop_u), uty_u->fid_i);
  }
  else {
    //  Initialize event processing.  Rawdog it.
    //
    {
      uty_u->fid_i = 0;                       //  stdin, yes we write to it...

      uv_pipe_init(u3L, &(uty_u->pop_u), 0);
      uv_pipe_open(&(uty_u->pop_u), uty_u->fid_i);
      uv_read_start((uv_stream_t*)&(uty_u->pop_u), _term_alloc, _term_read_cb);
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

      _utfo(out, clear);
      _utfo(out, el);
      // _utfo(out, el1);
      _utfo(out, ed);
      _utfo(out, bel);
      _utfo(out, cub1);
      _utfo(out, cuf1);
      _utfo(out, cuu1);
      _utfo(out, cud1);
      // _utfo(out, cub);
      // _utfo(out, cuf);
    }

    //  Load old terminal state to restore.
    //
    {
      if ( 0 != tcgetattr(uty_u->fid_i, &uty_u->bak_u) ) {
        c3_assert(!"init-tcgetattr");
      }
      if ( -1 == fcntl(uty_u->fid_i, F_GETFL, &uty_u->cug_i) ) {
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

      uty_u->tat_u.esc.ape = c3n;
      uty_u->tat_u.esc.bra = c3n;

      uty_u->tat_u.fut.len_w = 0;
      uty_u->tat_u.fut.wid_w = 0;
    }
  }

  //  This is terminal 1, linked in host.
  //
  {
    uty_u->tid_l = 1;
    uty_u->nex_u = 0;
    u3_Host.uty_u = uty_u;
  }

  if ( c3n == u3_Host.ops_u.tem ) {
    //  Start raw input.
    //
    {
      if ( 0 != _term_tcsetattr(uty_u->fid_i, TCSADRAIN, &uty_u->raw_u) ) {
        c3_assert(!"init-tcsetattr");
      }
      if ( -1 == fcntl(uty_u->fid_i, F_SETFL, uty_u->nob_i) ) {
        c3_assert(!"init-fcntl");
      }
    }

    //  initialize spinner timeout (if terminal/tty is enabled)
    //
    if ( c3n == u3_Host.ops_u.tem ) {
      uv_timer_init(u3L, &uty_u->tat_u.sun_u.tim_u);
      uty_u->tat_u.sun_u.tim_u.data = uty_u;
    }
  }
}

void
u3_term_io_talk(void)
{
}

/* u3_term_io_exit(): clean up terminal.
*/
void
u3_term_io_exit(void)
{
  if ( c3y == u3_Host.ops_u.tem ) {
    uv_close((uv_handle_t*)&u3_Host.uty_u->pop_u, 0);
  }
  else {
    u3_utty* uty_u;

    for ( uty_u = u3_Host.uty_u; uty_u; uty_u = uty_u->nex_u ) {
      if ( uty_u->fid_i == -1 ) { continue; }
      if ( 0 != _term_tcsetattr(uty_u->fid_i, TCSADRAIN, &uty_u->bak_u) ) {
        c3_assert(!"exit-tcsetattr");
      }
      if ( -1 == fcntl(uty_u->fid_i, F_SETFL, uty_u->cug_i) ) {
        c3_assert(!"exit-fcntl");
      }
      _write(uty_u->fid_i, "\r\n", 2);

      uv_close((uv_handle_t*)&uty_u->tat_u.sun_u.tim_u, 0);
    }
  }
}

/*  _term_tcsetattr(): tcsetattr w/retry on EINTR.
*/
static c3_i
_term_tcsetattr(int fildes, int optional_actions,
  const struct termios *termios_p)
{
  c3_i ret_i = 0;
  do {
    ret_i = tcsetattr(fildes, optional_actions, termios_p);
  } while (-1 == ret_i && errno == EINTR);
  return ret_i;
}

/* _term_it_buf(): create a data buffer.
*/
static u3_ubuf*
_term_it_buf(c3_w len_w, const c3_y* hun_y)
{
  u3_ubuf* buf_u = c3_malloc(len_w + sizeof(*buf_u));

  buf_u->len_w = len_w;
  memcpy(buf_u->hun_y, hun_y, len_w);

  buf_u->nex_u = 0;
  return buf_u;
}

/* _term_write_cb(): general write callback.
*/
static void
_term_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  if ( 0 != sas_i ) {
    u3l_log("term: write: %s\n", uv_strerror(sas_i));
  }

  free(wri_u->data);
  free(wri_u);
}

/* _term_it_write_buf(): write buffer uv style.
*/
static void
_term_it_write_buf(u3_utty* uty_u, uv_buf_t buf_u)
{
  uv_write_t* wri_u = c3_malloc(sizeof(uv_write_t));
  wri_u->data = buf_u.base;

  c3_w ret_w;
  if ( 0 != (ret_w = uv_write(wri_u,
                     (uv_stream_t*)&(uty_u->pop_u),
                     &buf_u, 1,
                     _term_write_cb)) )
  {
    u3l_log("term: write: %s\n", uv_strerror(ret_w));
  }
}

/* _term_it_write_old(): write buffer, transferring pointer.
*/
static void
_term_it_write_old(u3_utty* uty_u,
                   u3_ubuf* old_u)
{
  uv_buf_t buf_u;

  //  XX extra copy here due to old code.  Use hbod as base directly.
  //
  {
    c3_y* buf_y = c3_malloc(old_u->len_w);

    memcpy(buf_y, old_u->hun_y, old_u->len_w);
    buf_u = uv_buf_init((c3_c*)buf_y, old_u->len_w);

    free(old_u);
  }
  _term_it_write_buf(uty_u, buf_u);
}

/* _term_it_write_bytes(): write bytes, retaining pointer.
*/
static void
_term_it_write_bytes(u3_utty*    uty_u,
                     c3_w        len_w,
                     const c3_y* hun_y)
{
  _term_it_write_old(uty_u, _term_it_buf(len_w, hun_y));
}

/* _term_it_write_txt(): write null-terminated string, retaining pointer.
*/
static void
_term_it_write_txt(u3_utty*    uty_u,
                   const c3_y* hun_y)
{
  _term_it_write_bytes(uty_u, strlen((const c3_c*)hun_y), hun_y);
}

/* _term_it_write_str(): write null-terminated string, retaining pointer.
*/
static void
_term_it_write_str(u3_utty*    uty_u,
                   const c3_c* str_c)
{
  _term_it_write_txt(uty_u, (const c3_y*) str_c);
}

/* _term_it_show_wide(): show wide text, retaining.
*/
static void
_term_it_show_wide(u3_utty* uty_u, c3_w len_w, c3_w* txt_w)
{
  u3_noun wad   = u3i_words(len_w, txt_w);
  u3_noun txt   = u3do("tuft", wad);
  c3_c*   txt_c = u3r_string(txt);

  _term_it_write_str(uty_u, txt_c);
  free(txt_c);
  u3z(txt);

  uty_u->tat_u.mir.cus_w += len_w;
}

/* _term_it_show_clear(): clear to the beginning of the current line.
*/
static void
_term_it_show_clear(u3_utty* uty_u)
{
  if ( uty_u->tat_u.siz.col_l ) {
    _term_it_write_str(uty_u, "\r");
    _term_it_write_txt(uty_u, uty_u->ufo_u.out.el_y);

    uty_u->tat_u.mir.len_w = 0;
    uty_u->tat_u.mir.cus_w = 0;
  }
}

/* _term_it_show_blank(): blank the screen.
*/
static void
_term_it_show_blank(u3_utty* uty_u)
{
  _term_it_write_txt(uty_u, uty_u->ufo_u.out.clear_y);
}

/* _term_it_show_cursor(): set current line, transferring pointer.
*/
static void
_term_it_show_cursor(u3_utty* uty_u, c3_w cur_w)
{
  if ( cur_w < uty_u->tat_u.mir.cus_w ) {
    c3_w dif_w = (uty_u->tat_u.mir.cus_w - cur_w);

    while ( dif_w-- ) {
      _term_it_write_txt(uty_u, uty_u->ufo_u.out.cub1_y);
    }
  }
  else if ( cur_w > uty_u->tat_u.mir.cus_w ) {
    c3_w dif_w = (cur_w - uty_u->tat_u.mir.cus_w);

    while ( dif_w-- ) {
      _term_it_write_txt(uty_u, uty_u->ufo_u.out.cuf1_y);
    }
  }
  uty_u->tat_u.mir.cus_w = cur_w;
}

/* _term_it_show_line(): set current line
*/
static void
_term_it_show_line(u3_utty* uty_u, c3_w* lin_w, c3_w len_w)
{
  _term_it_show_wide(uty_u, len_w, lin_w);

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
_term_it_refresh_line(u3_utty* uty_u)
{
  c3_w len_w = uty_u->tat_u.mir.len_w;
  c3_w cus_w = uty_u->tat_u.mir.cus_w;

  _term_it_show_clear(uty_u);
  _term_it_show_line(uty_u, uty_u->tat_u.mir.lin_w, len_w);
  _term_it_show_cursor(uty_u, cus_w);
}

/* _term_it_show_more(): new current line.
*/
static void
_term_it_show_more(u3_utty* uty_u)
{
  if ( c3y == u3_Host.ops_u.tem ) {
    _term_it_write_str(uty_u, "\n");
  } else {
    _term_it_write_str(uty_u, "\r\n");
  }
  uty_u->tat_u.mir.cus_w = 0;
}

/* _term_it_path(): path for console file.
*/
static c3_c*
_term_it_path(c3_o fyl, u3_noun pax)
{
  c3_w len_w;
  c3_c *pas_c;

  //  measure
  //
  len_w = strlen(u3_Host.dir_c);
  {
    u3_noun wiz = pax;

    while ( u3_nul != wiz ) {
      len_w += (1 + u3r_met(3, u3h(wiz)));
      wiz = u3t(wiz);
    }
  }

  //  cut
  //
  pas_c = c3_malloc(len_w + 1);
  strncpy(pas_c, u3_Host.dir_c, len_w);
  pas_c[len_w] = '\0';
  {
    u3_noun wiz   = pax;
    c3_c*   waq_c = (pas_c + strlen(pas_c));

    while ( u3_nul != wiz ) {
      c3_w tis_w = u3r_met(3, u3h(wiz));

      if ( (c3y == fyl) && (u3_nul == u3t(wiz)) ) {
        *waq_c++ = '.';
      } else *waq_c++ = '/';

      u3r_bytes(0, tis_w, (c3_y*)waq_c, u3h(wiz));
      waq_c += tis_w;

      wiz = u3t(wiz);
    }
    *waq_c = 0;
  }
  u3z(pax);
  return pas_c;
}

/* _term_it_save(): save file by path.
*/
static void
_term_it_save(u3_noun pax, u3_noun pad)
{
  c3_c* pax_c;
  c3_c* bas_c = 0;
  c3_w  xap_w = u3kb_lent(u3k(pax));
  u3_noun xap = u3_nul;
  u3_noun urb = c3_s4('.','u','r','b');
  u3_noun put = c3_s3('p','u','t');

  // directory base and relative path
  if ( 2 < xap_w ) {
    u3_noun bas = u3nt(urb, put, u3_nul);
    bas_c = _term_it_path(c3n, bas);
    xap = u3qb_scag(xap_w - 2, pax);
  }

  pax = u3nt(urb, put, pax);
  pax_c = _term_it_path(c3y, pax);

  u3_walk_save(pax_c, 0, pad, bas_c, xap);

  free(pax_c);
  free(bas_c);
}

/* _term_io_belt(): send belt.
*/
static void
_term_io_belt(u3_utty* uty_u, u3_noun  blb)
{
  // Here, we use scot just to build out the plan.
  //
  u3_noun tid = u3dc("scot", c3__ud, uty_u->tid_l);
  u3_noun pax = u3nq(u3_blip, c3__term, tid, u3_nul);

  u3_pier_plan(pax, u3nc(c3__belt, blb));
}

/* _term_io_suck_char(): process a single character.
*/
static void
_term_io_suck_char(u3_utty* uty_u, c3_y cay_y)
{
  u3_utat* tat_u = &uty_u->tat_u;

  if ( c3y == tat_u->esc.ape ) {
    if ( c3y == tat_u->esc.bra ) {
      switch ( cay_y ) {
        default: {
          _term_it_write_txt(uty_u, uty_u->ufo_u.out.bel_y);
          break;
        }
        case 'A': _term_io_belt(uty_u, u3nc(c3__aro, 'u')); break;
        case 'B': _term_io_belt(uty_u, u3nc(c3__aro, 'd')); break;
        case 'C': _term_io_belt(uty_u, u3nc(c3__aro, 'r')); break;
        case 'D': _term_io_belt(uty_u, u3nc(c3__aro, 'l')); break;
      }
      tat_u->esc.ape = tat_u->esc.bra = c3n;
    }
    else {
      if ( (cay_y >= 'a') && (cay_y <= 'z') ) {
        tat_u->esc.ape = c3n;
        _term_io_belt(uty_u, u3nc(c3__met, cay_y));
      }
      else if ( '.' == cay_y ) {
        tat_u->esc.ape = c3n;
        _term_io_belt(uty_u, u3nc(c3__met, c3__dot));
      }
      else if ( 8 == cay_y || 127 == cay_y ) {
        tat_u->esc.ape = c3n;
        _term_io_belt(uty_u, u3nc(c3__met, c3__bac));
      }
      else if ( ('[' == cay_y) || ('O' == cay_y) ) {
        tat_u->esc.bra = c3y;
      }
      else {
        tat_u->esc.ape = c3n;

        _term_it_write_txt(uty_u, uty_u->ufo_u.out.bel_y);
      }
    }
  }
  else if ( 0 != tat_u->fut.wid_w ) {
    tat_u->fut.syb_y[tat_u->fut.len_w++] = cay_y;

    if ( tat_u->fut.len_w == tat_u->fut.wid_w ) {
      u3_noun huv = u3i_bytes(tat_u->fut.wid_w, tat_u->fut.syb_y);
      u3_noun wug;

      // u3l_log("muck-utf8 len %d\n", tat_u->fut.len_w);
      // u3l_log("muck-utf8 %x\n", huv);
      wug = u3do("taft", huv);
      // u3l_log("muck-utf32 %x\n", tat_u->fut.len_w);

      tat_u->fut.len_w = tat_u->fut.wid_w = 0;
      _term_io_belt(uty_u, u3nt(c3__txt, wug, u3_nul));
    }
  }
  else {
    if ( (cay_y >= 32) && (cay_y < 127) ) {
      _term_io_belt(uty_u, u3nt(c3__txt, cay_y, u3_nul));
    }
    else if ( 0 == cay_y ) {
      _term_it_write_txt(uty_u, uty_u->ufo_u.out.bel_y);
    }
    else if ( 8 == cay_y || 127 == cay_y ) {
      _term_io_belt(uty_u, u3nc(c3__bac, u3_nul));
    }
    else if ( 13 == cay_y ) {
      _term_io_belt(uty_u, u3nc(c3__ret, u3_nul));
    }
#if 0
    else if ( 6 == cay_y ) {
      _term_io_flow(uty_u);   // XX hack
    }
#endif
    else if ( cay_y <= 26 ) {
      _term_io_belt(uty_u, u3nc(c3__ctl, ('a' + (cay_y - 1))));
    }
    else if ( 27 == cay_y ) {
      tat_u->esc.ape = c3y;
    }
    else if ( cay_y >= 128 ) {
      tat_u->fut.len_w = 1;
      tat_u->fut.syb_y[0] = cay_y;

      if ( cay_y < 224 ) {
        tat_u->fut.wid_w = 2;
      } else if ( cay_y < 240 ) {
        tat_u->fut.wid_w = 3;
      } else tat_u->fut.wid_w = 4;
    }
  }
}

/* _term_suck(): process a chunk of input
*/

/*
 * `nread` (siz_w) is > 0 if there is data available, 0 if libuv is done reading for
 * now, or < 0 on error.
 *
 * The callee is responsible for closing the stream when an error happens
 * by calling uv_close(). Trying to read from the stream again is undefined.
 *
 * The callee is responsible for freeing the buffer, libuv does not reuse it.
 * The buffer may be a null buffer (where buf->base=NULL and buf->len=0) on
 * error.
 */

static inline void
_term_suck(u3_utty* uty_u, const c3_y* buf, ssize_t siz_i)
{
  {
    if ( siz_i == UV_EOF ) {
      //  We hear EOF (on the third read callback) if
      //  2x the _term_alloc() buffer size is pasted.
      //  The process hangs if we do nothing (and ctrl-z
      //  then corrupts the event log), so we force shutdown.
      //
      u3l_log("term: hangup (EOF)\r\n");
      u3_pier_exit(u3_pier_stub());
    }
    else if ( siz_i < 0 ) {
      u3l_log("term %d: read: %s\n", uty_u->tid_l, uv_strerror(siz_i));
    }
    else {
      c3_i i;

      for ( i=0; i < siz_i; i++ ) {
        _term_io_suck_char(uty_u, buf[i]);
      }
    }
  }
}

/* _term_read_cb(): server read callback.
*/
static void
_term_read_cb(uv_stream_t* tcp_u,
              ssize_t      siz_i,
              const uv_buf_t *     buf_u)
{
  u3_utty* uty_u = (u3_utty*)(void*)tcp_u;
  _term_suck(uty_u, (const c3_y*)buf_u->base, siz_i);
  free(buf_u->base);
}

/* _term_spin_write_str(): write null-terminated string
*/
static void
_term_spin_write_str(u3_utty*    uty_u,
                     const c3_c* str_c)
{
  // c3_i fid_i = uv_fileno(&uty_u->pop_u);
  c3_i fid_i = uty_u->pop_u.io_watcher.fd;  //  XX old libuv
  c3_w len_w = strlen(str_c);

  if (  len_w != write(fid_i, str_c, len_w) ) {
    // ignore, we just tryin
  }
}

/* _term_spin_move_left(): move the cursor left
*/
static void
_term_spin_move_left(u3_utty* uty_u)
{
  _term_spin_write_str(uty_u, (const c3_c*)uty_u->ufo_u.out.cub1_y);
}

/* _term_spin_timer_cb(): render spinner
*/
static void
_term_spin_timer_cb(uv_timer_t* tim_u)
{
  u3_utty* uty_u = tim_u->data;
  u3_utat* tat_u = &uty_u->tat_u;

  c3_w cus_w = tat_u->mir.cus_w;
  c3_l col_l = tat_u->siz.col_l;

  if ( cus_w >= col_l ) {  //  shenanigans!
    return;
  }

  c3_w bac_w = col_l - 1 - cus_w;  //  backoff from end of line
  c3_d lag_d = tat_u->sun_u.eve_d++;

  const c3_c daz_c[] = "|/-\\";
  const c3_c dal_c[] = "\xc2\xab";
  const c3_c dar_c[] = "\xc2\xbb";

  c3_c buf_c[1 + 2 +  4  + 2 + 1];
  //         | + « + why + » + \0

  c3_c* cur_c = buf_c;

  *cur_c++ = daz_c[lag_d % strlen(daz_c)];
  c3_w sol_w = 1;  //  spinner length (utf-32)

  if ( tat_u->sun_u.why_c ) {
    strncpy(cur_c, dal_c, 2);
    cur_c += 2;
    sol_w += 1;  //  length of dal_c (utf-32)

    // c3_w wel_w = strlen(tat_u.sun_u->why_c);
    strncpy(cur_c, tat_u->sun_u.why_c, 4);
    cur_c += 4;
    sol_w += 4;  // XX assumed utf-8

    strncpy(cur_c, dar_c, 2);
    cur_c += 2;
    sol_w += 1;  //  length of dar_c (utf-32)
  }

  *cur_c = '\0';

  // One-time cursor backoff.
  if ( c3n == tat_u->sun_u.diz_o ) {
    c3_w i_w;
    for ( i_w = bac_w; i_w < sol_w; i_w++ ) {
      _term_spin_move_left(uty_u);
    }
  }

  _term_spin_write_str(uty_u, buf_c);
  tat_u->sun_u.diz_o = c3y;

  //  Cursor stays on spinner.
  while ( sol_w-- ) {
    _term_spin_move_left(uty_u);
  }
}

#define _SPIN_COOL_US 500UL  //  spinner activation delay when cool
#define _SPIN_WARM_US 50UL   //  spinner activation delay when warm
#define _SPIN_RATE_US 250UL  //  spinner rate (ms/frame)
#define _SPIN_IDLE_US 500UL  //  spinner cools down if stopped this long

/* u3_term_start_spinner(): prepare spinner state
*/
void
u3_term_start_spinner(c3_c* why_c, c3_o now_o)
{
  if ( c3n == u3_Host.ops_u.tem ) {
    u3_utty* uty_u = _term_main();
    u3_utat* tat_u = &uty_u->tat_u;

    free(tat_u->  sun_u.why_c);
    tat_u->sun_u.why_c = why_c;

    tat_u->sun_u.eve_d = 0;
    // XX must be c3n for cursor backoff from EOL?
    tat_u->sun_u.diz_o = c3n;

    {
      c3_d now_d = _term_msc_out_host();
      c3_d end_d = tat_u->sun_u.end_d;
      c3_d wen_d = (c3y == now_o) ? 0UL :
                     (now_d - end_d < _SPIN_IDLE_US) ?
                     _SPIN_WARM_US : _SPIN_COOL_US;

      uv_timer_start(&tat_u->sun_u.tim_u,
                     _term_spin_timer_cb,
                     wen_d, _SPIN_RATE_US);
    }
  }
}

/* u3_term_stop_spinner(): reset spinner state and restore input line.
*/
void
u3_term_stop_spinner(void)
{
  if ( c3n == u3_Host.ops_u.tem ) {
    u3_utty* uty_u = _term_main();
    u3_utat* tat_u = &uty_u->tat_u;

    uv_timer_stop(&tat_u->sun_u.tim_u);

    if ( c3y == tat_u->sun_u.diz_o ) {
      _term_it_refresh_line(uty_u);
      tat_u->sun_u.end_d = _term_msc_out_host();
      tat_u->sun_u.diz_o = c3n;
    }
    else {
      tat_u->sun_u.end_d = 0;
    }
  }
}

/* _term_main(): return main or console terminal.
*/
static u3_utty*
_term_main()
{
  u3_utty* uty_u;

  for ( uty_u = u3_Host.uty_u; uty_u; uty_u = uty_u->nex_u ) {
    if ( (uty_u->fid_i != -1) && (uty_u->fid_i <= 2) ) {
      return uty_u;
    }
  }
  return u3_Host.uty_u;
}

/* _term_ef_get(): terminal by id.
*/
static u3_utty*
_term_ef_get(c3_l     tid_l)
{
  if ( 0 != tid_l ) {
    u3_utty* uty_u;

    for ( uty_u = u3_Host.uty_u; uty_u; uty_u = uty_u->nex_u ) {
      if ( tid_l == uty_u->tid_l ) {
        return uty_u;
      }
    }
  }
  return _term_main();
}

/* u3_term_get_blew(): return window size [columns rows].
*/
u3_noun
u3_term_get_blew(c3_l tid_l)
{
  u3_utty*       uty_u = _term_ef_get(tid_l);
  c3_l           col_l, row_l;

  struct winsize siz_u;
  if ( (c3n == u3_Host.ops_u.tem) &&
       uty_u && (0 == ioctl(uty_u->fid_i, TIOCGWINSZ, &siz_u)) )
  {
    col_l = siz_u.ws_col;
    row_l = siz_u.ws_row;
  }
  else {
    col_l = 80;
    row_l = 24;
  }

  if ( uty_u ) {
    uty_u->tat_u.siz.col_l = col_l;
    uty_u->tat_u.siz.row_l = row_l;
  }

  return u3nc(col_l, row_l);
}

/* u3_term_ef_winc(): window change.  Just console right now.
*/
void
u3_term_ef_winc(void)
{
  u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);

  u3_pier_plan(pax, u3nc(c3__blew, u3_term_get_blew(1)));
}

/* u3_term_ef_ctlc(): send ^C on console.
*/
void
u3_term_ef_ctlc(void)
{
  u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);

  u3_pier_plan(pax, u3nt(c3__belt, c3__ctl, 'c'));

  _term_it_refresh_line(_term_main());
}

/* u3_term_ef_verb(): initial effects for verbose events
*/
void
u3_term_ef_verb(void)
{
  u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);

  u3_pier_plan(pax, u3nc(c3__verb, u3_nul));
}

/* u3_term_ef_bake(): initial effects for new terminal.
*/
void
u3_term_ef_bake(void)
{
  u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);

  // u3_pier_plan(u3k(pax), u3nq(c3__flow, c3__seat, c3__dojo, u3_nul));
  u3_pier_plan(u3k(pax), u3nc(c3__blew, u3_term_get_blew(1)));
  u3_pier_plan(u3k(pax), u3nc(c3__hail, u3_nul));

  u3z(pax);
}

/* _term_ef_blit(): send blit to terminal.
*/
static void
_term_ef_blit(u3_utty* uty_u,
              u3_noun  blt)
{
  switch ( u3h(blt) ) {
    default: break;

    case c3__bel: {
      if ( c3n == u3_Host.ops_u.tem ) {
        _term_it_write_txt(uty_u, uty_u->ufo_u.out.bel_y);
      }
    } break;

    case c3__clr: {
      if ( c3n == u3_Host.ops_u.tem ) {
        _term_it_show_blank(uty_u);
        _term_it_refresh_line(uty_u);
      }
    } break;

    case c3__hop: {
      if ( c3n == u3_Host.ops_u.tem ) {
        _term_it_show_cursor(uty_u, u3t(blt));
      }
    } break;

    case c3__lin: {
      u3_noun lin = u3t(blt);
      c3_w    len_w = u3kb_lent(u3k(lin));
      c3_w*   lin_w = c3_malloc(4 * len_w);

      {
        c3_w i_w;

        for ( i_w = 0; u3_nul != lin; i_w++, lin = u3t(lin) ) {
          lin_w[i_w] = u3r_word(0, u3h(lin));
        }
      }

      if ( c3n == u3_Host.ops_u.tem ) {
        _term_it_show_clear(uty_u);
        _term_it_show_line(uty_u, lin_w, len_w);
      } else {
        _term_it_show_line(uty_u, lin_w, len_w);
      }
    } break;

    case c3__mor: {
      _term_it_show_more(uty_u);
    } break;

    case c3__sav: {
      _term_it_save(u3k(u3h(u3t(blt))), u3k(u3t(u3t(blt))));
    } break;

    case c3__sag: {
      u3_noun pib = u3k(u3t(u3t(blt)));
      u3_noun jam;

      jam = u3ke_jam(pib);

      _term_it_save(u3k(u3h(u3t(blt))), jam);
    } break;

    case c3__url: {
      if ( c3n == u3ud(u3t(blt)) ) {
        break;
      } else {
        c3_c* txt_c = u3r_string(u3t(blt));

        _term_it_show_clear(uty_u);
        _term_it_write_str(uty_u, txt_c);
        free(txt_c);

        _term_it_show_more(uty_u);
        _term_it_refresh_line(uty_u);
      }
    }
  }
  u3z(blt);

  return;
}

/* u3_term_ef_blit(): send %blit list to specific terminal.
*/
void
u3_term_ef_blit(c3_l     tid_l,
                u3_noun  bls)
{
  u3_utty* uty_u = _term_ef_get(tid_l);

  if ( 0 == uty_u ) {
    // u3l_log("no terminal %d\n", tid_l);
    // u3l_log("uty_u %p\n", u3_Host.uty_u);

    u3z(bls); return;
  }

  {
    u3_noun bis = bls;

    while ( c3y == u3du(bis) ) {
      _term_ef_blit(uty_u, u3k(u3h(bis)));
      bis = u3t(bis);
    }
    u3z(bls);
  }
}

/* u3_term_io_hija(): hijack console for fprintf, returning FILE*.
*/
FILE*
u3_term_io_hija(void)
{
  u3_utty* uty_u = _term_main();

  if ( uty_u ) {
    if ( uty_u->fid_i > 2 ) {
      //  We *should* in fact, produce some kind of fake FILE* for
      //  non-console terminals.  If we use this interface enough...
      //
      c3_assert(0);
    }
    else {
      if ( c3n == u3_Host.ops_u.tem ) {
        if ( 0 != _term_tcsetattr(1, TCSADRAIN, &uty_u->bak_u) ) {
          perror("hija-tcsetattr-1");
          c3_assert(!"hija-tcsetattr");
        }
        if ( -1 == fcntl(1, F_SETFL, uty_u->cug_i) ) {
          perror("hija-fcntl-1");
          c3_assert(!"hija-fcntl");
        }
        if ( 0 != _term_tcsetattr(0, TCSADRAIN, &uty_u->bak_u) ) {
          perror("hija-tcsetattr-0");
          c3_assert(!"hija-tcsetattr");
        }
        if ( -1 == fcntl(0, F_SETFL, uty_u->cug_i) ) {
          perror("hija-fcntl-0");
          c3_assert(!"hija-fcntl");
        }
        _write(uty_u->fid_i, "\r", 1);
        _write(uty_u->fid_i, uty_u->ufo_u.out.el_y,
                            strlen((c3_c*) uty_u->ufo_u.out.el_y));
      }
      return stdout;
    }
  }
  else return stdout;
}

/* u3_term_io_loja(): release console from fprintf.
*/
void
u3_term_io_loja(int x)
{
  u3_utty* uty_u = _term_main();

  if ( uty_u ) {
    if ( uty_u->fid_i > 2 ) {
      //  We *should* in fact, produce some kind of fake FILE* for
      //  non-console terminals.  If we use this interface enough...
      //
      c3_assert(0);
    }
    else {
      if ( c3y == u3_Host.ops_u.tem ) {
        fflush(stdout);
      }
      else {
        if ( 0 != _term_tcsetattr(1, TCSADRAIN, &uty_u->raw_u) ) {
          perror("loja-tcsetattr-1");
          c3_assert(!"loja-tcsetattr");
        }
        if ( -1 == fcntl(1, F_SETFL, uty_u->nob_i) ) {
          perror("hija-fcntl-1");
          c3_assert(!"loja-fcntl");
        }
        if ( 0 != _term_tcsetattr(0, TCSADRAIN, &uty_u->raw_u) ) {
          perror("loja-tcsetattr-0");
          c3_assert(!"loja-tcsetattr");
        }
        if ( -1 == fcntl(0, F_SETFL, uty_u->nob_i) ) {
          perror("hija-fcntl-0");
          c3_assert(!"loja-fcntl");
        }
        _term_it_refresh_line(uty_u);
      }
    }
  }
}

/* u3_term_it_log(): writes a log message
*/
void
u3_term_io_log(c3_c* line)
{
  FILE* stream = u3_term_io_hija();
  u3_term_io_loja(fprintf(stream, "%s", line));
}

/* u3_term_tape_to(): dump a tape to a file.
*/
void
u3_term_tape_to(FILE *fil_f, u3_noun tep)
{
  u3_noun tap = tep;

  while ( u3_nul != tap ) {
    c3_c car_c;

    if ( u3h(tap) >= 127 ) {
      car_c = '?';
    } else car_c = u3h(tap);

    putc(car_c, fil_f);
    tap = u3t(tap);
  }
  u3z(tep);
}

/* u3_term_tape(): dump a tape to stdout.
*/
void
u3_term_tape(u3_noun tep)
{
  FILE* fil_f = u3_term_io_hija();

  u3_term_tape_to(fil_f, tep);

  u3_term_io_loja(0);
}

/* u3_term_wall(): dump a wall to stdout.
*/
void
u3_term_wall(u3_noun wol)
{
  FILE* fil_f = u3_term_io_hija();
  u3_noun wal = wol;

  while ( u3_nul != wal ) {
    u3_term_tape_to(fil_f, u3k(u3h(wal)));

    putc(13, fil_f);
    putc(10, fil_f);

    wal = u3t(wal);
  }
  u3_term_io_loja(0);

  u3z(wol);
}
