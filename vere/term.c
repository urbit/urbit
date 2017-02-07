/* v/term.c
**
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
#include <uv.h>
#include <errno.h>
#include <curses.h>
#include <termios.h>
#include <term.h>
#include <anachronism/common.h>
#include <anachronism/nvt.h>
#include "all.h"
#include "vere/vere.h"

static        void _term_spinner_cb(void*);
static        void _term_read_tn_cb(uv_stream_t* tcp_u,
                                    ssize_t      siz_i,
                                    const uv_buf_t *     buf_u);
static        void _term_read_cb(uv_stream_t* tcp_u,
                                 ssize_t      siz_i,
                                 const uv_buf_t *     buf_u);
static inline void _term_suck(u3_utty*, const c3_y*, ssize_t);
static        void _tel_event(telnet_nvt*, telnet_event*);
static        void _tel_opt(telnet_nvt*, telnet_byte, telnet_telopt_event*);

#define _T_ECHO 1    //  local echo
#define _T_CTIM 3    //  suppress GA/char-at-a-time
#define _T_NAWS 31   //  negotiate about window size

#define _SPIN_COOL_US 500000  //  spinner activation delay when cool
#define _SPIN_WARM_US 50000   //  spinner activation delay when warm
#define _SPIN_RATE_US 250000  //  spinner rate (microseconds/frame)
#define _SPIN_IDLE_US 500000  //  spinner cools down if stopped this long

/* _term_msc_out_host(): unix microseconds from current host time.
*/
static c3_d
_term_msc_out_host()
{
  struct timeval tim_tv;
  gettimeofday(&tim_tv, 0);
  return 1000000ULL * tim_tv.tv_sec + tim_tv.tv_usec;
}

static void
_term_alloc(uv_handle_t* had_u,
            size_t len_i,
            uv_buf_t* buf
            )
{
  void* ptr_v = c3_malloc(len_i);
  *buf = uv_buf_init(ptr_v, len_i);
}


/* _term_close_cb(): free terminal.
*/
static void
_term_close_cb(uv_handle_t* han_t)
{
  u3_utty* tty_u = (void*) han_t;
  if ( u3_Host.uty_u == tty_u ) {
    u3_Host.uty_u = tty_u->nex_u;
  }
  else {
    u3_utty* uty_u;
    for (uty_u = u3_Host.uty_u; uty_u; uty_u = uty_u->nex_u ) {
      if ( uty_u->nex_u == tty_u ) {
        uty_u->nex_u = tty_u->nex_u;
        break;
      }
    }
  }

  {
    u3_noun tid = u3dc("scot", c3__ud, tty_u->tid_l);
    u3_noun pax = u3nq(u3_blip, c3__term, tid, u3_nul);
    u3_pier_plan(u3k(pax), u3nc(c3__hook, u3_nul));
    u3z(pax);
  }
  free(tty_u);
}

/* _tel_close_cb(): close telnet terminal
*/
static void
_tel_close_cb(uv_handle_t* han_t)
{
  u3_utel* pty_u = (u3_utel*)(void*)han_t;
  telnet_nvt_free(pty_u->tel_u);
  _term_close_cb(han_t);
}

/* u3_term_io_init(): initialize terminal.
*/
void
u3_term_io_init()
{
  u3_utty* uty_u = calloc(1, sizeof(u3_utty));

  if ( c3y == u3_Host.ops_u.dem ) {
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

      uty_u->ufo_u.inn.max_w = 0;

      _utfo(inn, kcuu1);
      _utfo(inn, kcud1);
      _utfo(inn, kcub1);
      _utfo(inn, kcuf1);

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

  if ( c3n == u3_Host.ops_u.dem ) {
    //  Start raw input.
    //
    {
      if ( 0 != tcsetattr(uty_u->fid_i, TCSADRAIN, &uty_u->raw_u) ) {
        c3_assert(!"init-tcsetattr");
      }
      if ( -1 == fcntl(uty_u->fid_i, F_SETFL, uty_u->nob_i) ) {
        c3_assert(!"init-fcntl");
      }
    }

    //  Start spinner thread.
    //
    {
      uty_u->tat_u.sun.sit_u = (uv_thread_t*)malloc(sizeof(uv_thread_t));
      if ( uty_u->tat_u.sun.sit_u ) {
        uv_mutex_init(&uty_u->tat_u.mex_u);
        uv_mutex_lock(&uty_u->tat_u.mex_u);

        c3_w ret_w = uv_thread_create(uty_u->tat_u.sun.sit_u,
                                      _term_spinner_cb,
                                      uty_u);
        if ( 0 != ret_w ) {
          uL(fprintf(uH, "term: spinner start: %s\n", uv_strerror(ret_w)));
          free(uty_u->tat_u.sun.sit_u);
          uty_u->tat_u.sun.sit_u = NULL;
          uv_mutex_unlock(&uty_u->tat_u.mex_u);
          uv_mutex_destroy(&uty_u->tat_u.mex_u);
        }
      }
    }
  }
}

void
_term_listen_cb(uv_stream_t *wax_u, int sas_i)
{
  u3_utel* pty_u = calloc(1, sizeof(*pty_u));
  u3_utty* tty_u = &pty_u->uty_t;
  uv_tcp_init(u3L, &tty_u->wax_u);
  c3_w ret_w;
  if ( 0 != (ret_w = uv_accept(wax_u, (uv_stream_t*)&tty_u->wax_u)) ) {
    uL(fprintf(uH, "term: accept: %s\n",
                    uv_strerror(ret_w)));

    uv_close((uv_handle_t*)&tty_u->wax_u, NULL);
    free(tty_u);
  }
  else {
    uv_read_start((uv_stream_t*)&tty_u->wax_u,
                  _term_alloc,
                  _term_read_tn_cb);

    tty_u->ufo_u.out.clear_y = (const c3_y*)"\033[H\033[J";
    tty_u->ufo_u.out.el_y    = (const c3_y*)"\033[K";
    tty_u->ufo_u.out.ed_y    = (const c3_y*)"\033[J";
    tty_u->ufo_u.out.bel_y   = (const c3_y*)"\007";
    tty_u->ufo_u.out.cub1_y  = (const c3_y*)"\010";
    tty_u->ufo_u.out.cud1_y  = (const c3_y*)"\033[B";
    tty_u->ufo_u.out.cuu1_y  = (const c3_y*)"\033[A";
    tty_u->ufo_u.out.cuf1_y  = (const c3_y*)"\033[C";

    tty_u->ufo_u.inn.kcuu1_y = (const c3_y*)"\033[A";
    tty_u->ufo_u.inn.kcud1_y = (const c3_y*)"\033[B";
    tty_u->ufo_u.inn.kcuf1_y = (const c3_y*)"\033[C";
    tty_u->ufo_u.inn.kcub1_y = (const c3_y*)"\033[D";
    tty_u->ufo_u.inn.max_w = strlen("\033[D");

    tty_u->fid_i = -1;

    tty_u->tat_u.mir.lin_w = 0;
    tty_u->tat_u.mir.len_w = 0;
    tty_u->tat_u.mir.cus_w = 0;

    tty_u->tat_u.esc.ape = c3n;
    tty_u->tat_u.esc.bra = c3n;

    tty_u->tat_u.fut.len_w = 0;
    tty_u->tat_u.fut.wid_w = 0;

    tty_u->tat_u.siz.col_l = 80;
    tty_u->tat_u.siz.row_l = 25;

    tty_u->tid_l = u3_Host.uty_u->tid_l + 1;
    tty_u->nex_u = u3_Host.uty_u;
    u3_Host.uty_u = tty_u;
    pty_u->tel_u = telnet_nvt_new(tty_u, _tel_event, _tel_opt, NULL);

    {
      u3_noun tid = u3dc("scot", c3__ud, tty_u->tid_l);
      u3_noun pax = u3nq(u3_blip, c3__term, tid, u3_nul);
      // u3_pier_plan(u3k(pax), u3nq(c3__flow, c3__seat, c3__dojo, u3_nul));
      u3_pier_plan(u3k(pax), u3nc(c3__blew, u3nc(80, 25)));
      u3_pier_plan(u3k(pax), u3nc(c3__hail, u3_nul));
      u3z(pax);
    }

    telnet_telopt_enable(pty_u->tel_u, _T_ECHO, TELNET_LOCAL);
    telnet_telopt_enable(pty_u->tel_u, _T_CTIM, TELNET_LOCAL);
    telnet_telopt_enable(pty_u->tel_u, _T_NAWS, TELNET_REMOTE);
  }
}

void
u3_term_io_talk(void)
{
  struct sockaddr_in add_u;
  u3_utel* tel_u = &u3_Host.tel_u;

  uv_tcp_init(u3L, &tel_u->uty_t.wax_u);
  tel_u->por_s = 10023;

  memset(&add_u, 0, sizeof(add_u));
  add_u.sin_family = AF_INET;
  add_u.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

  /*  Try ascending ports.
  */
  while ( 1 ) {
    add_u.sin_port = htons(tel_u->por_s);

    c3_w ret ;
    if ( 0 != (ret = uv_tcp_bind(&tel_u->uty_t.wax_u, (const struct sockaddr*) & add_u, 0))  ) {
      if ( UV_EADDRINUSE == ret ) {
        tel_u->por_s++;
        continue;
      }
      else {
        uL(fprintf(uH, "term: bind: %s\n", uv_strerror(ret)));
      }
    }
    c3_w ret_w;
    if ( 0 != (ret_w = uv_listen((uv_stream_t*)&tel_u->uty_t.wax_u,
                               16, _term_listen_cb)) )
    {
      if ( UV_EADDRINUSE == ret_w ) {
        tel_u->por_s++;
        continue;
      }
      else {
        uL(fprintf(uH, "term: listen: %s\n", uv_strerror(ret_w)));
      }
    }
#if 0
    uL(fprintf(uH, "term: live (but unsupported) on %d\n", tel_u->por_s));
#endif
    break;
  }
}

/* u3_term_io_exit(): clean up terminal.
*/
void
u3_term_io_exit(void)
{
  if ( c3y == u3_Host.ops_u.dem ) {
    uv_close((uv_handle_t*)&u3_Host.uty_u->pop_u, NULL);
  }
  else {
    u3_utty* uty_u;

    for ( uty_u = u3_Host.uty_u; uty_u; uty_u = uty_u->nex_u ) {
      if ( uty_u->fid_i == -1 ) { continue; }
      if ( 0 != tcsetattr(uty_u->fid_i, TCSADRAIN, &uty_u->bak_u) ) {
        c3_assert(!"exit-tcsetattr");
      }
      if ( -1 == fcntl(uty_u->fid_i, F_SETFL, uty_u->cug_i) ) {
        c3_assert(!"exit-fcntl");
      }
      write(uty_u->fid_i, "\r\n", 2);

#if 0
      if ( uty_u->tat_u.sun.sit_u ) {
        uv_thread_t* sit_u = uty_u->tat_u.sun.sit_u;
        uty_u->tat_u.sun.sit_u = NULL;

        uv_mutex_unlock(&uty_u->tat_u.mex_u);

        //  XX can block exit waiting for wakeup (max _SPIN_COOL_US)
        c3_w ret_w;
        if ( 0 != (ret_w = uv_thread_join(sit_u)) ) {
          uL(fprintf(uH, "term: spinner exit: %s\n", uv_strerror(ret_w)));
        }
        else {
          uv_mutex_destroy(&uty_u->tat_u.mex_u);
        }

        free(sit_u);
      }
#endif
    }
  }
}

void
u3_term_io_poll(void)
{
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

/* An unusual lameness in libuv.
*/
  typedef struct {
    uv_write_t wri_u;
    c3_y*      buf_y;
  } _u3_write_t;

/* _term_write_cb(): general write callback.
*/
static void
_term_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  _u3_write_t* ruq_u = (void *)wri_u;

  if ( 0 != sas_i ) {
    // uL(fprintf(uH, "term: write: ERROR\n"));
  }
  free(ruq_u->buf_y);
  free(ruq_u);
}

/* _term_it_write_buf(): write buffer uv style.
*/
static void
_term_it_write_buf(u3_utty* uty_u, uv_buf_t buf_u)
{
  _u3_write_t* ruq_u = (_u3_write_t*) c3_malloc(sizeof(_u3_write_t));

  ruq_u->buf_y = (c3_y*)buf_u.base;

  c3_w ret_w;
  if ( 0 != (ret_w = uv_write(&ruq_u->wri_u,
                     (uv_stream_t*)&(uty_u->pop_u),
                     &buf_u, 1,
                              _term_write_cb)) )
  {
    uL(fprintf(uH, "terminal: %s\n", uv_strerror(ret_w)));
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
  uty_u->tat_u.mir.cus_w = 0;
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
  if ( c3y == u3_Host.ops_u.dem ) {
    _term_it_write_str(uty_u, "\n");
  } else {
    _term_it_write_str(uty_u, "\r\n");
  }
  uty_u->tat_u.mir.cus_w = 0;
}

/* _term_it_path(): path for console file.
*/
static c3_c*
_term_it_path(u3_noun fyl, u3_noun pax)
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

  pax = u3nt(c3_s4('.','u','r','b'), c3_s3('p','u','t'), pax);
  pax_c = _term_it_path(c3y, pax);

  u3_walk_save(pax_c, 0, pad);
  free(pax_c);
}

/* _term_io_belt(): send belt.
*/
static void
_term_io_belt(u3_utty* uty_u, u3_noun  blb)
{
  u3_noun tid = u3dc("scot", c3__ud, uty_u->tid_l);
  u3_noun pax = u3nq(u3_blip, c3__term, tid, u3_nul);

  u3_pier_plan(pax, u3nc(c3__belt, blb));
}

/* _tel_event(): telnet sucker
*/
#define _te_nvt telnet_nvt
#define _te_evt telnet_event
#define _te_dvt telnet_data_event
#define _te_svt telnet_send_event
static void
_tel_event(_te_nvt* nvt, _te_evt* evt)
{
  u3_utel* tel_u;
  c3_assert(0 < telnet_get_userdata(nvt, (void**)&tel_u));
  switch (evt->type)
  {
    case TELNET_EV_DATA:
    {
      _te_dvt* dv = (_te_dvt*)evt;
      _term_suck((u3_utty*)tel_u, dv->data, dv->length);
      break;
    }

    case TELNET_EV_SEND:
    {
      _te_svt* sv = (_te_svt*)evt;
      _term_it_write_bytes((u3_utty*)tel_u, sv->length, sv->data);
      break;
    }
    default:
    {
      break;
    }
  }
}

#define _to_evt telnet_telopt_event
#define _to_dvt telnet_telopt_data_event
#define _to_tvt telnet_telopt_toggle_event
/* _tel_opt(): telnet event sucker
*/
static void
_tel_opt(_te_nvt* nvt, telnet_byte opt, _to_evt* evt)
{
  switch (evt->type)
  {
    default: break;
    case TELNET_EV_TELOPT_DATA:
    {
      _to_dvt* dv = (_to_dvt*)evt;
      u3_utel* tel_u;
      u3_noun pax;
      u3_noun blu;
      u3_noun tid;
      c3_s col_s;
      c3_s row_s;

      if ( opt != _T_NAWS ) {
        return;
      }

      c3_assert(0 < telnet_get_userdata(nvt, (void**)&tel_u));

      col_s = dv->data[1] | (dv->data[0] << 8);
      row_s = dv->data[3] | (dv->data[2] << 8);

      tel_u->uty_t.tat_u.siz.col_l = col_s;
      tel_u->uty_t.tat_u.siz.row_l = row_s;

      tid = u3dc("scot", c3__ud, tel_u->uty_t.tid_l);
      pax = u3nq(u3_blip, c3__term, tid, u3_nul);
      blu = u3nc(col_s, row_s);
      u3_pier_plan(pax, u3nc(c3__blew, blu));
      break;
    }
  }
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

      // uL(fprintf(uH, "muck-utf8 len %d\n", tat_u->fut.len_w));
      // uL(fprintf(uH, "muck-utf8 %x\n", huv));
      wug = u3do("turf", huv);
      // uL(fprintf(uH, "muck-utf32 %x\n", tat_u->fut.len_w));

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

/* _term_read_tn_cb(): telnet read callback.
*/
static void
_term_read_tn_cb(uv_stream_t* tcp_u,
                   ssize_t      siz_i,
                   const uv_buf_t *     buf_u)
{
  u3_utel* pty_u = (u3_utel*)(void*) tcp_u;

  {
    if ( siz_i == UV_EOF ) {
      // nothing
    } else if ( siz_i < 0 ) {
      uL(fprintf(uH, "term teln: read: %s\n", uv_strerror(siz_i)));
      uv_close((uv_handle_t*) tcp_u, _tel_close_cb);
      goto err;
    }
    else {
      telnet_receive(pty_u->tel_u, (const telnet_byte*) buf_u->base, siz_i, 0);
    }

  err:
    free(buf_u->base);
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
      // nothing
    } else   if ( siz_i < 0 ) {
      uL(fprintf(uH, "term %d: read: %s\n", uty_u->tid_l, uv_strerror(siz_i)));
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

/* _term_try_write_str(): write null-terminated string (off-thread, retain).
*/
static void
_term_try_write_str(u3_utty*    uty_u,
                    const c3_c* hun_y)
{
  // c3_i fid_i = uv_fileno(&uty_u->pop_u);
  c3_i fid_i = uty_u->pop_u.io_watcher.fd;  //  XX old libuv
  write(fid_i, hun_y, strlen(hun_y));
}

/* _term_try_move_left(): move the cursor left (off-thread).
*/
static void
_term_try_move_left(u3_utty* uty_u)
{
  _term_try_write_str(uty_u, (const c3_c*)uty_u->ufo_u.out.cub1_y);
}

/* _term_show_spinner(): render spinner (off-thread).
*/
static void
_term_show_spinner(u3_utty* uty_u, c3_d lag_d)
{
  if ( 0 == uty_u->tat_u.sun.eve_d ) {
    return;
  }

  c3_w cus_w = uty_u->tat_u.mir.cus_w;

  if ( cus_w >= uty_u->tat_u.siz.col_l ) {  //  shenanigans!
    return;
  }

  c3_w bac_w = uty_u->tat_u.siz.col_l - 1 - cus_w;  //  backoff from end of line

  const c3_c daz_c[] = "|/-\\";
  const c3_c dal_c[] = "\xc2\xab";
  const c3_c dar_c[] = "\xc2\xbb";

  c3_c buf_c[1 + 2 +  4  + 2 + 1];
  //         | + « + why + » + \0

  c3_c* cur_c = buf_c;

  *cur_c++ = daz_c[(lag_d / _SPIN_RATE_US) % strlen(daz_c)];
  c3_w sol_w = 1;  //  spinner length (utf-32)

  c3_c* why_c = uty_u->tat_u.sun.why_c;
  if ( why_c && strlen(why_c) <= 4 ) {
    strcpy(cur_c, dal_c);
    cur_c += strlen(dal_c);
    sol_w += 1;  //  length of dal_c (utf-32)

    c3_w wel_w = strlen(why_c);
    strcpy(cur_c, why_c);
    cur_c += wel_w;
    sol_w += wel_w;

    strcpy(cur_c, dar_c);
    cur_c += strlen(dar_c);
    sol_w += 1;  //  length of dar_c (utf-32)
  }
  *cur_c = '\0';

  //  One-time cursor backoff.
  if ( c3n == uty_u->tat_u.sun.diz_o ) {
    c3_w i_w;
    for ( i_w = bac_w; i_w < sol_w; i_w++ ) {
      _term_try_move_left(uty_u);
    }
  }

  _term_try_write_str(uty_u, buf_c);
  uty_u->tat_u.sun.diz_o = c3y;

  //  Cursor stays on spinner.
  while ( sol_w-- ) {
    _term_try_move_left(uty_u);
  }
}

/* _term_start_spinner(): prepare spinner state. RETAIN.
*/
static void
_term_start_spinner(u3_utty* uty_u, u3_noun ovo)
{
  uty_u->tat_u.sun.diz_o = c3n;

  c3_d now_d = _term_msc_out_host();

  //  If we receive an event shortly after a previous spin, use a shorter delay
  //  to avoid giving the impression of a half-idle system.
  //
  c3_d lag_d;
  if ( now_d - uty_u->tat_u.sun.end_d < _SPIN_IDLE_US ) {
    lag_d = _SPIN_WARM_US;
  }
  else {
    lag_d = _SPIN_COOL_US;
  }

  u3_noun why = u3h(u3t(u3h(u3t(ovo))));
  if ( c3__term == why ) {
    u3_noun eve = u3t(u3t(ovo));
    if ( c3__belt == u3h(eve) && c3__ret == u3h(u3t(eve)) ) {
      lag_d = 0;  //  No delay for %ret.
    }
  }
  else {
    uty_u->tat_u.sun.why_c = (c3_c*)u3r_string(why);
  }

  uty_u->tat_u.sun.eve_d = now_d + lag_d;

  uv_mutex_unlock(&uty_u->tat_u.mex_u);
}

/* _term_stop_spinner(): reset spinner state and restore input line.
*/
static void
_term_stop_spinner(u3_utty* uty_u)
{
  uv_mutex_lock(&uty_u->tat_u.mex_u);

  if ( c3y == uty_u->tat_u.sun.diz_o ) {
    _term_it_refresh_line(uty_u);
    uty_u->tat_u.sun.end_d = _term_msc_out_host();
  }
  else {
    uty_u->tat_u.sun.end_d = 0;
  }

  uty_u->tat_u.sun.diz_o = c3n;
  uty_u->tat_u.sun.eve_d = 0;
  free(uty_u->tat_u.sun.why_c);
  uty_u->tat_u.sun.why_c = NULL;
}

/* _term_spinner_cb(): manage spinner (off-thread).
*/
static void
_term_spinner_cb(void* ptr_v)
{
  //  This thread shouldn't receive signals.
  //
  {
    sigset_t set;
    sigfillset(&set);
    pthread_sigmask(SIG_BLOCK, &set, NULL);
  }

  u3_utty* uty_u = (u3_utty*)ptr_v;

  for ( uv_mutex_lock(&uty_u->tat_u.mex_u);
        uty_u->tat_u.sun.sit_u;
        uv_mutex_lock(&uty_u->tat_u.mex_u) )
  {
    c3_d eve_d = uty_u->tat_u.sun.eve_d;

    if ( 0 == eve_d ) {
      c3_o diz_o = uty_u->tat_u.sun.diz_o;
      uv_mutex_unlock(&uty_u->tat_u.mex_u);
      usleep(c3y == diz_o ? _SPIN_WARM_US : _SPIN_COOL_US);
    }
    else {
      c3_d now_d = _term_msc_out_host();

      if (now_d < eve_d) {
        uv_mutex_unlock(&uty_u->tat_u.mex_u);
        usleep(eve_d - now_d);
      }
      else {
        _term_show_spinner(uty_u, now_d - eve_d);
        uv_mutex_unlock(&uty_u->tat_u.mex_u);
        usleep(_SPIN_RATE_US);
      }
    }
  }

  uv_mutex_unlock(&uty_u->tat_u.mex_u);
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
  if ( uty_u && (0 == ioctl(uty_u->fid_i, TIOCGWINSZ, &siz_u)) ) {
    col_l = siz_u.ws_col;
    row_l = siz_u.ws_row;
  } else {
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
}

/* u3_term_ef_boil(): initial effects for loaded servers.
*/
void
u3_term_ef_boil(void)
{
  {
    u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);

    //  u3_pier_plan(u3k(pax), u3nc(c3__init, u3k(u3h(u3A->own))));
    u3_pier_plan(u3k(pax), u3nc(c3__harm, u3_nul));
    u3_pier_plan(u3k(pax), u3nc(c3__blew, u3_term_get_blew(1)));
    u3_pier_plan(u3k(pax), u3nc(c3__hail, u3_nul));

    u3z(pax);
  }
}

/* u3_term_ef_verb(): initial effects for verbose events
*/
void
u3_term_ef_verb(void)
{
  u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);

  u3_pier_plan(pax, u3nc(c3__verb, u3_nul));
}

/* u3_term_ef_ticket(): initial effects for new ticket.
*/
void
u3_term_ef_ticket(c3_c* who_c, c3_c* tic_c)
{
  u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);
  u3_noun who, tic;
  u3_noun whu, tuc; 
  
  whu = u3dc("slaw", 'p', u3i_string(who_c));
  if ( u3_nul == whu ) {
    fprintf(stderr, "ticket: invalid planet '%s'\r\n", who_c);
    exit(1);
  }
  else { who = u3k(u3t(whu)); u3z(whu); }

  tuc = u3dc("slaw", 'p', u3i_string(tic_c));
  if ( u3_nul == tuc ) {
    fprintf(stderr, "ticket: invalid secret '%s'\r\n", tic_c);
    exit(1);
  }
  else { tic = u3k(u3t(tuc)); u3z(tuc); }

  u3_pier_plan(pax, u3nt(c3__tick, who, tic));
}

/* u3_term_ef_bake(): initial effects for new terminal.
*/
void
u3_term_ef_bake(u3_noun fav)
{
  u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);

  u3_pier_plan(u3k(pax), u3nc(c3__boot, fav));
  // u3_pier_plan(u3k(pax), u3nq(c3__flow, c3__seat, c3__dojo, u3_nul));
  u3_pier_plan(u3k(pax), u3nc(c3__blew, u3_term_get_blew(1)));
  u3_pier_plan(u3k(pax), u3nc(c3__hail, u3_nul));

  u3z(pax);
}
void
u3_term_ef_bunk()
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
    case c3__bee: {
      if ( c3n == u3_Host.ops_u.dem ) {
        if ( u3_nul == u3t(blt) ) {
          _term_stop_spinner(uty_u);
        }
        else {
          _term_start_spinner(uty_u, u3t(blt));
        }
      }
    } break;

    case c3__bel: {
      if ( c3n == u3_Host.ops_u.dem ) {
        _term_it_write_txt(uty_u, uty_u->ufo_u.out.bel_y);
      }
    } break;

    case c3__clr: {
      if ( c3n == u3_Host.ops_u.dem ) {
        _term_it_show_blank(uty_u);
        _term_it_refresh_line(uty_u);
      }
    } break;

    case c3__hop: {
      if ( c3n == u3_Host.ops_u.dem ) {
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

      if ( c3n == u3_Host.ops_u.dem ) {
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
    // uL(fprintf(uH, "no terminal %d\n", tid_l));
    // uL(fprintf(uH, "uty_u %p\n", u3_Host.uty_u));

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
      if ( c3n == u3_Host.ops_u.dem ) {
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
        write(uty_u->fid_i, "\r", 1);
        write(uty_u->fid_i, uty_u->ufo_u.out.el_y,
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
      if ( c3y == u3_Host.ops_u.dem ) {
        fflush(stdout);
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
        _term_it_refresh_line(uty_u);
      }
    }
  }
}
