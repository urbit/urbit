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
#include <termios.h>

#include "all.h"
#include "vere/vere.h"

//  macros for string literal args/buffers
//
//    since (sizeof(s) - 1) is used for vector length, parameters
//    must be appropriately typed. use with care!
//
#define TERM_LIT(s)      sizeof(s) - 1, (const c3_y*)(s)
#define TERM_LIT_BUF(s)  uv_buf_init(s, sizeof(s) - 1)

static u3_utty* _term_main();
static void     _term_read_cb(uv_stream_t*    tcp_u,
                              ssize_t         siz_i,
                              const uv_buf_t* buf_u);
static c3_i     _term_tcsetattr(c3_i, c3_i, const struct termios*);

/* _write(): retry interrupts, continue partial writes, assert errors.
*/
static void
_write(c3_i fid_i, const void* buf_v, size_t len_i)
{
  ssize_t ret_i;

  while ( len_i > 0 ) {
    c3_w lop_w = 0;
    //  retry interrupt/async errors
    //
    do {
      //  abort pathological retry loop
    //
    if ( 100 == ++lop_w ) {
      fprintf(stderr, "term: write loop: %s\r\n", strerror(errno));
      return;
    }
      ret_i = write(fid_i, buf_v, len_i);
    }
    while (  (ret_i < 0)
          && (  (errno == EINTR)
             || (errno == EAGAIN)
             || (errno == EWOULDBLOCK) ));

    //  assert on true errors
    //
    //    NB: can't call u3l_log here or we would re-enter _write()
    //
    if ( ret_i < 0 ) {
      fprintf(stderr, "term: write failed %s\r\n", strerror(errno));
      c3_assert(0);
    }
    //  continue partial writes
    //
    else {
      len_i -= ret_i;
      buf_v += ret_i;
    }
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

//  XX unused, but %hook is in %zuse.
//  implement or remove
//
#if 0
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
  c3_free(tty_u);
}
#endif

/* u3_term_log_init(): initialize terminal for logging
*/
void
u3_term_log_init(void)
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

      if ( !isatty(uty_u->fid_i) ) {
        fprintf(stderr, "vere: unable to initialize terminal (not a tty)\r\n"
                        "      use -t to disable interactivity\r\n");
        u3_king_bail();
      }

      uv_pipe_init(u3L, &(uty_u->pop_u), 0);
      uv_pipe_open(&(uty_u->pop_u), uty_u->fid_i);
    }

    //  configure output escape sequences
    //
    //    our requirements are minimal here, so we bypass terminfo
    //    and simply use constant sequences.
    //
    {
      uty_u->ufo_u.out.clear_u = TERM_LIT_BUF("\033[H\033[2J");
      uty_u->ufo_u.out.el_u    = TERM_LIT_BUF("\033[K");
      // uty_u->ufo_u.out.el1_u   = TERM_LIT_BUF("\033[1K");
      uty_u->ufo_u.out.ed_u    = TERM_LIT_BUF("\033[J");
      uty_u->ufo_u.out.bel_u   = TERM_LIT_BUF("\x7");
      uty_u->ufo_u.out.cub1_u  = TERM_LIT_BUF("\x8");
      uty_u->ufo_u.out.cuf1_u  = TERM_LIT_BUF("\033[C");
      uty_u->ufo_u.out.cuu1_u  = TERM_LIT_BUF("\033[A");
      uty_u->ufo_u.out.cud1_u  = TERM_LIT_BUF("\xa");
      // uty_u->ufo_u.out.cub_u  = TERM_LIT_BUF("\033[%p1%dD");
      // uty_u->ufo_u.out.cuf_u  = TERM_LIT_BUF("\033[%p1%dC");
    }

    //  configure input escape sequences
    //
    //    NB: terminfo reports the wrong sequence for arrow keys on xterms.
    //    disabled, currently unused
    // {
    //   uty_u->ufo_u.inn.kcuu1_u = TERM_LIT_BUF("\033[A");  //  terminfo reports "\033OA"
    //   uty_u->ufo_u.inn.kcud1_u = TERM_LIT_BUF("\033[B");  //  terminfo reports "\033OB"
    //   uty_u->ufo_u.inn.kcuf1_u = TERM_LIT_BUF("\033[C");  //  terminfo reports "\033OC"
    //   uty_u->ufo_u.inn.kcub1_u = TERM_LIT_BUF("\033[D");  //  terminfo reports "\033OD"
    // }

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
      uty_u->tat_u.mir.lin_y = 0;
      uty_u->tat_u.mir.byt_w = 0;
      uty_u->tat_u.mir.wor_w = 0;
      uty_u->tat_u.mir.sap_w = 0;
      uty_u->tat_u.mir.cus_w = 0;

      uty_u->tat_u.esc.ape = c3n;
      uty_u->tat_u.esc.bra = c3n;

      uty_u->tat_u.fut.len_w = 0;
      uty_u->tat_u.fut.wid_w = 0;
    }

    //  default size
    //
    {
      uty_u->tat_u.siz.col_l = 80;
      uty_u->tat_u.siz.row_l = 24;
    }

    //  initialize spinner state
    //
    {
      uty_u->tat_u.sun_u.diz_o = c3n;
      uty_u->tat_u.sun_u.eve_d = 0;
      uty_u->tat_u.sun_u.end_d = 0;
    }
  }

  //  This is terminal 1, linked in host.
  //
  {
    uty_u->tid_l = 1;
    uty_u->nex_u = 0;
    u3_Host.uty_u = uty_u;
  }

  //  if terminal/tty is enabled
  //
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

    //  initialize spinner timeout
    //
    {
      uv_timer_init(u3L, &uty_u->tat_u.sun_u.tim_u);
      uty_u->tat_u.sun_u.tim_u.data = uty_u;
    }
  }
}

/* u3_term_log_exit(): clean up terminal.
*/
void
u3_term_log_exit(void)
{
  if ( c3n == u3_Host.ops_u.tem ) {
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
    }
  }

  if ( u3_Host.uty_u ) {
    uv_close((uv_handle_t*)&u3_Host.uty_u->pop_u, 0);
  }
}

/*  _term_tcsetattr(): tcsetattr w/retry on EINTR.
*/
static c3_i
_term_tcsetattr(c3_i fil_i, c3_i act_i, const struct termios* tms_u)
{
  c3_i ret_i = 0;
  c3_w len_w = 0;

  do {
    //  abort pathological retry loop
    //
    if ( 100 == ++len_w ) {
      fprintf(stderr, "term: tcsetattr loop: %s\r\n", strerror(errno));
      return -1;
    }
    ret_i = tcsetattr(fil_i, act_i, tms_u);
  } while ( (-1 == ret_i) && (EINTR == errno) );

  return ret_i;
}

/* _term_it_write_cb(): general write callback.
*/
static void
_term_it_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  //  write failure is logged, but otherwise ignored.
  //
  if ( 0 != sas_i ) {
    u3l_log("term: write: %s\n", uv_strerror(sas_i));
  }

  c3_free(wri_u->data);
  c3_free(wri_u);
}

/* _term_it_write(): write libuv buffer, freeing pointer.
*/
static void
_term_it_write(u3_utty*  uty_u,
               uv_buf_t* buf_u,
               void*     ptr_v)
{
  //  work off a local copy of the buffer, in case we need
  //  to manipulate the length/pointer
  //
  uv_buf_t     fub_u = { .base = buf_u->base, .len = buf_u->len };
  uv_stream_t* han_u = (uv_stream_t*)&(uty_u->pop_u);
  c3_i         ret_i;

  //  try to write synchronously
  //
  while ( 1 ) {
    ret_i = uv_try_write(han_u, &fub_u, 1);

    if ( (ret_i > 0) && (ret_i < fub_u.len) ) {
      fub_u.len  -= ret_i;
      fub_u.base += ret_i;
      continue;
    }
    else {
      break;
    }
  }

  //  cue an async write if necessary
  //
  if ( UV_EAGAIN == ret_i ) {
    uv_write_t* wri_u = c3_malloc(sizeof(*wri_u));
    wri_u->data = ptr_v;

    //  invoke callback manually on error
    //
    if ( (ret_i = uv_write(wri_u, han_u, &fub_u, 1, _term_it_write_cb)) ) {
      _term_it_write_cb(wri_u, ret_i);
    }
  }
  else {
    //  synchronous write failure is logged, but otherwise ignored
    //
    if ( ret_i < 0 ) {
      u3l_log("term: write: %s\n", uv_strerror(ret_i));
    }

    c3_free(ptr_v);
  }
}

/* _term_it_dump_buf(): write static buffer.
*/
static void
_term_it_dump_buf(u3_utty*  uty_u,
                  uv_buf_t* buf_u)
{
  _term_it_write(uty_u, buf_u, 0);
}

/* _term_it_dump(): write static vector.
*/
static void
_term_it_dump(u3_utty*    uty_u,
              c3_w        len_w,
              const c3_y* hun_y)
{
  uv_buf_t buf_u = uv_buf_init((c3_c*)hun_y, len_w);
  _term_it_dump_buf(uty_u, &buf_u);
}

/* _term_it_send(): write dynamic vector, freeing pointer.
*/
static void
_term_it_send(u3_utty*    uty_u,
              c3_w        len_w,
              const c3_y* hun_y)
{
  uv_buf_t buf_u = uv_buf_init((c3_c*)hun_y, len_w);
  _term_it_write(uty_u, &buf_u, (void*)hun_y);
}

/* _term_it_send_cord(): write a cord.
*/
static void
_term_it_send_cord(u3_utty*    uty_u,
                   u3_atom       txt)
{
  c3_w  len_w = u3r_met(3, txt);
  c3_y* hun_y = c3_malloc(len_w);
  u3r_bytes(0, len_w, hun_y, txt);

  _term_it_send(uty_u, len_w, hun_y);

  u3z(txt);
}

/* _term_it_show_clear(): clear to the beginning of the current line.
*/
static void
_term_it_show_clear(u3_utty* uty_u)
{
  if ( uty_u->tat_u.siz.col_l ) {
    _term_it_dump(uty_u, TERM_LIT("\r"));
    _term_it_dump_buf(uty_u, &uty_u->ufo_u.out.el_u);

    uty_u->tat_u.mir.wor_w = 0;
    uty_u->tat_u.mir.cus_w = 0;
  }
}

/* _term_it_show_blank(): blank the screen.
*/
static void
_term_it_show_blank(u3_utty* uty_u)
{
  _term_it_dump_buf(uty_u, &uty_u->ufo_u.out.clear_u);
}

/* _term_it_show_cursor(): set current line, transferring pointer.
*/
static void
_term_it_show_cursor(u3_utty* uty_u, c3_w cur_w)
{
  c3_w cus_w = uty_u->tat_u.mir.cus_w;
  c3_w dif_w;

  //NOTE  assumes all styled text precedes the cursor. drum enforces this.
  //
  cur_w += uty_u->tat_u.mir.sap_w;

  if ( cur_w < cus_w ) {
    dif_w = cus_w - cur_w;

    while ( dif_w-- ) {
      _term_it_dump_buf(uty_u, &uty_u->ufo_u.out.cub1_u);
    }
  }
  else if ( cur_w > cus_w ) {
    dif_w = cur_w - cus_w;

    while ( dif_w-- ) {
      _term_it_dump_buf(uty_u, &uty_u->ufo_u.out.cuf1_u);
    }
  }

  uty_u->tat_u.mir.cus_w = cur_w;
}

/* _term_it_show_line(): render current line.
*/
static void
_term_it_show_line(u3_utty* uty_u, c3_w wor_w, c3_w sap_w)
{
  u3_utat* tat_u = &uty_u->tat_u;

  //  we have to reallocate the current line on write,
  //  or we have a data race if a) the write is async,
  //  and b) a new output line arrives before the write completes.
  //
  {
    c3_w  len_w = tat_u->mir.byt_w;
    c3_y* hun_y = c3_malloc(len_w);
    memcpy(hun_y, tat_u->mir.lin_y, len_w);

    _term_it_send(uty_u, len_w, hun_y);
  }

  //  XX refactor to avoid updating state
  //
  tat_u->mir.cus_w += wor_w;
  tat_u->mir.wor_w = wor_w;
  tat_u->mir.sap_w = sap_w;
}

/* _term_it_refresh_line(): refresh current line.
*/
static void
_term_it_refresh_line(u3_utty* uty_u)
{
  u3_utat* tat_u = &uty_u->tat_u;
  c3_w     wor_w = tat_u->mir.wor_w;
  c3_w     sap_w = tat_u->mir.sap_w;
  c3_w     cus_w = tat_u->mir.cus_w;

  _term_it_show_clear(uty_u);
  _term_it_show_line(uty_u, wor_w, sap_w);
  _term_it_show_cursor(uty_u, cus_w);
}

/* _term_it_set_line(): set current line.
*/
static void
_term_it_set_line(u3_utty* uty_u,
                  c3_w*    lin_w,
                  c3_w     wor_w,
                  c3_w     sap_w)
{
  u3_utat* tat_u = &uty_u->tat_u;
  c3_y*    hun_y = (c3_y*)lin_w;
  c3_w     byt_w = 0;

  //  convert lin_w in-place from utf-32 to utf-8
  //
  //    (this is just a hand-translation of +tuft)
  //    XX refactor for use here and in a jet
  //
  {
    c3_w car_w, i_w;

    for ( i_w = 0; i_w < wor_w; i_w++ ) {
      car_w = lin_w[i_w];

      if ( 0x7f >= car_w ) {
        hun_y[byt_w++] = car_w;
      }
      else if ( 0x7ff >= car_w ) {
        hun_y[byt_w++] = 0xc0 ^ ((car_w >>  6) & 0x1f);
        hun_y[byt_w++] = 0x80 ^ (car_w & 0x3f);
      }
      else if ( 0xffff >= car_w ) {
        hun_y[byt_w++] = 0xe0 ^ ((car_w >> 12) &  0xf);
        hun_y[byt_w++] = 0x80 ^ ((car_w >>  6) & 0x3f);
        hun_y[byt_w++] = 0x80 ^ (car_w & 0x3f);
      }
      else {
        hun_y[byt_w++] = 0xf0 ^ ((car_w >> 18) &  0x7);
        hun_y[byt_w++] = 0x80 ^ ((car_w >> 12) & 0x3f);
        hun_y[byt_w++] = 0x80 ^ ((car_w >>  6) & 0x3f);
        hun_y[byt_w++] = 0x80 ^ (car_w & 0x3f);
      }
    }
  }

  c3_free(tat_u->mir.lin_y);
  tat_u->mir.lin_y = hun_y;
  tat_u->mir.byt_w = byt_w;
  tat_u->mir.wor_w = wor_w;
  tat_u->mir.sap_w = sap_w;

  _term_it_show_line(uty_u, wor_w, sap_w);
}

/* _term_it_show_more(): new current line.
*/
static void
_term_it_show_more(u3_utty* uty_u)
{
  if ( c3y == u3_Host.ops_u.tem ) {
    _term_it_dump(uty_u, TERM_LIT("\n"));
  }
  else {
    _term_it_dump(uty_u, TERM_LIT("\r\n"));
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

  c3_free(pax_c);
  c3_free(bas_c);
}

/* _term_ovum_plan(): plan term ovums, configuring spinner.
*/
static u3_ovum*
_term_ovum_plan(u3_auto* car_u, u3_noun wir, u3_noun cad)
{
  u3_ovum* egg_u = u3_auto_plan(car_u, u3_ovum_init(0, c3__d, wir, cad));

  //  term events have no spinner label
  //
  u3z(egg_u->pin_u.lab);
  egg_u->pin_u.lab = u3_blip;

  return egg_u;
}

/* _term_io_belt(): send belt.
*/
static void
_term_io_belt(u3_utty* uty_u, u3_noun blb)
{
  //  XX s/b u3dc("scot", c3__ud, uty_u->tid_l)
  //
  u3_noun wir = u3nt(c3__term, '1', u3_nul);
  u3_noun cad = u3nc(c3__belt, blb);

  c3_assert( 1 == uty_u->tid_l );
  c3_assert( uty_u->car_u );

  {
    u3_ovum* egg_u = _term_ovum_plan(uty_u->car_u, wir, cad);

    //  no spinner delay on %ret
    //
    if ( c3__ret == u3h(blb) ) {
      egg_u->pin_u.del_o = c3n;
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
          _term_it_dump_buf(uty_u, &uty_u->ufo_u.out.bel_u);
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

        _term_it_dump_buf(uty_u, &uty_u->ufo_u.out.bel_u);
      }
    }
  }
  else if ( 0 != tat_u->fut.wid_w ) {
    tat_u->fut.syb_y[tat_u->fut.len_w++] = cay_y;

    if ( tat_u->fut.len_w == tat_u->fut.wid_w ) {
      u3_noun huv = u3i_bytes(tat_u->fut.wid_w, tat_u->fut.syb_y);
      u3_noun wug;

      //  XX  implement directly here and jet
      //
      wug = u3do("taft", huv);

      tat_u->fut.len_w = tat_u->fut.wid_w = 0;
      _term_io_belt(uty_u, u3nt(c3__txt, wug, u3_nul));
    }
  }
  else {
    if ( (cay_y >= 32) && (cay_y < 127) ) {
      _term_io_belt(uty_u, u3nt(c3__txt, cay_y, u3_nul));
    }
    else if ( 0 == cay_y ) {
      _term_it_dump_buf(uty_u, &uty_u->ufo_u.out.bel_u);
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

      //  XX revise
      //
      u3_pier_bail(u3_king_stub());
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
  c3_free(buf_u->base);
}

/* _term_spin_step(): advance spinner state and (re-)render.
*/
static void
_term_spin_step(u3_utty* uty_u)
{
  u3_utat* tat_u = &uty_u->tat_u;
  c3_w     bac_w;

  //  calculate backoff from end of line, or bail out
  //
  {
    c3_w cus_w = tat_u->mir.cus_w;
    c3_l col_l = tat_u->siz.col_l;

    if ( cus_w >= col_l ) {  //  shenanigans!
      return;
    }

    bac_w = col_l - 1 - cus_w;
  }

  c3_d       lag_d   = tat_u->sun_u.eve_d++;
  const c3_c daz_c[] = "|/-\\";
  //               | + « + why + » + \0
  c3_c       buf_c[1 + 2 +  4  + 2 + 1];
  c3_c*      cur_c   = buf_c;
  c3_w       sol_w   = 1;  //  spinner length (utf-32)

  //  set spinner char
  //
  *cur_c++ = daz_c[lag_d % (sizeof(daz_c) - 1)];

  //  if we have a spinner, add it between brackets
  //
  if ( tat_u->sun_u.why_c[0] ) {
    *cur_c++ = '\xc2';
    *cur_c++ = '\xab';
    sol_w++;

    {
      c3_c* why_c = tat_u->sun_u.why_c;
      *cur_c++ = *why_c++;
      *cur_c++ = *why_c++;
      *cur_c++ = *why_c++;
      *cur_c++ = *why_c;
      //  XX assumes one glyph per char
      //
      sol_w += 4;
    }

    *cur_c++ = '\xc2';
    *cur_c++ = '\xbb';
    sol_w++;
  }

  *cur_c = '\0';

  //  write spinner, adjusting cursor as needed
  //
  //    NB: we simply bail out if anything goes wrong
  //
  {
    uv_buf_t lef_u = uty_u->ufo_u.out.cub1_u;
    c3_i fid_i;

    if ( uv_fileno((uv_handle_t*)&uty_u->pop_u, &fid_i) ) {
      return;
    }

    //  One-time cursor backoff.
    //
    if ( c3n == tat_u->sun_u.diz_o ) {
      c3_w i_w;

      for ( i_w = bac_w; i_w < sol_w; i_w++ ) {
        if ( lef_u.len != write(fid_i, lef_u.base, lef_u.len) ) {
          return;
        }
      }

      tat_u->sun_u.diz_o = c3y;
    }

    {
      c3_w len_w = cur_c - buf_c;
      if ( len_w != write(fid_i, buf_c, len_w) ) {
        return;
      }
    }

    //  Cursor stays on spinner.
    //
    while ( sol_w-- ) {
      if ( lef_u.len != write(fid_i, lef_u.base, lef_u.len) ) {
        return;
      }
    }
  }
}

/* _term_spin_timer_cb(): render spinner
*/
static void
_term_spin_timer_cb(uv_timer_t* tim_u)
{
  u3_utty* uty_u = tim_u->data;
  _term_spin_step(uty_u);
}

#define _SPIN_COOL_US 500UL  //  spinner activation delay when cool
#define _SPIN_WARM_US 50UL   //  spinner activation delay when warm
#define _SPIN_RATE_US 250UL  //  spinner rate (ms/frame)
#define _SPIN_IDLE_US 500UL  //  spinner cools down if stopped this long

/* u3_term_start_spinner(): prepare spinner state. RETAIN.
*/
void
u3_term_start_spinner(u3_atom say, c3_o del_o)
{
  if ( c3n == u3_Host.ops_u.tem ) {
    u3_utty* uty_u = _term_main();
    u3_utat* tat_u = &uty_u->tat_u;

    tat_u->sun_u.why_c[4] = 0;
    u3r_bytes(0, 4, (c3_y*)tat_u->sun_u.why_c, say);

    tat_u->sun_u.eve_d = 0;
    // XX must be c3n for cursor backoff from EOL?
    tat_u->sun_u.diz_o = c3n;

    {
      c3_d now_d = _term_msc_out_host();
      c3_d end_d = tat_u->sun_u.end_d;
      c3_d wen_d = (c3n == del_o) ? 0UL :
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

    memset(tat_u->sun_u.why_c, 0, 5);

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
_term_ef_get(c3_l tid_l)
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
  //  XX groace, this should be a global handler sent to each pier
  //
  if ( u3_Host.uty_u->car_u ) {
    u3_noun wir = u3nt(c3__term, '1', u3_nul);
    u3_noun cad = u3nc(c3__blew, u3_term_get_blew(1));

    c3_assert( 1 == u3_Host.uty_u->tid_l );

    _term_ovum_plan(u3_Host.uty_u->car_u, wir, cad);
  }
}

/* u3_term_ef_ctlc(): send ^C on console.
*/
void
u3_term_ef_ctlc(void)
{
  u3_utty* uty_u = _term_main();

  {
    u3_noun wir = u3nt(c3__term, '1', u3_nul);
    u3_noun cad = u3nt(c3__belt, c3__ctl, 'c');

    c3_assert( 1 == uty_u->tid_l );
    c3_assert( uty_u->car_u );

    _term_ovum_plan(uty_u->car_u, wir, cad);
  }

  _term_it_refresh_line(uty_u);
}

/* _term_it_put_tint(): put ansi color id on lin_w. RETAINS col.
*/
static void
_term_it_put_tint(c3_w* lin_w,
                  u3_noun col)
{
  switch ( col ) {
    default:
    case u3_nul: *lin_w = '9'; break;
    case 'k':    *lin_w = '0'; break;
    case 'r':    *lin_w = '1'; break;
    case 'g':    *lin_w = '2'; break;
    case 'y':    *lin_w = '3'; break;
    case 'b':    *lin_w = '4'; break;
    case 'm':    *lin_w = '5'; break;
    case 'c':    *lin_w = '6'; break;
    case 'w':    *lin_w = '7'; break;
  }
}

/* _term_it_put_deco(): put ansi sgr code on lin_w. RETAINS dec.
*/
static void
_term_it_put_deco(c3_w* lin_w,
                  u3_noun dec)
{
  switch ( dec ) {
    default:
    case u3_nul: *lin_w = '0'; break;
    case c3__br: *lin_w = '1'; break;
    case c3__un: *lin_w = '4'; break;
    case c3__bl: *lin_w = '5'; break;
  }
}

/* _term_it_show_stub(): send styled text to terminal as ansi escape sequences
*/
static void
_term_it_show_stub(u3_utty* uty_u,
                   u3_noun    tub)
{
  c3_w tuc_w = u3qb_lent(tub);

  //  count the amount of characters across all stubs
  //
  c3_w lec_w = 0;
  {
    u3_noun nub = tub;
    while ( u3_nul != nub ) {
      u3_noun nib = u3t(u3h(nub));
      lec_w = lec_w + u3qb_lent(nib);
      nub = u3t(nub);
    }
  }

  //  allocate enough memory for every display character, plus styles
  //
  //NOTE  we use max 20 characters per styl for escape codes:
  //      3 for opening, 4 for decorations, 4 for colors, 4 for closing,
  //      and 5 as separators between decorations and colors.
  //
  c3_w* lin_w = c3_malloc(  sizeof(c3_w) * (lec_w + (20 * tuc_w))  );

  //  write the contents to the buffer,
  //  tracking total and escape characters written
  //
  c3_w   i_w = 0;
  c3_w sap_w = 0;
  {
    u3_noun nub = tub;
    while ( u3_nul != nub ) {
      u3_noun tyl, nib, dec, bag, fog;
      u3x_cell(u3h(nub), &tyl, &nib);
      u3x_trel(tyl, &dec, &bag, &fog);

      c3_o tyl_o = c3n;
      if ( ( c3n == u3_Host.ops_u.tem ) &&
           ( (u3_nul != dec) || (u3_nul != bag) || (u3_nul != fog) ) ) {
        tyl_o = c3y;
      }

      //  write style escape sequences
      //
      if ( c3y == tyl_o ) {
        c3_o mor_o = c3n;
        lin_w[i_w++] = 27;
        lin_w[i_w++] = '[';
        sap_w += 2;

        //  text decorations
        //
        {
          u3_noun dos = u3qdi_tap(dec);
          u3_noun des = dos;
          while ( u3_nul != des ) {
            if ( c3y == mor_o ) {
              lin_w[i_w++] = ';';
              sap_w++;
            }
            _term_it_put_deco(&lin_w[i_w++], u3h(des));
            sap_w++;
            mor_o = c3y;
            des = u3t(des);
          }
          u3z(dos);
        }

        //  background color
        //
        if ( u3_nul != bag ) {
          if ( c3y == mor_o ) {
            lin_w[i_w++] = ';';
            sap_w++;
          }
          lin_w[i_w++] = '4';
          _term_it_put_tint(&lin_w[i_w++], bag);
          sap_w += 2;
          mor_o = c3y;
        }

        //  foreground color
        //
        if ( u3_nul != fog ) {
          if ( c3y == mor_o ) {
            lin_w[i_w++] = ';';
            sap_w++;
          }
          lin_w[i_w++] = '3';
          _term_it_put_tint(&lin_w[i_w++], fog);
          sap_w += 2;
          mor_o = c3y;
        }

        lin_w[i_w++] = 'm';
        sap_w++;
      }

      //  write the text itself
      //
      for ( i_w = i_w; u3_nul != nib; i_w++, nib = u3t(nib) ) {
        lin_w[i_w] = u3r_word(0, u3h(nib));
      }

      //  if we applied any styles, toggle them off
      //
      if ( c3y == tyl_o ) {
        lin_w[i_w++] = 27;
        lin_w[i_w++] = '[';
        lin_w[i_w++] = '0';
        lin_w[i_w++] = 'm';
        sap_w += 4;
      }

      nub = u3t(nub);
    }
  }

  _term_it_set_line(uty_u, lin_w, i_w, sap_w);

  u3z(tub);
}

/* _term_it_show_tour(): send utf32 to terminal.
*/
static void
_term_it_show_tour(u3_utty* uty_u,
                   u3_noun    lin)
{
  c3_w  len_w = u3qb_lent(lin);
  c3_w* lin_w = c3_malloc( sizeof(c3_w) * len_w );

  {
    c3_w i_w;

    for ( i_w = 0; u3_nul != lin; i_w++, lin = u3t(lin) ) {
      lin_w[i_w] = u3r_word(0, u3h(lin));
    }
  }

  _term_it_set_line(uty_u, lin_w, len_w, 0);

  u3z(lin);
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
        _term_it_dump_buf(uty_u, &uty_u->ufo_u.out.bel_u);
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

    case c3__klr: {
      if ( c3n == u3_Host.ops_u.tem ) {
        _term_it_show_clear(uty_u);
      }
      _term_it_show_stub(uty_u, u3k(u3t(blt)));
    } break;

    case c3__lin: {
      if ( c3n == u3_Host.ops_u.tem ) {
        _term_it_show_clear(uty_u);
      }
      _term_it_show_tour(uty_u, u3k(u3t(blt)));
    } break;

    case c3__mor: {
      _term_it_show_more(uty_u);
    } break;

    case c3__sav: {
      u3_noun pax, dat;
      u3x_cell(u3t(blt), &pax, &dat);

      _term_it_save(u3k(pax), u3k(dat));
    } break;

    case c3__sag: {
      u3_noun pax, dat;
      u3x_cell(u3t(blt), &pax, &dat);

      _term_it_save(u3k(pax), u3qe_jam(dat));
    } break;

    case c3__url: {
      u3_noun txt = u3t(blt);

      //  XX check u3_Host.ops_u.tem ?
      //  XX this looks to be broken,
      //      multiple calls to _show_clear will discard the mirror state
      //
      if ( c3y == u3a_is_atom(txt) ) {
        _term_it_show_clear(uty_u);

        _term_it_send_cord(uty_u, u3k(txt));

        _term_it_show_more(uty_u);
        _term_it_refresh_line(uty_u);
      }
    } break;
  }

  u3z(blt);
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
        {
          uv_buf_t* buf_u = &uty_u->ufo_u.out.el_u;
          _write(uty_u->fid_i, buf_u->base, buf_u->len);
        }
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

/* _term_io_talk():
*/
static void
_term_io_talk(u3_auto* car_u)
{
  if ( c3n == u3_Host.ops_u.tem ) {
    u3_utty* uty_u = _term_main();

    uv_read_start((uv_stream_t*)&(uty_u->pop_u),
                  _term_alloc,
                  _term_read_cb);
  }

  //  XX groace hardcoded terminal number
  //
  u3_noun wir = u3nt(c3__term, '1', u3_nul);
  u3_noun cad;

  //  send terminal dimensions
  //
  {
    cad = u3nc(c3__blew, u3_term_get_blew(1));
    _term_ovum_plan(car_u, u3k(wir), cad);
  }

  //  NB, term.c used to also start :dojo
  //
  // u3nq(c3__flow, c3__seat, c3__dojo, u3_nul)

  //  refresh terminal state
  //
  {
    cad = u3nc(c3__hail, u3_nul);
    _term_ovum_plan(car_u, wir, cad);
  }
}

/*  _reck_orchid(): parses only a number as text
 *
 *    Parses a text string which contains a decimal number. In practice, this
 *    number is always '1'.
 */
static u3_noun
_reck_orchid(u3_noun fot, u3_noun txt, c3_l* tid_l)
{
  c3_c* str = u3r_string(txt);
  c3_d ato_d = strtol(str, NULL, 10);
  c3_free(str);

  if ( ato_d >= 0x80000000ULL ) {
    return c3n;
  } else {
    *tid_l = (c3_l) ato_d;

    return c3y;
  }
}

/* _term_io_kick(): apply effects.
*/
static c3_o
_term_io_kick(u3_auto* car_u, u3_noun wir, u3_noun cad)
{
  u3_noun tag, dat, i_wir, t_wir;
  c3_o ret_o;

  if (  (c3n == u3r_cell(wir, &i_wir, &t_wir))
     || (c3n == u3r_cell(cad, &tag, &dat))
     || (c3__term != i_wir) )
  {
    ret_o = c3n;
  }
  else {
    u3_noun pud = t_wir;
    u3_noun p_pud, q_pud;
    c3_l    tid_l;

    if (  (c3n == u3r_cell(pud, &p_pud, &q_pud))
       || (u3_nul != q_pud)
       || (c3n == _reck_orchid(c3__ud, u3k(p_pud), &tid_l)) )
    {
      u3l_log("term: bad tire\n");
      ret_o = c3n;
    }
    else {
      switch ( tag ) {
        default: {
          ret_o = c3n;
        } break;

        //  XX review, accepted and ignored
        //
        case c3__bbye: {
          ret_o = c3y;
        } break;

        case c3__blit: {
          ret_o = c3y;

          {
            u3_utty* uty_u = _term_ef_get(tid_l);
            if ( 0 == uty_u ) {
              // u3l_log("no terminal %d\n", tid_l);
              // u3l_log("uty_u %p\n", u3_Host.uty_u);
            }
            else {
              u3_noun bis = dat;

              while ( c3y == u3du(bis) ) {
                _term_ef_blit(uty_u, u3k(u3h(bis)));
                bis = u3t(bis);
              }
            }
          }
        } break;

        //  XX obsolete %ames
        //
        // case c3__send:

        case c3__logo: {
          ret_o = c3y;
          u3_pier_exit(car_u->pir_u);
          //  XX validate? ignore?
          //
          u3_Host.xit_i = dat;
        } break;

        //  XX obsolete, remove in %zuse and %dill
        case c3__init: {
          // daemon ignores %init
          // u3A->own = u3nc(u3k(p_fav), u3A->own);
          // u3l_log("kick: init: %d\n", p_fav);
          ret_o = c3y;
        } break;

        case c3__mass: {
          ret_o = c3y;

          //  gc the daemon area
          //
          //    XX disabled due to known leaks; uncomment for dev
          //
          // uv_timer_start(&u3K.tim_u, (uv_timer_cb)u3_king_grab, 0, 0);
        } break;

        case c3__meld: {
          ret_o = c3y;
          u3_pier_meld(car_u->pir_u);
        } break;

        case c3__pack: {
          ret_o = c3y;
          u3_pier_pack(car_u->pir_u);
        } break;
      }
    }
  }

  u3z(wir); u3z(cad);
  return ret_o;
}

static void
_term_io_exit_cb(uv_handle_t* han_u)
{
  u3_auto* car_u = han_u->data;
  c3_free(car_u);
}

/* _term_io_exit(): clean up terminal.
*/
static void
_term_io_exit(u3_auto* car_u)
{
  u3_utty* uty_u = _term_main();

  //  NB, closed in u3_term_log_exit()
  //
  uv_read_stop((uv_stream_t*)&(uty_u->pop_u));

  if ( c3n == u3_Host.ops_u.tem ) {
    uv_timer_t* han_u = &(uty_u->tat_u.sun_u.tim_u);
    han_u->data       = car_u;

    uv_close((uv_handle_t*)han_u, _term_io_exit_cb);
  }
  else {
    c3_free(car_u);
  }
}

/* u3_term_io_init(): initialize terminal
*/
u3_auto*
u3_term_io_init(u3_pier* pir_u)
{
  u3_auto* car_u = c3_calloc(sizeof(*car_u));

  c3_assert( u3_Host.uty_u );
  u3_Host.uty_u->car_u = car_u;

  car_u->nam_m = c3__term;
  car_u->liv_o = c3y;
  car_u->io.talk_f = _term_io_talk;
  car_u->io.kick_f = _term_io_kick;
  car_u->io.exit_f = _term_io_exit;

  return car_u;
}
