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
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "vere/vere.h"



/* 
   TLDR:
       This code handles the mechanics of terminal input and output.

   Philosophy:

       Hoon is a perfect, clean, crystaline mathematical idea.  It
       does not interact with the grubby real world.

       Q: So how does it read and write the terminal?

       A: Hoon is Martian and pure, but the Hoon interpretter is a big
          pile of Earth technology, that DOES live in the real world.

       The code in this file listens to the terminal and sends clean
       Martian events into Hoon (Hoon is always willing to be
       evaluated on a new subject).

       The code in other files that interprets Hoon is also return nouns that contain effects.
       If term.c .  These are merely
       terrestrial side effects; the Hoon computation itself knows
       nothing of such cruft.

   This code does a few things:
       * use termios to deal with terminal configuration
       * hooks stdin into libuv, so that a keypress in the terminal pings libuv, which invokes a callback, which lets us
       * uses ncurses to do text output on the screen in an X,Y grid
       * runs a spinner thread, which animates a sprite at the left hand margin when urbit is "thinking"

   API:
      The API is broken up into three parts:

        * init / shutdown

            * u3_term_io_init(): init terminal                         (called from pier.c)
            * u3_term_io_port(): init listening on a port
            * u3_term_io_exit(): shutdown terminal                     (called from king.c)

        * trigger effects ( u3_term_ef ... )

            * u3_term_ef_verb(): initial effects for verbose events    (called from pier.c)
            * u3_term_ef_winc(): SIGWINCH recieved bc terminal resized (called from unix.c)
            * u3_term_ef_ctlc(): send ^C.                              (called from unix.c)
            * u3_term_ef_bake(): initial effects for new server        (called from pier.c)
            * u3_term_ef_blit(): send %blit effect to terminal         (called from reck.c)

        * io hijacking (let some other module take over the terminal)

            * u3_term_io_hija(): hijack console for cooked print.
            * u3_term_io_loja(): release console from cooked print.

    Blits:
      There is a sub-API.  Every call of u3_term_ef_blit() carries a command (a mote) and data.  Commands include:
         * c3__nop - NEW: no op; carries a (list @c) payload, so that developer can signal from dill.hoon to term.c 
         * c3__bog - NEW: set background color for future printing
         * c3__fog - NEW: set foreground color for future printing
         * c3__eff - NEW: set text effect for future printing
         * c3__pri - NEW: print 1 some text (starting at location of cursor), move cursor to end of text
         * c3__sxy - NEW: move cursor to x,y position

         * c3__bee - toggle spinner
         * c3__bel - ring the bell
         * c3__clr - clear the entire screen
         * c3__hop - set cursor location
         * c3__lin - clear current line, print 1 line, move cursor to end of line
         * c3__seg - starting at current location of cursor, place text, move cursor to end of emitted text
         * c3__mor - emit linefeed ("\r\n"), move cursor to far left
         * c3__sav - save file by path (??)
         * c3__sag - jam then save file by path (??)
         * c3__url - write url (??)

   Some things to note:
       * stdin / stdout / stderr are file descriptors w values 0 / 1 / 2 (
         https://en.wikipedia.org/wiki/File_descriptor )

       * we maintain a linked list of terminals (u3_utty has a nex_u pointer),
         but we don't seem to use that feature?

       * we print output to terminal via ncurses printw(). This accepts
         formating (e.g. "%i") which we do not use. Thus we need to escape all %
         (e.g. "%.y" -> "%%.y"). This has a bad code smell, but there seem to be
         no other options (mcprint() is for printers).

       * ncurses supports multiple windows; we always use default window #1 ( stdscr )

   BUGS:

      * reimplement u3_term_io_hija() and u3_term_io_loja() - current code
        breaks abstraction barrier (lets other parts of the system just hammer
        at the tty, and future more sophisticated terminal code will want to
        have 100% ownership of the terminal).

        Tentative idea: keep these two functions, but the way they will work in
        the future is to give a 'fake' file descriptor (libuv?  fmemopen() ? )
        to the caller, and then callback code on that FD takes the text written
        to the FD and packages it as a c3_lin blit and calls _term_ef_blit().

      * two calls to u3do("tuft"...) are retarded - we're unpacking a noun, repacking it, 
           sending it to Hoon, then unpacking it. Get a native utf32 -> utf8 translation code

      * uty_u->tat_u.mir.lin_w wants to store utf32, but we're being inconsistent in its use

      * collapse into 1 the funcs that write bytes to libuv
              * _term_it_write_buf
              * _term_it_write_old
              * _term_it_write_bytes
              * _term_it_write_txt
              * _term_it_write_str

      * the next three funcs (a) call each other, (b) call down into the above
        stack...and there's libuv at the bottom.  WHY?!?!  What is going on
        here?  We'd really really like to rip this out and replace it with calls
        to ncurses functions of the code by changing #define ENABLE_WRITE to 0
        ...but that ... breaks things?

              * _term_it_refresh_line (which CLAIMS to refresh a line), calls
              * _term_it_show_line (makes sense so far), which calls

              * _term_it_show_wide (still makes sense) ... but then this calls
                    down into the term_it_write_str stack which ... writes to
                    libuv?  Why?  Who's reading it? No one, I think.

      * figure out if the union of { pop_u , wax_u } in u3_utty in vere.h (line
        477) does anything
              * experiment: can remove wax_u without any problem
              * I'm not sure that pop_u does anything useful 

*/      
static        void _term_spinner_cb(void*);
static        void _term_read_cb(uv_stream_t* tcp_u,
                                 ssize_t      siz_i,
                                 const uv_buf_t *     buf_u);
static inline void _term_suck(u3_utty*, const c3_y*, ssize_t);


// init funcs
static void _term_init_terminfo(u3_utty* uty_u);
static void _term_init_ncurses(u3_utty* uty_u);
static void _term_init_spinner(u3_utty* uty_u);
static void _term_init_libuv(u3_utty* uty_u);


// ncurse primitives
static void _term_ncurs_left();
static void _term_ncurs_clrline();
static void _term_ncurs_clrline_all(u3_utty* uty_u);
  
// blits

#define _T_ECHO 1    //  local echo
#define _T_CTIM 3    //  suppress GA/char-at-a-time
#define _T_NAWS 31   //  negotiate about window size

#define _SPIN_COOL_US 500000  //  spinner activation delay when cool
#define _SPIN_WARM_US 50000   //  spinner activation delay when warm
#define _SPIN_RATE_US 250000  //  spinner rate (microseconds/frame)
#define _SPIN_IDLE_US 500000  //  spinner cools down if stopped this long

#define ENABLE_WRITE 1

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
_term_libuv_alloc(uv_handle_t* had_u,
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

  // (1) termios setup
  _term_init_terminfo(uty_u);

  // (2) spinner setup
  _term_init_spinner(uty_u);
  
  // (3) init ncurses for stdout (and some slight hacking of stdin ?)
  _term_init_ncurses(uty_u);
  
  // (4) connect stdin to libuv, so that every keypress pings libuv (Rawdog it.)
  _term_init_libuv(uty_u);
  
  
}

void _term_init_terminfo(u3_utty* uty_u)
{

  // datastructure housekeeping

  //  This is terminal 1, linked in host.
  uty_u->tid_l = 1;
  uty_u->nex_u = 0;
  u3_Host.uty_u = uty_u;


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

#   define _utfo(way, nam)                                      \
    {                                                           \
      uty_u->ufo_u.way.nam##_y = (const c3_y *) tigetstr(#nam); \
      c3_assert(uty_u->ufo_u.way.nam##_y);                      \
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


  //  Start raw input.
  //
  if ( c3n == u3_Host.ops_u.dem ) {
    if ( 0 != tcsetattr(uty_u->fid_i, TCSADRAIN, &uty_u->raw_u) ) {
      c3_assert(!"init-tcsetattr");
    }
    if ( -1 == fcntl(uty_u->fid_i, F_SETFL, uty_u->nob_i) ) {
      c3_assert(!"init-fcntl");
    }
  }

  
}


//  spinning cursor, which is controlled with its own thread
//
void _term_init_spinner(u3_utty* uty_u)
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

void _term_init_ncurses(u3_utty* uty_u)
{
  initscr();            // init ncurses
  noecho();             // don't echo user's keypresses to the screen; we'll do that
  cbreak();             // grab each character as it comes; don't wait for RETURN keypress  (alt choice: raw() )
  curs_set(1);          // normal cursor  (0 = invisible, 1=normal, 2=bright)
  keypad(stdscr, TRUE);	// allow in keys like F1, F2 etc..
  scrollok(stdscr, TRUE); // turn on scrolling mode, which allows use of scroll(), scrl(), wscrl() later
  
  if ( c3n == u3_Host.ops_u.dem ) { 
    start_color();      // allow colors
  }

  u3z( u3_term_get_blew(1));   // get current window size; store
}

void _term_init_libuv(u3_utty* uty_u)
{
  uty_u->fid_i = 0;                       
  uv_pipe_init(u3L, &(uty_u->pop_u), 0);
  uv_pipe_open(&(uty_u->pop_u), uty_u->fid_i);
  uv_read_start((uv_stream_t*)&(uty_u->pop_u), _term_libuv_alloc, _term_read_cb);
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

    }
  }
}

#if ENABLE_WRITE

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

 
/* _term_it_write_buf(): 
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
  return;
  
  c3_w len_w = uty_u->tat_u.mir.len_w;  // what ?
  c3_w cus_w = uty_u->tat_u.mir.cus_w;  // what L/R position is cursor on?

  _term_ncurs_clrline_all(uty_u);
  _term_it_show_line(uty_u, uty_u->tat_u.mir.lin_w, len_w);
  _term_it_show_cursor(uty_u, cus_w);
}

#else

static void
_term_it_refresh_line(u3_utty* uty_u)
{
}

static void
_term_it_write_txt(u3_utty*    uty_u,
                   const c3_y* hun_y)
{
}

 
#endif

// refresh() with error handling
static void
_term_ncurs_refresh()
{
  c3_w ret_w = refresh();
  if (0 != ret_w){
    fprintf(stderr, "term.c: _term_ncurs_refresh() failed %i\n", ret_w);
    u3m_bail(c3__fail); 
  }

}


/* _term_ncurs_clrline_all(): clear to the beginning of the current line.
*/
static void
_term_ncurs_clrline_all(u3_utty* uty_u)
{
  _term_ncurs_left();
  _term_ncurs_clrline();
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


/* _term_io_belt(): send belt.
*/
static void
_term_io_belt(u3_utty* uty_u, u3_noun  blb)
{
  u3_noun tid = u3dc("scot", c3__ud, uty_u->tid_l);
  u3_noun pax = u3nq(u3_blip, c3__term, tid, u3_nul);

  u3v_plan(pax, u3nc(c3__belt, blb));
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
      wug = u3do("taft", huv);
      // uL(fprintf(uH, "muck-utf32 %x\n", tat_u->fut.len_w));

      tat_u->fut.len_w = tat_u->fut.wid_w = 0;
      _term_io_belt(uty_u, u3nt(c3__txt, wug, u3_nul));
    }
  }
  else {
    if ( (cay_y >= 32) && (cay_y < 127) ) { // alphanumerics and other standard glyphs
      _term_io_belt(uty_u, u3nt(c3__txt, cay_y, u3_nul));
    }
    else if ( 0 == cay_y ) {
      _term_it_write_txt(uty_u, uty_u->ufo_u.out.bel_y);
    }
    else if ( 8 == cay_y || 127 == cay_y ) {  // DEL character (backspace)
      _term_io_belt(uty_u, u3nc(c3__bac, u3_nul));
    }
    else if ( 13 == cay_y ) { // carriage return
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
  u3_lo_open();
  {
    if ( siz_i == UV_EOF ) {
      //  We hear EOF (on the third read callback) if
      //  2x the _term_alloc() buffer size is pasted.
      //  The process hangs if we do nothing (and ctrl-z
      //  then corrupts the event log), so we force shutdown.
      //
      fprintf(stderr, "term: hangup (EOF)\r\n");
      u3_lo_bail();
    }
    else if ( siz_i < 0 ) {
      uL(fprintf(uH, "term %d: read: %s\n", uty_u->tid_l, uv_strerror(siz_i)));
    }
    else {
      c3_i i;

      for ( i=0; i < siz_i; i++ ) {
        _term_io_suck_char(uty_u, buf[i]);
      }
    }
  }
  u3_lo_shut(c3y);
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

/* _term_show_spinner(): render the spinner glyph
*/
static void
_term_show_spinner(u3_utty* uty_u, c3_d lag_d)
{
  if (NULL == uty_u){
    return;
  }
  
  c3_y glyphs_y[4] = { '/', '-', '\\', '|'};
  
  uty_u->tat_u.sun.seq_y = (uty_u->tat_u.sun.seq_y + 1 ) % 4 ;

  c3_y buf_y[2];
  buf_y[0] = glyphs_y[uty_u->tat_u.sun.seq_y];
  buf_y[1] = 0;
  
  _term_ncurs_left();
  c3_w ret_w = printw((char *) buf_y);
  if (0 != ret_w){
    fprintf(stderr, "term.c: ncurses _term_show_spinner() failed %i\n", ret_w);
    u3m_bail(c3__fail); 
  }

  _term_ncurs_refresh();
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
  uty_u->tat_u.sun.seq_y = 0;  // init spinner sequence to start
  
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
  // which tty specifically?
  u3_utty*       uty_u = _term_ef_get(tid_l);

  // ask ncurses
  c3_w col_w, row_w;
  if ( uty_u ){
    getmaxyx(stdscr, row_w, col_w);
  } else  {
    col_w = 80;
    row_w = 24;
  }

  // store
  if ( uty_u ) {
    uty_u->tat_u.siz.col_l = (c3_l) col_w;
    uty_u->tat_u.siz.row_l = (c3_l) row_w;
  }

  return u3nc((c3_l)  col_w, (c3_l) row_w);
}

/* u3_term_ef_winc(): window change.  Just console right now.
*/
void
u3_term_ef_winc(void)
{
  u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);

  u3v_plan(pax, u3nc(c3__blew, u3_term_get_blew(1)));
}

/* u3_term_ef_ctlc(): send ^C on console.
*/
void
u3_term_ef_ctlc(void)
{
  u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);

  u3v_plan(pax, u3nt(c3__belt, c3__ctl, 'c'));
  _term_it_refresh_line(_term_main());
}

/* u3_term_ef_boil(): initial effects for loaded servers.
*/
void
u3_term_ef_boil(void)
{
  u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);

  // u3v_plan(u3k(pax), u3nq(c3__flow, c3__seat, c3__dojo, u3_nul));
  u3v_plan(u3k(pax), u3nc(c3__blew, u3_term_get_blew(1)));
  u3v_plan(u3k(pax), u3nc(c3__hail, u3_nul));

  u3z(pax);
}


// level 0: utilities
//

/* given a mote, convert to a constant that ncurses understands Part
   of the dill.hoon concept is that a color can be "default", so we
   take the 2nd argument so that the foreground (or background ) code
   can tell us what to use as default if dill tells us that.

 */
static int _term_ncurs_mote_to_colr(uint32_t mote, uint32_t default_color)
{
  switch(mote){
  case('k'): { return(COLOR_BLACK);   break; }
  case('r'): { return(COLOR_RED);     break; }
  case('g'): { return(COLOR_GREEN);   break; }
  case('y'): { return(COLOR_YELLOW);  break; }
  case('b'): { return(COLOR_BLUE);    break; }
  case('m'): { return(COLOR_MAGENTA); break; }
  case('c'): { return(COLOR_CYAN);    break; }
  case('w'): { return(COLOR_WHITE);   break; }
  case('\0'):{ return(default_color); break; }
  default: {
    fprintf(stderr, "term: illegal mote color\n");
    exit(-1);
  }
  }
}

/* given a mote, convert to a constant that ncurses understands */
static int _term_ncurs_mote_to_effect(uint32_t mote)
{
  switch(mote){
  case(c3__eff_bl): { return(A_BLINK);     break; }
  case(c3__eff_br): { return(A_BOLD);      break; }
  case(c3__eff_un): { return(A_UNDERLINE); break; }  
  case(c3__eff_no): { return(A_NORMAL);    break; }
  default: {
    fprintf(stderr, "term: illegal mote effect \n");
    exit(-1);
  }
  }
}

  // copy text from noun into buffer, escaping any '%' to '%%', because printw() is like printf()
  // and failure to do so will create man-made horrors beyond your comprehension

// RETAINS 
static c3_c * _term_util_noun_to_str(u3_noun lin)
{
  c3_w    len_w = u3qb_lent(lin);
  c3_w*   lin_w = c3_malloc(4 * len_w);

  c3_w i_w;
  {
    for ( i_w = 0; u3_nul != lin; i_w++, lin = u3t(lin) ) {
      lin_w[i_w] = u3r_word(0, u3h(lin));
    }
  }

  // this is retarded: we just copied it out of noun 'lin' into
  // bytes, then we're copying it back into noun 'wad', calling 
  // into hoon.hoon 'tuft' to transform the utf32 to utf8, then
        
  u3_noun wad   = u3i_words(len_w, lin_w);
  u3_noun txt   = u3do("tuft", wad);  
  c3_c*   txt_c = u3r_string(txt);
  u3z(txt);
    
  // count the '%'s so we can malloc a new string
  c3_w ii, cnt_w;
  for (ii=0, cnt_w=0; txt_c[ii]; ii++){
    cnt_w += (txt_c[ii] == '%');
  }
  c3_c*   esc_c = (c3_c*) c3_malloc(strlen(txt_c) + cnt_w + 1);

  // copy from the old string into the new string, escaping as necessary
  c3_w jj;
  for (ii=jj=0; txt_c[ii]; ii++, jj++){
    esc_c[jj] = txt_c[ii];
    if ('%' == txt_c[ii]){
      jj++;
      esc_c[jj] = '%';
    }
  }
  esc_c[jj] = 0;


  free(lin_w);
  free(txt_c);
  
  return(esc_c);
}

// level 1: simple ncurses operations (with error checking)
//

static void _term_ncurs_left()
{
  int x, y;
  getyx(stdscr, y, x);
  (void) x; // supress compiler warning re variable set-but-not-used
  
  c3_w  ret_w = move(y, 0); 
  if (0 != ret_w){
    fprintf(stderr, "term.c: _term_ncurs_left() failed %i\n", ret_w);
    u3m_bail(c3__fail); 
  }
}

static void _term_ncurs_xmove(u3_utty* uty_u, c3_w nex_w)
{
  int x, y;
  getyx(stdscr, y, x);
  (void) x; // supress compiler warning re variable set-but-not-used

  if (nex_w > uty_u->tat_u.siz.col_l){
    fprintf(stderr, "term.c: ncurses _term_ncurs_xmove() ask col %i but max %i\n", nex_w, nex_w > uty_u->tat_u.siz.col_l);
    u3m_bail(c3__fail); 
  }
  
  c3_w ret_w = move(y, nex_w);
  if (0 != ret_w){
    fprintf(stderr, "term.c: ncurses _term_ncurs_xmove() failed %i\n", ret_w);
    u3m_bail(c3__fail); 
  }

}

//
static void _term_ncurs_ymove(u3_utty* uty_u, c3_w ney_w)
{
  int x, y;
  getyx(stdscr, y, x);
  (void) x; // supress compiler warning re variable set-but-not-used

  
  if (ney_w >= (c3_w) uty_u->tat_u.siz.row_l ){
    fprintf(stderr, "term.c: ncurses  _term_ncurs_ymove() y too big %i > %i\n", ney_w, (c3_w) uty_u->tat_u.siz.row_l);
    u3m_bail(c3__fail); 
  }

  if (y == ney_w){
    return;
  }
  
  c3_w ret_w = move(ney_w, 0); 
  if (0 != ret_w){
    fprintf(stderr, "term.c: _term_ncurs_ymove() failed %i\n", ret_w);
    u3m_bail(c3__fail); 
  }
  _term_ncurs_refresh();
}

static void _term_ncurs_clrline()
{
  c3_w  ret_w = clrtoeol();             // clear line
  if (0 != ret_w){
    fprintf(stderr, "term.c: ncurses clrtobot() failed %i\n", ret_w);
    u3m_bail(c3__fail); 
  }
  
}

// level 2: blits
//

static void _term_blit_bee(u3_utty* uty_u, u3_noun  blt)
{
  if ( c3n == u3_Host.ops_u.dem ) {
    if ( u3_nul == u3t(blt) ) {
      _term_stop_spinner(uty_u);
    }
    else {
      _term_start_spinner(uty_u, u3t(blt));
    }
  }
}

static void _term_blit_bel()
{
  if ( c3n == u3_Host.ops_u.dem ) {
    beep();
  }
}

// clear line
static void _term_blit_clr()
{
  if ( c3n == u3_Host.ops_u.dem ) {
    _term_ncurs_left();
    _term_ncurs_clrline();
  }
}

// move vertically to line <x>
static void _term_blit_hop(u3_utty* uty_u, u3_noun  blt)
{
  u3_noun hop = u3t(blt);
  c3_w siz_w = u3r_met(3, hop);
  if (1 < siz_w){
    fprintf(stderr, "term.c: blit c3__hop argument wrong size %i\n", siz_w);
    u3m_bail(c3__fail); 
  }
  c3_w hop_w = u3r_word(0, hop);

  hop_w = (hop_w >= uty_u->tat_u.siz.row_l ) ? uty_u->tat_u.siz.row_l - 1 : hop_w;
  
  _term_ncurs_ymove(uty_u, hop_w);
}

// clear the current line
// insert a string at left
// move cursor to end of string
static void _term_blit_lin(u3_utty* uty_u, u3_noun  blt)
{
  u3_noun lin  = u3t(blt);
  c3_c * txt_c = _term_util_noun_to_str(lin);
  c3_w len_w   = strlen(txt_c);
  
  _term_ncurs_left();
  _term_ncurs_clrline();
  c3_w ret_w = printw((char *) txt_c);
  if (0 != ret_w){
    fprintf(stderr, "term.c: ncurses _term_blit_lin() failed %i\n", ret_w);
    u3m_bail(c3__fail); 
  }

  _term_ncurs_xmove(uty_u, len_w);

  _term_ncurs_refresh();

#if STORE_LINE  
  if (uty_u->tat_u.mir.lin_w){
    free(uty_u->tat_u.mir.lin_w);
  }
  uty_u->tat_u.mir.lin_w = (c3_w*) strdup(txt_c); //ugly hack
#endif
  
  free(txt_c);
}

// move cursor down 1 line & to the far left
static void _term_blit_mor(u3_utty* uty_u)
{
  int x, y;
  c3_w ret_w;


  getyx(stdscr, y,x );
  (void) x; // supress compiler warning re variable set-but-not-used

  
  if (y + 1 >= uty_u->tat_u.siz.row_l){
    ret_w = scroll(stdscr); // scroll window 1 line
    if (0 != ret_w){
      fprintf(stderr, "term.c: _term_blit_mor()  1 %i\n", ret_w);
      u3m_bail(c3__fail);
    }
    _term_ncurs_left();

  } else {
    ret_w = move(y + 1, 0);           // move cursor to left and down one
    if (0 != ret_w){
      fprintf(stderr, "term.c: blit c3__more ncurses move()  %i\n", ret_w);
      u3m_bail(c3__fail);
    }

  }
      
  _term_ncurs_refresh();
}


// if you want to debug dill.hoon -> term.c
// stuff, you can change something in dill.hoon from, say
//
//          %+  done  %blit 
//          ;:  weld
//          `(list blit)`~[[%clr ~]]             :: clear the line
//            see
//          `(list blit)`~[[%hop pos]]
// to
//           
//           %+  done  %blit 
//           ;:  weld
//           `(list blit)`~[[%nop (tuba "POM - styled prompt - START")]]
//           `(list blit)`~[[%clr ~]]             :: clear the line
//             see
//           `(list blit)`~[[%hop pos]]
//           `(list blit)`~[[%nop (tuba "POM - styled prompt - END ")]]
// 
// and then those %nop blits will be processed by this function          

static void _term_blit_nop(u3_noun  blt)  
{
  u3_noun lin = u3t(blt);
  c3_c * txt_c =  _term_util_noun_to_str(lin);

  // set your breakpoint here; look at value of txt_c

  
  free(txt_c);
}

static void _term_blit_bog(u3_utty* uty_u, u3_noun  blt)
{
  if ( c3y == u3_Host.ops_u.dem ) {
    // ???
        
    return;
  } 
  /* ncurses doesn't allow us to just set the foreground color,
     or set the background color.  We have to set the 2-tuple of
     foreground color and background color.  ...and we have to
     build that pair, give it a name (an integer pointint to a
     specific slot), and then use that int.  But we have to use
     distinct ints - if we overwrite pair #73 later, stuff
     already on the scree in color #73 2-tuple might change
     color (??).  Solution: slots are defined as
     slot = fg x 8 + bg

     Labor saving hack: don't prepopulate or cache these, just
     recompute as needed.  Optimize later, if at all.

     Also, lack of a cache is find, because on recovery from
     persistence / snapshot, we rebuild everything.

     Also, bc there are distinct blit opcodes for set-FG and
     set-BG, we store current settings in u3_utty. { bog_y,
     fog_y }
  */

  u3_noun bog = u3t(blt);
  c3_w siz_w = u3r_met(3, bog);
  if (1 < siz_w){
    fprintf(stderr, "term.c: blit c3__bog argument wrong size %i\n", siz_w);
    u3m_bail(c3__fail); 
  }
  c3_w bog_w = u3r_word(0, bog);

  uty_u->bog_y = _term_ncurs_mote_to_colr(bog_w, COLOR_BLACK);

  int pair_num = (uty_u->fog_y) * 8 + uty_u->bog_y;

  init_pair(pair_num, uty_u->fog_y, uty_u->bog_y);

  // man page: "The return values of many of these routines are not meaningful"
  wattron(stdscr, COLOR_PAIR(pair_num)); 

  _term_ncurs_refresh();
}

static void _term_blit_fog(u3_utty* uty_u, u3_noun  blt)
{
  if ( c3y == u3_Host.ops_u.dem ) {
    // ???
        
    return;
  } 

  u3_noun fog = u3t(blt);
  c3_w siz_w = u3r_met(3, fog);
  if (1 < siz_w){
    fprintf(stderr, "term.c: blit c3__fog argument wrong size %i\n", siz_w);
    u3m_bail(c3__fail); 
  }
  c3_w fog_w = u3r_word(0, fog);

  uty_u->fog_y = _term_ncurs_mote_to_colr(fog_w, COLOR_WHITE);

  int pair_num = (uty_u->fog_y) * 8 + uty_u->bog_y;

  init_pair(pair_num, uty_u->fog_y, uty_u->bog_y);
        
  // man page: "The return values of many of these routines are  not  meaningful"
  wattron(stdscr, COLOR_PAIR(pair_num)); 

  _term_ncurs_refresh();
}

// text decoration (bl = blink, br = bold, un = underline, ~ = none)
static void _term_blit_eff(u3_utty* uty_u, u3_noun  blt)
{
  u3_noun eff = u3t(blt);
      
  if (c3__eff_no == eff) {
    uty_u->eff_y  = A_NORMAL;
  } else {
    uty_u->eff_y = uty_u->eff_y | _term_ncurs_mote_to_effect(eff) ;
  }

  attron(uty_u->eff_y);
}

static void _term_blit_pri(u3_utty* uty_u, u3_noun  blt)
{

  u3_noun lin = u3t(blt);

  c3_c*   txt_c = _term_util_noun_to_str(lin);

  c3_w ret_w = printw((char *) txt_c);
  if (0 != ret_w){
    fprintf(stderr, "term.c: ncurses _term_blit_pri() failed %i\n", ret_w);
    u3m_bail(c3__fail); 
  }

  _term_ncurs_refresh();
  
#if STORE_LINE  

  if (uty_u->tat_u.mir.lin_w){
    free(uty_u->tat_u.mir.lin_w);
  }
  uty_u->tat_u.mir.lin_w = (c3_w*) strdup(txt_c); //ugly hack
#endif
  
  free(txt_c);
}

static void _term_blit_sxy(u3_utty* uty_u, u3_noun  blt)
{
  if ( c3n == u3_Host.ops_u.dem ) {
    u3_noun pox = u3h(u3t(blt));
    u3_noun poy = u3t(u3t(blt));

    c3_w six_w = u3r_met(3, pox);
    if (4 != six_w){
      fprintf(stderr, "term.c: blit c3__sxy x argument is wrong size %i\n", six_w);
      u3m_bail(c3__fail); 
    }
    c3_w siy_w = u3r_met(3, poy);
    if (4 != siy_w){
      fprintf(stderr, "term.c: blit c3__sxy y argument is wrong size %i\n", siy_w);
      u3m_bail(c3__fail); 
    }

    c3_w pox_w = u3r_word(0, pox);
    c3_w poy_w = u3r_word(0, poy);
        
        
    if((pox_w < 0) || (pox_w > uty_u->tat_u.siz.col_l) ||
       (poy_w < 0) || (poy_w > uty_u->tat_u.siz.row_l)) {
      fprintf(stderr, "term.c: blit c3__sxy position outside of screen specified x = %i, maxcol = %i / y = %i / maxrow = %i\n", pox_w, uty_u->tat_u.siz.col_l, poy_w, uty_u->tat_u.siz.row_l);
      u3m_bail(c3__fail);
    }

    c3_w ret_w;
    ret_w = move(poy_w, pox_w);
    if (0 != ret_w){
      fprintf(stderr, "term.c: blit c3__sxy move() failed %i\n", ret_w);
      u3m_bail(c3__fail);
    }

  }
      
  _term_ncurs_refresh();
}


/* _term_blit_save(): save file by path.
*/
static void _term_blit_save(u3_noun pax, u3_noun pad)
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

// level 2: blit switcher
//


/* _term_ef_blit(): send blit to terminal.
*/
static void
_term_ef_blit(u3_utty* uty_u,
              u3_noun  blt)
{
  switch ( u3h(blt) ) {
  default: break;

  case c3__bee: {
    _term_blit_bee(uty_u, blt);
    break;
  } 

  case c3__bel: {
    _term_blit_bel();
    break;
  } 

  case c3__clr: {
    _term_blit_clr();
    break;
  }
            
  case c3__hop: {
    _term_blit_hop(uty_u, blt);
    break;
  }

  case c3__lin: {
    _term_blit_lin(uty_u, blt);
    break;
  }

  case c3__mor: {
    _term_blit_mor(uty_u);
    break;
  }

  case c3__nop: {
    _term_blit_nop(blt);
    break;
  }

  case c3__bog: {
    _term_blit_bog(uty_u, blt);
    break;
  }

  case c3__fog: {
    _term_blit_fog(uty_u, blt);
    break;
  }

  case c3__eff:{
    _term_blit_eff(uty_u, blt);
    break;
  }

  case c3__pri: {
    _term_blit_pri(uty_u, blt);
    break;
  }
      
  case c3__sxy: {
    _term_blit_sxy(uty_u, blt);
    break;
  }

  case c3__sav: {
    _term_blit_save(u3k(u3h(u3t(blt))), u3k(u3t(u3t(blt))));
  } break;

  case c3__sag: {
    u3_noun pib = u3k(u3t(u3t(blt)));
    u3_noun jam;

    jam = u3ke_jam(pib);

    _term_blit_save(u3k(u3h(u3t(blt))), jam);
  } break;

  case c3__url: {

      _term_blit_lin(uty_u, blt);
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
          perror("hija-tcsetattr-1");
          c3_assert(!"hija-tcsetattr");
        }
        if ( -1 == fcntl(1, F_SETFL, uty_u->cug_i) ) {
          perror("hija-fcntl-1");
          c3_assert(!"hija-fcntl");
        }
        if ( 0 != tcsetattr(0, TCSADRAIN, &uty_u->bak_u) ) {
          perror("hija-tcsetattr-0");
          c3_assert(!"hija-tcsetattr");
        }
        if ( -1 == fcntl(0, F_SETFL, uty_u->cug_i) ) {
          perror("hija-fcntl-0");
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
          perror("loja-tcsetattr-1");
          c3_assert(!"loja-tcsetattr");
        }
        if ( -1 == fcntl(1, F_SETFL, uty_u->nob_i) ) {
          perror("hija-fcntl-1");
          c3_assert(!"loja-fcntl");
        }
        if ( 0 != tcsetattr(0, TCSADRAIN, &uty_u->raw_u) ) {
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

/* u3_term_ef_verb(): initial effects for verbose events
*/
void
u3_term_ef_verb(void)
{
  u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);

  u3v_plan(pax, u3nc(c3__verb, u3_nul));
}

/* u3_term_ef_bake(): initial effects for new terminal.
*/
void
u3_term_ef_bake(u3_noun fav)
{
  u3_noun pax = u3nq(u3_blip, c3__term, '1', u3_nul);

  u3v_plan(u3k(pax), u3nc(c3__boot, fav));
  // u3v_plan(u3k(pax), u3nq(c3__flow, c3__seat, c3__dojo, u3_nul));
  u3v_plan(u3k(pax), u3nc(c3__blew, u3_term_get_blew(1)));
  u3v_plan(u3k(pax), u3nc(c3__hail, u3_nul));

  u3z(pax);
}
