/* compat/posix/ptty.c
**
*/
#include "all.h"
#include "vere/vere.h"
#include <sys/ioctl.h>
#include <termios.h>

/*  u3_ptty: POSIX terminal extension to u3_utty.
*/
typedef struct {
  u3_utty          tty_u;  //  common tty structure
  c3_i             cug_i;  //  blocking fcntl flags
  c3_i             nob_i;  //  nonblocking fcntl flags
  struct termios   bak_u;  //  cooked terminal state
  struct termios   raw_u;  //  raw terminal state
} u3_ptty;

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

/*  _ttyf_start_raw_input(): sets the tty to raw input.
*/
static c3_o
_ttyf_start_raw_input(u3_utty* uty_u)
{
  u3_ptty* pty_u = (u3_ptty*)uty_u;
  if ( 0 != _term_tcsetattr(uty_u->fid_i, TCSADRAIN, &pty_u->raw_u) ) {
    return c3n;
  }
  if ( -1 == fcntl(uty_u->fid_i, F_SETFL, pty_u->nob_i) ) {
    c3_assert(!"init-fcntl");
  }
  return c3y;
}

/*  _ttyf_start_raw_input(): ends raw input on the tty.
*/
static c3_o
_ttyf_end_raw_input(u3_utty* uty_u)
{
  u3_ptty* pty_u = (u3_ptty*)uty_u;
  if ( 0 != _term_tcsetattr(uty_u->fid_i, TCSADRAIN, &pty_u->bak_u) ) {
    return c3n;
  }
  if ( -1 == fcntl(uty_u->fid_i, F_SETFL, pty_u->cug_i) ) {
    c3_assert(!"exit-fcntl");
  }
  return c3y;
}

/*  _ttyf_hija(): hijacks the tty for cooked output.
*/
static c3_o
_ttyf_hija(u3_utty* uty_u)
{
  u3_ptty* pty_u = (u3_ptty*)uty_u;
  if ( 0 != _term_tcsetattr(1, TCSADRAIN, &pty_u->bak_u) ) {
    perror("hija-tcsetattr-1");
    c3_assert(!"hija-tcsetattr");
  }
  if ( -1 == fcntl(1, F_SETFL, pty_u->cug_i) ) {
    perror("hija-fcntl-1");
    c3_assert(!"hija-fcntl");
  }
  if ( 0 != _term_tcsetattr(0, TCSADRAIN, &pty_u->bak_u) ) {
    perror("hija-tcsetattr-0");
    c3_assert(!"hija-tcsetattr");
  }
  if ( -1 == fcntl(0, F_SETFL, pty_u->cug_i) ) {
    perror("hija-fcntl-0");
    c3_assert(!"hija-fcntl");
  }
  return c3y;
}

/*  _ttyf_loja(): releases the tty from cooked output.
*/
static c3_o
_ttyf_loja(u3_utty* uty_u)
{
  u3_ptty* pty_u = (u3_ptty*)uty_u;
  if ( 0 != _term_tcsetattr(1, TCSADRAIN, &pty_u->raw_u) ) {
    perror("loja-tcsetattr-1");
    c3_assert(!"loja-tcsetattr");
  }
  if ( -1 == fcntl(1, F_SETFL, pty_u->nob_i) ) {
    perror("hija-fcntl-1");
    c3_assert(!"loja-fcntl");
  }
  if ( 0 != _term_tcsetattr(0, TCSADRAIN, &pty_u->raw_u) ) {
    perror("loja-tcsetattr-0");
    c3_assert(!"loja-tcsetattr");
  }
  if ( -1 == fcntl(0, F_SETFL, pty_u->nob_i) ) {
    perror("hija-fcntl-0");
    c3_assert(!"loja-fcntl");
  }
  return c3y;
}

/*  _ttyf_get_winsize(): gets the tty window size.
*/
static c3_o
_ttyf_get_winsize(u3_utty* uty_u, c3_l* col_l, c3_l* row_l)
{
  struct winsize siz_u;
  if ( 0 == ioctl(uty_u->fid_i, TIOCGWINSZ, &siz_u) )
  {
    *col_l = siz_u.ws_col;
    *row_l = siz_u.ws_row;
    return c3y;
  } else {
    return c3n;
  }
}

/* u3_ptty_init(): initialize platform-specific tty.
*/
u3_utty*
u3_ptty_init(uv_loop_t* lup_u, const c3_c** err_c)
{
  u3_ptty* pty_u = c3_calloc(sizeof(u3_ptty));
  u3_utty* uty_u = &pty_u->tty_u;

  if ( !isatty(0) || !isatty(1) ) {
    *err_c = "not a tty";
    c3_free(pty_u);
    return NULL;
  }

  uv_pipe_init(lup_u, &uty_u->pin_u.pip_u, 0);
  uv_pipe_init(lup_u, &uty_u->pop_u.pip_u, 0);
  uv_pipe_open(&uty_u->pin_u.pip_u, 0);
  uv_pipe_open(&uty_u->pop_u.pip_u, 1);

  //  Load old terminal state to restore.
  //
  {
    if ( 0 != tcgetattr(uty_u->fid_i, &pty_u->bak_u) ) {
      c3_assert(!"init-tcgetattr");
    }
    if ( -1 == fcntl(uty_u->fid_i, F_GETFL, &pty_u->cug_i) ) {
      c3_assert(!"init-fcntl");
    }
    pty_u->cug_i &= ~O_NONBLOCK;                // could fix?
    pty_u->nob_i = pty_u->cug_i | O_NONBLOCK;   // O_NDELAY on older unix
  }

  //  Construct raw termios configuration.
  //
  //    makes input available per-character, does not echo input,
  //    disables special input pre-processing, output post-processing.
  //
  {
    pty_u->raw_u = pty_u->bak_u;

    pty_u->raw_u.c_lflag &= ~(ECHO | ECHONL | ICANON | IEXTEN);
    pty_u->raw_u.c_iflag &= ~(ICRNL | INPCK | ISTRIP);
    pty_u->raw_u.c_cflag &= ~(CSIZE | PARENB);
    pty_u->raw_u.c_cflag |= CS8;
    pty_u->raw_u.c_oflag &= ~(OPOST);
    pty_u->raw_u.c_cc[VMIN] = 0;
    pty_u->raw_u.c_cc[VTIME] = 0;
  }

  uty_u->fid_i = 1;
  uty_u->sta_f = _ttyf_start_raw_input;
  uty_u->sto_f = _ttyf_end_raw_input;
  uty_u->hij_f = _ttyf_hija;
  uty_u->loj_f = _ttyf_loja;
  uty_u->wsz_f = _ttyf_get_winsize;
  return uty_u;
}
