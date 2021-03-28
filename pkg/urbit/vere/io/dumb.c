/* vere/dumb.c
**
*/
#include "all.h"
#include "vere/vere.h"

/* u3_term_log_init(): initialize terminal for logging
*/
void
u3_term_log_init(void)
{
}

/* u3_term_log_exit(): clean up terminal.
*/
void
u3_term_log_exit(void)
{
}

/* u3_term_start_spinner(): prepare spinner state. RETAIN.
*/
void
u3_term_start_spinner(u3_atom say, c3_o del_o)
{
}

/* u3_term_stop_spinner(): reset spinner state and restore input line.
*/
void
u3_term_stop_spinner(void)
{
}

/* u3_term_get_blew(): return window size [columns rows].
*/
u3_noun
u3_term_get_blew(c3_l tid_l)
{
  return u3nc(80, 24);
}

/* u3_term_ef_winc(): window change.  Just console right now.
*/
void
u3_term_ef_winc(void)
{
}

/* u3_term_ef_ctlc(): send ^C on console.
*/
void
u3_term_ef_ctlc(void)
{
}

/* u3_term_io_hija(): hijack console for fprintf, returning FILE*.
*/
FILE*
u3_term_io_hija(void)
{
  return stdout;
}

/* u3_term_io_loja(): release console from fprintf.
*/
void
u3_term_io_loja(int x)
{
  fflush(stdout);
}

/* u3_term_it_log(): writes a log message
*/
void
u3_term_io_log(c3_c* line)
{
  FILE* stream = u3_term_io_hija();
  u3_term_io_loja(fprintf(stream, "%s", line));
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

/* _term_io_talk():
*/
static void
_term_io_talk(u3_auto* car_u)
{
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
    // eat everything
    ret_o = c3y;
  }

  u3z(wir); u3z(cad);
  return ret_o;
}

/* _term_io_exit(): clean up terminal.
*/
static void
_term_io_exit(u3_auto* car_u)
{
  c3_free(car_u);
}

/* u3_term_io_init(): initialize terminal
*/
u3_auto*
u3_term_io_init(u3_pier* pir_u)
{
  u3_auto* car_u = c3_calloc(sizeof(*car_u));

  car_u->nam_m = c3__term;
  car_u->liv_o = c3y;
  car_u->io.talk_f = _term_io_talk;
  car_u->io.kick_f = _term_io_kick;
  car_u->io.exit_f = _term_io_exit;

  return car_u;
}
