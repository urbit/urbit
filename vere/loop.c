/* vere/loop.c
**
*/
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sigsegv.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "vere/vere.h"

#if 0
static jmp_buf Signal_buf;
#ifndef SIGSTKSZ
# define SIGSTKSZ 16384
#endif
static uint8_t Sigstk[SIGSTKSZ];

uint8_t u3_Critical;

typedef enum {
  sig_none,
  sig_overflow,
  sig_interrupt,
  sig_terminate,
  sig_memory,
  sig_assert,
  sig_timer
} u3_kill;

volatile u3_kill Sigcause;            //  reasons for exception

static void _lo_cont(void *arg1, void *arg2, void *arg3)
{
  (void)(arg1);
  (void)(arg2);
  (void)(arg3);
  siglongjmp(Signal_buf, 1);
}

static void
_lo_signal_handle_over(int emergency, stackoverflow_context_t scp)
{
  if ( u3_Critical ) {
    //  Careful not to grow the stack during critical sections.
    //
    write(2, "stack disaster\n", strlen("stack disaster" + 2));
    abort();
  }

#if 0
  if ( 1 == emergency ) {
    write(2, "stack emergency\n", strlen("stack emergency" + 2));
    abort();
  } else
#endif
  {
    Sigcause = sig_overflow;
    sigsegv_leave_handler(_lo_cont, NULL, NULL, NULL);
  }
}

static void
_lo_signal_handle_term(int x)
{
  if ( !u3_Critical ) {
    Sigcause = sig_terminate;
    u3_Host.liv = c3n;
    longjmp(Signal_buf, 1);
  }
}

static void
_lo_signal_handle_intr(int x)
{
  if ( !u3_Critical ) {
    Sigcause = sig_interrupt;
    longjmp(Signal_buf, 1);
  }
}

static void
_lo_signal_handle_alrm(int x)
{
  if ( !u3_Critical ) {
    Sigcause = sig_timer;
    longjmp(Signal_buf, 1);
  }
}

/* _lo_signal_done():
*/
static void
_lo_signal_done()
{
  // signal(SIGINT, SIG_IGN);
  signal(SIGTERM, SIG_IGN);
  signal(SIGVTALRM, SIG_IGN);

  stackoverflow_deinstall_handler();
  {
    struct itimerval itm_u;

    timerclear(&itm_u.it_interval);
    timerclear(&itm_u.it_value);

    setitimer(ITIMER_VIRTUAL, &itm_u, 0);
  }
  u3_unix_ef_move();
}

/* _lo_signal_deep(): start deep processing; set timer for sec_w or 0.
*/
static void
_lo_signal_deep(c3_w sec_w)
{
  u3_unix_ef_hold();

  stackoverflow_install_handler(_lo_signal_handle_over, Sigstk, SIGSTKSZ);
  signal(SIGINT, _lo_signal_handle_intr);
  signal(SIGTERM, _lo_signal_handle_term);

  {
    struct itimerval itm_u;

    timerclear(&itm_u.it_interval);
    itm_u.it_value.tv_sec = sec_w;
    itm_u.it_value.tv_usec = 0;

    setitimer(ITIMER_VIRTUAL, &itm_u, 0);
  }
  signal(SIGVTALRM, _lo_signal_handle_alrm);
}
#endif

/* u3_loop_signal_memory(): end computation for out-of-memory.
*/
void
u3_loop_signal_memory()
{
  fprintf(stderr, "\r\nout of memory\r\n");
  c3_assert(0);

#if 0
  Sigcause = sig_memory;
  longjmp(Signal_buf, 1);
#endif
}

/* _lo_init(): initialize I/O across the process.
*/
static void
_lo_init()
{
  c3_l cod_l;

  cod_l = u3a_lush(c3__unix);
  u3_unix_io_init();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__ames);
  u3_ames_io_init();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__term);
  u3_term_io_init();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__http);
  u3_http_io_init();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__cttp);
  u3_cttp_io_init();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__save);
  u3_save_io_init();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__behn);
  u3_behn_io_init();
  u3a_lop(cod_l);
}

/* _lo_talk(): bring up listeners across the process.
*/
static void
_lo_talk()
{
  c3_l cod_l;

  cod_l = u3a_lush(c3__unix);
  u3_unix_io_talk();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__ames);
  u3_ames_io_talk();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__http);
  u3_http_io_talk();
  u3a_lop(cod_l);
   
}

/* u3_lo_exit(): terminate I/O across the process.
*/
void
u3_lo_exit(void)
{
  c3_l cod_l;

  cod_l = u3a_lush(c3__unix);
  u3_unix_io_exit();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__ames);
  u3_ames_io_exit();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__term);
  u3_term_io_exit();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__http);
  u3_http_io_exit();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__cttp);
  u3_cttp_io_exit();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__save);
  u3_save_io_exit();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__behn);
  u3_behn_io_exit();
  u3a_lop(cod_l);

  if ( c3y == __(u3C.wag_w & u3o_trace) ) {
    printf("saving trace file.\r\n");
    u3t_trace_close();
  }
}

#if 0
/* _lo_how(): print how.
*/
static const c3_c*
_lo_how(u3_noun how)
{
  switch ( how ) {
    default: c3_assert(0); break;

    case c3__ames: return "ames";
    case c3__behn: return "behn";
    case c3__term: return "cons";
    case c3__htcn: return "http-conn";
    case c3__htls: return "http-lisn";
    case c3__save: return "save";
    case c3__unix: return "unix";
  }
}
#endif

/* u3_lo_bail(): clean up all event state.
*/
void
u3_lo_bail(void)
{
  fflush(stdout);
  u3_lo_exit();

  exit(1);
}

/* _lo_tape(): dump a tape, old style.  Don't do this.
*/
static void
_lo_tape(FILE* fil_u, u3_noun tep)
{
  u3_noun tap = tep;

  while ( u3_nul != tap ) {
    c3_c car_c;

    if ( u3h(tap) >= 127 ) {
      car_c = '?';
    } else car_c = u3h(tap);

    putc(car_c, fil_u);
    tap = u3t(tap);
  }
  u3z(tep);
}

/* _lo_wall(): dump a wall, old style.  Don't do this.
*/
static void
_lo_wall(u3_noun wol)
{
  FILE* fil_u = u3_term_io_hija();
  u3_noun wal = wol;

  while ( u3_nul != wal ) {
    _lo_tape(fil_u, u3k(u3h(wal)));

    putc(13, fil_u);
    putc(10, fil_u);

    wal = u3t(wal);
  }
  u3_term_io_loja(0);
  u3z(wol);
}

/* u3_lo_tank(): dump single tank.
*/
void
u3_lo_tank(c3_l tab_l, u3_noun tac)
{
  u3_lo_punt(tab_l, u3nc(tac, u3_nul));
}

/* u3_lo_punt(): dump tank list.
*/
void
u3_lo_punt(c3_l tab_l, u3_noun tac)
{
  u3_noun blu   = u3_term_get_blew(0);
  c3_l    col_l = u3h(blu);
  u3_noun cat   = tac;

  //  We are calling nock here, but hopefully need no protection.
  //
  while ( c3y == u3r_du(cat) ) {
    u3_noun wol = u3dc("wash", u3nc(tab_l, col_l), u3k(u3h(cat)));

    _lo_wall(wol);
    cat = u3t(cat);
  }
  u3z(tac);
  u3z(blu);
}

/* u3_lo_sway(): print trace.
*/
void
u3_lo_sway(c3_l tab_l, u3_noun tax)
{
  u3_noun mok = u3dc("mook", 2, tax);

  u3_lo_punt(tab_l, u3k(u3t(mok)));
  u3z(mok);
}

/* _lo_time(): set time.
*/
static void
_lo_time(void)
{
  struct timeval tim_tv;

  gettimeofday(&tim_tv, 0);
  u3v_time(u3_time_in_tv(&tim_tv));
}

/* u3_lo_open(): begin callback processing.
*/
void
u3_lo_open(void)
{
#if 0
  if ( u3C.wag_w & u3o_debug_cpu ) {
    struct itimerval itm_u;

    getitimer(ITIMER_VIRTUAL, &itm_u);
    fprintf(stderr, "tv_sec %d, tv_usec %d, value %d/%d\r\n",
                     itm_u.it_interval.tv_sec,
                     itm_u.it_interval.tv_usec,
                     itm_u.it_value.tv_sec,
                     itm_u.it_interval.tv_usec);
  }
#endif

  _lo_time();
}

/* u3_lo_shut(): end callback processing.
*/
void
u3_lo_shut(c3_o inn)
{
  //  process actions
  //
  u3_raft_work();

  //  update time
  //
  _lo_time();

  //  for input operations, poll fs (XX not permanent)
  //  XX remove raty_lead guard
  //
  if ( c3y == inn ) {
    u3_unix_ef_look(c3n);
    u3_raft_work();
    _lo_time();
  }

  //  clean shutdown
  //
  if ( c3n == u3_Host.liv ) {
    //  direct save and die
    //
    u3_raft_play();
    // u3_loom_save(u3A->ent_d);
    // u3_loom_exit();
    u3t_damp();
    u3_lo_exit();

    //  save a checkpoint before exiting
    u3e_save();
    exit(u3_Host.xit_i);
  }
}

#if 0
//  _lo_bench_noop(): benchmark no-op events.
//
static void
_lo_bench_noop(c3_w num_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < num_w; i_w++ ) {
    u3_reck_plan(u3A, u3nq(u3_blip, c3__term, 1, u3_nul),
                      u3nc(c3__noop, u3_nul));
  }

  u3_raft_work(u3A);
}

//  _lo_bench_scot_p(): benchmark prettyprint.
//
static void
_lo_bench_scot_p(c3_w num_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < num_w; i_w++ ) {
    u3_noun soc = u3dc("scot", 'p', u3k(u3A->now));

    u3z(soc);
  }
}

//  _lo_bench_slay_p(): benchmark prettyprint.
//
static void
_lo_bench_slay_p(c3_w num_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < num_w; i_w++ ) {
    u3_noun soc = u3dc("scot", 'p', u3k(u3A->now));
    u3_noun dub = u3do("slay", soc);

    u3z(dub);
  }
}

//  _lo_bench_scot_da(): benchmark prettyprint.
//
static void
_lo_bench_scot_da(c3_w num_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < num_w; i_w++ ) {
    u3_noun soc = u3dc("scot", c3__da, u3k(u3A->now));

    u3z(soc);
  }
}

//  _lo_bench_dec(): benchmark decrement.
//
static void
_lo_bench_dec(c3_w num_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < num_w; i_w++ ) {
    u3_noun soc = u3do("dec", u3k(u3A->now));

    u3z(soc);
  }
}

//  _lo_bench_scot_ud(): benchmark prettyprint.
//
static void
_lo_bench_scot_ud(c3_w num_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < num_w; i_w++ ) {
    u3_noun soc = u3dc("scot", c3__ud, u3k(u3A->now));

    u3z(soc);
  }
}

//  _lo_bench(): lo-tech profiling.
//
static void
_lo_bench(const c3_c* lab_c, void (*fun)(c3_w), c3_w num_w)
{
  u3_noun old, new;

  uL(fprintf(uH, "bench: %s: start...\n", lab_c));
  u3_reck_time(u3A);
  old = u3k(u3A->now);

  fun(num_w);

  u3_reck_time(u3A);
  new = u3k(u3A->now);
  {
    c3_w tms_w = (c3_w)u3_time_gap_ms(old, new);

    if ( tms_w > (10 * num_w) ) {
      uL(fprintf(uH, "bench: %s*%d: %d ms, %d ms each.\n",
                      lab_c, num_w, tms_w, (tms_w / num_w)));
    }
    else {
      uL(fprintf(uH, "bench: %s*%d: %d ms, %d us each.\n",
                      lab_c, num_w, tms_w, ((tms_w * 1000) / num_w)));
    }
  }
}
#endif

/*  u3_lo_show(): generic noun print.
*/
void
u3_lo_show(c3_c* cap_c, u3_noun nun)
{
  u3_noun pav   = u3dc("pave", c3__noun, nun);
  c3_c*   txt_c = (c3_c*)u3r_tape(pav);

  fprintf(stderr, "%s: %s\r\n", cap_c, txt_c);
  u3z(pav);
  free(txt_c);
}

static void
_lo_slow()
{
#if 0
  _lo_bench("scot %p", _lo_bench_scot_p, 256);
  _lo_bench("scot %da", _lo_bench_scot_da, 256);
  _lo_bench("scot %ud", _lo_bench_scot_ud, 256);
  _lo_bench("slay %p", _lo_bench_slay_p, 256);
  _lo_bench("noop", _lo_bench_noop, 256);
#endif
}

/* u3_lo_loop(): begin main event loop.
*/
void
u3_lo_loop()
{
  uv_loop_t* lup_u = uv_default_loop();

  u3_Host.lup_u = lup_u;

  signal(SIGPIPE, SIG_IGN);     //  pipe, schmipe
  // signal(SIGIO, SIG_IGN);    //  linux is wont to produce for some reason

  _lo_init();

  u3_raft_init();

  if ( _(u3_Host.ops_u.tex) ) {
    u3t_boff();
    u3t_damp();
    u3_lo_exit();

    fprintf(stderr, "dry run: exit\r\n");
    exit(0);
  }
  else {
    if ( c3n == u3_Host.ops_u.bat ) {
      uv_run(u3L, UV_RUN_DEFAULT);
    }
  }
}

/* u3_lo_lead(): actions on promotion to leader.
*/
void
u3_lo_lead(void)
{
  //  Further server configuration.
  //
  {
    if ( c3n == u3_Host.ops_u.nuu ) {
      u3_ames_ef_bake();
      u3_behn_ef_bake();
    }

    u3_http_ef_bake();
  }

  _lo_talk();
  {
    u3_unix_ef_look(c3n);
    u3v_plan(u3nt(u3_blip, c3__ames, u3_nul),
               u3nc(c3__kick, u3k(u3A->now)));
  }

#if 0
  u3_loom_save(u3A->ent_d);

  u3_Host.sav_u.ent_d = rec_u->ent_d;
#endif

  if ( c3y == u3_Host.ops_u.nuu ) {
    u3_term_ef_boil(1);
  }

  if ( c3y == u3_Host.ops_u.veb ) {
    u3_term_ef_verb();
  }

#if 1
  _lo_slow();
#endif
}
