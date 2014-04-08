/* v/loop.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <sigsegv.h>
#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "v/vere.h"

static jmp_buf Signal_buf;
#ifndef SIGSTKSZ
# define SIGSTKSZ 16384
#endif
static uint8_t Sigstk[SIGSTKSZ];

typedef enum {
  sig_none,
  sig_overflow,
  sig_interrupt,
  sig_terminate,
  sig_memory,
  sig_assert,
  sig_timer
} u2_kill;

volatile u2_kill Sigcause;            //  reasons for exception

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
  if ( u2_Critical ) {
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
  if ( !u2_Critical ) {
    Sigcause = sig_terminate;
    u2_Host.liv = u2_no;
    longjmp(Signal_buf, 1);
  }
}

static void
_lo_signal_handle_intr(int x)
{
  if ( !u2_Critical ) {
    Sigcause = sig_interrupt;
    longjmp(Signal_buf, 1);
  }
}

static void
_lo_signal_handle_alrm(int x)
{
  if ( !u2_Critical ) {
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
  u2_unix_ef_move();
}

/* _lo_signal_deep(): start deep processing; set timer for sec_w or 0.
*/
static void
_lo_signal_deep(c3_w sec_w)
{
  u2_unix_ef_hold();

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

/* u2_loop_signal_memory(): end computation for out-of-memory.
*/
void
u2_loop_signal_memory()
{
  fprintf(stderr, "\r\nout of memory\r\n");
  Sigcause = sig_memory;
  longjmp(Signal_buf, 1);
}

/* _lo_init(): initialize I/O across the process.
*/
static void
_lo_init()
{
  u2_unix_io_init();
  u2_ames_io_init();
  u2_term_io_init();
  u2_http_io_init();
  u2_cttp_io_init();
  u2_save_io_init();
  u2_batz_io_init();
}

/* _lo_talk(): bring up listeners across the process.
*/
static void
_lo_talk()
{
  u2_unix_io_talk();
  u2_ames_io_talk();
  u2_http_io_talk();
}

/* u2_lo_exit(): terminate I/O across the process.
*/
void
u2_lo_exit(void)
{
  u2_unix_io_exit();
  u2_ames_io_exit();
  u2_term_io_exit();
  u2_http_io_exit();
  u2_cttp_io_exit();
  u2_save_io_exit();
  u2_batz_io_exit();
}

/* _lo_poll(): reset event flags across the process.
*/
static void
_lo_poll(void)
{
  u2_ames_io_poll();
  u2_http_io_poll();
  u2_term_io_poll();
  u2_save_io_poll();
  u2_unix_io_poll();
  u2_batz_io_poll();
}

#if 0
/* _lo_how(): print how.
*/
static const c3_c*
_lo_how(u2_noun how)
{
  switch ( how ) {
    default: c3_assert(0); break;

    case c3__ames: return "ames";
    case c3__batz: return "batz";
    case c3__term: return "cons";
    case c3__htcn: return "http-conn";
    case c3__htls: return "http-lisn";
    case c3__save: return "save";
    case c3__unix: return "unix";
  }
}
#endif

/* u2_lo_bail(): clean up all event state.
*/
void
u2_lo_bail(u2_reck* rec_u)
{
  fflush(stdout);
  u2_lo_exit();

  exit(1);
}
int c3_cooked() { u2_lo_exit(); return 0; }

/* _lo_tape(): dump a tape, old style.  Don't do this.
*/
static void
_lo_tape(u2_reck* rec_u, FILE* fil_u, u2_noun tep)
{
  u2_noun tap = tep;

  while ( u2_nul != tap ) {
    c3_c car_c;

    if ( u2h(tap) >= 127 ) {
      car_c = '?';
    } else car_c = u2h(tap);

    putc(car_c, fil_u);
    tap = u2t(tap);
  }
  u2z(tep);
}

/* _lo_wall(): dump a wall, old style.  Don't do this.
*/
static void
_lo_wall(u2_reck* rec_u, u2_noun wol)
{
  FILE* fil_u = u2_term_io_hija();
  u2_noun wal = wol;

  while ( u2_nul != wal ) {
    _lo_tape(rec_u, fil_u, u2k(u2h(wal)));

    putc(13, fil_u);
    putc(10, fil_u);

    wal = u2t(wal);
  }
  u2_term_io_loja(0);
  u2z(wol);
}

/* u2_lo_tank(): dump single tank.
*/
void
u2_lo_tank(c3_l tab_l, u2_noun tac)
{
  u2_lo_punt(tab_l, u2nc(tac, u2_nul));
}

/* u2_lo_punt(): dump tank list.
*/
void
u2_lo_punt(c3_l tab_l, u2_noun tac)
{
  u2_noun blu   = u2_term_get_blew(0);
  c3_l    col_l = u2h(blu);
  u2_noun cat   = tac;

  //  We are calling nock here, but hopefully need no protection.
  //
  while ( u2_yes == u2_cr_du(cat) ) {
    u2_noun wol = u2_dc("wash", u2nc(tab_l, col_l), u2k(u2h(cat)));

    _lo_wall(u2_Arv, wol);
    cat = u2t(cat);
  }
  u2z(tac);
  u2z(blu);
}

/* u2_lo_sway(): print trace.
*/
void
u2_lo_sway(c3_l tab_l, u2_noun tax)
{
  u2_noun mok = u2_dc("mook", 2, tax);

  u2_lo_punt(tab_l, u2k(u2t(mok)));
  u2z(mok);
}

/* u2_lo_soft(): standard soft wrapper.  unifies unix and nock errors.
**
**  Produces [%$ result] or [%error (list tank)].
*/
u2_noun
u2_lo_soft(u2_reck* rec_u, c3_w sec_w, u2_funk fun_f, u2_noun arg)
{
  u2_noun hoe, pro, rop;

  u2_rl_leap(u2_Wire, c3__rock);

  //  system level setjmp, for signals
  //
  c3_assert(u2_nul == u2_wire_tax(u2_Wire));
  c3_assert(0 == u2_wire_kit_r(u2_Wire));

  //  stop signals
  //
  u2_unix_ef_hold();
  _lo_signal_deep(sec_w);

  if ( 0 != sigsetjmp(Signal_buf, 1) ) {
    u2_noun tax, pre, mok;

    //  return to blank state
    //
    _lo_signal_done();

    //  acquire trace and reset memory
    //
    tax = u2_wire_tax(u2_Wire);
    u2_rl_fall(u2_Wire);
    u2z(arg);

    tax = u2_rl_take(u2_Wire, tax);
    u2_wire_tax(u2_Wire) = u2_nul;
    mok = u2_dc("mook", 2, tax);

    //  other ugly disgusting cleanups
    {
      u2_wire_kit_r(u2_Wire) = 0;

      u2_hevx_be(u2_wire_hev_r(u2_Wire), u2_pryr, god) = 0;
      u2_hevx_at(u2_wire_hev_r(u2_Wire), lad) = 0;
    }

    switch ( Sigcause ) {
      default:            pre = c3__wyrd; break;
      case sig_none:      pre = c3__none; break;
      case sig_overflow:  pre = c3__over; break;
      case sig_interrupt: pre = c3__intr; break;
      case sig_terminate: pre = c3__term; break;
      case sig_memory:    pre = c3__full; break;
      case sig_assert:    pre = c3__lame; break;
      case sig_timer:     fprintf(stderr, "timer!!\r\n"); pre = c3__slow; break;
    }
    rop = u2nc(pre, u2k(u2t(mok)));
    u2z(mok);
    fprintf(stderr, "error computed\r\n");
    return rop;
  }

  if ( 0 != (hoe = u2_cm_trap()) ) {
    u2_noun mok;

    u2_rl_fall(u2_Wire);
    hoe = u2_rl_take(u2_Wire, hoe);
    u2_rl_flog(u2_Wire);

    mok = u2_dc("mook", 2, u2k(u2t(hoe)));
    rop = u2nc(u2k(u2h(hoe)), u2k(u2t(mok)));

    u2z(arg);
    u2z(hoe);
    u2z(mok);
  }
  else {
    u2_noun pro = fun_f(rec_u, arg);

    _lo_signal_done();
    u2_cm_done();

    u2_rl_fall(u2_Wire);
    pro = u2_rl_take(u2_Wire, pro);
    u2_rl_flog(u2_Wire);

    u2z(arg);
    rop = u2nc(u2_blip, pro);
  }
  pro = rop;

  return pro;
}

#if 0
/* _lo_hard(): standard hard wrapper.  Produces result and/or asserts.
*/
static u2_noun
_lo_hard(u2_reck* rec_u, u2_funk fun_f, u2_noun arg)
{
  u2_noun pro = u2_lo_soft(rec_u, 0, fun_f, arg);

  if ( u2_blip == u2h(pro) ) {
    u2_noun poo = u2k(u2t(pro));

    u2z(pro); return poo;
  }
  else {
    u2_lo_punt(2, u2k(u2t(pro)));
    u2z(pro);
    c3_assert(0);
  }
}
#endif

/* u2_lo_open(): begin callback processing.
*/
void
u2_lo_open(void)
{
  //  update time
  //
  u2_reck_time(u2A);
}

/* u2_lo_shut(): end callback processing.
*/
void
u2_lo_shut(u2_bean inn)
{
  // u2_lo_grab("lo_shut a", u2_none);

  //  process actions
  //
  u2_raft_work(u2A);

  // u2_lo_grab("lo_shut b", u2_none);

  //  update time
  //
  u2_reck_time(u2A);

  // u2_lo_grab("lo_shut c", u2_none);

  //  for input operations, poll fs (XX not permanent)
  //  XX remove raty_lead guard
  //
  if ( u2R->typ_e == u2_raty_lead && u2_yes == inn ) {
    u2_unix_ef_look();
  }

  // u2_lo_grab("lo_shut d", u2_none);

  //  clean shutdown
  //
  if ( u2_no == u2_Host.liv ) {
    //  direct save and die
    //
    u2_cm_purge();
    // u2_lo_grab("lo_exit", u2_none);
    u2_loom_save(u2A->ent_d);
    u2_loom_exit();
    u2_lo_exit();

    exit(0);
  }
  else {
    //  poll arvo to generate any event binding changes
    //
    _lo_poll();
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
    u2_reck_plan(u2A, u2nq(c3__gold, c3__term, 1, u2_nul),
                      u2nc(c3__noop, u2_nul));
  }

  u2_raft_work(u2A);
}

//  _lo_bench_scot_p(): benchmark prettyprint.
//
static void
_lo_bench_scot_p(c3_w num_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < num_w; i_w++ ) {
    u2_noun soc = u2_dc("scot", 'p', u2k(u2A->now));

    u2z(soc);
  }
}

//  _lo_bench_slay_p(): benchmark prettyprint.
//
static void
_lo_bench_slay_p(c3_w num_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < num_w; i_w++ ) {
    u2_noun soc = u2_dc("scot", 'p', u2k(u2A->now));
    u2_noun dub = u2_do("slay", soc);

    u2z(dub);
  }
}

//  _lo_bench_scot_da(): benchmark prettyprint.
//
static void
_lo_bench_scot_da(c3_w num_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < num_w; i_w++ ) {
    u2_noun soc = u2_dc("scot", c3__da, u2k(u2A->now));

    u2z(soc);
  }
}

//  _lo_bench_dec(): benchmark decrement.
//
static void
_lo_bench_dec(c3_w num_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < num_w; i_w++ ) {
    u2_noun soc = u2_do("dec", u2k(u2A->now));

    u2z(soc);
  }
}

//  _lo_bench_scot_ud(): benchmark prettyprint.
//
static void
_lo_bench_scot_ud(c3_w num_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < num_w; i_w++ ) {
    u2_noun soc = u2_dc("scot", c3__ud, u2k(u2A->now));

    u2z(soc);
  }
}

//  _lo_bench(): lo-tech profiling.
//
static void
_lo_bench(const c3_c* lab_c, void (*fun)(c3_w), c3_w num_w)
{
  u2_noun old, new;

  uL(fprintf(uH, "bench: %s: start...\n", lab_c));
  u2_reck_time(u2A);
  old = u2k(u2A->now);

  fun(num_w);

  u2_reck_time(u2A);
  new = u2k(u2A->now);
  {
    c3_w tms_w = (c3_w)u2_time_gap_ms(old, new);

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

/*  u2_lo_show(): generic noun print.
*/
void
u2_lo_show(c3_c* cap_c, u2_noun nun)
{
  u2_noun pav   = u2_dc("pave", c3__noun, nun);
  c3_c*   txt_c = (c3_c*)u2_cr_tape(pav);

  fprintf(stderr, "%s: %s\r\n", cap_c, txt_c);
  u2z(pav);
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

/* u2_lo_loop(): begin main event loop.
*/
void
u2_lo_loop()
{
  uv_loop_t* lup_u = uv_default_loop();

  u2_Host.lup_u = lup_u;

  signal(SIGPIPE, SIG_IGN);     //  pipe, schmipe
  // signal(SIGIO, SIG_IGN);    //  linux is wont to produce for some reason

  _lo_init();
  u2_raft_init();

  if ( u2_no == u2_Host.ops_u.bat ) {
    uv_run(u2L, UV_RUN_DEFAULT);
  }
}

/* u2_lo_lead(): actions on promotion to leader.
*/
void
u2_lo_lead(u2_reck* rec_u)
{
  //  Further server configuration.
  //
  {
    u2_http_ef_bake();
  }

  _lo_talk();
  {
    u2_unix_ef_look();
    u2_reck_plan(rec_u, u2nt(c3__gold, c3__ames, u2_nul),
                        u2nc(c3__kick, u2k(rec_u->now)));
  }
  _lo_poll();

#if 1
  u2_loom_save(rec_u->ent_d);

  u2_Host.sav_u.ent_d = rec_u->ent_d;
#endif

  if ( u2_yes == u2_Host.ops_u.nuu ) {
    u2_term_ef_boil(1);
  }

#if 1
  _lo_slow();
#endif
}

/* _lo_mark_reck(): mark a reck.
*/
static c3_w
_lo_mark_reck(u2_reck* rec_u)
{
  c3_w siz_w = 0;
  c3_w egg_w;

  siz_w += u2_cm_mark_noun(rec_u->ken);
  siz_w += u2_cm_mark_noun(rec_u->roc);

  siz_w += u2_cm_mark_noun(rec_u->yot);
  siz_w += u2_cm_mark_noun(rec_u->now);
  siz_w += u2_cm_mark_noun(rec_u->wen);
  siz_w += u2_cm_mark_noun(rec_u->sen);
  siz_w += u2_cm_mark_noun(rec_u->own);
  siz_w += u2_cm_mark_noun(rec_u->roe);
  siz_w += u2_cm_mark_noun(rec_u->key);

  {
    u2_cart* egg_u;

    egg_w = 0;
    for ( egg_u = rec_u->ova.egg_u; egg_u; egg_u = egg_u->nex_u ) {
      egg_w += u2_cm_mark_noun(egg_u->vir);
    }
    siz_w += egg_w;
  }
#if 0
  fprintf(stderr, "ken %d, roc %d, yot %d, roe %d, egg %d\r\n",
                   ken_w, roc_w, yot_w, roe_w, egg_w);
#endif
  return siz_w;
}

/* _lo_mark(): mark the whole vere system.
*/
static c3_w
_lo_mark()
{
  c3_w siz_w;

  siz_w = u2_cm_mark_internal();
  siz_w += _lo_mark_reck(u2_Host.arv_u);

  return siz_w;
}

/* _lo_word(): print a word to the passed stream.
*/
static void
_lo_word(FILE* fil_u, c3_w wod_w)
{
  u2_bean top = u2_yes;

  if ( wod_w / (1000 * 1000 * 1000) ) {
    fprintf(fil_u, "%u.", wod_w / (1000 * 1000 * 1000));
    wod_w %= (1000 * 1000 * 1000);
    top = u2_no;
  }
  if ( wod_w / (1000 * 1000) ) {
    fprintf(fil_u, ((top == u2_yes) ? "%u." : "%03u."),
                   wod_w / (1000 * 1000));
    wod_w %= (1000 * 1000);
    top = u2_no;
  }
  if ( wod_w / 1000 ) {
    fprintf(fil_u, ((top == u2_yes) ? "%u." : "%03u."), wod_w / 1000);
    wod_w %= 1000;
    top = u2_no;
  }
  fprintf(fil_u, ((top == u2_yes) ? "%u" : "%03u"), wod_w);
}

/* u2_lo_grab(): garbage-collect the world, plus roots.
*/
void
u2_lo_grab(c3_c* cap_c, u2_noun som, ...)
{
  c3_w siz_w, lec_w;

  siz_w = _lo_mark();
  {
    va_list vap;
    u2_noun tur;

    va_start(vap, som);

    if ( som != u2_none ) {
      siz_w += u2_cm_mark_noun(som);

      while ( u2_none != (tur = va_arg(vap, u2_noun)) ) {
        siz_w += u2_cm_mark_noun(tur);
      }
    }
    va_end(vap);
  }
  lec_w = u2_cm_sweep(siz_w);

  // if ( lec_w || (u2_yes == u2_Flag_Verbose) )
  if ( lec_w  || !strcmp("init", cap_c) ) {
    FILE* fil_u = uH;
    fprintf(fil_u, "%s: gc: ", cap_c);
    if ( lec_w ) {
      _lo_word(fil_u, 4 * lec_w);
      fprintf(fil_u, " bytes shed; ");
    }
    _lo_word(fil_u, 4 * siz_w);
    uL(fprintf(fil_u, " bytes live\n"));

#if 0
    if ( lec_w ) {
      uL(fprintf(uH, "zero garbage tolerance!\n"));
      u2_lo_exit();
      c3_assert(0);
      exit(1);
    }
#endif
  }
  u2_wire_lan(u2_Wire) = u2_yes;
}
