/* v/loop.c
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
#include <sigsegv.h>
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

#define AMES

#if defined(U2_OS_linux)
#include <stdio_ext.h>
#define fpurge(fd) __fpurge(fd)
#define DEVRANDOM "/dev/urandom"
#else
#define DEVRANDOM "/dev/random"
#endif

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
    longjmp(Signal_buf, 1);
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
  signal(SIGINT, SIG_IGN);
  signal(SIGTERM, SIG_IGN);
  signal(SIGVTALRM, SIG_IGN);

  stackoverflow_deinstall_handler();
  {
    struct itimerval itm_u;

    timerclear(&itm_u.it_interval);
    timerclear(&itm_u.it_value);

    setitimer(ITIMER_VIRTUAL, &itm_u, 0);
  }
}

/* _lo_signal_deep(): start deep processing; set timer for sec_w or 0.
*/
static void
_lo_signal_deep(c3_w sec_w)
{
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
_lo_init(u2_reck* rec_u)
{
  u2_unix_io_init(rec_u);
  u2_ames_io_init(rec_u);
  u2_term_io_init(rec_u);
  u2_http_io_init(rec_u);
  u2_save_io_init(rec_u);
  u2_behn_io_init(rec_u);
}

/* _lo_exit(): terminate I/O across the process.
*/
static void
_lo_exit(u2_reck* rec_u)
{
  u2_unix_io_exit(rec_u);
  u2_ames_io_exit(rec_u);
  u2_term_io_exit(rec_u);
  u2_http_io_exit(rec_u);
  u2_save_io_exit(rec_u);
  u2_behn_io_exit(rec_u);
}

/* _lo_stop(): stop event I/O across the process.
*/
static void
_lo_stop(u2_reck*        rec_u,
         struct ev_loop* lup_u)
{
  u2_ames_io_stop(rec_u, lup_u);
  u2_http_io_stop(rec_u, lup_u);
  u2_term_io_stop(rec_u, lup_u);
  u2_save_io_stop(rec_u, lup_u);
  u2_unix_io_stop(rec_u, lup_u);
  u2_behn_io_stop(rec_u, lup_u);
}

/* _lo_poll(): reset event flags across the process.
*/
static void
_lo_poll(u2_reck*        rec_u,
         struct ev_loop* lup_u)
{
  u2_ames_io_poll(rec_u, lup_u);
  u2_http_io_poll(rec_u, lup_u);
  u2_term_io_poll(rec_u, lup_u);
  u2_save_io_poll(rec_u, lup_u);
  u2_unix_io_poll(rec_u, lup_u);
  u2_behn_io_poll(rec_u, lup_u);
}

/* _lo_spin(): restart event I/O across the process.
*/
static void
_lo_spin(u2_reck*        rec_u,
         struct ev_loop* lup_u)
{
  u2_ames_io_spin(rec_u, lup_u);
  u2_http_io_spin(rec_u, lup_u);
  u2_term_io_spin(rec_u, lup_u);
  u2_save_io_spin(rec_u, lup_u);
  u2_unix_io_spin(rec_u, lup_u);
  u2_behn_io_spin(rec_u, lup_u);
}

/* _lo_how(): print how.
*/
static const c3_c* 
_lo_how(u2_noun how)
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

/* _lo_time(): process timer on a socket.
*/
static void
_lo_time(u2_reck*         rec_u,
         struct ev_timer* tim_u,
         u2_noun          how)
{
  switch ( how ) {
    default: c3_assert(0); break;

    case c3__ames: u2_ames_io_time(rec_u, tim_u); break;
    case c3__behn: u2_behn_io_time(rec_u, tim_u); break;
    case c3__save: u2_save_io_time(rec_u, tim_u); break;
    case c3__unix: u2_unix_io_time(rec_u, tim_u); break;
  }
} 

/* _lo_sign(): process signal.
*/
static void
_lo_sign(u2_reck*          rec_u,
         struct ev_signal* sil_u,
         u2_noun           how)
{
  switch ( how ) {
    default: c3_assert(0); break;

    case c3__unix: u2_unix_io_sign(rec_u, sil_u); break;
  }
} 

/* _lo_stat(): process filesystem change.
*/
static void
_lo_stat(u2_reck*        rec_u,
         struct ev_stat* sat_u,
         u2_noun         how)
{
  switch ( how ) {
    default: c3_assert(0); break;

    case c3__unix: u2_unix_io_stat(rec_u, sat_u); break;
  }
} 

/* _lo_suck(): process input on a socket.
*/
static void
_lo_suck(u2_reck*      rec_u,
         struct ev_io* wax_u,
         u2_noun       how)
{
  switch ( how ) {
    default: c3_assert(0); break;

    case c3__ames: u2_ames_io_suck(rec_u, wax_u); break;
    case c3__term: u2_term_io_suck(rec_u, wax_u); break;
    case c3__htcn: u2_http_io_suck_conn(rec_u, wax_u); break;
    case c3__htls: u2_http_io_suck_lisn(rec_u, wax_u); break;
  }
}

/* _lo_fuck(): process output on a socket.
*/
static void
_lo_fuck(u2_reck*      rec_u,
         struct ev_io* wax_u,
         u2_noun       how)
{
  switch ( how ) {
    default: c3_assert(0); break;

    case c3__ames: u2_ames_io_fuck(rec_u, wax_u); break;
    case c3__term: u2_term_io_fuck(rec_u, wax_u); break;
    case c3__htcn: u2_http_io_fuck_conn(rec_u, wax_u); break;
    case c3__htls: c3_assert(0); break;
  }
}

/* u2_lo_bail(): clean up all event state.
*/
void
u2_lo_bail(u2_reck* rec_u)
{
  fflush(stdout);
  _lo_exit(rec_u);

  exit(1);
}
int c3_cooked() { _lo_exit(u2_Host.arv_u); return 0; }

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

/* _lo_punt(): dump tank list.  A last resort, hopefully soon abolished.
*/
static void
_lo_punt(u2_reck* rec_u, c3_l tab_l, u2_noun tac)
{
  c3_l    col_l = u2_ve_dump_columns();
  u2_noun cat   = tac;

  //  We are calling nock here, but hopefully need no protection.
  //
  while ( u2_yes == u2_cr_du(cat) ) {
    u2_noun wol = u2_dc("wash", u2nc(tab_l, col_l), u2k(u2h(cat)));
    _lo_wall(rec_u, wol);
    cat = u2t(cat);
  }
  u2z(tac);
}

/* _lo_soft(): standard soft wrapper.  unifies unix and nock errors.
**
**  Produces [%% result] or [%error (list tank)].
*/
static u2_noun
_lo_soft(u2_reck* rec_u, c3_w sec_w, u2_funk fun_f, u2_noun arg)
{
  u2_noun hoe, pro, rop;

  u2_rl_leap(u2_Wire, c3__rock);

  //  system level setjmp, for signals
  //
  c3_assert(u2_nul == u2_wire_tax(u2_Wire));
  c3_assert(0 == u2_wire_kit_r(u2_Wire));

  _lo_signal_deep(sec_w);

  if ( 0 != setjmp(Signal_buf) ) {
    u2_noun tax, pre, mok;

    //  return to blank state
    //
    _lo_signal_done();

    //  acquire trace and reset memory
    //
    tax = u2_wire_tax(u2_Wire);
    u2_rl_fall(u2_Wire);

    tax = u2_rl_take(u2_Wire, tax);
    mok = u2_dc("mook", 2, tax);
    u2_wire_tax(u2_Wire) = u2_nul;

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
      case sig_timer:     pre = c3__slow; break;
    }
    rop = u2nc(pre, u2k(u2t(mok)));
    u2z(mok);
    return rop;
  }

  if ( 0 != (hoe = u2_cm_trap()) ) {
    u2_noun mok;

    u2_rl_fall(u2_Wire);
    hoe = u2_rl_take(u2_Wire, hoe);
    u2_rl_flog(u2_Wire);

    mok = u2_dc("mook", 2, u2k(u2t(hoe)));
    rop = u2nc(u2k(u2h(hoe)), u2k(u2t(mok)));

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

    rop = u2nc(u2_blip, pro);
  }
  pro = rop;

  return pro;
}

/* _lo_hard(): standard hard wrapper.  Produces result and/or asserts.
*/
static u2_noun
_lo_hard(u2_reck* rec_u, u2_funk fun_f, u2_noun arg)
{
  u2_noun pro = _lo_soft(rec_u, 0, fun_f, arg);

  if ( u2_blip == u2h(pro) ) {
    u2_noun poo = u2k(u2t(pro));

    u2z(pro); return poo;
  } 
  else {
    _lo_punt(rec_u, 2, u2k(u2t(pro)));
    u2z(pro);
    c3_assert(0);
  }
}

/* _lo_mung(): formula wrapper with gate and sample.
*/
  static u2_noun
  _lo_mung_in(u2_reck* rec_u, u2_noun gam)
  {
    u2_noun pro = u2_cn_mung(u2k(u2h(gam)), u2k(u2t(gam)));

    u2z(gam); return pro;
  }
static u2_noun
_lo_mung(u2_reck* rec_u, c3_w sec_w, u2_noun gat, u2_noun sam)
{
  u2_noun gam = u2nc(gat, sam);

  return _lo_soft(rec_u, 0, _lo_mung_in, gam);
}

/* _lo_pack(): save an ovum at the present time.  sync; sync; sync.
*/
static void
_lo_pack(u2_reck* rec_u, u2_noun ron)
{
  u2_ulog* lug_u = &u2_Host.lug_u;
  c3_w     len_w, tar_w;
  c3_w*    img_w;
  u2_ular  lar_u;

  if ( rec_u->key ) {
    ron = u2_dc("en:crya", u2k(rec_u->key), ron);
  }

  len_w = u2_cr_met(5, ron);
  tar_w = (lug_u->len_w + len_w);

  lar_u.syn_w = u2_mug(tar_w);
  lar_u.mug_w = u2_mug(ron);
  lar_u.ent_w = rec_u->ent_w;
  lar_u.len_w = len_w;

  //  XX: this is not in any way, shape or form a proper 2PC!
  //
  if ( -1 == lseek(lug_u->fid_i, 4 * tar_w, SEEK_SET) ) {
    perror("lseek");
    uL(fprintf(uH, "lo_save: seek failed\n"));
    c3_assert(0);
  }
  if ( sizeof(lar_u) != write(lug_u->fid_i, &lar_u, sizeof(lar_u)) ) {
    perror("write");
    uL(fprintf(uH, "lo_save: write failed\n"));
    c3_assert(0);
  }
  if ( -1 == lseek(lug_u->fid_i, 4 * lug_u->len_w, SEEK_SET) ) {
    perror("lseek");
    uL(fprintf(uH, "lo_save: seek failed\n"));
    c3_assert(0);
  }
#if 0 
  uL(fprintf(uH, "log: write: at %d, %d: lar ent %d, len %d, mug %x\n", 
                  lug_u->len_w,
                  tar_w,
                  lar_u.ent_w,
                  lar_u.len_w,
                  lar_u.mug_w));
#endif
  img_w = malloc(4 * len_w);
  u2_cr_words(0, len_w, img_w, ron);

  if ( (4 * len_w) != write(lug_u->fid_i, img_w, (4 * len_w)) ) {
    perror("lseek");
    uL(fprintf(uH, "lo_save: write failed\n"));
    c3_assert(0);
  }
  lug_u->len_w += (lar_u.len_w + c3_wiseof(lar_u));
  free(img_w);

  // Sync.  Or, what goes by sync.
  {
    fsync(lug_u->fid_i);    //  fsync is useless, F_FULLFSYNC too slow
  }
}

/* _lo_save(): log an ovum at the present time.
*/
static void
_lo_save(u2_reck* rec_u, u2_noun ovo)
{
  u2_noun ron = u2_cke_jam(u2nc(u2k(rec_u->now), ovo));

  if ( u2_Host.lug_u.len_w ) {
    _lo_pack(rec_u, ron);
    rec_u->ent_w += 1;
  } else {
    rec_u->roe = u2nc(ron, rec_u->roe);
  }
}

/* _lo_sing(): replay ovum from the past, time already set.
*/
static void
_lo_sing(u2_reck* rec_u, u2_noun ovo)
{
  u2_noun gon = _lo_soft(rec_u, 0, u2_reck_poke, u2k(ovo));

  if ( u2_blip != u2h(gon) ) {
    uL(fprintf(uH, "sing: ovum failed!\n"));
    _lo_punt(rec_u, 2, u2k(u2t(gon)));
    c3_assert(0);
  }
  else {
    //  Discard (most) effects and continue result.
    //
    u2_noun gax = u2t(gon);
 
    {
      u2_noun hux = u2h(gax);

      while ( u2_nul != hux ) {
        u2_noun ovo = u2h(hux);
        u2_noun fav = u2t(ovo);

        if ( (c3__init == u2h(fav)) || (c3__inuk == u2h(fav)) ) {
          rec_u->own = u2nc(u2k(u2t(fav)), rec_u->own);
        }
        hux = u2t(hux);
      }
    }
    u2z(rec_u->roc);
    rec_u->roc = u2k(u2t(gax));
  }
  u2z(gon);
  u2z(ovo);
}

/* _lo_pike(): poke with floating core.
*/
static u2_noun 
_lo_pike(u2_reck* rec_u, u2_noun ovo, u2_noun cor)
{
  u2_noun fun = u2_cn_nock(u2k(cor), u2k(u2_cx_at(42, cor)));
  u2_noun sam = u2nc(u2k(rec_u->now), ovo);

  return _lo_mung(rec_u, 0, fun, sam);
}

/* _lo_sure(): apply and save an input ovum and its result.
*/
static void
_lo_sure(u2_reck* rec_u, u2_noun ovo, u2_noun vir, u2_noun cor)
{
  //  Whatever worked, save it.  (XX - should be concurrent with execute.)
  //  We'd like more events that don't change the state but need work here.
  {
    u2_mug(cor);
    u2_mug(rec_u->roc);

    if ( u2_no == u2_sing(cor, rec_u->roc) ) {
      _lo_save(rec_u, u2k(ovo));
      u2z(rec_u->roc);
      rec_u->roc = cor;
    }
    else {
      u2z(cor);
    }
    u2z(ovo);
  }

  //  Evaluate external side effects.  Not allowed to fail.
  //
  {
    u2_noun hux = vir;

    while ( u2_nul != hux ) {
      u2_reck_kick(rec_u, u2k(u2h(hux)));
      hux = u2t(hux);
    }
    u2z(vir);
  }
}

/* _lo_lame(): handle an application failure.
*/
static void
_lo_lame(u2_reck* rec_u, u2_noun ovo, u2_noun why, u2_noun tan)
{
  u2_noun bov, gon;

  //  Formal error in a network packet generates a hole card.
  //
  //  There should be a separate path for crypto failures,
  //  to prevent timing attacks, but isn't right now.  To deal
  //  with a crypto failure, just drop the packet.
  //
  if ( (c3__exit == why) && (c3__hear == u2h(u2t(ovo))) ) {
    _lo_punt(rec_u, 2, u2k(tan));

    bov = u2nc(u2k(u2h(ovo)), u2nc(c3__hole, u2k(u2t(u2t(ovo)))));
    u2z(why);
  }
  else {
    bov = u2nc(u2k(u2h(ovo)), u2nt(c3__crud, why, u2k(tan)));
  }
  u2z(ovo); 

  gon = _lo_soft(rec_u, 0, u2_reck_poke, u2k(bov));
  if ( u2_blip == u2h(gon) ) {
    _lo_sure(rec_u, bov, u2k(u2h(u2t(gon))), u2k(u2t(u2t(gon))));
   
    u2z(gon);
  }
  else {
    u2z(gon);
    {
      u2_noun vab = u2nc(u2k(u2h(bov)), 
                         u2nc(c3__warn, u2_ci_tape("crude crash!")));
      u2_noun nog = _lo_soft(rec_u, 0, u2_reck_poke, u2k(vab));

      if ( u2_blip == u2h(nog) ) {
        _lo_sure(rec_u, vab, u2k(u2h(u2t(nog))), u2k(u2t(u2t(nog))));
        u2z(nog);
      }
      else {
        u2z(nog);
        u2z(vab);

        uL(fprintf(uH, "crude: all delivery failed!\n"));
      }
    }
  }
}

static void _lo_punk(u2_reck* rec_u, u2_noun ovo);

/* _lo_nick(): transform enveloped packets, [vir cor].
*/
static u2_noun 
_lo_nick(u2_reck* rec_u, u2_noun vir, u2_noun cor)
{
  if ( u2_nul == vir ) {
    return u2nt(u2_blip, vir, cor);
  } 
  else {
    u2_noun i_vir = u2h(vir);
    u2_noun pi_vir, qi_vir;
    u2_noun vix;

    if ( (u2_yes == u2_cr_cell((i_vir=u2h(vir)), &pi_vir, &qi_vir)) &&
         (u2_yes == u2du(qi_vir)) &&
         (c3__hear == u2h(qi_vir)) )
    {
      u2_noun gon;

      gon = _lo_pike(rec_u, u2k(i_vir), cor);
      if ( u2_blip != u2h(gon) ) {
        u2z(vir);
        return gon;
      }
      else {
        u2_noun viz;

        vix = u2k(u2h(u2t(gon)));
        cor = u2k(u2t(u2t(gon)));
        u2z(gon);

        viz = u2_ckb_weld(vix, u2k(u2t(vir)));
        u2z(vir);

        return _lo_nick(rec_u, viz, cor);
      }
    }
    else {
      u2_noun nez = _lo_nick(rec_u, u2k(u2t(vir)), cor);

      if ( u2_blip != u2h(nez) ) {
        u2z(vir);
        return nez;
      } else {
        u2_noun viz;

        viz = u2nc(u2k(i_vir), u2k(u2h(u2t(nez))));
        cor = u2k(u2t(u2t(nez)));

        u2z(vir);
        u2z(nez);

        return u2nt(u2_blip, viz, cor);
      }
    }
  }
}

/* _lo_punk(): insert and apply an input ovum (unprotected).
*/
static void
_lo_punk(u2_reck* rec_u, u2_noun ovo)
{
  c3_w sec_w;
  u2_noun gon;

  //  XX this is wrong - the timer should be on the original hose.
  //
  if ( (c3__term == u2h(u2t(u2h(ovo)))) || 
       (c3__behn == u2h(u2t(u2h(ovo)))) ) {
    sec_w = 0;
  } else sec_w = 5;

  gon = _lo_soft(rec_u, sec_w, u2_reck_poke, u2k(ovo));

  if ( u2_blip != u2h(gon) ) {
    _lo_lame(rec_u, ovo, u2k(u2h(gon)), u2k(u2t(gon)));
  }
  else {
    u2_noun vir = u2k(u2h(u2t(gon)));
    u2_noun cor = u2k(u2t(u2t(gon)));
    u2_noun nug = _lo_nick(rec_u, vir, cor);

    if ( u2_blip != u2h(nug) ) {
      _lo_lame(rec_u, ovo, u2k(u2h(nug)), u2k(u2t(nug)));
      u2z(nug);
    } 
    else {
      vir = u2k(u2h(u2t(nug)));
      cor = u2k(u2t(u2t(nug)));

      _lo_sure(rec_u, ovo, vir, cor);
    }
  }
  u2z(gon);
}

/* _lo_work(): work in rec_u.
*/
static void
_lo_work(u2_reck* rec_u)
{
  while ( rec_u->ova.egg_u ) {
    u2_cart* egg_u = rec_u->ova.egg_u;
    u2_noun  egg = egg_u->egg;

    rec_u->ova.egg_u = egg_u->nex_u;
    if ( 0 == rec_u->ova.egg_u ) {
      c3_assert(egg_u == rec_u->ova.geg_u);
      rec_u->ova.geg_u = 0;
    }
    free(egg_u);

    _lo_punk(rec_u, egg);
  }
}

/* u2_lo_call(): central callback.
*/
void
u2_lo_call(u2_reck*        rec_u,
           struct ev_loop* lup_u,
           void*           wev_u,
           u2_noun         how,
           c3_i            revents)
{
  u2_bean inn = (revents & EV_READ) ? u2_yes : u2_no;
  u2_bean out = (revents & EV_WRITE) ? u2_yes : u2_no;
  u2_bean tim = (revents & EV_TIMEOUT) ? u2_yes : u2_no;
  u2_bean sig = (revents & EV_SIGNAL) ? u2_yes : u2_no;
  u2_bean sat = (revents & EV_STAT) ? u2_yes : u2_no;

  _lo_stop(rec_u, lup_u);

#if 0
  {
    uL(fprintf(uH, "call %s inn %s out %s tim %s sig %s sat %s\n", 
                      _lo_how(how), 
                      (inn == u2_yes) ? "yes" : "no", 
                      (out == u2_yes) ? "yes" : "no",
                      (tim == u2_yes) ? "yes" : "no",
                      (sig == u2_yes) ? "yes" : "no",
                      (sat == u2_yes) ? "yes" : "no"));
  }
#endif

  {
    //  update time
    //
    u2_reck_time(rec_u);

    //  XX poll the filesystem on genuine input
    //
    if ( u2_yes == inn ) {
      u2_unix_ef_look(rec_u);
    }

    //  process input on this socket
    //
    if ( u2_yes == inn ){
      _lo_suck(rec_u, wev_u, how);
    }

    //  process output on this socket
    //
    if ( u2_yes == out ) {
      _lo_fuck(rec_u, wev_u, how);
    }

    if ( u2_yes == tim ) {
      _lo_time(rec_u, wev_u, how);
    }

    if ( u2_yes == sig ) {
      _lo_sign(rec_u, wev_u, how);
    }

    if ( u2_yes == sat ) {
      _lo_stat(rec_u, wev_u, how);
    }

    //  process actions
    //
    _lo_work(rec_u);

    //  update time
    //
    u2_reck_time(rec_u);

    //  clean shutdown
    //
    if ( u2_no == u2_Host.liv ) {
      //  direct save and die
      //
      u2_cm_purge();
      u2_loom_save(rec_u->ent_w);
      _lo_exit(rec_u);

      exit(0);
    }
  }
  _lo_poll(rec_u, lup_u);
  _lo_spin(rec_u, lup_u);
}

/* _lo_home(): create ship directory.
*/
static void
_lo_home(u2_reck* rec_u)
{
  c3_c    ful_c[2048];

  //  Create subdirectories.
  //
  {
    mkdir(u2_Host.cpu_c, 0700);

    sprintf(ful_c, "%s/get", u2_Host.cpu_c);
    if ( 0 != mkdir(ful_c, 0700) ) {
      perror(ful_c);
      u2_lo_bail(rec_u);
    }

    sprintf(ful_c, "%s/put", u2_Host.cpu_c);
    if ( 0 != mkdir(ful_c, 0700) ) {
      perror(ful_c);
      u2_lo_bail(rec_u);
    }
  }

  //  Copy urbit.pill.
  //
  {
    sprintf(ful_c, "cp %s/urbit.pill %s", 
                    u2_Host.ops_u.hom_c, u2_Host.cpu_c);
    if ( 0 != system(ful_c) ) {
      uL(fprintf(uH, "could not %s\n", ful_c));
      u2_lo_bail(rec_u);
    }
  }
}

/* _lo_cask(): ask for a passcode.
*/
static u2_noun
_lo_cask(u2_reck* rec_u, c3_c* dir_c, u2_bean nun)
{
  c3_c   paw_c[60];
  u2_noun key;

  uH;
  while ( 1 ) {
    printf("passcode for %s%s? ~", dir_c, (u2_yes == nun) ? " [none]" : "");

    paw_c[0] = 0;
    fpurge(stdin);
    fgets(paw_c, 59, stdin);

    if ( '\n' == paw_c[0] ) {
      if ( u2_yes == nun ) {
        key = 0; break;
      }
      else {
        continue;
      }
    }
    else {
      c3_c* say_c = malloc(strlen(paw_c) + 2);
      u2_noun say; 

      say_c[0] = '~';
      say_c[1] = 0;
      strncat(say_c, paw_c, strlen(paw_c) - 1);

      say = u2_do("slay", u2_ci_string(say_c));
      if ( (u2_nul == say) || 
           (u2_blip != u2h(u2t(say))) ||
           ('p' != u2h(u2t(u2t(say)))) )
      {
        printf("invalid passcode\n");
        continue;
      }
      key = u2k(u2t(u2t(u2t(say))));

      u2z(say);
      break;
    }
  }
  uL(0);
  return key;
}

/* _lo_text(): ask for a name string.
*/
static u2_noun
_lo_text(u2_reck* rec_u, c3_c* pom_c)
{
  c3_c   paw_c[60];
  u2_noun say;

  uH;
  while ( 1 ) {
    printf("%s: ", pom_c);

    paw_c[0] = 0;
    fpurge(stdin);
    fgets(paw_c, 59, stdin);

    if ( '\n' == paw_c[0] ) {
      continue;
    }
    else {
      c3_w len_w = strlen(paw_c);

      if ( paw_c[len_w - 1] == '\n' ) {
        paw_c[len_w-1] = 0;
      }
      say = u2_ci_string(paw_c);
      break;
    }
  }
  uL(0);
  return say;
}

/* _lo_bask(): ask a yes or no question.
*/
static u2_bean
_lo_bask(c3_c* pop_c, u2_bean may)
{
  u2_bean yam;

  uH;
  while ( 1 ) {
    c3_c ans_c[3];

    printf("%s [y/n]? ", pop_c);
    ans_c[0] = 0;

    fpurge(stdin);
    fgets(ans_c, 2, stdin);
   
    if ( (ans_c[0] != 'y') && (ans_c[0] != 'n') ) {
      continue;
    } else {
      yam = (ans_c[0] != 'n') ? u2_yes : u2_no;
      break;
    }
  }
  uL(0);
  return yam;
}

/* _lo_rand(): fill a 256-bit (8-word) buffer.
*/
static void
_lo_rand(u2_reck* rec_u, c3_w* rad_w)
{
  c3_i fid_i = open(DEVRANDOM, O_RDONLY);

  if ( 32 != read(fid_i, (c3_y*) rad_w, 32) ) {
    c3_assert(!"lo_rand");
  }
  close(fid_i);
}

/* _lo_fast(): offer to save passcode by mug in home directory.
*/
static void
_lo_fast(u2_reck* rec_u, u2_noun key)
{
  c3_c    ful_c[2048];
  c3_c*   hom_c = getenv("HOME");
  c3_l    mug_l = u2_mug(key);
  u2_noun gum   = u2_dc("scot", 'p', mug_l);
  c3_c*   gum_c = u2_cr_string(gum);
  u2_noun yek   = u2_dc("scot", 'p', u2k(key));
  c3_c*   yek_c = u2_cr_string(yek);

  sprintf(ful_c, "save passcode as %s/.urbit/%s.txt", hom_c, gum_c);
  if ( u2_no == _lo_bask(ful_c, u2_no) ) {
    uL(fprintf(uH, "passcode: %s.   write it down!\n", yek_c));
  }
  else {
    c3_i fid_i;

    sprintf(ful_c, "%s/.urbit", hom_c);
    mkdir(ful_c, 0700);

    sprintf(ful_c, "%s/.urbit/%s.txt", hom_c, gum_c);
    if ( (fid_i = open(ful_c, O_CREAT | O_TRUNC | O_WRONLY, 0600)) < 0 ) {
      uL(fprintf(uH, "fast: could not save %s\n", ful_c));
      u2_lo_bail(rec_u);
    }
    write(fid_i, yek_c, strlen(yek_c));
    close(fid_i);
  }
  free(gum_c);
  u2z(gum);

  free(yek_c);
  u2z(yek);
}

/* _lo_staf(): try to load passcode by mug from home directory.
*/
static u2_noun
_lo_staf(u2_reck* rec_u, c3_l mug_l)
{
  c3_c    ful_c[2048];
  c3_c*   hom_c = getenv("HOME");
  u2_noun gum   = u2_dc("scot", 'p', mug_l);
  c3_c*   gum_c = u2_cr_string(gum);
  u2_noun txt;

  sprintf(ful_c, "%s/.urbit/%s.txt", hom_c, gum_c);
  txt = u2_walk_safe(ful_c);

  if ( 0 == txt ) {
    return 0;
  }
  else {
    u2_noun say = u2_do("slay", txt);
    u2_noun key;

    if ( (u2_nul == say) || 
         (u2_blip != u2h(u2t(say))) ||
         ('p' != u2h(u2t(u2t(say)))) ) 
    {
      uL(fprintf(uH, "staf: %s is corrupt\n", ful_c));
      u2_lo_bail(rec_u);
    }
    key = u2k(u2t(u2t(u2t(say))));

    u2z(say);
    return key;
  }
}

/* _lo_zest(): create a new, empty record.
*/
static void
_lo_zest(u2_reck* rec_u)
{
  struct stat buf_b;
  c3_i        fid_i;
  c3_c        ful_c[8193];

  //  Create the ship directory.
  //
  _lo_home(rec_u);
  
  //  Create the record file.
  {
    sprintf(ful_c, "%s/egz.hope", u2_Host.cpu_c);

    if ( ((fid_i = open(ful_c, O_CREAT | O_WRONLY | O_EXCL, 0600)) < 0) || 
         (fstat(fid_i, &buf_b) < 0) ) 
    {
      uL(fprintf(uH, "can't create record (%s)\n", ful_c));
      u2_lo_bail(rec_u);
    }
    u2_Host.lug_u.fid_i = fid_i;
  }

  //  See if the user wants to use a passcode.
  //
  {
    rec_u->key = _lo_cask(rec_u, u2_Host.cpu_c, u2_yes);

    if ( 0 == rec_u->key ) {
      if ( u2_no == _lo_bask("generate a random passcode", u2_yes) ) {
        uL(fprintf(uH, "events in %s will be saved in the clear.\n", 
                        u2_Host.cpu_c));
        rec_u->key = 0;
      }
      else {
        c3_w    rad_w[8];

        _lo_rand(rec_u, rad_w);
        rec_u->key = u2_ci_words(2, rad_w);
      }
    } 

    if ( 0 != rec_u->key ) {
      _lo_fast(rec_u, u2k(rec_u->key));
    }
  }

  //  Write the header.
  {
    u2_uled led_u;

    led_u.mag_l = u2_mug('e');
    led_u.kno_w = rec_u->kno_w;

    if ( 0 == rec_u->key ) {
      led_u.key_l = 0; 
    } else {
      led_u.key_l = u2_mug(rec_u->key);

      c3_assert(!(led_u.key_l >> 31));
    }
    led_u.sev_l = rec_u->sev_l;
    led_u.tno_l = 1;

    if ( sizeof(led_u) != write(fid_i, &led_u, sizeof(led_u)) ) {
      uL(fprintf(uH, "can't write record (%s)\n", ful_c));
      u2_lo_bail(rec_u);
    }

    u2_Host.lug_u.len_w = c3_wiseof(led_u);
  }

  //  Save the boot events.
  {
    u2_noun nor = u2_ckb_flop(rec_u->roe);

    rec_u->roe = nor;

    while ( u2_nul != nor ) {
      _lo_pack(rec_u, u2k(u2h(nor)));
      rec_u->ent_w += 1;
      nor = u2t(nor);
    }
    u2z(rec_u->roe);
    rec_u->roe = 0;
  }

#if 0
  //  Copy the egz into ham, the factory default.
  {
    sprintf(ful_c, "rm -f %s/~ham.hope; cp %s/~egz.hope %s/~ham.hope",
                   u2_Host.cpu_c, u2_Host.cpu_c, u2_Host.cpu_c);

    if ( 0 != system(ful_c) ) {
      uL(fprintf(uH, "zest: could not save ham\n"));
      u2_lo_bail(rec_u);
    }
  }
#endif
}

/* _lo_make(): boot from scratch.
*/
static void
_lo_make(u2_reck* rec_u, u2_noun fav)
{
  //  Authenticate and initialize terminal.
  //
  u2_term_ef_bake(rec_u, fav);

  //  Work through start sequence.
  //
  _lo_work(rec_u);

  //  Further server configuration.
  //
  {
    u2_http_ef_bake(rec_u);
  }

  //  Work some more.
  //
  _lo_work(rec_u);

  //  Create the ship directory.
  //
  _lo_zest(rec_u);
}

/* _lo_rest(): restore from record, or exit.
*/
static void
_lo_rest(u2_reck* rec_u)
{
  struct stat buf_b;
  c3_i        fid_i;
  c3_c        ful_c[2048];
  c3_w        old_w = rec_u->ent_w;
  c3_w        las_w = 0;
  u2_noun     roe = u2_nul;
  u2_noun     sev_l, tno_l, key_l;

  if ( 0 != rec_u->ent_w ) {
    uL(fprintf(uH, "rest: checkpoint to event %d\n", rec_u->ent_w));
  }

  //  Open the fscking file.  Does it even exist?
  {
    sprintf(ful_c, "%s/egz.hope", u2_Host.cpu_c);

    if ( ((fid_i = open(ful_c, O_RDWR)) < 0) || 
         (fstat(fid_i, &buf_b) < 0) ) 
    {
      uL(fprintf(uH, "rest: can't open record (%s)\n", ful_c));
      u2_lo_bail(rec_u);

      return;
    }
    u2_Host.lug_u.fid_i = fid_i;
    u2_Host.lug_u.len_w = ((buf_b.st_size + 3) >> 2);
  }

  //  Check the fscking header.  It's probably corrupt.
  {
    u2_uled led_u;

    if ( sizeof(led_u) != read(fid_i, &led_u, sizeof(led_u)) ) {
      uL(fprintf(uH, "record (%s) is corrupt (a)\n", ful_c));
      u2_lo_bail(rec_u);
    }

    if ( u2_mug('e') != led_u.mag_l ) {
      uL(fprintf(uH, "record (%s) is obsolete (or corrupt)\n", ful_c));
      u2_lo_bail(rec_u);
    }

    if ( led_u.kno_w != rec_u->kno_w ) {
      //  XX perhaps we should actually do something here
      //
      uL(fprintf(uH, "rest: (not) translating events (old %d, now %d)\n",
                     led_u.kno_w,
                     rec_u->kno_w));
    }
    sev_l = led_u.sev_l;
    key_l = led_u.key_l;
    tno_l = led_u.tno_l;

    {
      u2_noun old = u2_dc("scot", c3__uv, sev_l);
      u2_noun nuu = u2_dc("scot", c3__uv, rec_u->sev_l);
      c3_c* old_c = u2_cr_string(old);
      c3_c* nuu_c = u2_cr_string(nuu);

      uL(fprintf(uH, "rest: old %s, new %s\n", old_c, nuu_c));
      free(old_c); free(nuu_c);

      u2z(old); u2z(nuu);
    }
    c3_assert(sev_l != rec_u->sev_l);   //  1 in 2 billion, just retry
  }

  //  Oh, and let's hope you didn't forget the fscking passcode.
  {
    if ( 0 != key_l ) {
      rec_u->key = _lo_staf(rec_u, key_l);

      while ( 0 == rec_u->key ) {
        rec_u->key = _lo_cask(rec_u, u2_Host.cpu_c, u2_no);

        if ( u2_mug(rec_u->key) != key_l ) {
          uL(fprintf(uH, "incorrect passcode\n"));
          u2z(rec_u->key);
          rec_u->key = 0;
          continue;
        }
        _lo_fast(rec_u, u2k(rec_u->key));
        break;
      } 
    }
  }

  //  Read in the fscking events.  These are probably corrupt as well.
  {
    c3_w end_w, ent_w;

    end_w = u2_Host.lug_u.len_w;
    ent_w = 0;

    if ( -1 == lseek(fid_i, 4 * end_w, SEEK_SET) ) {
      uL(fprintf(uH, "record (%s) is corrupt (c)\n", ful_c));
      u2_lo_bail(rec_u);
    }

    while ( end_w != c3_wiseof(u2_uled) ) {
      c3_w    tar_w = (end_w - c3_wiseof(u2_ular));
      u2_ular lar_u;
      c3_w*   img_w;
      u2_noun ron;

      // hL(fprintf(uH, "rest: reading event at %d\n", end_w));

      if ( -1 == lseek(fid_i, 4 * tar_w, SEEK_SET) ) {
        uL(fprintf(uH, "record (%s) is corrupt (d)\n", ful_c));
        u2_lo_bail(rec_u);
      }
      if ( sizeof(u2_ular) != read(fid_i, &lar_u, sizeof(u2_ular)) ) {
        uL(fprintf(uH, "record (%s) is corrupt (e)\n", ful_c));
        u2_lo_bail(rec_u);
      }

      if ( lar_u.syn_w != u2_mug(tar_w) ) {
        uL(fprintf(uH, "record (%s) is corrupt (f)\n", ful_c));
        u2_lo_bail(rec_u);
      }

#if 0
      uL(fprintf(uH, "log: read: at %d, %d: lar ent %d, len %d, mug %x\n", 
                      (tar_w - lar_u.len_w),
                      tar_w,
                      lar_u.ent_w,
                      lar_u.len_w,
                      lar_u.mug_w));
#endif
      img_w = malloc(4 * lar_u.len_w);

      if ( end_w == u2_Host.lug_u.len_w ) {
        ent_w = las_w = lar_u.ent_w;
      } 
      else {
        if ( lar_u.ent_w != (ent_w - 1) ) {
          uL(fprintf(uH, "record (%s) is corrupt (g)\n", ful_c));
          uL(fprintf(uH, "lar_u.ent_w %x, ent_w %x\n", lar_u.ent_w, ent_w));
          u2_lo_bail(rec_u);
        }
        ent_w -= 1;
      }
      end_w = (tar_w - lar_u.len_w);

      if ( ent_w < old_w ) {
        break;
      }

      if ( -1 == lseek(fid_i, 4 * end_w, SEEK_SET) ) {
        uL(fprintf(uH, "record (%s) is corrupt (h)\n", ful_c));
        u2_lo_bail(rec_u);
      }
      if ( (4 * lar_u.len_w) != read(fid_i, img_w, (4 * lar_u.len_w)) ) {
        uL(fprintf(uH, "record (%s) is corrupt (i)\n", ful_c));
        u2_lo_bail(rec_u);
      }

      ron = u2_ci_words(lar_u.len_w, img_w);
      free(img_w);

      if ( u2_mug(ron) != lar_u.mug_w ) {
        uL(fprintf(uH, "record (%s) is corrupt (j)\n", ful_c));
        u2_lo_bail(rec_u);
      }

      if ( rec_u->key ) {
        u2_noun dep;

        dep = u2_dc("de:crya", u2k(rec_u->key), ron);
        if ( u2_no == u2du(dep) ) {
          uL(fprintf(uH, "record (%s) is corrupt (k)\n", ful_c));
          u2_lo_bail(rec_u);
        } 
        else {
          ron = u2k(u2t(dep));
          u2z(dep);
        }
      }
      roe = u2nc(u2_cke_cue(ron), roe);
    }
    rec_u->ent_w = c3_max(las_w + 1, old_w);
  }

  if ( u2_nul == roe ) {
    //  Nothing in the log that was not also in the checkpoint.
    //
    c3_assert(rec_u->ent_w == old_w);
    c3_assert((las_w + 1) == old_w);
  }
  else {
    u2_noun rou = roe;
    c3_w    xno_w;

    //  Execute the fscking things.  This is pretty much certain to crash.
    //
    uL(fprintf(uH, "rest: replaying through event %d\n", las_w));
    fprintf(uH, "---------------- playback starting----------------\n");

    xno_w = 0;
    while ( u2_nul != roe ) {
      u2_noun i_roe = u2h(roe);
      u2_noun t_roe = u2t(roe);
      u2_noun now = u2h(i_roe);
      u2_noun ovo = u2t(i_roe);

      u2_reck_wind(rec_u, u2k(now));
      _lo_sing(rec_u, u2k(ovo));

      fputc('.', stderr);
      // uL(fprintf(uH, "playback: sing: %d\n", xno_w));

      roe = t_roe;
      xno_w++;
    }
    u2z(rou);
  }
  uL(fprintf(stderr, "\n---------------- playback complete----------------\n"));

#if 0
  //  If you see this error, your record is totally fscking broken!
  //  Which probably serves you right.  Please consult a consultant.
  {
    if ( u2_nul == rec_u->own ) {
      uL(fprintf(uH, "record did not install a master!\n"));
      u2_lo_bail(rec_u);
    }
    rec_u->our = u2k(u2h(rec_u->own));
    rec_u->pod = u2_dc("scot", 'p', u2k(rec_u->our)));
  }

  //  Now, who the fsck are you?  No, really.
  { 
    u2_noun who;
    c3_c*   fil_c;
    c3_c*   who_c;

    if ( (fil_c = strrchr(u2_Host.cpu_c, '/')) ) {
      fil_c++;
    } else fil_c = u2_Host.cpu_c;

    who = u2_dc("scot", 'p', u2k(rec_u->our)));
    who_c = u2_cr_string(who);
    u2z(who);

    if ( strncmp(fil_c, who_c + 1, strlen(fil_c)) ) {
      uL(fprintf(uH, "record master (%s) does not match filename!\n", who_c));
      u2_lo_bail(rec_u);
    }
    free(who_c);
  }
#endif

  //  Rewrite the header.  Will probably corrupt the record.
  { 
    u2_uled led_u;

    led_u.mag_l = u2_mug('e');
    led_u.sev_l = rec_u->sev_l;
    led_u.key_l = rec_u->key ? u2_mug(rec_u->key) : 0;
    led_u.kno_w = rec_u->kno_w;         //  may need actual translation!
    led_u.tno_l = 1;
   
    if ( (-1 == lseek(fid_i, 0, SEEK_SET)) ||
         (sizeof(led_u) != write(fid_i, &led_u, sizeof(led_u))) )
    {
      uL(fprintf(uH, "record (%s) failed to rewrite\n", ful_c));
      u2_lo_bail(rec_u);
    }
  }

  //  Hey, fscker!  It worked.
  {
    u2_term_ef_boil(rec_u, tno_l);
  }
}

/* _lo_zen(): get OS entropy.
*/
static u2_noun 
_lo_zen(u2_reck* rec_u)
{
  c3_w rad_w[8];

  _lo_rand(rec_u, rad_w);
  return u2_ci_words(8, rad_w);
}

/* u2_lo_loop(): begin main event loop.
*/
void
u2_lo_loop(u2_reck* rec_u)
{
  signal(SIGHUP, SIG_IGN);  //  nohup, who needs u?
  signal(SIGIO, SIG_IGN);   //  linux is wont to produce for some reason

  _lo_init(rec_u);

  if ( u2_yes == u2_Host.ops_u.nuu ) {
    u2_noun ten = _lo_zen(rec_u);
    u2_noun pig;

    if ( 0 == u2_Host.ops_u.imp_c ) {
      uL(fprintf(uH, "generating 2048-bit RSA pair...\n"));

      pig = u2nq(c3__make, u2_ci_string("ephemeral"), 11, ten);
    }
    else {
      u2_noun imp = u2_ci_string(u2_Host.ops_u.imp_c);
      u2_noun whu = u2_dc("slaw", 'p', u2k(imp));
      if ( (u2_nul == whu) ) {
        fprintf(stderr, "czar: incorrect format\r\n");
        exit(1);
      }
      else {
        u2_noun gen = _lo_text(rec_u, "generator");
        u2_noun gun = u2_dc("slaw", c3__uw, gen);

        if ( u2_nul == gun ) {
          fprintf(stderr, "czar: incorrect format\r\n");
          exit(1);
        }
        pig = u2nt(c3__sith, u2k(u2t(whu)), u2k(u2t(gun)));

        u2z(whu); u2z(gun);
      }
      u2z(imp);
    }
    _lo_make(rec_u, pig);
  }
  else {
    _lo_rest(rec_u);
  }

  {
    u2_unix_ef_look(rec_u);
    u2_reck_plan(rec_u, u2nt(c3__gold, c3__ames, u2_nul),
                        u2nc(c3__kick, u2k(rec_u->now)));
  }

#if 1
  u2_loom_save(rec_u->ent_w);

  u2_Host.sav_u.ent_w = rec_u->ent_w;
#endif

  if ( u2_yes == u2_Host.ops_u.nuu ) {
    u2_term_ef_boil(rec_u, 1);
  }

  {
    struct ev_loop *lup_u = ev_default_loop(0);

    _lo_poll(rec_u, lup_u);
    _lo_spin(rec_u, lup_u);

    ev_loop(lup_u, 0);

    _lo_exit(rec_u);
  }
}
