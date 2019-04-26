/* worker/main.c
**
**  the main loop of a worker process.
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
#include <ncurses/curses.h>
#include <termios.h>
#include <ncurses/term.h>

#include "all.h"
#include <vere/vere.h>

    typedef struct _u3_worker {
      c3_w    len_w;                        //  boot sequence length
      c3_d    evt_d;                        //  last event processed
      c3_l    mug_l;                        //  hash of state
      c3_d    key_d[4];                     //  disk key
      u3_moat inn_u;                        //  message input
      u3_mojo out_u;                        //  message output
      c3_c*   dir_c;                        //  execution directory (pier)
    } u3_worker;
    static u3_worker u3V;

/*
::  worker to daemon protocol
::
|%
::  +plea: from worker to daemon
::
+$  plea
  $%  ::  status on startup
      ::
      $:  %play
          $=  p
          ::  ~ if no snapshot
          ::
          %-  unit
          ::  p: event number expected
          ::  q: mug of kernel
          ::  r: identity, fake flag
          ::
          [p=@ q=@ r=[our=@p fak=?]]
      ==
      ::  event executed unchanged (in response to %work)
      ::
      $:  %done
          ::  p: event number
          ::  q: mug of state (or 0)
          ::  r: effects
          ::
          [p=@ q=@ r=(list ovum)]
      ==
      ::  replace event and retry (in response to %work)
      ::
      $:  %work
          ::  p: event number
          ::  q: mug of state (or 0)
          ::  r: replacement event (at date)
          ::
          [p=@ q=@ r=(pair date ovum)]
      ==
      ::  sends a line to stderr while computing event
      ::
      $:  %stdr
          ::  p: event number
          ::  q: output cord
          ::
          [p=@ q=cord]
      ==
      ::  send slog hint while computing event
      ::
      $:  %slog
          ::  p: event number
          ::  q: priority
          ::  r: output tank
          ::
          [p=@ q=@ r=tank]
  ==  ==
::  +writ: from daemon to worker
::
+$  writ
  $%  ::  prepare to boot
      ::
      ::  p: identity
      ::  q: fake?
      ::  r: number of boot formulas
      ::
      [%boot p=@p q=? r=@]
      ::  exit immediately
      ::
      ::  p: exit code
      ::
      [%exit p=@]
      ::  save snapshot to disk
      ::
      ::  p: event number
      ::
      [%save p=@]
      ::  execute event
      ::
      $:  %work
          ::  p: event number
          ::  q: a jammed noun [mug [date ovum]]
          ::
          [p=@ q=@]
  ==  ==
--
*/

/* _worker_space(): print n spaces.
*/
void _worker_space(FILE* fil_u,  c3_w n)
{
  for (; n > 0; n--)
    (fprintf(fil_u," "));
}

/* _worker_print_memory(): print memory amount.
**
**  Helper for _worker_prof(), just an un-captioned u3a_print_memory().
*/
void
_worker_print_memory(FILE* fil_u, c3_w wor_w)
{
  c3_w byt_w = (wor_w * 4);
  c3_w gib_w = (byt_w / 1000000000);
  c3_w mib_w = (byt_w % 1000000000) / 1000000;
  c3_w kib_w = (byt_w % 1000000) / 1000;
  c3_w bib_w = (byt_w % 1000);

  if ( gib_w ) {
    (fprintf(fil_u, "GB/%d.%03d.%03d.%03d\r\n",
        gib_w, mib_w, kib_w, bib_w));
  }
  else if ( mib_w ) {
    (fprintf(fil_u, "MB/%d.%03d.%03d\r\n", mib_w, kib_w, bib_w));
  }
  else if ( kib_w ) {
    (fprintf(fil_u, "KB/%d.%03d\r\n", kib_w, bib_w));
  }
  else {
    (fprintf(fil_u, "B/%d\r\n", bib_w));
  }
}

/* _worker_prof(): print memory profile. RETAIN.
*/
c3_w
_worker_prof(FILE* fil_u, c3_w den, u3_noun mas)
{
  c3_w tot_w = 0;
  u3_noun h_mas, t_mas;

  if ( c3n == u3r_cell(mas, &h_mas, &t_mas) ) {
    _worker_space(fil_u, den);
    fprintf(fil_u, "mistyped mass\r\n");
    return tot_w;
  }
  else if ( _(u3du(h_mas)) ) {
    _worker_space(fil_u, den);
    fprintf(fil_u, "mistyped mass head\r\n");
    {
      c3_c* lab_c = u3m_pretty(h_mas);
      fprintf(fil_u, "h_mas: %s", lab_c);
      free(lab_c);
    }
    return tot_w;
  }
  else {
    _worker_space(fil_u, den);

    {
      c3_c* lab_c = u3m_pretty(h_mas);
      fprintf(fil_u, "%s: ", lab_c);
      free(lab_c);
    }

    u3_noun it_mas, tt_mas;

    if ( c3n == u3r_cell(t_mas, &it_mas, &tt_mas) ) {
      fprintf(fil_u, "mistyped mass tail\r\n");
      return tot_w;
    }
    else if ( c3y == it_mas ) {
      tot_w += u3a_mark_noun(tt_mas);
      _worker_print_memory(fil_u, tot_w);

#if 1
      /* The basic issue here is that tt_mas is included in
       * u3A->sac, so they can't both be roots in the normal
       * sense. When we mark u3A->sac later on, we want tt_mas
       * to appear unmarked, but its children should be already
       * marked.
      */
      if ( _(u3a_is_dog(tt_mas)) ) {
        u3a_box* box_u = u3a_botox(u3a_to_ptr(tt_mas));
#ifdef U3_MEMORY_DEBUG
        if ( 1 == box_u->eus_w ) {
          box_u->eus_w = 0xffffffff;
        }
        else {
          box_u->eus_w -= 1;
        }
#else
        if ( -1 == (c3_w)box_u->use_w ) {
          box_u->use_w = 0x80000000;
        }
        else {
          box_u->use_w += 1;
        }
#endif
      }
#endif

      return tot_w;
    }
    else if ( c3n == it_mas ) {
      fprintf(fil_u, "\r\n");

      while ( _(u3du(tt_mas)) ) {
        tot_w += _worker_prof(fil_u, den+2, u3h(tt_mas));
        tt_mas = u3t(tt_mas);
      }

      _worker_space(fil_u, den);
      fprintf(fil_u, "--");
      _worker_print_memory(fil_u, tot_w);

      return tot_w;

    }
    else {
      _worker_space(fil_u, den);
      fprintf(fil_u, "mistyped (strange) mass tail\r\n");
      return tot_w;
    }
  }
}

/* _worker_grab(): garbage collect, checking for profiling. RETAIN.
*/
static void
_worker_grab(u3_noun sac, u3_noun ovo, u3_noun vir)
{
  if ( u3_nul == sac) {
    if ( u3C.wag_w & (u3o_debug_ram | u3o_check_corrupt) ) {
      u3m_grab(sac, ovo, vir, u3_none);
    }
  }
  else {
    c3_w usr_w = 0, man_w = 0, sac_w = 0, ova_w = 0, vir_w = 0;

    FILE* fil_u;

#ifdef U3_MEMORY_LOG
    {
      u3_noun wen = u3dc("scot", c3__da, u3k(u3A->now));
      c3_c* wen_c = u3r_string(wen);

      c3_c nam_c[2048];
      snprintf(nam_c, 2048, "%s/.urb/put/mass", u3P.dir_c);

      struct stat st;
      if ( -1 == stat(nam_c, &st) ) {
        mkdir(nam_c, 0700);
      }

      c3_c man_c[2048];
      snprintf(man_c, 2048, "%s/%s.txt", nam_c, wen_c);

      fil_u = fopen(man_c, "w");
      fprintf(fil_u, "%s\r\n", wen_c);

      free(wen_c);
      u3z(wen);
    }
#else
    {
      fil_u = stderr;
    }
#endif

    c3_assert( u3R == &(u3H->rod_u) );

    fprintf(fil_u, "\r\n");
    usr_w = _worker_prof(fil_u, 0, sac);
    u3a_print_memory(fil_u, "total userspace", usr_w);

    man_w = u3m_mark(fil_u);

    sac_w = u3a_mark_noun(sac);
    u3a_print_memory(fil_u, "space profile", sac_w);

    ova_w = u3a_mark_noun(ovo);
    u3a_print_memory(fil_u, "event", ova_w);

    vir_w = u3a_mark_noun(vir);
    u3a_print_memory(fil_u, "effects", vir_w);

    u3a_print_memory(fil_u, "total marked", usr_w + man_w + sac_w + ova_w + vir_w);

    u3a_print_memory(fil_u, "sweep", u3a_sweep());

#ifdef U3_MEMORY_LOG
    {
      fclose(fil_u);
    }
#endif
  }
}

/* _worker_fail(): failure stub.
*/
static void
_worker_fail(void* vod_p, const c3_c* wut_c)
{
  u3l_log("work: fail: %s\r\n", wut_c);
  exit(1);
}

/* _worker_send(): send result back to daemon.
*/
static void
_worker_send(u3_noun job)
{
  u3_newt_write(&u3V.out_u, u3ke_jam(job), 0);
}

/* _worker_send_replace(): send replacement job back to daemon.
*/
static void
_worker_send_replace(c3_d evt_d, u3_noun ovo)
{
  u3l_log("worker_send_replace %" PRIu64 " %s\r\n",
          evt_d,
          u3r_string(u3h(u3t(ovo))));

  _worker_send(u3nt(c3__work,
                    u3i_chubs(1, &evt_d),
                    u3ke_jam(u3nt(u3V.mug_l,
                                  u3k(u3A->now),
                                  ovo))));
}

/* _worker_send_complete(): report completion.
*/
static void
_worker_send_complete(u3_noun vir)
{
  _worker_send(u3nq(c3__done,
                    u3i_chubs(1, &u3V.evt_d),
                    u3V.mug_l,
                    vir));
}

/* _worker_send_stdr(): send stderr output
*/
static void
_worker_send_stdr(c3_c* str_c)
{
  _worker_send(u3nt(c3__stdr, u3i_chubs(1, &u3V.evt_d), u3i_string(str_c)));
}

/* _worker_send_slog(): send hint output (hod is [priority tank]).
*/
static void
_worker_send_slog(u3_noun hod)
{
  _worker_send(u3nt(c3__slog, u3i_chubs(1, &u3V.evt_d), hod));
}

/* _worker_lame(): event failed, replace with error event.
*/
static void
_worker_lame(c3_d evt_d, u3_noun ovo, u3_noun why, u3_noun tan)
{
  u3_noun rep;
  u3_noun wir, tag, cad;

  u3x_trel(ovo, &wir, &tag, &cad);

  //  a deterministic error (%exit) in a network packet (%hear)
  //  generates a negative-acknowlegement attempt (%hole).
  //
  //    A comment from the old implementation:
  //      There should be a separate path for crypto failures,
  //      to prevent timing attacks, but isn't right now.  To deal
  //      with a crypto failure, just drop the packet.
  //
  if ( (c3__hear == tag) && (c3__exit == why) ) {
    rep = u3nt(u3k(wir), c3__hole, u3k(cad));
  }
  //  failed event notifications (%crud) are replaced with
  //  an even more generic notifications, on a generic arvo wire.
  //  N.B this must not be allowed to fail!
  //
  //    [%warn original-event-tag=@tas combined-trace=(list tank)]
  //
  else if ( c3__crud == tag ) {
    u3_noun lef = u3nc(c3__leaf, u3i_tape("crude crashed!"));
    u3_noun nat = u3kb_weld(u3k(u3t(cad)), u3nc(lef, u3k(tan)));
    rep = u3nc(u3nt(u3_blip, c3__arvo, u3_nul),
               u3nt(c3__warn, u3k(u3h(cad)), nat));
  }
  //  failed failure failing fails
  //
  else if ( c3__warn == tag ) {
    _worker_fail(0, "%warn replacement event failed");
    c3_assert(0);
  }
  //  failure notifications are sent on the same wire
  //
  //    [%crud event-tag=@tas event-trace=(list tank)]
  //
  else {
    //  prepend failure mote to tank
    //
    u3_noun lef = u3nc(c3__leaf, u3kb_weld(u3i_tape("bail: "),
                                           u3qc_rip(3, why)));
    u3_noun nat = u3kb_weld(u3k(tan), u3nc(lef, u3_nul));
    rep = u3nc(u3k(wir), u3nt(c3__crud, u3k(tag), nat));
  }

  _worker_send_replace(evt_d, rep);

  u3z(ovo); u3z(why); u3z(tan);
}

/* _worker_sure(): event succeeded, report completion.
*/
static void
_worker_sure(u3_noun ovo, u3_noun vir, u3_noun cor)
{
  u3z(u3A->roc);
  u3A->roc = cor;
  u3V.mug_l = u3r_mug(u3A->roc);

  u3_noun sac = u3_nul;

  //  intercept |mass, observe |reset
  //
  {
    u3_noun riv = vir;
    c3_w    i_w = 0;

    while ( u3_nul != riv ) {
      u3_noun fec = u3t(u3h(riv));

      //  assumes a max of one %mass effect per event
      //
      if ( c3__mass == u3h(fec) ) {
        //  save a copy of the %mass data
        //
        sac = u3k(u3t(fec));
        //  replace the %mass data with ~
        //
        //    For efficient transmission to daemon.
        //
        riv = u3kb_weld(u3qb_scag(i_w, vir),
                        u3nc(u3nt(u3k(u3h(u3h(riv))), c3__mass, u3_nul),
                             u3qb_slag(1 + i_w, vir)));
        u3z(vir);
        vir = riv;
        break;
      }

      //  reclaim memory from persistent caches on |reset
      //
      if ( c3__vega == u3h(fec) ) {
        u3m_reclaim();
      }

      riv = u3t(riv);
      i_w++;
    }
  }

  //  XX this runs on replay too
  //
  _worker_grab(sac, ovo, vir);
  _worker_send_complete(vir);

  u3z(sac); u3z(ovo);
}

/* _worker_work_live(): apply event.
*/
static void
_worker_work_live(c3_d    evt_d,              //  event number
                  c3_l    mug_l,              //  mug of state
                  u3_noun job)                //  event date
{
  u3_noun now, ovo, gon;

  c3_assert(evt_d == u3V.evt_d + 1ULL);
  if ( 0 != mug_l ) {
    c3_assert(u3V.mug_l == mug_l);
  }

  u3x_cell(job, &now, &ovo);

  u3z(u3A->now);
  u3A->now = u3k(now);

  //  XX why is this set before u3v_poke?
  //
  u3A->ent_d = evt_d;

#ifdef U3_EVENT_TIME_DEBUG
  {
    struct timeval b4, f2, d0;
    gettimeofday(&b4, 0);

    if ( c3__belt != u3h(u3t(ovo)) ) {
      c3_c* txt_c = u3r_string(u3h(u3t(ovo)));

      u3l_log("work: %s (%" PRIu64 ") live\r\n", txt_c, evt_d);
    }
  }
#endif

  gon = u3m_soft(0, u3v_poke, u3k(ovo));

#ifdef U3_EVENT_TIME_DEBUG
  {
    c3_c* txt_c = u3r_string(u3h(u3t(ovo)));
    c3_w ms_w;
    c3_w clr_w;

    gettimeofday(&f2, 0);
    timersub(&f2, &b4, &d0);
    ms_w = (d0.tv_sec * 1000) + (d0.tv_usec / 1000);
    clr_w = ms_w > 1000 ? 1 : ms_w < 100 ? 2 : 3; //  red, green, yellow
    if (c3__belt != u3h(u3t(ovo)) || clr_w != 2) {
      u3l_log("\x1b[3%dm%%%s (%" PRIu64 ") %4d.%02dms\x1b[0m\n",
              clr_w, txt_c, evt_d, ms_w,
              (int) (d0.tv_usec % 1000) / 10);
    }
    free(txt_c);
  }
#endif

  if ( u3_blip != u3h(gon) ) {
    //  event rejected
    //
    u3_noun why, tan;
    u3x_cell(gon, &why, &tan);

    u3k(ovo); u3k(why); u3k(tan);
    u3z(gon); u3z(job);

    _worker_lame(evt_d, ovo, why, tan);
  }
  else {
    //  event accepted
    //
    //    XX reconcile/dedupe with u3A->ent_d
    //
    u3V.evt_d = evt_d;
    //  vir/(list ovum)  list of effects
    //  cor/arvo         arvo core
    //
    u3_noun vir, cor;
    u3x_trel(gon, 0, &vir, &cor);

    u3k(ovo); u3k(vir); u3k(cor);
    u3z(gon); u3z(job);

    _worker_sure(ovo, vir, cor);

    //  reclaim memory from persistent caches on |reset
    //
    if ( 0 == (u3A->ent_d % 1000ULL) ) {
      u3m_reclaim();
    }
  }
}

/* _worker_boot_fire(): execute boot sequence.
*/
static u3_noun
_worker_boot_fire(u3_noun eve)
{
  //  XX virtualize? use u3v_boot?
  //
  u3_noun cor = u3n_nock_on(eve, u3nt(2, u3nc(0, 3), u3nc(0, 2)));
  u3_noun pro;

  pro = u3k(u3r_at(7, cor));

  u3z(cor);
  return pro;
}

/* _worker_work_boot(): apply initial-stage event.
*/
static void
_worker_work_boot(c3_d    evt_d,
                  c3_l    mug_l,
                  u3_noun job)
{
  c3_assert(evt_d == u3V.evt_d + 1ULL);
  if ( 0 != mug_l ) {
    c3_assert(u3V.mug_l == mug_l);
  }

  u3V.evt_d = evt_d;

  u3A->roe = u3nc(job, u3A->roe);

  u3l_log("work: (%" PRIu64 ")| boot\r\n", evt_d);

  if ( u3V.len_w == evt_d ) {
    u3_noun eve, pru;

    eve = u3kb_flop(u3A->roe);
    u3A->roe = 0;

    u3l_log("work: (%" PRIu64 ")| pill: %x\r\n", evt_d, u3r_mug(eve));

    pru = u3m_soft(0, _worker_boot_fire, eve);

    if ( u3_blip != u3h(pru) ) {
      u3l_log("boot failed\r\n");
      exit(1);
    }

    u3A->roc = u3k(u3t(pru));
    u3V.mug_l = u3r_mug(u3A->roc);

    u3l_log("work: (%" PRIu64 ")| core: %x\r\n", evt_d, u3V.mug_l);

    //  XX set u3A->evt_d ?
    //
    u3z(pru);
  }
  else {
    u3V.mug_l = u3r_mug(job);
  }

  _worker_send(u3nq(c3__done,
                    u3i_chubs(1, &evt_d),
                    u3V.mug_l,
                    u3_nul));
}

/* _worker_poke_work(): apply event.
*/
static void
_worker_poke_work(c3_d    evt_d,              //  event number
                  c3_l    mug_l,              //  mug of state
                  u3_noun job)                //  full event
{
  if ( u3C.wag_w & u3o_trace ) {
    if ( u3_Host.tra_u.con_w == 0  && u3_Host.tra_u.fun_w == 0 ) {
      u3t_trace_open(u3V.dir_c);
    }
    else if ( u3_Host.tra_u.con_w >= 100000 ) {
      u3t_trace_close();
      u3t_trace_open(u3V.dir_c);
    }
  }

  if ( evt_d <= u3V.len_w ) {
    c3_c lab_c[8];
    snprintf(lab_c, 8, "boot: %" PRIu64 "", evt_d);

    u3t_event_trace(lab_c, 'B');
    _worker_work_boot(evt_d, mug_l, job);
    u3t_event_trace(lab_c, 'E');
  }
  else {
    u3_noun wir = u3h(u3t(job));
    u3_noun cad = u3h(u3t(u3t(job)));

    c3_c lab_c[2048];
    snprintf(lab_c, 2048, "event %" PRIu64 ": [%s %s]", evt_d,
             u3m_pretty_path(wir), u3m_pretty(cad));

    u3t_event_trace(lab_c, 'B');
    _worker_work_live(evt_d, mug_l, job);
    u3t_event_trace(lab_c, 'E');
  }
}

/* _worker_poke_exit(): exit on command.
*/
static void
_worker_poke_exit(c3_w cod_w)                 //  exit code
{
  if ( u3C.wag_w & u3o_debug_cpu ) {
    u3t_damp();
  }

  exit(cod_w);
}

/* _worker_poke_boot(): prepare to boot.
*/
static void
_worker_poke_boot(u3_noun who, u3_noun fak, c3_w len_w)
{
  c3_assert( u3_none == u3A->our );
  c3_assert( 0 != len_w );

  u3A->our = who;
  u3A->fak = fak;
  u3V.len_w = len_w;
}

/* _worker_poke():
*/
void
_worker_poke(void* vod_p, u3_noun mat)
{
  u3_noun jar = u3ke_cue(mat);

  if ( c3y != u3du(jar) ) {
    goto error;
  }
  else {
    switch ( u3h(jar) ) {
      default: {
        goto error;
      }

      case c3__boot: {
        u3_noun who, fak, len;
        c3_w len_w;

        if ( (c3n == u3r_qual(jar, 0, &who, &fak, &len)) ||
             (c3n == u3ud(who)) ||
             (1 < u3r_met(7, who)) ||
             (c3n == u3ud(fak)) ||
             (1 < u3r_met(0, fak)) ||
             (c3n == u3ud(len)) ||
             (1 < u3r_met(3, len)) )
        {
          goto error;
        }

        len_w = u3r_word(0, len);
        u3k(who);
        u3k(fak);
        u3z(jar);

        return _worker_poke_boot(who, fak, len_w);
      }

      case c3__work: {
        u3_noun evt, jammed_entry, mug, job;
        c3_d evt_d;
        c3_l mug_l;

        if ( (c3n == u3r_trel(jar, 0, &evt, &jammed_entry)) ||
             (c3n == u3ud(evt)) ||
             (1 != u3r_met(6, evt)) )
        {
          goto error;
        }

        u3_noun entry = u3qe_cue(jammed_entry);
        if ( (c3y != u3du(entry)) ||
             (c3n == u3r_cell(entry, &mug, &job)) ||
             (c3n == u3ud(mug)) ||
             (1 < u3r_met(5, mug)) ) {
          goto error;
        }

        evt_d = u3r_chub(0, evt);
        mug_l = u3r_word(0, mug);
        u3k(job);
        u3z(entry);
        u3z(jar);

        return _worker_poke_work(evt_d, mug_l, job);
      }

      case c3__exit: {
        u3_noun cod;
        c3_w cod_w;

        if ( (c3n == u3r_cell(jar, 0, &cod)) ||
             (c3n == u3ud(cod)) ||
             (1 < u3r_met(3, cod)) )
        {
          goto error;
        }

        cod_w = u3r_word(0, cod);
        u3z(jar);

        return _worker_poke_exit(cod_w);
      }

      case c3__save: {
        u3_noun evt;
        c3_d evt_d;

        if ( (c3n == u3r_cell(jar, 0, &evt)) ||
             (c3n == u3ud(evt)) )
        {
          goto error;
        }

        evt_d = u3r_chub(0, evt);
        u3z(jar);

        c3_assert( evt_d == u3V.evt_d );

        return u3e_save();
      }
    }
  }

  error: {
    u3z(jar);
    _worker_fail(0, "bad jar");
  }
}

/* u3_worker_boot(): send startup message to manager.
*/
void
u3_worker_boot(void)
{
  c3_d nex_d  = 1ULL;
  u3_noun dat = u3_nul;

  if ( u3_none != u3A->our ) {
    u3V.mug_l = u3r_mug(u3A->roc);
    nex_d = u3A->ent_d + 1ULL;
    dat   = u3nc(u3_nul, u3nt(u3i_chubs(1, &nex_d),
                              u3V.mug_l,
                              u3nc(u3k(u3A->our), u3k(u3A->fak))));

    //  disable hashboard for fake ships
    //
    if ( c3y == u3A->fak ) {
      u3C.wag_w |= u3o_hashless;
    }

    //  no boot sequence expected
    //
    u3V.len_w = 0;
  }

  u3l_log("work: play %" PRIu64 "\r\n", nex_d);

  _worker_send(u3nc(c3__play, dat));
}

/* main(): main() when run as urbit-worker
*/
c3_i
main(c3_i argc, c3_c* argv[])
{
  uv_loop_t* lup_u = uv_default_loop();
  c3_c*      dir_c = argv[1];
  c3_c*      key_c = argv[2];
  c3_c*      wag_c = argv[3];

  c3_assert(4 == argc);

  /* load passkey
  */
  {
    sscanf(key_c, "%" PRIx64 ":%" PRIx64 ":%" PRIx64 ":%" PRIx64 "",
                  &u3V.key_d[0],
                  &u3V.key_d[1],
                  &u3V.key_d[2],
                  &u3V.key_d[3]);
  }

  /* load runtime config
  */
  {
    sscanf(wag_c, "%" SCNu32, &u3C.wag_w);
  }

  /* load pier directory
  */
  {
    u3V.dir_c = strdup(dir_c);
  }

  /*  clear tracing struct
  */
  {
    u3_Host.tra_u.nid_w = 0;
    u3_Host.tra_u.fil_u = NULL;
    u3_Host.tra_u.con_w = 0;
    u3_Host.tra_u.fun_w = 0;
  }

  /* boot image
  */
  {
    u3V.evt_d = u3m_boot_new(dir_c);
    u3C.stderr_log_f = _worker_send_stdr;
    u3C.slog_f = _worker_send_slog;
  }

  /* configure pipe to daemon process
  */
  {
    c3_i err_i;

    err_i = uv_pipe_init(lup_u, &u3V.inn_u.pyp_u, 0);
    c3_assert(!err_i);
    uv_pipe_open(&u3V.inn_u.pyp_u, 0);

    err_i = uv_pipe_init(lup_u, &u3V.out_u.pyp_u, 0);
    c3_assert(!err_i);
    uv_pipe_open(&u3V.out_u.pyp_u, 1);
  }

  /* set up writing
  */
  u3V.out_u.bal_f = _worker_fail;

  /* start reading
  */
  u3V.inn_u.vod_p = &u3V;
  u3V.inn_u.pok_f = _worker_poke;
  u3V.inn_u.bal_f = _worker_fail;

  u3_newt_read(&u3V.inn_u);

  /* send start request
  */
  u3_worker_boot();

  /* enter loop
  */
  uv_run(lup_u, UV_RUN_DEFAULT);
  return 0;
}
