/* worker/serf.c
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

#include "all.h"
#include <vere/vere.h>
#include <vere/serf.h>

/*
|%
::  +writ: from king to serf
::
::    next steps:
::    - %peek persistent dates (in arvo or serf)?
::    - |mass should be a query of the serf directly
::    - add duct or vane stack for spinner
::
+$  writ
  $%  $:  %live
          $%  [%cram eve=@]
              [%exit cod=@]
              [%save eve=@]
              [%meld ~]
              [%pack ~]
      ==  ==
      [%peek mil=@ sam=*]  :: gang (each path $%([%once @tas @tas path] [beam @tas beam]))
      [%play eve=@ lit=(list ?((pair @da ovum) *))]
      [%work mil=@ job=(pair @da ovum)]
  ==
::  +plea: from serf to king
::
+$  plea
  $%  [%live ~]
      [%ripe [pro=%1 hon=@ nok=@] eve=@ mug=@]
      [%slog pri=@ tank]
      [%flog cord]
      $:  %peek
          $%  [%done dat=(unit (cask))]
              [%bail dud=goof]
      ==  ==
      $:  %play
          $%  [%done mug=@]
              [%bail eve=@ mug=@ dud=goof]
      ==  ==
      $:  %work
          $%  [%done eve=@ mug=@ fec=(list ovum)]
              [%swap eve=@ mug=@ job=(pair @da ovum) fec=(list ovum)]
              [%bail lud=(list goof)]
      ==  ==
  ==
--
*/

/* _serf_space(): print n spaces.
*/
static void
_serf_space(FILE* fil_u,  c3_w n)
{
  for (; n > 0; n--)
    (fprintf(fil_u," "));
}

/* _serf_print_memory(): print memory amount.
**
**  Helper for _serf_prof(), just an un-captioned u3a_print_memory().
*/
static void
_serf_print_memory(FILE* fil_u, c3_w wor_w)
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

/* _serf_prof(): print memory profile. RETAIN.
*/
c3_w
_serf_prof(FILE* fil_u, c3_w den, u3_noun mas)
{
  c3_w tot_w = 0;
  u3_noun h_mas, t_mas;

  if ( c3n == u3r_cell(mas, &h_mas, &t_mas) ) {
    _serf_space(fil_u, den);
    fprintf(fil_u, "mistyped mass\r\n");
    return tot_w;
  }
  else if ( _(u3du(h_mas)) ) {
    _serf_space(fil_u, den);
    fprintf(fil_u, "mistyped mass head\r\n");
    {
      c3_c* lab_c = u3m_pretty(h_mas);
      fprintf(fil_u, "h_mas: %s", lab_c);
      c3_free(lab_c);
    }
    return tot_w;
  }
  else {
    _serf_space(fil_u, den);

    {
      c3_c* lab_c = u3m_pretty(h_mas);
      fprintf(fil_u, "%s: ", lab_c);
      c3_free(lab_c);
    }

    u3_noun it_mas, tt_mas;

    if ( c3n == u3r_cell(t_mas, &it_mas, &tt_mas) ) {
      fprintf(fil_u, "mistyped mass tail\r\n");
      return tot_w;
    }
    else if ( c3y == it_mas ) {
      tot_w += u3a_mark_noun(tt_mas);
      _serf_print_memory(fil_u, tot_w);

#if 1
      /* The basic issue here is that tt_mas is included in .sac
       * (the whole profile), so they can't both be roots in the
       * normal sense. When we mark .sac later on, we want tt_mas
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
        tot_w += _serf_prof(fil_u, den+2, u3h(tt_mas));
        tt_mas = u3t(tt_mas);
      }

      _serf_space(fil_u, den);
      fprintf(fil_u, "--");
      _serf_print_memory(fil_u, tot_w);

      return tot_w;

    }
    else {
      _serf_space(fil_u, den);
      fprintf(fil_u, "mistyped (strange) mass tail\r\n");
      return tot_w;
    }
  }
}

/* _serf_grab(): garbage collect, checking for profiling. RETAIN.
*/
static void
_serf_grab(u3_serf* sef_u)
{
  if ( u3_nul == sef_u->sac) {
    if ( u3C.wag_w & (u3o_debug_ram | u3o_check_corrupt) ) {
      u3m_grab(sef_u->sac, u3_none);
    }
  }
  else {
    c3_w tot_w = 0;
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

      c3_c man_c[2054];
      snprintf(man_c, 2053, "%s/%s-serf.txt", nam_c, wen_c);

      fil_u = fopen(man_c, "w");
      fprintf(fil_u, "%s\r\n", wen_c);

      c3_free(wen_c);
      u3z(wen);
    }
#else
    {
      fil_u = stderr;
    }
#endif

    c3_assert( u3R == &(u3H->rod_u) );
    fprintf(fil_u, "\r\n");

    tot_w += u3a_maid(fil_u, "total userspace", _serf_prof(fil_u, 0, sef_u->sac));
    tot_w += u3m_mark(fil_u);
    tot_w += u3a_maid(fil_u, "space profile", u3a_mark_noun(sef_u->sac));

    u3a_print_memory(fil_u, "total marked", tot_w);
    u3a_print_memory(fil_u, "free lists", u3a_idle(u3R));
    u3a_print_memory(fil_u, "sweep", u3a_sweep());

    fflush(fil_u);

#ifdef U3_MEMORY_LOG
    {
      fclose(fil_u);
    }
#endif

    u3z(sef_u->sac);
    sef_u->sac = u3_nul;

    u3l_log("\n");
  }
}

/* u3_serf_grab(): garbage collect.
*/
void
u3_serf_grab(void)
{
  c3_assert( u3R == &(u3H->rod_u) );

  fprintf(stderr, "serf: measuring memory:\r\n");
  u3a_print_memory(stderr, "total marked", u3m_mark(stderr));
  u3a_print_memory(stderr, "free lists", u3a_idle(u3R));
  u3a_print_memory(stderr, "sweep", u3a_sweep());
  fprintf(stderr, "\r\n");
  fflush(stderr);
}

/* u3_serf_post(): update serf state post-writ.
*/
void
u3_serf_post(u3_serf* sef_u)
{
  if ( u3_no_reclaim != sef_u->rec_o ) {
    u3m_reclaim((u3_reclaim_all == sef_u->rec_o) ? c3y : c3n);
    sef_u->rec_o = u3_no_reclaim;
  }

  //  XX this runs on replay too, |mass s/b elsewhere
  //
  if ( c3y == sef_u->mut_o ) {
    _serf_grab(sef_u);
    sef_u->mut_o = c3n;
  }

  if ( c3y == sef_u->pac_o ) {
    u3a_print_memory(stderr, "serf: pack: gained", u3m_pack());
    u3l_log("\n");
    sef_u->pac_o = c3n;
  }
}

/* _serf_sure_feck(): event succeeded, send effects.
*/
static u3_noun
_serf_sure_feck(u3_serf* sef_u, c3_w pre_w, u3_noun vir)
{
  c3_o rec_o = u3_no_reclaim;
  c3_o pac_o = c3n;

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
        sef_u->sac = u3k(u3t(fec));
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
        rec_o = u3_reclaim_all;
      }

      riv = u3t(riv);
      i_w++;
    }
  }

  //  after a successful event, we check for memory pressure.
  //
  //    if we've exceeded either of two thresholds, we reclaim
  //    from our persistent caches, and notify the daemon
  //    (via a "fake" effect) that arvo should trim state
  //    (trusting that the daemon will enqueue an appropriate event).
  //    For future flexibility, the urgency of the notification is represented
  //    by a *decreasing* number: 0 is maximally urgent, 1 less so, &c.
  //
  //    high-priority: 2^22 contiguous words remaining (~8 MB)
  //    low-priority:  2^27 contiguous words remaining (~536 MB)
  //    XX maybe use 2^23 (~16 MB) and 2^26 (~268 MB?
  //
  //    XX these thresholds should trigger notifications sent to the king
  //    instead of directly triggering these remedial actions.
  //
  {
    u3_noun pri = u3_none;
    c3_w pos_w = u3a_open(u3R);
    c3_w low_w = (1 << 27);
    c3_w hig_w = (1 << 22);

    if ( (pre_w > low_w) && !(pos_w > low_w) ) {
      //  XX set flag(s) in u3V so we don't repeat endlessly?
      //
      pac_o = c3y;
      rec_o = u3_reclaim_all;
      pri   = 1;
    }
    else if ( (pre_w > hig_w) && !(pos_w > hig_w) ) {
      pac_o = c3y;
      rec_o = u3_reclaim_all;
      pri   = 0;
    }
    //  reclaim memory from persistent caches periodically
    //
    //    XX this is a hack to work two things
    //    - bytecode caches grow rapidly and can't be simply capped
    //    - we don't make very effective use of our free lists
    //
    else if ( 0 == (sef_u->dun_d % 1000ULL) ) {
      rec_o = u3_reclaim_partial;
    }

    //  notify daemon of memory pressure via "fake" effect
    //
    if ( u3_none != pri ) {
      u3_noun cad = u3nc(u3nt(u3_blip, c3__arvo, u3_nul),
                         u3nc(c3__trim, pri));
      vir = u3nc(cad, vir);
    }
  }

  sef_u->rec_o = (sef_u->rec_o < rec_o) ? rec_o : sef_u->rec_o;
  sef_u->pac_o = c3o(sef_u->pac_o, pac_o);

  return vir;
}

/* _serf_sure_core(): event succeeded, save state.
*/
static void
_serf_sure_core(u3_serf* sef_u, u3_noun cor)
{
  sef_u->dun_d = sef_u->sen_d;

  u3z(u3A->roc);
  u3A->roc     = cor;
  u3A->eve_d   = sef_u->dun_d;
  sef_u->mug_l = u3r_mug(u3A->roc);
  sef_u->mut_o = c3y;
}

/* _serf_sure(): event succeeded, save state and process effects.
*/
static u3_noun
_serf_sure(u3_serf* sef_u, c3_w pre_w, u3_noun par)
{
  //  vir/(list ovum)  list of effects
  //  cor/arvo         arvo core
  //
  u3_noun vir, cor;
  u3x_cell(par, &vir, &cor);

  _serf_sure_core(sef_u, u3k(cor));
  vir = _serf_sure_feck(sef_u, pre_w, u3k(vir));

  u3z(par);
  return vir;
}

/* _serf_make_crud():
*/
static u3_noun
_serf_make_crud(u3_noun job, u3_noun dud)
{
  u3_noun now, ovo, new;
  u3x_cell(job, &now, &ovo);

  new = u3nt(u3i_vint(u3k(now)),
             u3nt(u3_blip, c3__arvo, u3_nul),
             u3nt(c3__crud, dud, u3k(ovo)));

  u3z(job);
  return new;
}

/* _serf_poke(): RETAIN
*/
static u3_noun
_serf_poke(u3_serf* sef_u, c3_c* cap_c, c3_w mil_w, u3_noun job)
{
  u3_noun now, ovo, wen, gon;
  u3x_cell(job, &now, &ovo);

  wen      = u3A->now;
  u3A->now = u3k(now);

#ifdef U3_EVENT_TIME_DEBUG
  struct timeval b4;
  c3_c*       txt_c;

  gettimeofday(&b4, 0);

  {
    u3_noun tag = u3h(u3t(ovo));
    txt_c = u3r_string(tag);

    if (  (c3__belt != tag)
       && (c3__crud != tag) )
    {
      u3l_log("serf: %s (%" PRIu64 ") %s\r\n", cap_c, sef_u->sen_d, txt_c);
    }
  }
#endif

  gon = u3m_soft(mil_w, u3v_poke, u3k(ovo));

#ifdef U3_EVENT_TIME_DEBUG
  {
    struct timeval f2, d0;
    c3_w ms_w;
    c3_w clr_w;

    gettimeofday(&f2, 0);
    timersub(&f2, &b4, &d0);

    ms_w = (d0.tv_sec * 1000) + (d0.tv_usec / 1000);
    clr_w = ms_w > 1000 ? 1 : ms_w < 100 ? 2 : 3; //  red, green, yellow

    if ( clr_w != 2 ) {
      u3l_log("\x1b[3%dm%%%s (%" PRIu64 ") %4d.%02dms\x1b[0m\n",
              clr_w, txt_c, sef_u->sen_d, ms_w,
              (int) (d0.tv_usec % 1000) / 10);
    }

    c3_free(txt_c);
  }
#endif

  if ( u3_blip != u3h(gon) ) {
    u3z(u3A->now);
    u3A->now = wen;
  }
  else {
    u3z(wen);
  }

  return gon;
}

/* _serf_work():  apply event, capture effects.
*/
static u3_noun
_serf_work(u3_serf* sef_u, c3_w mil_w, u3_noun job)
{
  u3_noun gon;
  c3_w  pre_w = u3a_open(u3R);

  //  event numbers must be continuous
  //
  c3_assert( sef_u->sen_d == sef_u->dun_d);
  sef_u->sen_d++;

  gon = _serf_poke(sef_u, "work", mil_w, job);  // retain

  //  event accepted
  //
  if ( u3_blip == u3h(gon) ) {
    u3_noun vir = _serf_sure(sef_u, pre_w, u3k(u3t(gon)));

    u3z(gon); u3z(job);
    return u3nc(c3__done, u3nt(u3i_chubs(1, &sef_u->dun_d),
                               sef_u->mug_l,
                               vir));
  }
  //  event rejected
  //
  else {
    //  stash $goof from first crash
    //
    u3_noun dud = u3k(gon);

    // XX reclaim on %meme first?
    //

    job = _serf_make_crud(job, dud);
    gon = _serf_poke(sef_u, "crud", mil_w, job);  // retain

    //  error notification accepted
    //
    if ( u3_blip == u3h(gon) ) {
      u3_noun vir = _serf_sure(sef_u, pre_w, u3k(u3t(gon)));

      u3z(gon); u3z(dud);
      return u3nc(c3__swap, u3nq(u3i_chubs(1, &sef_u->dun_d),
                                 sef_u->mug_l,
                                 job,
                                 vir));
    }
    //  error notification rejected
    //
    else {
      sef_u->sen_d = sef_u->dun_d;

      // XX reclaim on %meme ?
      //

      u3z(job);
      return u3nq(c3__bail, gon, dud, u3_nul);
    }
  }
}

/* u3_serf_work(): apply event, producing effects.
*/
u3_noun
u3_serf_work(u3_serf* sef_u, c3_w mil_w, u3_noun job)
{
  c3_t  tac_t = ( u3C.wag_w & u3o_trace );
  c3_c  lab_c[2056];
  u3_noun pro;

  // XX refactor tracing
  //
  if ( tac_t ) {
    u3_noun wir = u3h(u3t(job));
    u3_noun cad = u3h(u3t(u3t(job)));

    {
      c3_c* cad_c = u3m_pretty(cad);
      c3_c* wir_c = u3m_pretty_path(wir);
      snprintf(lab_c, 2056, "work [%s %s]", wir_c, cad_c);
      c3_free(cad_c);
      c3_free(wir_c);
    }

    u3t_event_trace(lab_c, 'B');
  }

  //  %work must be performed against an extant kernel
  //
  c3_assert( 0 != sef_u->mug_l);

  pro = u3nc(c3__work, _serf_work(sef_u, mil_w, job));

  if ( tac_t ) {
    u3t_event_trace(lab_c, 'E');
  }

  return pro;
}

/* _serf_play_life():
*/
static u3_noun
_serf_play_life(u3_serf* sef_u, u3_noun eve)
{
  u3_noun gon;

  c3_assert( 0ULL == sef_u->sen_d );

  {
    u3_noun len = u3qb_lent(eve);
    c3_assert( c3y == u3r_safe_chub(len, &sef_u->sen_d) );
    u3z(len);
  }

  //  ensure zero-initialized kernel
  //
  //    XX assert?
  //
  u3A->roc = 0;

  gon = u3m_soft(0, u3v_life, eve);

  //  lifecycle sequence succeeded
  //
  if ( u3_blip == u3h(gon) ) {
    //  save product as initial arvo kernel
    //
    _serf_sure_core(sef_u, u3k(u3t(gon)));

    u3z(gon);
    return u3nc(c3__done, sef_u->mug_l);
  }
  //  lifecycle sequence failed
  //
  else {
    //  send failure message and trace
    //
    sef_u->dun_d = sef_u->sen_d = 0;

    return u3nq(c3__bail, 0, 0, gon);
  }
}

/* _serf_play_poke(): RETAIN
*/
static u3_noun
_serf_play_poke(u3_noun job)
{
  u3_noun now, ovo, wen, gon;
  u3x_cell(job, &now, &ovo);

  wen      = u3A->now;
  u3A->now = u3k(now);
  gon      = u3m_soft(0, u3v_poke, u3k(ovo));

  if ( u3_blip != u3h(gon) ) {
    u3z(u3A->now);
    u3A->now = wen;
  }
  else {
    u3z(wen);
  }

  return gon;
}

/* _serf_play_list():
*/
static u3_noun
_serf_play_list(u3_serf* sef_u, u3_noun eve)
{
  c3_w pre_w = u3a_open(u3R);
  u3_noun vev = eve;
  u3_noun job, gon;

  while ( u3_nul != eve ) {
    job = u3h(eve);

    //  bump sent event counter
    //
    sef_u->sen_d++;

    gon = _serf_play_poke(job);

    //  event succeeded, save and continue
    //
    if ( u3_blip == u3h(gon) ) {
      //  vir/(list ovum)  list of effects
      //  cor/arvo         arvo core
      //
      u3_noun vir, cor;
      u3x_trel(gon, 0, &vir, &cor);

      _serf_sure_core(sef_u, u3k(cor));

      //  process effects to set u3_serf_post flags
      //
      u3z(_serf_sure_feck(sef_u, pre_w, u3k(vir)));

      u3z(gon);

      //  skip |mass on replay
      //
      u3z(sef_u->sac);
      sef_u->sac = u3_nul;

      eve = u3t(eve);
    }
    //  event succeeded, save and continue
    //
    else {
      u3_noun dud = u3k(u3t(gon));

      //  reset sent event counter
      //
      sef_u->sen_d   = sef_u->dun_d;

      u3z(gon);

      //  XX reclaim on meme ?
      //

      //  send failure notification
      //
      u3z(vev);
      return u3nc(c3__bail, u3nt(u3i_chubs(1, &sef_u->dun_d),
                                 sef_u->mug_l,
                                 dud));
    }
  }

  u3z(vev);
  return u3nc(c3__done, sef_u->mug_l);
}

/* u3_serf_play(): apply event list, producing status.
*/
u3_noun
u3_serf_play(u3_serf* sef_u, c3_d eve_d, u3_noun lit)
{
  c3_assert( eve_d == 1ULL + sef_u->sen_d );

  //  XX better condition for no kernel?
  //
  return u3nc(c3__play, ( 0ULL == sef_u->dun_d )
                        ? _serf_play_life(sef_u, lit)
                        : _serf_play_list(sef_u, lit));
}

/* u3_serf_peek(): dereference namespace.
*/
u3_noun
u3_serf_peek(u3_serf* sef_u, c3_w mil_w, u3_noun sam)
{
  u3_noun gon = u3m_soft(mil_w, u3v_peek, sam);
  u3_noun pro;

  {
    u3_noun tag, dat;
    u3x_cell(gon, &tag, &dat);

    //  read succeeded, produce result
    //
    if ( u3_blip == tag ) {
      pro = u3nc(c3__done, u3k(dat));
      u3z(gon);
    }
    //  read failed, produce trace
    //
    //    NB, reads should *not* fail deterministically
    //
    else {
      pro = u3nc(c3__bail, gon);
    }
  }

  return u3nc(c3__peek, pro);
}

/* _serf_writ_live_exit(): exit on command.
*/
static void
_serf_writ_live_exit(u3_serf* sef_u, c3_w cod_w)
{
  if ( u3C.wag_w & u3o_debug_cpu ) {
    FILE* fil_u;

    {
      u3_noun wen = u3dc("scot", c3__da, u3k(u3A->now));
      c3_c* wen_c = u3r_string(wen);

      c3_c nam_c[2048];
      snprintf(nam_c, 2048, "%s/.urb/put/profile", u3P.dir_c);

      struct stat st;
      if ( -1 == stat(nam_c, &st) ) {
        mkdir(nam_c, 0700);
      }

      c3_c man_c[2054];
      snprintf(man_c, 2053, "%s/%s.txt", nam_c, wen_c);

      fil_u = fopen(man_c, "w");

      c3_free(wen_c);
      u3z(wen);
    }

    u3t_damp(fil_u);

    {
      fclose(fil_u);
    }
  }

  //  XX move to jets.c
  //
  c3_free(u3D.ray_u);

  sef_u->xit_f();

  exit(cod_w);
}

/* _serf_writ_live_save(): save snapshot.
*/
static void
_serf_writ_live_save(u3_serf* sef_u, c3_d eve_d)
{
  if( eve_d != sef_u->dun_d ) {
    fprintf(stderr, "serf (%" PRIu64 "): save failed: %" PRIu64 "\r\n",
                    sef_u->dun_d,
                    eve_d);
    exit(1);
  }

  u3e_save();
}

/* u3_serf_live(): apply %live command [com], producing *ret on c3y.
*/
c3_o
u3_serf_live(u3_serf* sef_u, u3_noun com, u3_noun* ret)
{
  u3_noun tag, dat;

  //  refcounts around snapshots require special handling
  //
  if ( c3n == u3r_cell(com, &tag, &dat) ) {
    u3z(com);
    return c3n;
  }

  switch ( tag ) {
    default: {
      u3z(com);
      return c3n;
    }

    case c3__exit: {
      c3_y cod_y;

      if ( c3n == u3r_safe_byte(dat, &cod_y) ) {
        u3z(com);
        return c3n;
      }

      u3z(com);
      //  NB, doesn't return
      //
      _serf_writ_live_exit(sef_u, cod_y);
      *ret = u3nc(c3__live, u3_nul);
      return c3y;
    }

    //  NB: the %cram $writ only saves the rock, it doesn't load it
    //
    case c3__cram: {
      c3_d eve_d;

      if ( c3n == u3r_safe_chub(dat, &eve_d) ) {
        u3z(com);
        return c3n;
      }

      u3z(com);

      if( eve_d != sef_u->dun_d ) {
        fprintf(stderr, "serf (%" PRIu64 "): cram failed: %" PRIu64 "\r\n",
                        sef_u->dun_d,
                        eve_d);
        return c3n;
      }

      u3l_log("serf (%" PRIu64 "): saving rock\r\n", sef_u->dun_d);

      if ( c3n == u3u_cram(sef_u->dir_c, eve_d) ) {
        fprintf(stderr, "serf (%" PRIu64 "): unable to jam state\r\n", eve_d);
        return c3n;
      }

      if ( u3r_mug(u3A->roc) != sef_u->mug_l ) {
        fprintf(stderr, "serf (%" PRIu64 "): mug mismatch 0x%08x 0x%08x\r\n",
                        eve_d, sef_u->mug_l, u3r_mug(u3A->roc));
        return c3n;
      }

      u3e_save();
      u3_serf_grab();

      *ret = u3nc(c3__live, u3_nul);
      return c3y;
    }

    case c3__pack: {
      if ( u3_nul != dat ) {
        u3z(com);
        return c3n;
      }
      else {
        u3z(com);
        u3a_print_memory(stderr, "serf: pack: gained", u3m_pack());
        *ret = u3nc(c3__live, u3_nul);
        return c3y;
      }
    }

    case c3__meld: {
      if ( u3_nul != dat ) {
        u3z(com);
        return c3n;
      }
      else {
        u3z(com);
        u3u_meld();
        *ret = u3nc(c3__live, u3_nul);
        return c3y;
      }
    }

    case c3__save: {
      c3_d eve_d;

      if ( c3n == u3r_safe_chub(dat, &eve_d) ) {
        u3z(com);
        return c3n;
      }

      u3z(com);
      _serf_writ_live_save(sef_u, eve_d);
      *ret = u3nc(c3__live, u3_nul);
      return c3y;
    }
  }
}

/* u3_serf_writ(): apply writ [wit], producing plea [*pel] on c3y.
*/
c3_o
u3_serf_writ(u3_serf* sef_u, u3_noun wit, u3_noun* pel)
{
  u3_noun tag, com;
  c3_o  ret_o;

  if ( c3n == u3r_cell(wit, &tag, &com) ) {
    ret_o = c3n;
  }
  else {
    switch ( tag ) {
      default: {
        ret_o = c3n;
      } break;

      case c3__live: {
        //  since %live can take snapshots, it's refcount protocol is unique
        //
        u3k(com);
        u3z(wit);
        return u3_serf_live(sef_u, com, pel);
      } break;

      case c3__peek: {
        u3_noun tim, sam;
        c3_w  mil_w;

        if ( (c3n == u3r_cell(com, &tim, &sam)) ||
             (c3n == u3r_safe_word(tim, &mil_w)) )
        {
          ret_o = c3n;
        }
        else {
          *pel = u3_serf_peek(sef_u, mil_w, u3k(sam));
          ret_o = c3y;
        }
      } break;

      case c3__play: {
        u3_noun eve, lit;
        c3_d eve_d;

        if ( (c3n == u3r_cell(com, &eve, &lit)) ||
             (c3n == u3a_is_cell(lit)) ||
             (c3n == u3r_safe_chub(eve, &eve_d)) )
        {
          ret_o = c3n;
        }
        else {
          *pel = u3_serf_play(sef_u, eve_d, u3k(lit));
          ret_o = c3y;
        }
      } break;

      case c3__work: {
        u3_noun tim, job;
        c3_w  mil_w;

        if ( (c3n == u3r_cell(com, &tim, &job)) ||
             (c3n == u3r_safe_word(tim, &mil_w)) )
        {
          ret_o = c3n;
        }
        else {
          *pel = u3_serf_work(sef_u, mil_w, u3k(job));
          ret_o = c3y;
        }
      } break;
    }
  }

  u3z(wit);
  return ret_o;
}

/* _serf_ripe(): produce initial serf state as [eve=@ mug=@]
*/
static u3_noun
_serf_ripe(u3_serf* sef_u)
{
  // u3l_log("serf: ripe %" PRIu64 "\r\n", sef_u->dun_d);

  sef_u->mug_l = ( 0 == sef_u->dun_d )
                 ? 0
                 : u3r_mug(u3A->roc);

  return u3nc(u3i_chubs(1, &sef_u->dun_d), sef_u->mug_l);
}

/* u3_serf_init(): init or restore, producing status.
*/
u3_noun
u3_serf_init(u3_serf* sef_u)
{
  u3_noun rip;

  {
    c3_w  pro_w = 1;
    c3_y  hon_y = 141;
    c3_y  noc_y = 4;
    u3_noun ver = u3nt(pro_w, hon_y, noc_y);

    rip = u3nt(c3__ripe, ver, _serf_ripe(sef_u));
  }

  //  XX move to u3_serf_post()
  //
  //  measure/print static memory usage if < 1/2 of the loom is available
  //
  // {
  //   c3_w pen_w = u3a_open(u3R);

  //   if ( !(pen_w > (1 << 28)) ) {
  //     fprintf(stderr, "\r\n");
  //     u3a_print_memory(stderr, "serf: contiguous free space", pen_w);
  //     u3_serf_grab();
  //   }
  // }

  sef_u->pac_o = c3n;
  sef_u->rec_o = u3_no_reclaim;
  sef_u->mut_o = c3n;
  sef_u->sac   = u3_nul;

  return rip;
}
