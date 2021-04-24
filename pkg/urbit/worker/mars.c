/* worker/mar.c
**
**  the main loop of a serf process.
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
#include <vere/db/lmdb.h>

/*
::  peek=[gang (each path $%([%once @tas @tas path] [%beam @tas beam]))]
::  ovum=ovum
::
|$  [peek ovum]
|%
::  +task: from urth to mars
::
::    next steps:
::    - |mass should be a query of the serf directly
::    - add duct or vane stack for spinner
::
+$  task
  $%  [%live ?(%meld %pack) ~] :: XX rename
      [%exit ~]
      [%peek mil=@ peek]
      [%poke mil=@ ovum]  ::  XX replacement y/n
      [%sync ?(%cram %save) ~] :: XX remove cram?
  ==
::  +gift: from mars to urth
::
+$  gift
  $%  [%live ~]
      [%flog cord]
      [%slog pri=@ tank]
      [%peek p=(each (unit (cask)) goof)]
      [%poke p=(each (list ovum) (list goof))]
      [%ripe [pro=%2 kel=wynn] [who=@p fake=?] eve=@ mug=@]
      [%sync eve=@ mug=@]
  ==
--
*/

static void
_mars_fact(u3_mars* mar_u,
           u3_noun    job,
           u3_noun    pro)
{
  {
    u3_fact tac_u = {
      .job   = job,
      .mug_l = mar_u->mug_l,
      .eve_d = mar_u->dun_d
    };

    u3_disk_plan(mar_u->log_u, &tac_u);
    u3z(job);
  }

  {
    cw_gift* gif_u = c3_malloc(sizeof(*gif_u));
    gif_u->nex_u = 0;
    gif_u->sat_e = _cwe_gift_poke;
    gif_u->eve_d = mar_u->dun_d;

    u3s_jam_xeno(pro, &gif_u->len_d, &gif_u->hun_y);
    u3z(pro);

    if ( !mar_u->gif_u.ent_u ) {
      c3_assert( !mar_u->gif_u.ext_u );
      mar_u->gif_u.ent_u = mar_u->gif_u.ext_u = gif_u;
    }
    else {
      mar_u->gif_u.ent_u->nex_u = gif_u;
      mar_u->gif_u.ent_u = gif_u;
    }
  }
}

static void
_mars_gift(u3_mars* mar_u, u3_noun pro)
{
  cw_gift* gif_u = c3_malloc(sizeof(*gif_u));
  gif_u->nex_u = 0;
  gif_u->sat_e = _cwe_gift_rest;
  gif_u->ptr_v = 0;

  u3s_jam_xeno(pro, &gif_u->len_d, &gif_u->hun_y);
  u3z(pro);

  if ( !mar_u->gif_u.ent_u ) {
    c3_assert( !mar_u->gif_u.ext_u );
    mar_u->gif_u.ent_u = mar_u->gif_u.ext_u = gif_u;
  }
  else {
    mar_u->gif_u.ent_u->nex_u = gif_u;
    mar_u->gif_u.ent_u = gif_u;
  }
}

/* _mars_make_crud(): construct error-notification event.
*/
static u3_noun
_mars_make_crud(u3_noun job, u3_noun dud)
{
  u3_noun now, ovo, new;
  u3x_cell(job, &now, &ovo);

  new = u3nt(u3k(now),
             u3nt(u3_blip, c3__arvo, u3_nul),
             u3nt(c3__crud, dud, u3k(ovo)));

  u3z(job);
  return new;
}

static c3_o
_mars_poke(c3_w     mil_w,
           c3_o     rep_o,
           u3_noun*   eve,
           u3_noun*   out)
{
  u3_noun pro;

  if ( c3y == u3_poke_sure(mil_w, u3k(*eve), &pro) ) {
    *out = pro;
    return c3y;
  }
  else if ( c3n == rep_o ) {
    *out = u3nc(pro, u3_nul);
    return c3n;
  }
  else {
    u3_noun dud = pro;

    *eve = _mars_make_crud(*eve, u3k(dud));

    if ( c3y == u3_poke_sure(mil_w, u3k(*eve), &pro) ) {
      *out = pro;
      return c3y;
    }
    else {
      *out = u3nc(dud, pro);
      return c3n;
    }
  }
}


static c3_o
_mars_work(u3_mars* mar_u, u3_noun jar)
{
  u3_noun tag, dat, pro;

  if ( c3n == u3r_cell(jar, &tag, &dat) ) {
    fprintf(stderr, "mars: fail a\r\n");
    u3z(jar);
    return c3n;
  }

  switch ( tag ) {
    default: {
      fprintf(stderr, "mars: fail b\r\n");
      u3z(jar);
      return c3n;
    }

    case c3__poke: {
      u3_noun tim, job;
      c3_w  mil_w;

      if ( (c3n == u3r_cell(dat, &tim, &job)) ||
           (c3n == u3r_safe_word(tim, &mil_w)) )
      {
        fprintf(stderr, "mars: poke fail\r\n");
        u3z(jar);
        return c3n;
      }

      //  XX timestamp
      //
      {
        u3_noun now;
        struct timeval tim_u;
        gettimeofday(&tim_u, 0);

        now   = u3_time_in_tv(&tim_u);
        job = u3nc(now, u3k(job));
      }
      u3z(jar);

      mar_u->sen_d++;

      if ( c3y == _mars_poke(mil_w, c3y, &job, &pro) ) {
        mar_u->dun_d = mar_u->sen_d;
        mar_u->mug_l = u3r_mug(u3A->roc);

        //  XX process effects
        //
        _mars_fact(mar_u, job, u3nt(c3__poke, c3y, pro));
      }
      else {
        mar_u->sen_d = mar_u->dun_d;
        u3z(job);
        _mars_gift(mar_u, u3nt(c3__poke, c3n, pro));
      }

      c3_assert( mar_u->dun_d == u3A->eve_d );
    } break;

    case c3__peek: {
      u3_noun tim, sam, pro;
      c3_w  mil_w;

      if ( (c3n == u3r_cell(dat, &tim, &sam)) ||
           (c3n == u3r_safe_word(tim, &mil_w)) )
      {
        u3z(jar);
        return c3n;
      }

      u3k(sam); u3z(jar);
      _mars_gift(mar_u, u3nc(c3__peek, u3v_soft_peek(mil_w, sam)));
    } break;

    // XX remove /support cram?
    case c3__sync: {
      u3_noun nul;

      if (  (c3n == u3r_p(dat, c3__save, &nul))
         || (u3_nul != nul) )
      {
        u3z(jar);
        return c3n;
      }

      mar_u->sat_e = _cwe_mars_save;
    } break;

    //  $%  [%live ?(%meld %pack) ~] :: XX rename
    //
    case c3__live: {
      u3_noun com, nul;

      if ( (c3n == u3r_cell(dat, &com, &nul)) ||
           (u3_nul != nul) )
      {
        u3z(jar);
        return c3n;
      }

      switch ( com ) {
        default: {
          u3z(jar);
          return c3n;
        }

        case c3__pack: {
          u3z(jar);
          u3a_print_memory(stderr, "serf: pack: gained", u3m_pack());
        } break;

        case c3__meld: {
          u3z(jar);
          u3u_meld();
        } break;
      }

      _mars_gift(mar_u, u3nc(c3__live, u3_nul));
    } break;

    case c3__exit: {
      mar_u->sat_e = _cwe_mars_exit;
      //  XX wire up to signal handler
      //
      u3_disk_info(mar_u->log_u);
    } break;
  }

  return c3y;
}

static u3_weak
_mars_cue(u3_mars* mar_u, c3_d len_d, c3_y* hun_y)
{
  u3_weak jar;

#ifdef SERF_TRACE_CUE
  u3t_event_trace("serf ipc cue", 'B');
#endif

  jar = u3s_cue_xeno_with(mar_u->sil_u, len_d, hun_y);

#ifdef SERF_TRACE_CUE
  u3t_event_trace("serf ipc cue", 'E');
#endif

  return jar;
}

static void
_mars_flush(u3_mars* mar_u)
{
top:
  {
    cw_gift* gif_u = mar_u->gif_u.ext_u;

    //  XX gather i/o
    //
    while (  gif_u
          && (  (_cwe_gift_rest == gif_u->sat_e)
             || (gif_u->eve_d <= mar_u->log_u->dun_d)) )
    {
      u3_newt_send(mar_u->out_u, gif_u->len_d, gif_u->hun_y);

      mar_u->gif_u.ext_u = gif_u->nex_u;
      c3_free(gif_u);
      gif_u = mar_u->gif_u.ext_u;
    }

    if ( !mar_u->gif_u.ext_u ) {
      mar_u->gif_u.ent_u = 0;
    }
  }

  if (  (_cwe_mars_work != mar_u->sat_e)
     && (mar_u->log_u->dun_d == mar_u->dun_d) )
  {
    if ( _cwe_mars_save == mar_u->sat_e ) {
      u3e_save();
      _mars_gift(mar_u,
        u3nt(c3__sync, u3i_chubs(1, &mar_u->dun_d), mar_u->mug_l));
      mar_u->sat_e = _cwe_mars_work;
      goto top;
    }
    else if ( _cwe_mars_exit == mar_u->sat_e ) {
      //  XX exit cb ?
      //
      u3e_save();
      exit(0);
    }
  }
}

c3_o
u3_mars_kick(u3_mars* mar_u, c3_d len_d, c3_y* hun_y)
{
  c3_o ret_o = c3n;

  //  XX optimize for save/cram w/ peek-next
  //
  if ( _cwe_mars_work == mar_u->sat_e ) {
    u3_weak jar = _mars_cue(mar_u, len_d, hun_y);

    //  parse errors are fatal
    // 
    if (  (u3_none == jar)
       || (c3n == _mars_work(mar_u, jar)) )
    {
      fprintf(stderr, "mars: bad\r\n");
      //  XX error cb?
      //
      exit(1);
    }
    
    //  XX u3_serf_post()
    //
    // _cw_serf_step_trace();

    ret_o = c3y;
  }

  _mars_flush(mar_u);

  return ret_o;
}

static void
_mars_timer_cb(uv_timer_t* tim_u)
{
  u3_mars* mar_u = tim_u->data;
  mar_u->sat_e = _cwe_mars_save;

  _mars_flush(mar_u);
}

static void
_mars_save_done(void* ptr_v, c3_d eve_d, c3_o ret_o)
{
  u3_mars* mar_u = ptr_v;

  if ( c3n == ret_o ) {
    //  XX better
    //
    fprintf(stderr, "mars: commit fail\r\n");
    exit(1);
  }
  else {
    _mars_flush(mar_u);
  }
}

/* _mars_poke_play(): replay an event.
*/
static u3_noun
_mars_poke_play(c3_d eve_d, u3_noun job)
{
  u3_noun pro;

  if ( c3y == u3_poke_sure(0, job, &pro) ) {
    //  XX check effects
    //
    u3z(pro);
    return c3y;
  }

  //  XX reclaim on meme, retry on %intr, &c
  //
  // {
  //   u3_noun mot, tan;
  //   u3x_cell(pro, &mot, &tan);
  // }
  u3z(pro);
  return c3n;
}

/* _mars_play_batch(): replay a batch of events.
*/
static c3_o
_mars_play_batch(u3_mars* mar_u, c3_o mug_o, c3_w bat_w)
{
  u3_disk*      log_u = mar_u->log_u;
  u3_disk_walk* wok_u = u3_disk_walk_init(log_u, mar_u->dun_d + 1, bat_w);
  u3_fact       tac_u;

  while ( c3y == u3_disk_walk_live(wok_u) ) {
    if ( c3n == u3_disk_walk_step(wok_u, &tac_u) ) {
      u3_disk_walk_done(wok_u);
      return c3n;
    }

    c3_assert( ++mar_u->sen_d == tac_u.eve_d );

    if ( c3n == _mars_poke_play(tac_u.eve_d, tac_u.job) ) {
      fprintf(stderr, "play (%" PRIu64 "): failed\r\n", tac_u.eve_d);
      mar_u->sen_d--;
      u3_disk_walk_done(wok_u);
      return c3n;
    }

    mar_u->dun_d++;
    mar_u->mug_l = u3r_mug(u3A->roc);

    if ( tac_u.mug_l && (mar_u->mug_l != tac_u.mug_l) ) {
      fprintf(stderr, "play (%" PRIu64 "): mug mismatch "
                      "expected %08x, actual %08x\r\n",
                      tac_u.eve_d, tac_u.mug_l, mar_u->mug_l);

      if ( c3y == mug_o ) {
        mar_u->sen_d--;
        mar_u->dun_d--;
        u3_disk_walk_done(wok_u);
        return c3n;
      }
    }
  }

  u3_disk_walk_done(wok_u);

  return c3y;
}

/* u3_mars_play(): replay all newer logged events.
*/
void
u3_mars_play(u3_mars* mar_u)
{
  u3_disk* log_u = mar_u->log_u;

  u3l_log("---------------- playback starting ----------------\r\n");

  if ( (1ULL + mar_u->dun_d) == log_u->dun_d ) {
    u3l_log("play: event %" PRIu64 "\r\n", log_u->dun_d);
  }
  else {
    u3l_log("play: events %" PRIu64 "-%" PRIu64 "\r\n",
            (c3_d)(1ULL + mar_u->dun_d),
            log_u->dun_d);
  }

  while ( mar_u->dun_d < log_u->dun_d ) {
    //  XX get batch from args
    //
    if ( c3n == _mars_play_batch(mar_u, c3n, 500) ) {
      u3l_log("play (%" PRIu64 "): failed\r\n", mar_u->dun_d);
      //  XX exit code, cb
      //
      exit(1);
    }

    u3l_log("play (%" PRIu64 "): done\r\n", mar_u->dun_d);
  }

  u3l_log("---------------- playback complete ----------------\r\n");
}

/* u3_mars_init(): init mars, replay if necessary.
*/
u3_mars*
u3_mars_init(u3_disk* log_u,
             u3_moat* inn_u,
             u3_mojo* out_u,
             c3_c*    dir_c,
             u3_cue_xeno* sil_u)
{
  c3_assert( log_u->sen_d == log_u->dun_d );

  u3_mars* mar_u = c3_malloc(sizeof(*mar_u));
  mar_u->sil_u = sil_u;
  mar_u->log_u = log_u;
  mar_u->inn_u = inn_u;
  mar_u->out_u = out_u;
  mar_u->sen_d = mar_u->dun_d = u3A->eve_d;
  mar_u->mug_l = u3r_mug(u3A->roc);
  mar_u->pac_o = mar_u->rec_o = mar_u->mut_o = c3n;
  mar_u->sac   = u3_nul;
  mar_u->sat_e = _cwe_mars_work;
  mar_u->gif_u.ent_u = mar_u->gif_u.ext_u = 0;
  mar_u->xit_f = 0;

  if ( log_u->dun_d > mar_u->dun_d ) {
    u3_mars_play(mar_u);
    u3e_save();
  }

  u3_disk_async(log_u, mar_u, _mars_save_done);

  //  XX check return, make interval configurable
  //
  uv_timer_init(u3L, &mar_u->tim_u);
  uv_timer_start(&mar_u->tim_u, _mars_timer_cb, 120000, 120000);

  mar_u->tim_u.data = mar_u;

  return mar_u;
}
