/* worker/mars.c
**
**  the main loop of a mars process.
*/
#include "all.h"
#include <vere/vere.h>
#include <vere/mars.h>

/* _mars_step_trace(): initialize or rotate trace file.
*/
static void
_mars_step_trace(const c3_c* dir_c)
{
  if ( u3C.wag_w & u3o_trace ) {
    if ( u3_Host.tra_u.con_w == 0  && u3_Host.tra_u.fun_w == 0 ) {
      u3t_trace_open(dir_c);
    }
    else if ( u3_Host.tra_u.con_w >= 100000 ) {
      u3t_trace_close();
      u3t_trace_open(dir_c);
    }
  }
}

/* _mars_poke_play(): replay an event.
*/
static u3_weak
_mars_poke_play(u3_mars* mar_u, c3_d eve_d, u3_noun job)
{
  u3_noun vir;

  if ( c3n == u3v_poke_sure(0, job, &vir) ) {
    return vir;
  }

  u3z(vir);
  return u3_none;
}

typedef enum {
  _play_yes_e,  //  success
  _play_mem_e,  //  %meme
  _play_int_e,  //  %intr
  _play_log_e,  //  event log fail
  _play_mug_e,  //  mug mismatch
  _play_bad_e   //  total failure
} _mars_play_e;

/* _mars_play_batch(): replay a batch of events.
*/
static _mars_play_e
_mars_play_batch(u3_mars* mar_u, c3_o mug_o, c3_w bat_w)
{
  u3_disk*      log_u = mar_u->log_u;
  u3_disk_walk* wok_u = u3_disk_walk_init(log_u, mar_u->dun_d + 1, bat_w);
  u3_fact       tac_u;
  u3_noun         dud;

  while ( c3y == u3_disk_walk_live(wok_u) ) {
    if ( c3n == u3_disk_walk_step(wok_u, &tac_u) ) {
      u3_disk_walk_done(wok_u);
      return _play_log_e;
    }

    c3_assert( ++mar_u->sen_d == tac_u.eve_d );

    if ( u3_none != (dud = _mars_poke_play(mar_u, tac_u.eve_d, tac_u.job)) ) {
      c3_m mot_m;

      mar_u->sen_d = mar_u->dun_d;
      u3_disk_walk_done(wok_u);

      c3_assert( c3y == u3r_safe_word(u3h(dud), &mot_m) );

      switch ( mot_m ) {
        case c3__meme: {
          fprintf(stderr, "play (%" PRIu64 "): %%meme\r\n", tac_u.eve_d);
          u3z(dud);
          return _play_mem_e;
        }

        case c3__intr: {
          fprintf(stderr, "play (%" PRIu64 "): %%intr\r\n", tac_u.eve_d);
          u3z(dud);
          return _play_int_e;
        }

        default: {
          fprintf(stderr, "play (%" PRIu64 "): failed\r\n", tac_u.eve_d);
          u3_pier_punt_goof("play", dud);
          //  XX say something uplifting
          //
          return _play_bad_e;
        }
      }
    }

    mar_u->mug_l = u3r_mug(u3A->roc);

    if ( tac_u.mug_l && (mar_u->mug_l != tac_u.mug_l) ) {
      fprintf(stderr, "play (%" PRIu64 "): mug mismatch "
                      "expected %08x, actual %08x\r\n",
                      tac_u.eve_d, tac_u.mug_l, mar_u->mug_l);

      if ( c3y == mug_o ) {
        mar_u->sen_d = mar_u->dun_d;
        u3_disk_walk_done(wok_u);
        return _play_mug_e;
      }
    }

    mar_u->dun_d = mar_u->sen_d;
  }

  u3_disk_walk_done(wok_u);

  return _play_yes_e;
}

static c3_o
_mars_do_boot(u3_disk* log_u, c3_d eve_d)
{
  u3_weak eve;
  c3_l  mug_l;

  if ( u3_none == (eve = u3_disk_read_list(log_u, 1, eve_d, &mug_l)) ) {
    fprintf(stderr, "boot: read failed\r\n");
    return c3n;
  }

  u3l_log("--------------- bootstrap starting ----------------");

  u3l_log("boot: 1-%u", u3qb_lent(eve));

  if ( c3n == u3v_boot(eve) ) {
    return c3n;
  }

  u3l_log("--------------- bootstrap complete ----------------");
  return c3y;
}

/* u3_mars_play(): replay logged events up to [eve_d].
*/
void
u3_mars_play(u3_mars* mar_u, c3_d eve_d)
{
  u3_disk* log_u = mar_u->log_u;

  if ( !eve_d ) {
    eve_d = log_u->dun_d;
  }
  else if ( eve_d <= mar_u->dun_d ) {
    u3l_log("mars: already computed %" PRIu64, eve_d);
    u3l_log("      state=%" PRIu64 ", log=%" PRIu64,
            mar_u->dun_d, log_u->dun_d);
    return;
  }
  else {
    eve_d = c3_min(eve_d, log_u->dun_d);
  }

  if ( !mar_u->dun_d ) {
    c3_w lif_w;

    if ( c3n == u3_disk_read_meta(log_u, 0, 0, &lif_w) ) {
      fprintf(stderr, "mars: disk read meta fail\r\n");
      //  XX exit code, cb
      //
      exit(1);
    }

    if ( c3n == _mars_do_boot(mar_u->log_u, lif_w) ) {
      fprintf(stderr, "mars: boot fail\r\n");
      //  XX exit code, cb
      //
      exit(1);
    }

    mar_u->sen_d = mar_u->dun_d = lif_w;
  }

  if ( mar_u->dun_d == log_u->dun_d ) {
    u3l_log("mars: nothing to do!");
    return;
  }

  u3l_log("---------------- playback starting ----------------");

  if ( (1ULL + eve_d) == log_u->dun_d ) {
    u3l_log("play: event %" PRIu64, log_u->dun_d);
  }
  else if ( eve_d != log_u->dun_d ) {
    u3l_log("play: events %" PRIu64 "-%" PRIu64 " of %" PRIu64,
            (c3_d)(1ULL + mar_u->dun_d),
            eve_d,
            log_u->dun_d);
  }
  else {
    u3l_log("play: events %" PRIu64 "-%" PRIu64,
            (c3_d)(1ULL + mar_u->dun_d),
            eve_d);
  }

  {
    c3_d mem_d = 0;             // last event to meme
    c3_w try_w = 0;             // [mem_d] retry count

    while ( mar_u->dun_d < eve_d ) {
      _mars_step_trace(mar_u->dir_c);

      //  XX get batch from args
      //
      switch ( _mars_play_batch(mar_u, c3y, 1024) ) {
        case _play_yes_e: {
          u3l_log("play (%" PRIu64 "): done", mar_u->dun_d);
          u3m_reclaim();

          //  XX save a snapshot every N events?
          //
        } break;

        case _play_mem_e: {
          if ( mem_d != mar_u->dun_d ) {
            mem_d = mar_u->dun_d;
            try_w = 0;
          }
          else if ( 3 == ++try_w ) {
            fprintf(stderr, "play (%" PRIu64 "): failed\r\n", mar_u->dun_d + 1);
            u3m_save();
            //  XX check loom size, suggest --loom X
            //  XX exit code, cb
            //
            u3_disk_exit(log_u);
            exit(1);
          }

          //  XX pack before meld?
          //
          if ( u3C.wag_w & u3o_auto_meld ) {
            u3a_print_memory(stderr, "mars: meld: gained", u3u_meld());
          }
          else {
            u3a_print_memory(stderr, "mars: pack: gained", u3m_pack());
          }
        } break;

        //  XX handle any specifically?
        //
        case _play_int_e:
        case _play_log_e:
        case _play_mug_e:
        case _play_bad_e: {
          fprintf(stderr, "play (%" PRIu64 "): failed\r\n", mar_u->dun_d + 1);
          u3m_save();
          //  XX exit code, cb
          //
          u3_disk_exit(log_u);
          exit(1);
        }
      }
    }
  }

  u3l_log("---------------- playback complete ----------------");
  u3m_save();
}
