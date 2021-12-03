/* worker/mars.c
**
**  the main loop of a serf process.
*/
#include "all.h"
#include <vere/vere.h>
#include <vere/db/lmdb.h>

/*
::  peek=[gang (each path $%([%once @tas @tas path] [%beam @tas beam]))]
::  ovum=ovum
::
::    next steps:
::    - |mass should be a query of the serf directly
::    - add duct or vane stack for spinner
::
|$  [peek ovum]
|%
+$  task                                                ::  urth -> mars
  $%  [%live ?(%meld %pack) ~] :: XX rename
      [%exit ~]
      [%peek mil=@ peek]
      [%poke mil=@ ovum]  ::  XX replacement y/n
      [%sync ?(%cram %save) ~] :: XX remove cram?
  ==
+$  gift                                                ::  mars -> urth
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

/* _mars_fact(): commit a fact and enqueue its effects.
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
    u3_gift* gif_u = c3_malloc(sizeof(*gif_u));
    gif_u->nex_u = 0;
    gif_u->sat_e = u3_gift_poke_e;
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

/* _mars_gift(): enqueue response message.
*/
static void
_mars_gift(u3_mars* mar_u, u3_noun pro)
{
  u3_gift* gif_u = c3_malloc(sizeof(*gif_u));
  gif_u->nex_u = 0;
  gif_u->sat_e = u3_gift_rest_e;
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

/* _mars_poke(): attempt to compute an event.
*/
static c3_o
_mars_poke(c3_w   mil_w,
           c3_o   rep_o,
           u3_noun* eve,
           u3_noun* out)
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
      *out = u3nt(dud, pro, u3_nul);
      return c3n;
    }
  }
}

/* _mars_work(): perform a task.
*/
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

      mar_u->sat_e = u3_mars_save_e;
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
          u3a_print_memory(stderr, "mars: pack: gained", u3m_pack());
        } break;

        case c3__meld: {
          u3z(jar);
          u3u_meld();
        } break;
      }

      _mars_gift(mar_u, u3nc(c3__live, u3_nul));
    } break;

    case c3__exit: {
      u3z(jar);
      mar_u->sat_e = u3_mars_exit_e;
      //  XX wire up to signal handler
      //
      u3_disk_info(mar_u->log_u);
    } break;
  }

  return c3y;
}

/* _mars_work(): deserialize a task.
*/
static u3_weak
_mars_cue(u3_mars* mar_u, c3_d len_d, c3_y* hun_y)
{
  u3_weak jar;

#ifdef MARS_TRACE_CUE
  u3t_event_trace("mars ipc cue", 'B');
#endif

  jar = u3s_cue_xeno_with(mar_u->sil_u, len_d, hun_y);

#ifdef MARS_TRACE_CUE
  u3t_event_trace("mars ipc cue", 'E');
#endif

  return jar;
}

/* _mars_flush(): send pending gifts.
*/
static void
_mars_flush(u3_mars* mar_u)
{
top:
  {
    u3_gift* gif_u = mar_u->gif_u.ext_u;

    //  XX gather i/o
    //
    while (  gif_u
          && (  (u3_gift_rest_e == gif_u->sat_e)
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

  if (  (u3_mars_work_e != mar_u->sat_e)
     && (mar_u->log_u->dun_d == mar_u->dun_d) )
  {
    if ( u3_mars_save_e == mar_u->sat_e ) {
      u3e_save();
      _mars_gift(mar_u,
        u3nt(c3__sync, u3i_chub(mar_u->dun_d), mar_u->mug_l));
      mar_u->sat_e = u3_mars_work_e;
      goto top;
    }
    else if ( u3_mars_exit_e == mar_u->sat_e ) {
      //  XX exit cb ?
      //
      u3e_save();
      exit(0);
    }
  }
}

/* u3_mars_kick(): maybe perform a task.
*/
c3_o
u3_mars_kick(u3_mars* mar_u, c3_d len_d, c3_y* hun_y)
{
  c3_o ret_o = c3n;

  //  XX optimize for save/cram w/ peek-next
  //
  if ( u3_mars_work_e == mar_u->sat_e ) {
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

/* _mars_disk_cb(): mars commit result callback.
*/
static void
_mars_timer_cb(uv_timer_t* tim_u)
{
  u3_mars* mar_u = tim_u->data;
  mar_u->sat_e = u3_mars_save_e;

  _mars_flush(mar_u);
}

/* _mars_disk_cb(): mars commit result callback.
*/
static void
_mars_disk_cb(void* ptr_v, c3_d eve_d, c3_o ret_o)
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

static c3_o
_mars_do_boot(u3_disk* log_u, c3_d eve_d)
{
  u3_weak eve;
  c3_l  mug_l;

  if ( u3_none == (eve = u3_disk_read_list(log_u, 1, eve_d, &mug_l)) ) {
    fprintf(stderr, "boot: read failed\r\n");
    return c3n;
  }

  u3l_log("--------------- bootstrap starting ----------------\r\n");

  if ( c3n == u3v_boot(eve) ) {
    return c3n;
  }

  u3l_log("--------------- bootstrap complete ----------------\r\n");
  return c3y;
}

/* u3_mars_init(): init mars, replay if necessary.
*/
u3_mars*
u3_mars_init(c3_c*    dir_c,
             u3_moat* inn_u,
             u3_mojo* out_u)
{
  u3_mars* mar_u = c3_malloc(sizeof(*mar_u));
  mar_u->inn_u = inn_u;
  mar_u->out_u = out_u;
  mar_u->sen_d = mar_u->dun_d = u3A->eve_d;
  mar_u->mug_l = u3r_mug(u3A->roc);
  mar_u->pac_o = mar_u->rec_o = mar_u->mut_o = c3n;
  mar_u->sac   = u3_nul;
  mar_u->sat_e = u3_mars_work_e;
  mar_u->gif_u.ent_u = mar_u->gif_u.ext_u = 0;
  mar_u->xit_f = 0;

  mar_u->sil_u = u3s_cue_xeno_init();

  //  initialize persistence
  //
  //    XX load/set secrets
  //
  if ( !(mar_u->log_u = u3_disk_init(dir_c)) ) {
    fprintf(stderr, "mars: disk init fail\r\n");
    c3_free(mar_u);
    return 0;
  }

  if ( c3n == u3_disk_read_meta(mar_u->log_u, &(mar_u->met_u)) ) {
    fprintf(stderr, "mars: disk meta fail\r\n");
    u3_disk_exit(mar_u->log_u);
    c3_free(mar_u);
    return 0;
  }

  if ( !mar_u->dun_d ) {
    if ( c3n == _mars_do_boot(mar_u->log_u, mar_u->met_u.lif_w) ) {
      fprintf(stderr, "mars: boot fail\r\n");
      u3_disk_exit(mar_u->log_u);
      c3_free(mar_u);
      return 0;
    }

    mar_u->sen_d = mar_u->dun_d = mar_u->met_u.lif_w;
    u3e_save();
  }

  if ( mar_u->log_u->dun_d > mar_u->dun_d ) {
    u3_mars_play(mar_u);
    u3e_save();
  }

  //  send ready status message
  //
  //    XX version negotiation
  //
  {
    c3_d  len_d;
    c3_y* hun_y;
    u3_noun wyn = u3_nul;
    u3_noun msg = u3nq(c3__ripe,
                       u3nc(2, wyn),
                       u3nc(u3i_chubs(2, mar_u->met_u.who_d),
                            mar_u->met_u.fak_o),
                       u3nc(u3i_chub(mar_u->dun_d),
                            mar_u->mug_l));

    u3s_jam_xeno(msg, &len_d, &hun_y);
    u3_newt_send(mar_u->out_u, len_d, hun_y);
    u3z(msg);
  }

  u3_disk_async(mar_u->log_u, mar_u, _mars_disk_cb);

  //  XX check return, make interval configurable
  //
  uv_timer_init(u3L, &mar_u->tim_u);
  uv_timer_start(&mar_u->tim_u, _mars_timer_cb, 120000, 120000);

  mar_u->tim_u.data = mar_u;

  return mar_u;
}

/* _mars_wyrd_card(): construct %wyrd.
*/
static u3_noun
_mars_wyrd_card(c3_m nam_m, c3_w ver_w, c3_l sev_l)
{
  //  ghetto (scot %ta)
  //
  u3_noun ver = u3nt(c3__vere, u3i_string("~." URBIT_VERSION), u3_nul);
  // u3_noun sen = u3dc("scot", c3__uv, sev_l); //  lol no
  u3_noun sen = u3i_string("0v1s.vu178");
  u3_noun kel;

  //  special case versions requiring the full stack
  //
  if (  ((c3__zuse == nam_m) && (419 == ver_w))
     || ((c3__lull == nam_m) && (330 == ver_w))
     || ((c3__arvo == nam_m) && (240 == ver_w)) )
  {
    kel = u3nl(u3nc(c3__zuse, 419),
               u3nc(c3__lull, 330),
               u3nc(c3__arvo, 240),
               u3nc(c3__hoon, 140),
               u3nc(c3__nock, 4),
               u3_none);
  }
  else {
    kel = u3nc(nam_m, u3i_word(ver_w));
  }

  return u3nt(c3__wyrd, u3nc(sen, ver), kel);
}

/* _mars_sift_pill(): extract boot formulas and module/userspace ova from pill
*/
static c3_o
_mars_sift_pill(u3_noun  pil,
                u3_noun* bot,
                u3_noun* mod,
                u3_noun* use)
{
  u3_noun pil_p, pil_q;

  if ( c3n == u3r_cell(pil, &pil_p, &pil_q) ) {
    return c3n;
  }

  {
    //  XX use faster cue
    //
    u3_noun pro = u3m_soft(0, u3ke_cue, u3k(pil_p));
    u3_noun mot, tag, dat;

    if (  (c3n == u3r_trel(pro, &mot, &tag, &dat))
       || (u3_blip != mot) )
    {
      u3m_p("mot", u3h(pro));
      fprintf(stderr, "boot: failed: unable to parse pill\r\n");
      return c3n;
    }

    if ( c3y == u3r_sing_c("ivory", tag) ) {
      fprintf(stderr, "boot: failed: unable to boot from ivory pill\r\n");
      return c3n;
    }
    else if ( c3__pill != tag ) {
      if ( c3y == u3a_is_atom(tag) ) {
        u3m_p("pill", tag);
      }
      fprintf(stderr, "boot: failed: unrecognized pill\r\n");
      return c3n;
    }

    {
      u3_noun typ;
      c3_c* typ_c;

      if ( c3n == u3r_qual(dat, &typ, bot, mod, use) ) {
        fprintf(stderr, "boot: failed: unable to extract pill\r\n");
        return c3n;
      }

      if ( c3y == u3a_is_atom(typ) ) {
        c3_c* typ_c = u3r_string(typ);
        fprintf(stderr, "boot: parsing %%%s pill\r\n", typ_c);
        c3_free(typ_c);
      }
    }

    u3k(*bot); u3k(*mod); u3k(*use);
    u3z(pro);
  }

  //  optionally replace filesystem in userspace
  //
  if ( u3_nul != pil_q ) {
    c3_w  len_w = 0;
    u3_noun ova = *use;
    u3_noun new = u3_nul;
    u3_noun ovo;

    while ( u3_nul != ova ) {
      ovo = u3h(ova);

      if ( c3__into == u3h(u3t(ovo)) ) {
        c3_assert( 0 == len_w );
        len_w++;
        ovo = u3t(pil_q);
      }

      new = u3nc(u3k(ovo), new);
      ova = u3t(ova);
    }

    c3_assert( 1 == len_w );

    u3z(*use);
    *use = u3kb_flop(new);
  }

  u3z(pil);

  return c3y;
}

/* _mars_boot_make(): construct boot sequence
*/
static c3_o
_mars_boot_make(u3_boot_opts* inp_u,
                u3_noun         com,
                u3_noun*        ova,
                u3_meta*      met_u)
{
  u3_noun pil, ven, mor, who;

  //  parse boot command
  //
  if ( c3n == u3r_trel(com, &pil, &ven, &mor) ) {
    fprintf(stderr, "boot: invalid command\r\n");
    return c3n;
  }

  //  parse boot event
  //
  {
    u3_noun tag, dat;

    if ( c3n == u3r_cell(ven, &tag, &dat) ) {
      return c3n;
    }

    switch ( tag ) {
      default: {
        fprintf(stderr, "boot: unknown boot event\r\n");
        u3m_p("tag", tag);
        return c3n;
      }

      case c3__fake: {
        met_u->fak_o = c3y;
        who          = dat;
      } break;

      case c3__dawn: {
        met_u->fak_o = c3n;

        if ( c3n == u3r_cell(dat, &who, 0) ) {
          return c3n;
        }
        if ( c3n == u3r_cell(who, &who, 0) ) {
          return c3n;
        }
      } break;
    }
  }

  //  validate and extract identity
  //
  if (  (c3n == u3a_is_atom(who))
     || (1 < u3r_met(7, who)) )
  {
    fprintf(stderr, "boot: invalid identity\r\n");
    u3m_p("who", who);
    return c3n;
  }

  u3r_chubs(0, 2, met_u->who_d, who);

  {
    u3_noun bot, mod, use;

    //  parse pill
    //
    if ( c3n == _mars_sift_pill(pil, &bot, &mod, &use) ) { // transfer [pil]
      return c3n;
    }

    met_u->lif_w = u3qb_lent(bot);

    //  break symmetry in the module sequence
    //
    //    version negotation, verbose, identity, entropy
    //
    {
      u3_noun cad, wir = u3nt(u3_blip, c3__arvo, u3_nul);

      cad = u3nc(c3__wack, u3i_words(16, inp_u->eny_w));
      mod = u3nc(u3nc(u3k(wir), cad), mod);

      cad = u3nc(c3__whom, u3k(who));
      mod = u3nc(u3nc(u3k(wir), cad), mod);

      cad = u3nt(c3__verb, u3_nul, inp_u->veb_o);
      mod = u3nc(u3nc(u3k(wir), cad), mod);

      cad = _mars_wyrd_card(inp_u->ver_u.nam_m,
                            inp_u->ver_u.ver_w,
                            inp_u->sev_l);
      mod = u3nc(u3nc(wir, cad), mod);              // transfer [wir]
    }

    //  prepend legacy boot event to the userpace sequence
    //
    //    XX do something about this wire
    //
    {
      u3_noun wir = u3nq(c3__d, c3__term, '1', u3_nul);
      u3_noun cad = u3nt(c3__boot, inp_u->lit_o, ven); // transfer
      use = u3nc(u3nc(wir, cad), use);
    }

    u3_noun mos = mor;
    u3_noun pre = u3_nul;
    u3_noun aft = u3_nul;
    while ( u3_nul != mos ) {
      u3_noun mot = u3h(mos);

      switch ( u3h(mot) ) {
        case c3__prop: {
          u3_noun ter, met, ves;

          if ( c3n == u3r_trel(u3t(mot), &met, &ter, &ves) ) {
            u3m_p("invalid prop", u3t(mot));
            break;
          }

          if ( c3y == u3r_sing_c("pre-userspace", ter) ) {
            u3m_p("prop: pre-userspace", met);
            pre = u3kb_weld(pre, ves);
          }
          else if ( c3y == u3r_sing_c("post-userspace", ter) ) {
            u3m_p("prop: post-userspace", met);
            aft = u3kb_weld(aft, ves);
          }
          else {
            u3m_p("unrecognized prop tier", ter);
          }
        } break;

        default: u3m_p("unrecognized boot sequence enhancement", u3h(mot));
      }

      mos = u3t(mos);
    }

    //  timestamp events, cons list
    //
    {
      u3_noun now = u3_time_in_tv(&inp_u->tim_u);
      u3_noun bit = u3qc_bex(48);   //  1/2^16 seconds
      u3_noun eve = u3kb_flop(bot);

      {
        u3_noun  lit = u3kb_weld(mod, u3kb_weld(pre, u3kb_weld(use, aft)));
        u3_noun i, t = lit;

        while ( u3_nul != t ) {
          u3x_cell(t, &i, &t);
          now = u3ka_add(now, u3k(bit));
          eve = u3nc(u3nc(u3k(now), u3k(i)), eve);
        }

        u3z(lit);
      }

      *ova = u3kb_flop(eve);
      u3z(now); u3z(bit);
    }
  }

  return c3y;
}

/* u3_mars_boot(): boot a ship.
*
*  $=  com
*  $:  pill=[p=@ q=(unit ovum)]
*      $=  vent
*      $%  [%fake p=ship]
*          [%dawn p=seed]
*      ==
*      more=(list prop)
*  ==
*
*/
c3_o
u3_mars_boot(c3_c* dir_c, u3_noun com)
{
  u3_boot_opts inp_u;
  u3_meta      met_u;
  u3_noun        ova;

  //  XX source properly
  //
  inp_u.veb_o = c3n;
  inp_u.lit_o = c3y;
  inp_u.ver_u.nam_m = c3__zuse;
  inp_u.ver_u.ver_w = 419;

  gettimeofday(&inp_u.tim_u, 0);
  c3_rand(inp_u.eny_w);

  {
    u3_noun now = u3_time_in_tv(&inp_u.tim_u);
    inp_u.sev_l = u3r_mug(now);
    u3z(now);
  }

  if ( c3n == _mars_boot_make(&inp_u, com, &ova, &met_u) ) {
    fprintf(stderr, "boot: preparation failed\r\n");
    return c3n;
  }

  u3_disk* log_u;

  if ( !(log_u = u3_disk_init(dir_c)) ) {
    fprintf(stderr, "boot: disk init fail\r\n");
    return c3n;
  }

  if ( c3n == u3_disk_save_meta(log_u, &met_u) ) {
    return c3n;  //  XX cleanup
  }

  u3_disk_plan_list(log_u, ova);

  if ( c3n == u3_disk_sync(log_u) ) {
    return c3n;  //  XX cleanup
  }

  if ( c3n == _mars_do_boot(log_u, log_u->dun_d) ) {
    return c3n;  //  XX cleanup
  }

  u3e_save();
  u3_disk_exit(log_u);

  return c3y;
}
