/* worker/main.c
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
#include <vere/serf.h>
#include <vere/db/lmdb.h>

#include "ur/hashcons.h"

static u3_serf        u3V;             //  one serf per process
static u3_moat      inn_u;             //  input stream
static u3_mojo      out_u;             //  output stream
static u3_cue_xeno* sil_u;             //  cue handle
static u3_disk*     log_u;
//  completed events awaiting commit queue

// open_tx()
// while ( eve_d < gol_d ) {
//   mdb_get()
//   cue()
//   arvo_poke()
//   // error handling logic
//   eve_d++
// }
// abort_tx()

//  get potential event (ipc read)
//  enqueue
//  timestamp
//  poke
//    maybe crud
//    maybe complete failure reponse
//  queue write (accumulate batch)
//  stash effects

//  write batch
//  release effects (ipc write)


#undef SERF_TRACE_JAM
#undef SERF_TRACE_CUE

/* _cw_serf_fail(): failure stub.
*/
static void
_cw_serf_fail(void* ptr_v, ssize_t err_i, const c3_c* err_c)
{
  if ( UV_EOF == err_i ) {
    fprintf(stderr, "serf: pier unexpectedly shut down\r\n");
  }
  else {
    fprintf(stderr, "serf: pier error: %s\r\n", err_c);
  }

  exit(1);
}

/* _cw_serf_send(): send plea back to daemon.
*/
static void
_cw_serf_send(u3_noun pel)
{
  c3_d  len_d;
  c3_y* byt_y;

#ifdef SERF_TRACE_JAM
  u3t_event_trace("serf ipc jam", 'B');
#endif

  u3s_jam_xeno(pel, &len_d, &byt_y);

#ifdef SERF_TRACE_JAM
  u3t_event_trace("serf ipc jam", 'E');
#endif

  u3_newt_send(&out_u, len_d, byt_y);
  u3z(pel);
}

/* _cw_serf_send_slog(): send hint output (hod is [priority tank]).
*/
static void
_cw_serf_send_slog(u3_noun hod)
{
  _cw_serf_send(u3nc(c3__slog, hod));
}

/* _cw_serf_send_stdr(): send stderr output (%flog)
*/
static void
_cw_serf_send_stdr(c3_c* str_c)
{
  _cw_serf_send(u3nc(c3__flog, u3i_string(str_c)));
}


/* _cw_serf_step_trace(): initialize or rotate trace file.
*/
static void
_cw_serf_step_trace(void)
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
}

/* _cw_serf_writ(): process a command from the king.
*/
static c3_o
_cw_serf_writ(void* vod_p, c3_d len_d, c3_y* byt_y)
{
  u3_weak jar;
  u3_noun ret;

  _cw_serf_step_trace();

#ifdef SERF_TRACE_CUE
  u3t_event_trace("serf ipc cue", 'B');
#endif

  jar = u3s_cue_xeno_with(sil_u, len_d, byt_y);

#ifdef SERF_TRACE_CUE
  u3t_event_trace("serf ipc cue", 'E');
#endif

  if (  (u3_none == jar)
     || (c3n == u3_serf_writ(&u3V, jar, &ret)) )
  {
    _cw_serf_fail(0, -1, "bad jar");
  }
  else {
    _cw_serf_send(ret);

    //  all references must now be counted, and all roots recorded
    //
    u3_serf_post(&u3V);
  }

  return c3y;
}

/* _cw_serf_stdio(): fix up std io handles
*/
static void
_cw_serf_stdio(c3_i* inn_i, c3_i* out_i)
{
  //  the serf is spawned with [FD 0] = events and [FD 1] = effects
  //  we dup [FD 0 & 1] so we don't accidently use them for something else
  //  we replace [FD 0] (stdin) with a fd pointing to /dev/null
  //  we replace [FD 1] (stdout) with a dup of [FD 2] (stderr)
  //
  c3_i nul_i = open("/dev/null", O_RDWR, 0);

  *inn_i = dup(0);
  *out_i = dup(1);

  dup2(nul_i, 0);
  dup2(2, 1);

  close(nul_i);
}

/* _cw_serf_stdio(): cleanup on serf exit.
*/
static void
_cw_serf_exit(void)
{
  u3s_cue_xeno_done(sil_u);
  u3t_trace_close();
}

/* _cw_serf_commence(); initialize and run serf
*/
static void
_cw_serf_commence(c3_i argc, c3_c* argv[])
{
  c3_i inn_i, out_i;
  _cw_serf_stdio(&inn_i, &out_i);

  c3_assert( 7 == argc );

  uv_loop_t* lup_u = uv_default_loop();
  c3_c*      dir_c = argv[2];
  c3_c*      key_c = argv[3];
  c3_c*      wag_c = argv[4];
  c3_c*      hap_c = argv[5];
  c3_d       eve_d = 0;

  if ( 1 != sscanf(argv[6], "%" PRIu64 "", &eve_d) ) {
    fprintf(stderr, "serf: rock: invalid number '%s'\r\n", argv[4]);
  }

  memset(&u3V, 0, sizeof(u3V));
  memset(&u3_Host.tra_u, 0, sizeof(u3_Host.tra_u));

  //  load passkey
  //
  //    XX and then ... use passkey
  //
  {
    sscanf(key_c, "%" PRIx64 ":%" PRIx64 ":%" PRIx64 ":%" PRIx64 "",
                  &u3V.key_d[0],
                  &u3V.key_d[1],
                  &u3V.key_d[2],
                  &u3V.key_d[3]);
  }

  //  load runtime config
  //
  {
    sscanf(wag_c, "%" SCNu32, &u3C.wag_w);
    sscanf(hap_c, "%" SCNu32, &u3_Host.ops_u.hap_w);
  }

  //  Ignore SIGPIPE signals.
  //
  {
    struct sigaction sig_s = {{0}};
    sigemptyset(&(sig_s.sa_mask));
    sig_s.sa_handler = SIG_IGN;
    sigaction(SIGPIPE, &sig_s, 0);
  }

  //  configure pipe to daemon process
  //
  {
    c3_i err_i;

    err_i = uv_timer_init(lup_u, &inn_u.tim_u);
    c3_assert(!err_i);
    err_i = uv_pipe_init(lup_u, &inn_u.pyp_u, 0);
    c3_assert(!err_i);
    uv_pipe_open(&inn_u.pyp_u, inn_i);

    err_i = uv_pipe_init(lup_u, &out_u.pyp_u, 0);
    c3_assert(!err_i);
    uv_pipe_open(&out_u.pyp_u, out_i);

    uv_stream_set_blocking((uv_stream_t*)&out_u.pyp_u, 1);
  }

  sil_u = u3s_cue_xeno_init();

  //  set up writing
  //
  out_u.ptr_v = &u3V;
  out_u.bal_f = _cw_serf_fail;

  //  set up reading
  //
  inn_u.ptr_v = &u3V;
  inn_u.pok_f = _cw_serf_writ;
  inn_u.bal_f = _cw_serf_fail;

  //  setup loom
  //
  {
    u3V.dir_c = strdup(dir_c);
    u3V.sen_d = u3V.dun_d = u3m_boot(dir_c);

    if ( eve_d ) {
      //  XX need not be fatal, need a u3m_reboot equivalent
      //  XX can spuriously fail do to corrupt memory-image checkpoint,
      //  need a u3m_half_boot equivalent
      //  workaround is to delete/move the checkpoint in case of corruption
      //
      if ( c3n == u3u_uncram(u3V.dir_c, eve_d) ) {
        fprintf(stderr, "serf (%" PRIu64 "): rock load failed\r\n", eve_d);
        exit(1);
      }
    }
  }

  //  set up logging
  //
  //    XX must be after u3m_boot due to u3l_log
  //
  {
    u3C.stderr_log_f = _cw_serf_send_stdr;
    u3C.slog_f = _cw_serf_send_slog;
  }

  u3V.xit_f = _cw_serf_exit;

#if defined(SERF_TRACE_JAM) || defined(SERF_TRACE_CUE)
  u3t_trace_open(u3V.dir_c);
#endif

  //  start serf
  //
  {
    _cw_serf_send(u3_serf_init(&u3V));
  }

  //  start reading
  //
  u3_newt_read(&inn_u);

  //  enter loop
  //
  uv_run(lup_u, UV_RUN_DEFAULT);
}

/* _cw_info(); print pier info
*/
static void
_cw_info(c3_i argc, c3_c* argv[])
{
  c3_assert( 3 <= argc );

  c3_c* dir_c = argv[2];
  c3_d  eve_d = u3m_boot(dir_c);

  fprintf(stderr, "urbit-worker: %s at event %" PRIu64 "\r\n", dir_c, eve_d);
}

/* _cw_grab(); gc pier.
*/
static void
_cw_grab(c3_i argc, c3_c* argv[])
{
  c3_assert( 3 <= argc );

  c3_c* dir_c = argv[2];
  u3m_boot(dir_c);
  u3_serf_grab();
}

/* _cw_cram(); jam persistent state (rock), and exit.
*/
static void
_cw_cram(c3_i argc, c3_c* argv[])
{
  c3_assert( 3 <= argc );

  c3_c* dir_c = argv[2];
  c3_d  eve_d = u3m_boot(dir_c);
  c3_o  ret_o;

  fprintf(stderr, "urbit-worker: cram: preparing\r\n");

  if ( c3n == (ret_o = u3u_cram(dir_c, eve_d)) ) {
    fprintf(stderr, "urbit-worker: cram: unable to jam state\r\n");
  }
  else {
    fprintf(stderr, "urbit-worker: cram: rock saved at event %" PRIu64 "\r\n", eve_d);
  }

  //  save even on failure, as we just did all the work of deduplication
  //
  u3e_save();

  if ( c3n == ret_o ) {
    exit(1);
  }
}

/* _cw_queu(); cue rock, save, and exit.
*/
static void
_cw_queu(c3_i argc, c3_c* argv[])
{
  c3_assert( 4 <= argc );

  c3_c* dir_c = argv[2];
  c3_c* eve_c = argv[3];
  c3_d  eve_d;

  if ( 1 != sscanf(eve_c, "%" PRIu64 "", &eve_d) ) {
    fprintf(stderr, "urbit-worker: queu: invalid number '%s'\r\n", eve_c);
    exit(1);
  }
  else {
    fprintf(stderr, "urbit-worker: queu: preparing\r\n");

    memset(&u3V, 0, sizeof(u3V));
    u3V.dir_c = strdup(dir_c);
    u3V.sen_d = u3V.dun_d = u3m_boot(dir_c);

    //  XX can spuriously fail do to corrupt memory-image checkpoint,
    //  need a u3m_half_boot equivalent
    //  workaround is to delete/move the checkpoint in case of corruption
    //
    if ( c3n == u3u_uncram(dir_c, eve_d) ) {
      fprintf(stderr, "urbit-worker: queu: failed\r\n");
      exit(1);
    }

    u3e_save();

    fprintf(stderr, "urbit-worker: queu: rock loaded at event %" PRIu64 "\r\n", eve_d);
  }
}

/* _cw_uniq(); deduplicate persistent nouns
*/
static void
_cw_meld(c3_i argc, c3_c* argv[])
{
  c3_assert( 3 <= argc );

  c3_c* dir_c = argv[2];

  u3m_boot(dir_c);

  u3_serf_grab();

  u3u_meld();

  u3_serf_grab();

  u3e_save();
}

/* _cw_pack(); compact memory, save, and exit.
*/
static void
_cw_pack(c3_i argc, c3_c* argv[])
{
  c3_assert( 3 <= argc );

  c3_c* dir_c = argv[2];

  u3m_boot(dir_c);
  u3a_print_memory(stderr, "urbit-worker: pack: gained", u3m_pack());

  u3e_save();
}

// static void
// _urth_epoch_init(c3_d poc_d)
// {
//   //  XX wat do
//   //
//   {
//     c3_c* log_c[2048];
//     snprintf(log_c, 2048, "%s/.urb/log", u3V.dir_c);

//     if (  (0 != mkdir(log_c, 0700))
//        && (EEXISTS != errno) )
//     {
//       fprintf(stderr, "urth: epoch mkdir .urb/log %s\n", strerror(errno));
//       exit(1);
//     }
//   }

//   {
//     c3_c* poc_c[2048];
//     snprintf(poc_c, 2048, "%s/.urb/log/0i%" PRIu64, u3V.dir_c, poc_d);

//     if ( 0 != mkdir(poc_c, 0700) ) {
//       fprintf(stderr, "urth: epoch mkdir %s\n", strerror(errno));
//       exit(1);
//     }
//   }
// }

#define VERE_NAME  "vere"
#define VERE_VERSION  "~.1.2"
#define VERE_ZUSE  420

/* _urth_wyrd_init(): construct %wyrd.
*/
static u3_noun
_urth_wyrd_card(void)
{
  fprintf(stderr, "boot: wyrd card %s %s\n", VERE_NAME, URBIT_VERSION);
  u3_noun ver = u3nt(u3i_string(VERE_NAME),
                     u3i_string(VERE_VERSION),
                     u3_nul);
  fprintf(stderr, "boot: wyrd card ver\n");
  u3_noun kel = u3nl(u3nc(c3__zuse, VERE_ZUSE),  //  XX from both king and serf?
                     u3nc(c3__lull, 330),        //  XX define
                     u3nc(c3__arvo, 240),        //  XX from both king and serf?
                     u3nc(c3__hoon, 140),        //  god_u->hon_y
                     u3nc(c3__nock, 4),          //  god_u->noc_y
                     u3_none);
  fprintf(stderr, "boot: wyrd card kel\n");
  u3_noun wir = u3nc(c3__arvo, u3_nul);
  fprintf(stderr, "boot: wyrd card wir\n");
  return u3nt(c3__wyrd, u3nc(u3i_string("0v1s.vu178"), ver), kel);
}

/* _urth_pill_parse(): extract boot formulas and module/userspace ova from pill
*/
static u3_boot
_urth_pill_parse(u3_noun pil)
{
  fprintf(stderr, "boot: pill parse\n");
  u3_boot bot_u;
  u3_noun pil_p, pil_q;

  c3_assert( c3y == u3du(pil) );
  u3x_cell(pil, &pil_p, &pil_q);

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
      u3_king_bail();
      exit(1);
    }

    if ( c3y == u3r_sing_c("ivory", tag) ) {
      fprintf(stderr, "boot: failed: unable to boot from ivory pill\r\n");
      u3_king_bail();
      exit(1);
    }
    else if ( c3__pill != tag ) {
      if ( c3y == u3a_is_atom(tag) ) {
        u3m_p("pill", tag);
      }
      fprintf(stderr, "boot: failed: unrecognized pill\r\n");
      u3_king_bail();
      exit(1);
    }

    {
      u3_noun typ;
      c3_c* typ_c;

      if ( c3n == u3r_qual(dat, &typ, &bot_u.bot, &bot_u.mod, &bot_u.use) ) {
        fprintf(stderr, "boot: failed: unable to extract pill\r\n");
        u3_king_bail();
        exit(1);
      }

      if ( c3y == u3a_is_atom(typ) ) {
        c3_c* typ_c = u3r_string(typ);
        fprintf(stderr, "boot: parsing %%%s pill\r\n", typ_c);
        c3_free(typ_c);
      }
    }

    u3k(bot_u.bot); u3k(bot_u.mod); u3k(bot_u.use);
    u3z(pro);
  }

  //  optionally replace filesystem in userspace
  //
  if ( u3_nul != pil_q ) {
    c3_w len_w = 0;
    u3_noun ova = bot_u.use;
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

    u3z(bot_u.use);
    bot_u.use = u3kb_flop(new);
  }

  u3z(pil);

  return bot_u;
}

/* _urth_boot_make(): construct boot sequence
*/
static u3_boot
_urth_boot_make(u3_noun who, u3_noun wyr, u3_noun ven, u3_noun pil)
{
    fprintf(stderr, "boot: make\n");
  u3_boot bot_u = _urth_pill_parse(pil); // transfer

  fprintf(stderr, "boot: make prepare\n");

  //  prepend entropy and identity to the module sequence
  //
  {
    u3_noun cad, wir = u3nt(u3_blip, c3__arvo, u3_nul);
    c3_w    eny_w[16];
    c3_rand(eny_w);

    cad = u3nt(c3__verb, u3_nul, ( c3y == u3_Host.ops_u.veb ) ? c3n : c3y);
    bot_u.mod = u3nc(u3nc(u3k(wir), cad), bot_u.mod);

    cad = u3nc(c3__wack, u3i_words(16, eny_w));
    bot_u.mod = u3nc(u3nc(u3k(wir), cad), bot_u.mod);

    cad = u3nc(c3__whom, who);                    // transfer [who]
    bot_u.mod = u3nc(u3nc(u3k(wir), cad), bot_u.mod);

    wir = u3nt(u3_blip, c3__arvo, u3_nul);
    bot_u.mod = u3nc(u3nc(wir, wyr), bot_u.mod);  // transfer [wir] and [wyr]
  }

  //  prepend legacy boot event to the userspace sequence
  //
  {
    //  XX do something about this wire
    //  XX route directly to %jael?
    //
    c3_assert( c3y == u3a_is_cell(ven) );

    u3_noun wir = u3nq(c3__d, c3__term, '1', u3_nul);
    u3_noun cad = u3nt(c3__boot, u3_Host.ops_u.lit, ven); // transfer

    bot_u.use = u3nc(u3nc(wir, cad), bot_u.use);
  }

  return bot_u;
}

/* _cw_bootstrap(): run the Arvo bootstrap sequence
*/
static c3_o
_cw_bootstrap(c3_d fin_d)
{
  u3_noun eve;
  c3_l  mug_l;

  if ( u3_none == (eve = u3_disk_read_list(log_u, 1, fin_d, &mug_l)) ) {
    fprintf(stderr, "boot: read failed\r\n");
    return c3n;
  }
  else {
    fprintf(stderr, "boot: bout to play\r\n");

    //  XX check return, mug
    //
    u3_serf_play(&u3V, 1, eve);
    return c3y;
  }
}

static c3_o
_cw_boot_boot(u3_noun jar)
{
  fprintf(stderr, "boot: boot\n");
  u3_noun tag, dat;

  if ( c3n == u3r_cell(jar, &tag, &dat) ) {
    return c3n;
  }
  else if ( c3__boot != tag ) {
    return c3n;
  }
  else {
    u3_noun pill, vent, lull, verb;

    //  XX put lull in commandline args?

    fprintf(stderr, "boot: boot: qual\n");
    //
    if ( c3n == u3r_qual(dat, &pill, &vent, &lull, &verb) ) {
      fprintf(stderr, "boot: boot: qual no\n");
      return c3n;
    }
    else {
      fprintf(stderr, "boot: boot: qual yes\n");
      c3_o  fak_o;
      u3_noun who;

      if ( c3__fake == u3h(vent) ) {
        fak_o = c3y;
        who   = u3t(vent);
      }
      else {
        c3_assert( c3__dawn == u3h(vent) );
        fak_o = c3n;
        who   = u3h(u3t(vent));
      }

      fprintf(stderr, "boot: boot: bout to wyrd\n");

      u3_noun wyr = _urth_wyrd_card();
      fprintf(stderr, "boot: boot: bout to make\n");
      u3_boot bot_u = _urth_boot_make(who, wyr, vent, pill);

      // _urth_epoch_init(0);
      // _urth_epoch_arvo(u3_noun);
      // _urth_epoch_log()
      // _urth_epoch_done()

      {
        c3_w lif_w = u3qb_lent(bot_u.bot);
        // XX validate
        c3_d who_d[2];
        u3r_chubs(0, 2, who_d, who);

        u3m_p("who", who);
        u3m_p("fak", fak_o);
        u3m_p("lif", lif_w);

        //  XX
        if ( c3n == u3_disk_save_meta(log_u, who_d, fak_o, lif_w) ) {
          exit(1);
        }
      }

      //  insert boot sequence directly
      //
      //    Note that these are not ovum or (pair @da ovum) events,
      //    but raw nock formulas to be directly evaluated as the
      //    subject of the lifecycle formula [%2 [%0 3] %0 2].
      //    All subsequent events will be (pair date ovum).
      //
      {
        u3_noun fol = bot_u.bot;

        while ( u3_nul != fol ) {
          u3_disk_boot_plan(log_u, u3k(u3h(fol)));
          fol = u3t(fol);
        }
      }

      //  insert module and userspace events
      //
      {
        u3_noun ova = bot_u.mod;
        u3_noun bit = u3qc_bex(48);   //  1/2^16 seconds
        u3_noun now;

        {
          struct timeval tim_tv;
          gettimeofday(&tim_tv, 0);
          now = u3_time_in_tv(&tim_tv);
        }


        while ( u3_nul != ova ) {
          u3_disk_boot_plan(log_u, u3nc(u3k(now), u3k(u3h(ova))));
          now = u3ka_add(now, u3k(bit));
          ova = u3t(ova);
        }

        ova = bot_u.use;

        while ( u3_nul != ova ) {
          u3_disk_boot_plan(log_u, u3nc(u3k(now), u3k(u3h(ova))));
          now = u3ka_add(now, u3k(bit));
          ova = u3t(ova);
        }

        u3z(bit); u3z(now);
      }

      u3_disk_boot_save_sync(log_u);

      u3z(bot_u.bot);
      u3z(bot_u.mod);
      u3z(bot_u.use);

      fprintf(stderr, "boot: finished commit\r\n");

      if ( c3n == _cw_bootstrap(log_u->dun_d) ) {
        return c3n;
      }

      fprintf(stderr, "boot: bout to save\r\n");

      u3e_save();

      return c3y;
    }
  }
}

/* _cw_boot_writ(): process boot command
*/
static c3_o
_cw_boot_writ(void* vod_p, c3_d len_d, c3_y* byt_y)
{
  fprintf(stderr, "boot: writ\n");

  u3_noun jar = u3s_cue_xeno_with(sil_u, len_d, byt_y);

  if (  (u3_none == jar)
     || (c3n == _cw_boot_boot(jar)) )
  {
    fprintf(stderr, "boot: fail\n");
    exit(1);
  }
  else {
    fprintf(stderr, "boot: good\n");
    exit(0);
  }

  return c3y;
}

static void
_cw_boot(c3_i argc, c3_c* argv[])
{
  if ( 6 > argc ) {
    fprintf(stderr, "boot: missing args\n");
    exit(1);
  }

  c3_i inn_i, out_i;
  _cw_serf_stdio(&inn_i, &out_i);

  uv_loop_t* lup_u = uv_default_loop();
  c3_c*      dir_c = argv[2];
  c3_c*      key_c = argv[3];
  c3_c*      wag_c = argv[4];
  c3_c*      hap_c = argv[5];

  fprintf(stderr, "boot: %s\n", dir_c);

  memset(&u3V, 0, sizeof(u3V));
  memset(&u3_Host.tra_u, 0, sizeof(u3_Host.tra_u));

  //  load passkey
  //
  //    XX and then ... use passkey
  //
  {
    sscanf(key_c, "%" PRIx64 ":%" PRIx64 ":%" PRIx64 ":%" PRIx64 "",
                  &u3V.key_d[0],
                  &u3V.key_d[1],
                  &u3V.key_d[2],
                  &u3V.key_d[3]);
  }

  //  load runtime config
  //
  {
    sscanf(wag_c, "%" SCNu32, &u3C.wag_w);
    sscanf(hap_c, "%" SCNu32, &u3_Host.ops_u.hap_w);
  }

  //  Ignore SIGPIPE signals.
  //
  {
    struct sigaction sig_s = {{0}};
    sigemptyset(&(sig_s.sa_mask));
    sig_s.sa_handler = SIG_IGN;
    sigaction(SIGPIPE, &sig_s, 0);
  }

  //  configure pipe to daemon process
  //
  {
    c3_i err_i;

    err_i = uv_timer_init(lup_u, &inn_u.tim_u);
    c3_assert(!err_i);
    err_i = uv_pipe_init(lup_u, &inn_u.pyp_u, 0);
    c3_assert(!err_i);
    uv_pipe_open(&inn_u.pyp_u, inn_i);

    err_i = uv_pipe_init(lup_u, &out_u.pyp_u, 0);
    c3_assert(!err_i);
    uv_pipe_open(&out_u.pyp_u, out_i);

    uv_stream_set_blocking((uv_stream_t*)&out_u.pyp_u, 1);
  }

  sil_u = u3s_cue_xeno_init();

  //  set up writing
  //
  out_u.ptr_v = &u3V;
  out_u.bal_f = _cw_serf_fail;

  //  set up reading
  //
  inn_u.ptr_v = &u3V;
  inn_u.pok_f = _cw_boot_writ;
  inn_u.bal_f = _cw_serf_fail;

  //  setup loom
  //
  {
    //  XX create directory, error if exists (non-empty?)
    u3V.dir_c = strdup(dir_c);

    // if ( 0 != mkdir(dir_c, 0700) ) {
    //   fprintf(stderr, "boot: mkdir %s\n", strerror(errno));
    //   exit(1);
    // }

    // {
    //   c3_c* urb_c[2048];
    //   snprintf(urb_c, 2048, "%s/.urb", u3V.dir_c);

    //   if ( 0 != mkdir(urb_c, 0700) ) {
    //     fprintf(stderr, "boot: mkdir .urb %s\n", strerror(errno));
    //     exit(1);
    //   }
    // }


  //  initialize persistence
  //
  {
    //  XX load/set secrets
    //

    if ( !(log_u = u3_disk_init(dir_c)) ) {
      // c3_free(pir_u);
      // return 0;
      fprintf(stderr, "boot: dist init fail\n");
      exit(1);
    }
  }

    u3V.sen_d = u3V.dun_d = u3m_boot(dir_c);
  }

  //  set up logging
  //
  //    XX must be after u3m_boot due to u3l_log
  //
  {
    u3C.stderr_log_f = _cw_serf_send_stdr;
    u3C.slog_f = _cw_serf_send_slog;
  }

  u3V.xit_f = _cw_serf_exit;

// #if defined(SERF_TRACE_JAM) || defined(SERF_TRACE_CUE)
//   u3t_trace_open(u3V.dir_c);
// #endif

  //  send boot status message
  //
  {
    _cw_serf_send(u3nc(c3__boot, u3_nul));
  }

  //  start reading
  //
  u3_newt_read(&inn_u);

  //  enter loop
  //
  uv_run(lup_u, UV_RUN_DEFAULT);
}

static void _cw_replay();
/* _cw_work(): resume and run; replay and start event processing
*/
static void
_cw_work(c3_i argc, c3_c* argv[])
{
  if ( 6 > argc ) {
    fprintf(stderr, "work: missing args\n");
    exit(1);
  }

  c3_i inn_i, out_i;
  _cw_serf_stdio(&inn_i, &out_i);

  uv_loop_t* lup_u = u3_Host.lup_u = uv_default_loop();
  c3_c*      dir_c = argv[2];
  c3_c*      key_c = argv[3];
  c3_c*      wag_c = argv[4];
  c3_c*      hap_c = argv[5];

  fprintf(stderr, "work: %s\r\n", dir_c);

  memset(&u3V, 0, sizeof(u3V));
  memset(&u3_Host.tra_u, 0, sizeof(u3_Host.tra_u));

  //  load passkey
  //
  //    XX and then ... use passkey
  //
  {
    sscanf(key_c, "%" PRIx64 ":%" PRIx64 ":%" PRIx64 ":%" PRIx64 "",
                  &u3V.key_d[0],
                  &u3V.key_d[1],
                  &u3V.key_d[2],
                  &u3V.key_d[3]);
  }

  //  load runtime config
  //
  {
    sscanf(wag_c, "%" SCNu32, &u3C.wag_w);
    sscanf(hap_c, "%" SCNu32, &u3_Host.ops_u.hap_w);
  }

  //  Ignore SIGPIPE signals.
  //
  {
    struct sigaction sig_s = {{0}};
    sigemptyset(&(sig_s.sa_mask));
    sig_s.sa_handler = SIG_IGN;
    sigaction(SIGPIPE, &sig_s, 0);
  }

  //  configure pipe to daemon process
  //
  {
    c3_i err_i;

    err_i = uv_timer_init(lup_u, &inn_u.tim_u);
    c3_assert(!err_i);
    err_i = uv_pipe_init(lup_u, &inn_u.pyp_u, 0);
    c3_assert(!err_i);
    uv_pipe_open(&inn_u.pyp_u, inn_i);

    err_i = uv_pipe_init(lup_u, &out_u.pyp_u, 0);
    c3_assert(!err_i);
    uv_pipe_open(&out_u.pyp_u, out_i);

    uv_stream_set_blocking((uv_stream_t*)&out_u.pyp_u, 1);
  }

  sil_u = u3s_cue_xeno_init();

  //  set up writing
  //
  out_u.ptr_v = &u3V;
  out_u.bal_f = _cw_serf_fail;

  //  set up reading
  //
  // inn_u.ptr_v = &u3V;
  // inn_u.pok_f = _cw_boot_writ;
  inn_u.bal_f = _cw_serf_fail;

  //  setup loom
  //
  {
    u3V.dir_c = strdup(dir_c);

  //  initialize persistence
  //
  {
    //  XX load/set secrets
    //
    if ( !(log_u = u3_disk_init(dir_c)) ) {
      // c3_free(pir_u);
      // return 0;
      fprintf(stderr, "work: dist init fail\n");
      exit(1);
    }
  }

    u3V.sen_d = u3V.dun_d = u3m_boot(dir_c);
  }

  //  set up logging
  //
  //    XX must be after u3m_boot due to u3l_log
  //
  {
    u3C.stderr_log_f = _cw_serf_send_stdr;
    u3C.slog_f = _cw_serf_send_slog;
  }

  u3V.xit_f = _cw_serf_exit;

// #if defined(SERF_TRACE_JAM) || defined(SERF_TRACE_CUE)
//   u3t_trace_open(u3V.dir_c);
// #endif

  //  XX need to send status messages during replay
  //

  {
    c3_d who_d[2];
    c3_o fak_o;
    c3_w lif_w;
    c3_o ret_o = u3_disk_read_meta(log_u, who_d, &fak_o, &lif_w);

    if ( 0 == u3V.dun_d ) {
      if ( c3n == _cw_bootstrap(lif_w) ) {
        exit(1);
      }
    }

    _cw_replay();
    u3e_save();


    {
      u3_mars* mar_u = u3_mars_init(log_u, &inn_u, &out_u, dir_c, sil_u);
      inn_u.ptr_v = mar_u;
      inn_u.pok_f = (u3_moor_poke)u3_mars_kick;
    }

    //  send ready status message
    //
    //    XX version negotiatioon
    {
      u3_noun wyn = u3_nul;
      _cw_serf_send(u3nq(c3__ripe,
                         u3nc(2, wyn),
                         u3nc(u3i_chubs(2, who_d), fak_o),
                         u3nc(u3i_chubs(1, &u3A->eve_d),
                              u3r_mug(u3A->roc))));
    }
  }

  //  start reading
  //
  u3_newt_read(&inn_u);

  //  enter loop
  //
  uv_run(lup_u, UV_RUN_DEFAULT);
}

/* _cw_replay(): replay events on disk
*/
static void
_cw_replay()
{
  u3_lmdb_iter itr_u;

  if ( u3V.dun_d < log_u->dun_d ) {
    c3_o ret_o = u3_lmdb_iter_init(log_u->mdb_u,
                                   &itr_u,
                                   u3V.dun_d + 1,
                                   log_u->dun_d);
    if ( c3n == ret_o ) {
      fprintf(stderr, "urth: replay: db iter init fail\r\n");
      exit(1);
    }
    fprintf(stderr, "replaying from event %" PRIu64 "\r\n", itr_u.eve_d);

    size_t len_i;
    void*  buf_v;
    while ( itr_u.eve_d <= itr_u.fin_d ) {
      ret_o = u3_lmdb_read_one_sync(&itr_u, &len_i, &buf_v);
      if ( c3n == ret_o ) {
        fprintf(stderr, "urth: replay: db event #%" PRIu64 " read fail\r\n",
                        itr_u.eve_d);
        exit(1);
      }
      else {
        u3_noun job;
        c3_l  mug_l;

        if ( c3n == u3_disk_sift(log_u, len_i, (c3_y*)buf_v, &mug_l, &job) ) {
          fprintf(stderr, "replay: bad event\r\n");
          exit(1);
        }

        fprintf(stderr, ".");

        {
          u3_noun res = u3_serf_play(&u3V, itr_u.eve_d - 1, u3nc(job, u3_nul));

          u3_noun mug, tal;
          c3_assert( c3y == u3r_p(res, c3__play, &tal) );

          if ( c3n == u3r_p(tal, c3__done, &mug) ) {
            fprintf(stderr, "replay: event failed\r\n");
            exit(1);
          }
          else if ( mug_l && (mug_l != mug) ) {
            fprintf(stderr, "replay: result mug mismatch expected %x, actual %x\r\n",
                            mug_l, mug);
            exit(1);
          }

          u3z(res);
        }
      }
    }
    u3_lmdb_iter_shut(&itr_u);
    fprintf(stderr, "\r\nreplay: complete\r\n");
  }
}

/* _cw_usage(): print urbit-worker usage.
*/
static void
_cw_usage(c3_i argc, c3_c* argv[])
{
  fprintf(stderr,
          "\rurbit-worker usage:\n"
          "  print pier info:\n"
          "    %s info <pier>\n\n"
          "  gc persistent state:\n"
          "    %s grab <pier>\n\n"
          "  compact persistent state:\n"
          "    %s pack <pier>\n\n"
          "  deduplicate persistent state:\n"
          "    %s meld <pier>\n\n"
          "  jam persistent state:\n"
          "    %s cram <pier>\n\n"
          "  cue persistent state:\n"
          "    %s queu <pier> <at-event>\n\n"
          "  run as a 'serf':\n"
          "    %s serf <pier> <key> <flags> <cache-size> <at-event>\n",
          argv[0], argv[0], argv[0], argv[0], argv[0], argv[0], argv[0]);
}

/* main(): main() when run as urbit-worker
*/
c3_i
main(c3_i argc, c3_c* argv[])
{
  //  urbit-worker commands and positional arguments, by analogy
  //
  //    $@  ~               ;; usage
  //    $%  [%cram dir=@t]
  //        [%queu dir=@t eve=@ud]
  //        [%pack dir=@t]
  //        [%serf dir=@t key=@t wag=@t hap=@ud eve=@ud]
  //    ==
  //
  //    NB: don't print to anything other than stderr;
  //    other streams may have special requirements (in the case of "serf")
  //
  if ( 2 > argc ) {
    _cw_usage(argc, argv);
    exit(1);
  }
  else {
    if ( 0 == strcmp("work", argv[1]) ) {
      _cw_work(argc, argv);
    }
    if ( 0 == strcmp("serf", argv[1]) ) {
      _cw_serf_commence(argc, argv);
    }
    else if ( 0 == strcmp("boot", argv[1]) ) {
      _cw_boot(argc, argv);
    }
    else if ( 0 == strcmp("info", argv[1]) ) {
      _cw_info(argc, argv);
    }
    else if ( 0 == strcmp("grab", argv[1]) ) {
      _cw_grab(argc, argv);
    }
    else if ( 0 == strcmp("cram", argv[1]) ) {
      _cw_cram(argc, argv);
    }
    else if ( 0 == strcmp("queu", argv[1]) ) {
      _cw_queu(argc, argv);
    }
    else if ( 0 == strcmp("meld", argv[1]) ) {
      _cw_meld(argc, argv);
    }
    else if ( 0 == strcmp("pack", argv[1]) ) {
      _cw_pack(argc, argv);
    }
    else {
      fprintf(stderr, "unknown command '%s'\r\n", argv[1]);
      _cw_usage(argc, argv);
      exit(1);
    }
  }

  return 0;
}
