/* worker/main.c
**
**  mars process entrypoints; utility grab-bag.
*/
#include "all.h"
#include "rsignal.h"
#include "vere/vere.h"
#include "vere/lock.h"
#include "vere/mars.h"

static u3_moat      inn_u;  //  input stream
static u3_mojo      out_u;  //  output stream

/* _cw_io_fail(): failure stub.
*/
static void
_cw_io_fail(void* ptr_v, ssize_t err_i, const c3_c* err_c)
{
  if ( UV_EOF == err_i ) {
    fprintf(stderr, "mars: urth unexpectedly shut down\r\n");
  }
  else {
    fprintf(stderr, "mars: ipc error: %s\r\n", err_c);
  }

  exit(1);
}

/* _cw_io_send(): send plea back to daemon.
*/
static void
_cw_io_send(u3_noun pel)
{
  c3_d  len_d;
  c3_y* byt_y;

  u3s_jam_xeno(pel, &len_d, &byt_y);
  u3_newt_send(&out_u, len_d, byt_y);

  u3z(pel);
}

/* _cw_io_send_slog(): send hint output (hod is [priority tank]).
*/
static void
_cw_io_send_slog(u3_noun hod)
{
  _cw_io_send(u3nc(c3__slog, hod));
}

/* _cw_io_send_stdr(): send stderr output (%flog)
*/
static void
_cw_io_send_stdr(c3_c* str_c)
{
  _cw_io_send(u3nc(c3__flog, u3i_string(str_c)));
}

/* _cw_init_io(): initialize i/o streams.
*/
static void
_cw_init_io(uv_loop_t* lup_u)
{
  //  mars is spawned with [FD 0] = events and [FD 1] = effects
  //  we dup [FD 0 & 1] so we don't accidently use them for something else
  //  we replace [FD 0] (stdin) with a fd pointing to /dev/null
  //  we replace [FD 1] (stdout) with a dup of [FD 2] (stderr)
  //
  c3_i nul_i = c3_open(c3_dev_null, O_RDWR, 0);
  c3_i inn_i = dup(0);
  c3_i out_i = dup(1);

  dup2(nul_i, 0);
  dup2(2, 1);

  close(nul_i);

  //  set stream I/O to unbuffered because it's now a pipe not a console
  //
  setvbuf(stdout, NULL, _IONBF, 0);
  setvbuf(stderr, NULL, _IONBF, 0);

  //  Ignore SIGPIPE signals.
  //
#ifndef U3_OS_mingw
  {
    struct sigaction sig_s = {{0}};
    sigemptyset(&(sig_s.sa_mask));
    sig_s.sa_handler = SIG_IGN;
    sigaction(SIGPIPE, &sig_s, 0);
  }
#endif

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
}

#ifdef U3_OS_mingw
/* _cw_intr_win_cb(): invoked when urth signals ctrl-c.
*/
static void
_cw_intr_win_cb(PVOID param, BOOLEAN timedOut)
{
  rsignal_raise(SIGINT);
}

/* _cw_intr_win(): initialize ctrl-c handling.
*/
static void
_cw_intr_win(c3_c* han_c)
{
  HANDLE h;
  if ( 1 != sscanf(han_c, "%u", &h) ) {
    fprintf(stderr, "mars: ctrl-c event: bad handle %s: %s\r\n",
            han_c, strerror(errno));
  }
  else {
    if ( !RegisterWaitForSingleObject(&h, h, _cw_intr_win_cb,
                                      NULL, INFINITE, 0) )
    {
      fprintf(stderr,
        "mars: ctrl-c event: RegisterWaitForSingleObject(%u) failed (%d)\r\n",
        h, GetLastError());
    }
  }
}
#endif

//! Get a handle to the event log.
//!
//! @param[in] dir_c  Pier directory.
static inline u3_evlo*
_cw_evlo_open(const c3_c* const dir_c)
{
  if ( !dir_c ) {
    return NULL;
  }
  c3_path* pax_u = c3_path_fv(1, dir_c);
  u3_lock_acquire(pax_u);
  c3_path_push(pax_u, ".urb");
  c3_path_push(pax_u, "log");

  u3_meta met_u;
  u3_evlo* log_u = u3_evlo_open(pax_u, &met_u);
  
  c3_path_free(pax_u);
  return log_u;
}

//! Close handle to event log.
//!
//! @param[in] dir_c  Pier directory.
//! @param[in] log_u  Event log handle.
static inline void
_cw_evlo_close(const c3_c* const dir_c, u3_evlo* const log_u)
{
  if ( !dir_c || !log_u ) {
    return;
  }
  u3_evlo_close(log_u);
  c3_free(log_u);
  c3_path* pax_u = c3_path_fv(1, dir_c);
  u3_lock_release(pax_u);
  c3_path_free(pax_u);
}

//! Print pier info.
static void
_cw_info(c3_i argc, c3_c* argv[])
{
  c3_assert( 1 <= argc );

  c3_c*    dir_c = argv[0];
  c3_d     eve_d = u3m_boot(dir_c, u3e_live);
  u3_evlo* log_u = _cw_evlo_open(dir_c);

  fprintf(stderr,
          "urbit-worker: %s at event %" PRIu64 "\r\n",
          dir_c,
          eve_d);

  u3_evlo_info(log_u);

  _cw_evlo_close(dir_c, log_u);

  u3m_stop();
}

/* _cw_grab(); gc pier.
*/
static void
_cw_grab(c3_i argc, c3_c* argv[])
{
  c3_assert( 1 <= argc );

  c3_c* dir_c = argv[0];
  u3m_boot(dir_c, u3e_live);
  u3C.wag_w |= u3o_hashless;
  u3_mars_grab();
  u3m_stop();
}

//! Jam persistent state (rock) and exit.
//!
//! @n (1) XX s/b try_aquire lock.
static void
_cw_cram(c3_i argc, c3_c* argv[])
{
  c3_assert( 1 <= argc );

  c3_o     ret_o = c3n;
  c3_c*    dir_c = argv[0];
  c3_d     eve_d = u3m_boot(dir_c, u3e_live);
  u3_evlo* log_u = _cw_evlo_open(dir_c);

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
  _cw_evlo_close(dir_c, log_u);

  if ( c3n == ret_o ) {
    exit(1);
  }

  u3m_stop();
}

/* _cw_queu(); cue rock, save, and exit.
*/
static void
_cw_queu(c3_i argc, c3_c* argv[])
{
  c3_assert( 2 <= argc );

  c3_c*    dir_c = argv[0];
  c3_c*    eve_c = argv[1];
  u3_evlo* log_u = _cw_evlo_open(dir_c);
  c3_d     eve_d;

  if ( 1 != sscanf(eve_c, "%" PRIu64 "", &eve_d) ) {
    fprintf(stderr, "urbit-worker: queu: invalid number '%s'\r\n", eve_c);
    exit(1);
  }
  else {
    fprintf(stderr, "urbit-worker: queu: preparing\r\n");

    u3m_boot(dir_c, u3e_live);

    //  XX can spuriously fail due to corrupt memory-image checkpoint,
    //  need a u3m_half_boot equivalent
    //  workaround is to delete/move the checkpoint in case of corruption
    //
    if ( c3n == u3u_uncram(dir_c, eve_d) ) {
      fprintf(stderr, "urbit-worker: queu: failed\r\n");
      exit(1);
    }

    u3e_save();
    _cw_evlo_close(dir_c, log_u);

    fprintf(stderr, "urbit-worker: queu: rock loaded at event %" PRIu64 "\r\n", eve_d);
    u3m_stop();
  }
}

/* _cw_uniq(); deduplicate persistent nouns
*/
static void
_cw_meld(c3_i argc, c3_c* argv[])
{
  c3_assert( 1 <= argc );

  c3_c*    dir_c = argv[0];
  u3_evlo* log_u = _cw_evlo_open(dir_c);
  c3_w     pre_w;

  u3C.wag_w |= u3o_hashless;
  u3m_boot(dir_c, u3e_live);

  pre_w = u3a_open(u3R);

  u3u_meld();

  u3a_print_memory(stderr, "urbit-worker: meld: gained", (u3a_open(u3R) - pre_w));

  u3e_save();
  _cw_evlo_close(dir_c, log_u);
}

/* _cw_pack(); compact memory, save, and exit.
*/
static void
_cw_pack(c3_i argc, c3_c* argv[])
{
  c3_assert( 1 <= argc );

  c3_c*    dir_c = argv[0];
  u3_evlo* log_u = _cw_evlo_open(dir_c);

  u3m_boot(dir_c, u3e_live);
  u3a_print_memory(stderr, "urbit-worker: pack: gained", u3m_pack());

  u3e_save();
  _cw_evlo_close(dir_c, log_u);
  u3m_stop();
}

/* _cw_boot_writ(): process boot command
*/
static c3_o
_cw_boot_writ(void* vod_p, c3_d len_d, c3_y* byt_y)
{
  u3_weak jar = u3s_cue_xeno(len_d, byt_y);

  u3_noun com;

  if (  (u3_none == jar)
     || (c3n == u3r_p(jar, c3__boot, &com)) )
  {
    fprintf(stderr, "boot: parse fail\r\n");
    exit(1);
  }
  else {
    u3k(com);
    u3z(jar);

    //  XX get [dir_c] from elsewhere
    //
    if ( c3n == u3_mars_boot(u3P.dir_c, com) ) {
      fprintf(stderr, "boot: fail\r\n");
      exit(1);
    }
  }

  exit(0);

  return c3y;
}

/* _cw_boot(): initialize, await boot msg.
*/
static void
_cw_boot(c3_i argc, c3_c* argv[])
{
  if ( 4 > argc ) {
    fprintf(stderr, "boot: missing args\r\n");
    exit(1);
  }

  uv_loop_t* lup_u = u3_Host.lup_u = uv_default_loop();
  c3_c*      dir_c = argv[0];
  c3_c*      key_c = argv[1]; // XX use passkey
  c3_c*      wag_c = argv[2];
  c3_c*      hap_c = argv[3];

  //  XX windows ctrl-c?

  _cw_init_io(lup_u);

  fprintf(stderr, "boot: %s\r\n", dir_c);

  //  load runtime config
  //
  {
    memset(&u3_Host.tra_u, 0, sizeof(u3_Host.tra_u));
    sscanf(wag_c, "%" SCNu32, &u3C.wag_w);
    sscanf(hap_c, "%" SCNu32, &u3_Host.ops_u.hap_w);
  }

  //  set up stdio read/write callbacks
  //
  inn_u.ptr_v = 0;
  inn_u.pok_f = _cw_boot_writ;
  inn_u.bal_f = _cw_io_fail;
  out_u.ptr_v = 0;
  out_u.bal_f = _cw_io_fail;

  //  setup loom
  //
  //    XX s/b explicitly initialization, not maybe-restore
  //
  u3m_boot(dir_c, u3e_live);

  //  set up logging
  //
  //    XX must be after u3m_boot due to u3l_log
  //
  {
    u3C.stderr_log_f = _cw_io_send_stdr;
    u3C.slog_f = _cw_io_send_slog;
  }

  //  start reading
  //
  u3_newt_read(&inn_u);

  //  enter loop
  //
  uv_run(lup_u, UV_RUN_DEFAULT);
  u3m_stop();
}

/* _cw_work(): resume and run; replay and start event processing
*/
static void
_cw_work(c3_i argc, c3_c* argv[])
{
#ifdef U3_OS_mingw
  if ( 6 > argc ) {
#else
  if ( 5 > argc ) {
#endif
    fprintf(stderr, "work: missing args\n");
    exit(1);
  }

  c3_d       eve_d = 0;
  uv_loop_t* lup_u = u3_Host.lup_u = uv_default_loop();
  c3_c*      dir_c = argv[0];
  c3_c*      key_c = argv[1]; // XX use passkey
  c3_c*      wag_c = argv[2];
  c3_c*      hap_c = argv[3];
  c3_c*      eve_c = argv[4];
#ifdef U3_OS_mingw
  c3_c*      han_c = argv[5];
  _cw_intr_win(han_c);
#endif

  _cw_init_io(lup_u);

  fprintf(stderr, "work: %s\r\n", dir_c);

  //  load runtime config
  //
  {
    memset(&u3_Host.tra_u, 0, sizeof(u3_Host.tra_u));
    sscanf(wag_c, "%" SCNu32, &u3C.wag_w);
    sscanf(hap_c, "%" SCNu32, &u3_Host.ops_u.hap_w);

    if ( 1 != sscanf(eve_c, "%" PRIu64 "", &eve_d) ) {
      fprintf(stderr, "mars: replay-to invalid: '%s'\r\n", eve_c);
    }
  }

  //  setup loom XX strdup?
  //
  u3m_boot(dir_c, u3e_live);

  //  set up logging
  //
  //    XX must be after u3m_boot due to u3l_log
  //
  {
    u3C.stderr_log_f = _cw_io_send_stdr;
    u3C.slog_f = _cw_io_send_slog;
  }

  //  setup mars
  //
  {
    //  XX set exit cb
    //
    u3_mars* mar_u = u3_mars_init(dir_c, &inn_u, &out_u, eve_d);

    if ( !mar_u ) {
      fprintf(stderr, "mars: init failed\r\n");
      //  XX cleanup, exit codes
      //
      exit(1);
    }

    //  set up stdio read/write callbacks
    //
    inn_u.ptr_v = mar_u;
    inn_u.pok_f = (u3_moor_poke)u3_mars_kick;
    inn_u.bal_f = _cw_io_fail; // XX cleanup
    out_u.ptr_v = mar_u;
    out_u.bal_f = _cw_io_fail; // XX cleanup
  }

  //  start reading
  //
  u3_newt_read(&inn_u);

  //  enter loop
  //
  uv_run(lup_u, UV_RUN_DEFAULT);
  u3m_stop();
}

/* _cw_usage(): print urbit-worker usage.
*/
static void
_cw_usage(c3_c* s)
{
  fprintf(stderr,
    "\rusage:\n"
    "  %s cram <pier>               jam state:\n"
    "  %s grab <pier>               measure memory usage:\n"
    "  %s info <pier>               print pier info:\n"
    "  %s meld <pier>               deduplicate snapshot:\n"
    "  %s pack <pier>               defragment snapshot:\n"
    "  %s queu <pier> <at-event>    cue state:\n"
    "\nmars, ipc:\n"
    "  boot a pier:\n"
    "    %s boot <pier> <key> <flags> <cache-size>\n"
    "  run a pier:\n"
    "    %s work <pier> <key> <flags> <cache-size> <replay-to>"
#ifdef U3_OS_mingw
    " <ctrlc-handle>"
#endif
    "\n",
    s, s, s, s, s, s, s, s);
}

/* main(): main() when run as urbit-worker
*/
c3_i
main(c3_i argc, c3_c* argv[])
{
  //  urbit-worker commands and positional arguments, by analogy
  //
  //    $@  ~                                             ::  usage
  //    $%  [%cram dir=@t]                                ::  jam state
  //        [%grab dir=@t]                                ::  gc
  //        [%info dir=@t]                                ::  print
  //        [%meld dir=@t]                                ::  deduplicate
  //        [%pack dir=@t]                                ::  defragment
  //        [%queu dir=@t eve=@ud]                        ::  cue state
  //    ::                                                ::    ipc:
  //        [%boot dir=@t key=@t wag=@t hap=@ud]          ::  boot
  //        [%work dir=@t key=@t wag=@t hap=@ud eve=@ud]  ::  run
  //    ==
  //
  //    NB: don't print to anything other than stderr;
  //    other streams may be used for ipc.
  //
  if ( (2 < argc) && 4 == strlen(argv[1]) ) {
    c3_m mot_m;
    {
      c3_c* s = argv[1]; mot_m = c3_s4(s[0], s[1], s[2], s[3]);
    }

    argc -= 2;
    argv += 2;

    switch ( mot_m ) {
      case c3__cram: _cw_cram(argc, argv); break;
      case c3__grab: _cw_grab(argc, argv); break;
      case c3__info: _cw_info(argc, argv); break;
      case c3__meld: _cw_meld(argc, argv); break;
      case c3__pack: _cw_pack(argc, argv); break;
      case c3__queu: _cw_queu(argc, argv); break;

      case c3__boot: _cw_boot(argc, argv); break;
      case c3__work: _cw_work(argc, argv); break;
    }

    return 0;
  }

  if ( 1 < argc) {
    fprintf(stderr, "unknown command '%s'\r\n", argv[1]);
  }

  _cw_usage(argv[0]);
  return 1;
}
