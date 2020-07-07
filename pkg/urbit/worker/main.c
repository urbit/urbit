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

static u3_serf   u3V;             //  one serf per process
static u3_moat inn_u;             //  input stream
static u3_mojo out_u;             //  output stream

/* _newt_fail(): failure stub.
*/
static void
_newt_fail(void* vod_p, const c3_c* wut_c)
{
  fprintf(stderr, "serf: fail: %s\r\n", wut_c);
  exit(1);
}

/* _newt_send(): send plea back to daemon.
*/
static void
_newt_send(u3_noun pel)
{
  u3_newt_write(&out_u, u3ke_jam(pel));
}

/* _newt_send_slog(): send hint output (hod is [priority tank]).
*/
static void
_newt_send_slog(u3_noun hod)
{
  _newt_send(u3nc(c3__slog, hod));
}

/* _newt_send_stdr(): send stderr output
*/
static void
_newt_send_stdr(c3_c* str_c)
{
  _newt_send_slog(u3nc(0, u3i_string(str_c)));
}

/* _newt_writ():
*/
static void
_newt_writ(void* vod_p, u3_noun mat)
{
  u3_noun ret;

  if ( c3n == u3_serf_writ(&u3V, u3ke_cue(mat), &ret) ) {
    _newt_fail(0, "bad jar");
  }
  else {
    _newt_send(ret);

    //  all references must now be counted, and all roots recorded
    //
    u3_serf_post(&u3V);
  }
}

/* main(): main() when run as urbit-worker
*/
c3_i
main(c3_i argc, c3_c* argv[])
{
  //  the serf is spawned with [FD 0] = events and [FD 1] = effects
  //  we dup [FD 0 & 1] so we don't accidently use them for something else
  //  we replace [FD 0] (stdin) with a fd pointing to /dev/null
  //  we replace [FD 1] (stdout) with a dup of [FD 2] (stderr)
  //
  c3_i nul_i = open("/dev/null", O_RDWR, 0);
  c3_i inn_i = dup(0);
  c3_i out_i = dup(1);
  dup2(nul_i, 0);
  dup2(2, 1);
  close(nul_i);

  c3_assert( 6 == argc );

  uv_loop_t* lup_u = uv_default_loop();
  c3_c*      dir_c = argv[1];
  c3_c*      key_c = argv[2];
  c3_c*      wag_c = argv[3];
  c3_c*      hap_c = argv[4];
  c3_d       eve_d = 0;

  if ( 1 != sscanf(argv[5], "%" PRIu64 "", &eve_d) ) {
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

  //  set up writing
  //
  out_u.ptr_v = &u3V;
  out_u.bal_f = _newt_fail;

  //  set up reading
  //
  inn_u.ptr_v = &u3V;
  inn_u.pok_f = _newt_writ;
  inn_u.bal_f = _newt_fail;

  //  setup loom
  //
  {
    u3V.dir_c = strdup(dir_c);
    u3V.sen_d = u3V.dun_d = u3m_boot(dir_c);

    if ( eve_d ) {
      u3_serf_unpack(&u3V, eve_d);
    }
  }

  //  set up logging
  //
  //    XX must be after u3m_boot due to u3l_log
  //
  {
    u3C.stderr_log_f = _newt_send_stdr;
    u3C.slog_f = _newt_send_slog;
  }

  if (u3_Host.ops_u.hap_w == 1337) {
    u3a_compact();
    u3e_save();
    return 0;
  }

  //  start serf
  //
  {
    _newt_send(u3_serf_init(&u3V));
  }

  //  start reading
  //
  u3_newt_read_sync(&inn_u);

  //  enter loop
  //
  uv_run(lup_u, UV_RUN_DEFAULT);

  return 0;
}
