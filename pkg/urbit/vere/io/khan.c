/* vere/khan.c
**
*/
#include <inttypes.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <dirent.h>
#include <uv.h>
#include <errno.h>

#include "all.h"
#include "vere/vere.h"

/* u3_chan: incoming control plane connection. used as u3_shan->mor_u's ptr_v.
*/
  typedef struct _u3_chan {
    struct _u3_moor   mor_u;            //  message handler
    c3_w              coq_l;            //  connection number
    struct _u3_shan*  san_u;            //  server backpointer
  } u3_chan;

/* u3_shan: control plane server.
*/
  typedef struct _u3_shan {
    uv_pipe_t         pyp_u;            //  server stream handler
    struct _u3_chan*  can_u;            //  connection list
    struct _u3_khan*  kan_u;            //  device backpointer
  } u3_shan;

/* u3_khan: control plane device.
*/
  typedef struct _u3_khan {
    u3_auto           car_u;            //  driver
    c3_l              sev_l;            //  instance number
    struct _u3_shan*  san_u;            //  server reference
  } u3_khan;

static const c3_c URB_SOCK_PATH[] = ".urb/khan.sock";

/* _khan_close_cb(): socket close callback.
*/
static void
_khan_close_cb(uv_handle_t* had_u)
{
  c3_free(had_u);
}

/* _khan_read_cb(): socket read callback.
*/
static void
_khan_read_cb(uv_stream_t* cli_u, ssize_t red_i, const uv_buf_t* buf_u)
{
  u3l_log("khan: read %zd\n", red_i);
  // TODO interact
  //
  // There are four cases to think about here:
  //
  // 1. peek, runtime
  // 2. poke, runtime
  // 3. peek, arvo
  // 4. poke, arvo
  //
  // For 1, the driver itself parses and responds to a few basic text-mode
  // commands:
  // `ok`:   driver responds with "ok\n" (basic health test)
  // `dump`: dumps runtime stat counters in simple machine/human-readable format
  //
  // There is not yet a use case for 2.
  //
  // For 3 and 4, we speak a binary protocol using jammed nouns, as (to be)
  // documented in pkg/arvo/sys/vane/khan.hoon.
  //
  // The transition to arvo is signalled by a 0x80 (i.e. byte with MSB high) at
  // the start of a line (i.e. as first character, or immediately following a
  // '\n'.) Everything after that is simply forwarded to arvo, and responses
  // forwarded back, for the rest of the duration of the connection.
  //
  // Some alternate protocol designs:
  //
  // a. Forward everything to arvo.
  // b. Add a command that sends a single-shot jammed noun, which receives a
  //    single-shot response.
  // c. Listen on two or more different sockets.
  //
  // (a) is undesirable since we don't want to sync the runtime stats with
  // arvo. Continuous sync would add unnecessary load, and on-demand sync would
  // add unnecessary implementation complexity in the form of extra round-trips
  // between arvo and the runtime. (b) is probably fine, but I'm not smart
  // enough to know how to tell from the runtime when a jammed noun has
  // finished sending without base64-encoding it or something. (c) is tedious;
  // the same effect can be achieved by just opening two connections to the
  // socket, keeping one in text mode, and sending a 0x80 over the other.
}

static void
_khan_moor_poke(void* ptr_v, c3_d len_d, c3_y* byt_y)
{
  u3l_log("khan: poke called %p %" PRIu64 " %s\n", ptr_v, len_d, byt_y);
}

static void
_khan_moor_bail(void* ptr_v, ssize_t err_i, const c3_c* err_c)
{
  u3l_log("khan: bail called %p %zd %s\n", ptr_v, err_i, err_c);
}

/* _khan_conn_cb(): socket connection callback.
*/
static void
_khan_conn_cb(uv_stream_t* sem_u, c3_i tas_i)
{
  u3_shan*  san_u = (u3_shan*)sem_u;
  u3_khan*  kan_u = san_u->kan_u;
  u3_chan*  can_u;
  c3_i      err_i;

  can_u = c3_calloc(sizeof(u3_chan));
  can_u->mor_u.ptr_v = can_u;
  can_u->mor_u.pok_f = _khan_moor_poke;
  can_u->mor_u.bal_f = _khan_moor_bail;
  // XX maybe want mug(now) or something
  can_u->coq_l = ( san_u->can_u ) ? 1 + san_u->can_u->coq_l : 0;
  can_u->san_u = san_u;
  err_i = uv_timer_init(u3L, &can_u->mor_u.tim_u);
  c3_assert(!err_i);
  err_i = uv_pipe_init(u3L, &can_u->mor_u.pyp_u, 0);
  c3_assert(!err_i);
  err_i = uv_accept(sem_u, (uv_stream_t*)&can_u->mor_u.pyp_u);
  c3_assert(!err_i);
  u3_newt_read_sync((u3_moat*)&can_u->mor_u);
  can_u->mor_u.nex_u = (u3_moor*)san_u->can_u;
  san_u->can_u = can_u;
}

/* _khan_sock_init(): initialize socket device.
*/
static void
_khan_sock_init(u3_shan* san_u)
{
  // The full socket path is limited to about 108 characters, and we want it to
  // be relative to the pier. So we save our current path, chdir to the pier,
  // open the socket at the desired path, then chdir back. Hopefully there
  // aren't any threads.
  c3_c pax_c[2048];
  c3_i err_i;

  if ( NULL == getcwd(pax_c, sizeof(pax_c)) ) {
    u3l_log("khan: getcwd: %s\n", uv_strerror(errno));
    u3_king_bail();
  }
  if ( 0 != chdir(u3_Host.dir_c) ) {
    u3l_log("khan: chdir: %s\n", uv_strerror(errno));
    u3_king_bail();
  }
  if ( 0 != unlink(URB_SOCK_PATH) && errno != ENOENT ) {
    u3l_log("khan: unlink: %s\n", uv_strerror(errno));
    goto _khan_sock_err_chdir;
  }
  if ( 0 != (err_i = uv_pipe_init(u3L, &san_u->pyp_u, 0)) ) {
    u3l_log("khan: uv_pipe_init: %s\n", uv_strerror(err_i));
    goto _khan_sock_err_chdir;
  }
  if ( 0 != (err_i = uv_pipe_bind(&san_u->pyp_u, URB_SOCK_PATH)) ) {
    u3l_log("khan: uv_pipe_bind: %s\n", uv_strerror(err_i));
    goto _khan_sock_err_chdir;
  }
  if ( 0 != (err_i = uv_listen((uv_stream_t*)&san_u->pyp_u, 0,
                               _khan_conn_cb)) ) {
    u3l_log("khan: uv_listen: %s\n", uv_strerror(err_i));
    goto _khan_sock_err_unlink;
  }
  if ( 0 != chdir(pax_c) ) {
    u3l_log("khan: chdir: %s\n", uv_strerror(errno));
    goto _khan_sock_err_close;
  }
  return;

_khan_sock_err_close:
  uv_close((uv_handle_t*)&san_u->pyp_u, _khan_close_cb);
_khan_sock_err_unlink:
  if ( 0 != unlink(URB_SOCK_PATH) ) {
    u3l_log("khan: unlink: %s\n", uv_strerror(errno));
  }
_khan_sock_err_chdir:
  if ( 0 != chdir(pax_c) ) {
    u3l_log("khan: chdir: %s\n", uv_strerror(errno));
  }
  u3_king_bail();
}

/* _khan_born_news(): initialization complete, open socket.
*/
static void
_khan_born_news(u3_ovum* egg_u, u3_ovum_news new_e)
{
  u3_auto* car_u = egg_u->car_u;
  u3_khan* kan_u = (u3_khan*)car_u;
  u3_shan* san_u;

  if ( u3_ovum_done == new_e ) {
    c3_assert(!kan_u->san_u);
    san_u = c3_calloc(sizeof(*san_u));
    _khan_sock_init(san_u);
    san_u->kan_u = kan_u;
    kan_u->san_u = san_u;
    car_u->liv_o = c3y;
    u3l_log("khan: live on %s/%s\n", u3_Host.dir_c, URB_SOCK_PATH);
  }
}

/* _khan_born_bail(): nonessential failure; log it and keep going.
*/
static void
_khan_born_bail(u3_ovum* egg_u, u3_noun lud)
{
  u3l_log("khan: %%born failure; socket not opened\n");
}

/* _khan_io_talk(): notify %khan that we're live
*/
static void
_khan_io_talk(u3_auto* car_u)
{
  u3_khan* kan_u = (u3_khan*)car_u;

  u3_noun wir = u3nt(c3__khan,
                     u3dc("scot", c3__uv, kan_u->sev_l),
                     u3_nul);
  u3_noun cad = u3nc(c3__born, u3_nul);

  u3_auto_peer(
    u3_auto_plan(car_u, u3_ovum_init(0, c3__k, wir, cad)),
    0,
    _khan_born_news,
    _khan_born_bail);
}

/* _khan_io_kick(): apply effects.
*/
static c3_o
_khan_io_kick(u3_auto* car_u, u3_noun wir, u3_noun cad)
{
  u3_khan* kan_u = (u3_khan*)car_u;

  u3_noun tag, dat, i_wir;
  c3_o ret_o;

  if (  (c3n == u3r_cell(wir, &i_wir, 0))
     || (c3n == u3r_cell(cad, &tag, &dat))
     || (c3__khan != i_wir) )
  {
    ret_o = c3n;
  }
  else {
    ret_o = c3y;
    // TODO do something
  }

  u3z(wir); u3z(cad);
  return ret_o;
}

static void
_khan_moat_free(void* ptr_v, ssize_t err_i, const c3_c* err_c)
{
  c3_free(ptr_v);
}

/* _khan_io_exit(): unlink socket, shut down connections.
*/
static void
_khan_io_exit(u3_auto* car_u)
{
  u3_khan*          kan_u = (u3_khan*)car_u;

  {
    c3_c*           pax_c = u3_Host.dir_c;
    c3_w            len_w = strlen(pax_c) + 1 + sizeof(URB_SOCK_PATH);
    c3_c*           paf_c = c3_malloc(len_w);
    c3_i            wit_i;

    wit_i = snprintf(paf_c, len_w, "%s/%s", pax_c, URB_SOCK_PATH);
    c3_assert(wit_i > 0);
    c3_assert(len_w == (c3_w)wit_i + 1);

    if ( 0 != unlink(paf_c) ) {
      u3l_log("khan: failed to unlink socket: %s\n", uv_strerror(errno));
    }
    c3_free(paf_c);
  }

  {
    u3_shan*        san_u = kan_u->san_u;
    u3_chan*        can_u = san_u->can_u;
    u3_chan*        nex_u;

    while ( can_u ) {
      nex_u = (u3_chan*)can_u->mor_u.nex_u;
      u3_newt_moat_stop((u3_moat*)&can_u->mor_u, _khan_moat_free);
      can_u = nex_u;
    }
    uv_close((uv_handle_t*)&san_u->pyp_u, _khan_close_cb);
  }

  c3_free(kan_u);
}

/* u3_khan(): initialize control plane socket.
*/
u3_auto*
u3_khan_io_init(u3_pier* pir_u)
{
  u3_khan* kan_u = c3_calloc(sizeof(*kan_u));
  u3_auto* car_u = &kan_u->car_u;

  car_u->nam_m = c3__khan;
  car_u->liv_o = c3n;
  car_u->io.talk_f = _khan_io_talk;
  car_u->io.kick_f = _khan_io_kick;
  car_u->io.exit_f = _khan_io_exit;

  {
    u3_noun         now;
    struct timeval  tim_u;

    gettimeofday(&tim_u, 0);
    now = u3_time_in_tv(&tim_u);
    kan_u->sev_l = u3r_mug(now);
    u3z(now);
  }

  return car_u;
}
