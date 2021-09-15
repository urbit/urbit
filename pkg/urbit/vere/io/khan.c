/* vere/khan.c
**
*/
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <dirent.h>
#include <uv.h>
#include <errno.h>

#include "all.h"
#include "vere/vere.h"

/* u3_chan: incoming control plane connection.
*/
  typedef struct _u3_chan {
    uv_pipe_t         pyp_u;            //  client stream handler
    c3_w              coq_l;            //  connection number
    struct _u3_khan*  cop_u;            //  control plane backlink
    struct _u3_chan*  nex_u;            //  next in list
  } u3_chan;

/* u3_khan: control plane socket
*/
  typedef struct _u3_khan {
    u3_auto           car_u;            //  driver
    uv_pipe_t         pyp_u;            //  socket
    c3_l              sev_l;            //  instance number
    struct _u3_chan*  can_u;            //  client list
  } u3_khan;

static const c3_c URB_SOCK_PATH[] = ".urb/khan.sock";

/* _khan_alloc(): libuv-style allocator.
*/
static void
_khan_alloc(uv_handle_t* had_u,
            size_t len_i,
            uv_buf_t* buf_u)
{
  void* ptr_v = c3_malloc(len_i);

  *buf_u = uv_buf_init(ptr_v, len_i);
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

/* _khan_conn_cb(): socket connection callback.
*/
static void
_khan_conn_cb(uv_stream_t* sem_u, c3_i tas_i)
{
  u3_khan*  cop_u = (u3_khan*)sem_u;
  u3_chan*  can_u;
  c3_i      err_i;

  can_u = c3_malloc(sizeof(u3_chan));
  can_u->coq_l = ( cop_u->can_u ) ? 1 + cop_u->can_u->coq_l : 0;
  can_u->cop_u = cop_u;
  can_u->nex_u = cop_u->can_u;
  if ( 0 != (err_i = uv_pipe_init(u3L, &can_u->pyp_u, 0)) ) {
    u3l_log("khan: client init failed: %s\n", uv_strerror(err_i));
    c3_free(can_u);
    u3_king_bail();
  }
  if ( 0 != (err_i = uv_accept(sem_u, (uv_stream_t*)&can_u->pyp_u)) ) {
    u3l_log("khan: accept failed: %s\n", uv_strerror(err_i));
    c3_free(can_u);
    u3_king_bail();
  }

  if ( 0 != (err_i = uv_read_start((uv_stream_t*)&can_u->pyp_u, _khan_alloc,
                                   _khan_read_cb)) ) {
    u3l_log("khan: read_start failed: %s\n", uv_strerror(err_i));
    // TODO close handle
    c3_free(can_u);
    u3_king_bail();
  }

  cop_u->can_u = can_u;
}

/* _khan_close_cb(): socket close callback.
*/
static void
_khan_close_cb(uv_handle_t* had_u)
{
  // TODO remove
  u3l_log("khan: socket closed\n");
}

/* _khan_born_news(): initialization complete, open socket.
*/
static void
_khan_born_news(u3_ovum* egg_u, u3_ovum_news new_e)
{
  u3_auto* car_u = egg_u->car_u;
  u3_khan* cop_u = (u3_khan*)car_u;

  if ( u3_ovum_done == new_e ) {
    car_u->liv_o = c3y;

    // Open socket. The full socket path is limited to about 108 characters, and
    // we want it to be relative to the pier. So we save our current path, chdir
    // to the pier, open the socket at the desired path, then chdir back.
    // Hopefully there aren't any threads.
    {
      c3_c pax_c[2048];
      c3_i err_i;

      if ( NULL == getcwd(pax_c, sizeof(pax_c)) ) {
        u3l_log("khan: getcwd: %s\n", uv_strerror(errno));
        u3_king_bail();
      }
      else {
        if ( 0 != chdir(u3_Host.dir_c) ) {
          u3l_log("khan: chdir: %s\n", uv_strerror(errno));
          u3_king_bail();
        }
        else {
          if ( 0 != unlink(URB_SOCK_PATH) && errno != ENOENT ) {
            u3l_log("khan: unlink: %s\n", uv_strerror(errno));
            goto _khan_err_chdir;
          }
          if ( 0 != (err_i = uv_pipe_init(u3L, &cop_u->pyp_u, 0)) ) {
            u3l_log("khan: uv_pipe_init: %s\n", uv_strerror(err_i));
            goto _khan_err_chdir;
          }
          if ( 0 != (err_i = uv_pipe_bind(&cop_u->pyp_u, URB_SOCK_PATH)) ) {
            u3l_log("khan: uv_pipe_bind: %s\n", uv_strerror(err_i));
            goto _khan_err_chdir;
          }
          if ( 0 != (err_i = uv_listen((uv_stream_t*)&cop_u->pyp_u, 0,
                                       _khan_conn_cb)) ) {
            u3l_log("khan: uv_listen: %s\n", uv_strerror(err_i));
            goto _khan_err_unlink;
          }
          if ( 0 != chdir(pax_c) ) {
            u3l_log("khan: chdir: %s\n", uv_strerror(errno));
            goto _khan_err_close;
          }
        }
      }
      return;
_khan_err_close:
      uv_close((uv_handle_t*)&cop_u->pyp_u, _khan_close_cb);
_khan_err_unlink:
      if ( 0 != unlink(URB_SOCK_PATH) ) {
        u3l_log("khan: unlink: %s\n", uv_strerror(errno));
      }
_khan_err_chdir:
      if ( 0 != chdir(pax_c) ) {
        u3l_log("khan: chdir: %s\n", uv_strerror(errno));
      }

      u3_king_bail();
    }
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
  u3_khan* cop_u = (u3_khan*)car_u;

  u3_noun wir = u3nt(c3__khan,
                     u3dc("scot", c3__uv, cop_u->sev_l),
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
  u3_khan* cop_u = (u3_khan*)car_u;

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

/* _khan_io_exit(): unlink socket.
*/
static void
_khan_io_exit(u3_auto* car_u)
{
  u3_khan* cop_u = (u3_khan*)car_u;
  c3_c*    pax_c = u3_Host.dir_c;
  c3_w     len_w = strlen(pax_c) + 1 + sizeof(URB_SOCK_PATH);
  c3_c*    paf_c = c3_malloc(len_w);
  c3_i     wit_i;

  wit_i = snprintf(paf_c, len_w, "%s/%s", pax_c, URB_SOCK_PATH);
  c3_assert(wit_i > 0);
  c3_assert(len_w == (c3_w)wit_i + 1);

  unlink(paf_c);
  c3_free(paf_c);
}

/* u3_khan(): initialize control plane socket.
*/
u3_auto*
u3_khan_io_init(u3_pier* pir_u)
{
  u3_khan* cop_u = c3_calloc(sizeof(*cop_u));

  u3_auto* car_u = &cop_u->car_u;
  car_u->nam_m = c3__khan;
  car_u->io.talk_f = _khan_io_talk;
  car_u->io.kick_f = _khan_io_kick;
  car_u->io.exit_f = _khan_io_exit;

  {
    u3_noun now;
    struct timeval tim_u;
    gettimeofday(&tim_u, 0);

    now = u3_time_in_tv(&tim_u);
    cop_u->sev_l = u3r_mug(now);
    u3z(now);
  }

  return car_u;
}
