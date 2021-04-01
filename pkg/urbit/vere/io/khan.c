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

/* u3_khan: control plane socket
*/
  typedef struct _u3_khan {
    u3_auto           car_u;            //  driver
    uv_pipe_t         pyp_u;            //  socket
    c3_l              sev_l;            //  number (of instance)
  } u3_khan;

static const c3_c URB_SOCK_PATH[] = ".urb/khan.sock";

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

  u3_auto_plan(car_u, u3_ovum_init(0, c3__k, wir, cad));
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

/* _khan_conn_cb(): socket connection callback.
*/
static void
_khan_conn_cb(uv_stream_t* sem_u, c3_i tas_i)
{
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

  // Open socket. The full socket path is limited to about 108 characters, and
  // we want it to be relative to the pier. So we save our current path, chdir
  // to the pier, open the socket at the desired path, then chdir back.
  // Hopefully there aren't any threads.
  {
    c3_c pax_c[2048];

    // XX needs better error handling
    if ( NULL == getcwd(pax_c, sizeof(pax_c)) ) {
      c3_assert(!"khan-getcwd");
    }
    else {
      if ( 0 != chdir(u3_Host.dir_c) ) {
        c3_assert(!"khan-chdir");
      }
      else {
        // TODO handle errors
        unlink(URB_SOCK_PATH);
        uv_pipe_init(u3L, &cop_u->pyp_u, 0);
        uv_pipe_bind(&cop_u->pyp_u, URB_SOCK_PATH);
        uv_listen((uv_stream_t*)&cop_u->pyp_u, 0, _khan_conn_cb);

        if ( 0 != chdir(pax_c) ) {
          c3_assert(!"khan-back");
        }
      }
    }
  }

  return car_u;
}
