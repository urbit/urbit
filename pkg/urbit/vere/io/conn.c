/* vere/conn.c
**
**  implements the control plane: a socket that can be used to
**  query and interact with an urbit ship from earth.
**
**  the control plane nominally consumes input described by:
**
**      $:  request-id=@                                  ::  id for response
**      $%  [%fyrd fyrd-args=*]                           ::  run a thread
**          [%peek peek-args=*]                           ::  scry
**          [%peel peel-args=*]                           ::  runtime peek
**          [%ovum ovum-args=*]                           ::  inject raw ovum
**          [%urth urth-args=*]                           ::  runtime command
**      ==  ==                                            ::
**
**  request-id is a client-supplied atomic identifier that will
**  be returned along with the response, to allow correlating
**  responses with requests.
**
**  %fyrd is a request to run a thread. its arguments are
**  described in the ++khan section of sys/lull.hoon. to
**  summarize:
**
**      +$  task  $%(... [%fyrd p=(fyrd cast)])           ::
**      +$  bear  $@(desk beak)                           ::  partial $beak
**      +$  cast  (pair mark page)                        ::  output mark +input
**      ++  fyrd  |$  [a]  [=bear name=term args=a]       ::  thread run request
**
**  the cast's mark is applied to the output. the page (untyped
**  cage, or pair of mark and noun) in args contains the input
**  mark. e.g. to run -hi with the ship ~zod as input, receiving
**  output as a tape, with request-id set to 32, send the +jam of
**  this noun over the socket with newt framing:
**
**      [32 %fyrd [%base %hi %tape [%ship ~zod]]]
**
**  responses to %fyrd are either %fail if something went wrong
**  in the driver, or %avow to indicate success or failure from
**  %khan. the body of avow is (each page goof).
**
**  %peek is a namespace read request (aka scry), and will be
**  forwarded directly to arvo. its arguments are the nom of the
**  external peek interface in arvo, at arm 22. (lyc is always
**  `~, i.e. request from self.) that is:
**
**      $+  each  path
**      $%  [%once vis=view syd=desk tyl=spur]
**          [%beam vis=view bem=beam]
**      ==
**
**  %peel is a runtime "peek". it exposes an unprincipled
**  scry-like namespace allowing querying of various metrics
**  about the state of vere. it accepts $path-like arguments,
**  i.e. nul-terminated lists of $knot. it responds with a
**  (unit).
**
**  the %peel path /help produces a list of available commands.
**  /info produces runtime metrics at the moment in time that the
**  request was received.
**
**  the type produced by /info is $mass:
**
**      +$  mass  (pair cord (each * (list mass)))
**
**  where the leaves are conventionally @ud, but the precise
**  interpretation depends on the metric (e.g. loobean settings
**  or integer counters.)
**
**  %ovum is a raw kernel move, to be injected directly into
**  arvo. needless to say this will void your warranty. usually
**  you want to use %fyrd instead. an update will be sent: %done
**  on successful completion, %bail with a stack trace on error,
**  or %drop if the ovum is dropped.
**
**  %urth is a command for the runtime. these are acked with %.y
**  on receipt. no further updates are provided.
**
**  messages use newt framing. because the framing begins with
**  a null byte, any messages that do not contain that byte are
**  reserved for future use.
**
*/
#include <inttypes.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <dirent.h>
#include <uv.h>
#include <errno.h>

#include "all.h"
#include "vere/vere.h"

#ifdef  _WIN32

//  TODO  windows
//
u3_auto*
u3_conn_io_init(u3_pier* pir_u)
{
  return NULL;
}

#else   //  _WIN32

/* u3_cran: control plane request.
*/
  typedef struct _u3_cran {
    u3_atom           rid;              //  client-supplied request id
    struct _u3_chan*  can_u;            //  connection backpointer
    struct _u3_cran*  nex_u;            //  next pointer
  } u3_cran;

/* u3_chan: incoming control plane connection.
*/
  typedef struct _u3_chan {
    struct _u3_moor   mor_u;            //  message handler
    c3_l              coq_l;            //  connection number
    struct _u3_shan*  san_u;            //  server backpointer
    struct _u3_cran*  ran_u;            //  request list
  } u3_chan;

/* u3_shan: control plane server.
*/
  typedef struct _u3_shan {
    uv_pipe_t         pyp_u;            //  server stream handler
    c3_l              nex_l;            //  next connection number
    struct _u3_conn*  con_u;            //  device backpointer
    struct _u3_chan*  can_u;            //  connection list
  } u3_shan;

/* u3_conn: control plane device.
*/
  typedef struct _u3_conn {
    u3_auto           car_u;            //  driver
    c3_l              sev_l;            //  instance number
    struct _u3_shan*  san_u;            //  server reference
    u3_cue_xeno*      sil_u;            //  cue handle
    c3_o              kan_o;            //  %khan present?
  } u3_conn;

static const c3_c URB_SOCK_PATH[] = ".urb/conn.sock";

/* _conn_close_cb(): socket close callback.
*/
static void
_conn_close_cb(uv_handle_t* had_u)
{
  c3_free(had_u);
}

/* _conn_mote_free(): u3_moat-shaped close callback.
*/
static void
_conn_moat_free(void* ptr_v, ssize_t err_i, const c3_c* err_c)
{
  c3_free(ptr_v);
}

/* _conn_send_noun(): jam and send noun over chan.
*/
static void
_conn_send_noun(u3_chan* can_u, u3_noun nun)
{
  c3_y* byt_y;
  c3_d  len_d;

  u3s_jam_xeno(nun, &len_d, &byt_y);
  u3z(nun);
  u3_newt_send((u3_mojo*)&can_u->mor_u, len_d, byt_y);
}

/* _conn_find_chan(): lookup channel by connection number.
*/
static u3_chan*
_conn_find_chan(u3_conn* con_u, c3_l sev_l, c3_l coq_l)
{
  u3_chan* ret_u;

  for ( ret_u = con_u->san_u->can_u;
        ret_u;
        ret_u = (u3_chan*)ret_u->mor_u.nex_u ) {
    if ( coq_l == ret_u->coq_l ) {
      return ret_u;
    }
  }
  return 0;
}

/* _conn_read_wire(): check tag, decompose wire into /sev/coq/rid
*/
static c3_o
_conn_read_wire(u3_noun   wir,
                c3_l      tag_l,
                c3_l*     sev_l,
                c3_l*     coq_l,
                u3_atom*  rid)
{
  u3_noun   i_wir, t_wir;

  if ( (c3n == u3r_cell(wir, &i_wir, &t_wir)) ||
       (tag_l != i_wir) )
  {
    u3z(wir); return c3n;
  }
  else {
    u3_noun pud = t_wir;
    u3_noun p_pud, t_pud, tt_pud, q_pud, r_pud, s_pud,
            uco, p_uco, q_uco;

    if ( (c3n == u3r_cell(pud, &p_pud, &t_pud)) ||
         (c3n == u3v_lily(c3__uv, u3k(p_pud), sev_l)) )
    {
      u3z(wir); return c3n;
    }

    if ( u3_nul == t_pud ) {
      *coq_l = 0; *rid = 0;
    }
    else {
      if ( (c3n == u3r_cell(t_pud, &q_pud, &tt_pud)) ||
           (c3n == u3v_lily(c3__ud, u3k(q_pud), coq_l)) )
      {
        u3z(wir); return c3n;
      }

      if ( u3_nul == tt_pud ) {
        *rid = 0;
      }
      else {
        if ( (c3n == u3r_cell(tt_pud, &r_pud, &s_pud)) ||
             (u3_nul != s_pud) ||
             (c3n == u3ud(r_pud)) )
        {
          u3z(wir); return c3n;
        }

        uco = u3dc("slaw", c3__uv, u3k(r_pud));
        if ( (c3n == u3r_cell(uco, &p_uco, &q_uco)) ||
             (u3_nul != p_uco) )
        {
          u3z(wir); return c3n;
        }
        *rid = u3k(q_uco);
      }
    }
    u3z(wir); return c3y;
  }
}

/* _conn_poke_bail(): error function on failed %fyrd.
*/
static void
_conn_poke_bail(u3_ovum* egg_u, u3_noun lud)
{
  u3_conn*  con_u = (u3_conn*)egg_u->car_u;
  u3_chan*  can_u;
  u3_noun   wir = egg_u->wir;
  c3_l      sev_l, coq_l;
  u3_atom   rid;

  u3_auto_bail_slog(egg_u, u3k(lud));
  if ( (c3n == _conn_read_wire(u3k(wir), c3__khan, &sev_l, &coq_l, &rid)) ||
       (con_u->sev_l != sev_l) )
  {
    //  wtf?
    //
    c3_assert(!"not reached");
    u3z(lud); return;
  }
  can_u = _conn_find_chan(con_u, sev_l, coq_l);
  if ( can_u ) {
    _conn_send_noun(can_u, u3nt(rid, c3__bail, lud));
  }
  else {
    u3z(rid); u3z(lud);
  }
  u3_ovum_free(egg_u);
}

/* _conn_close_chan(): close given channel, freeing.
*/
static void
_conn_close_chan(u3_shan* san_u, u3_chan* can_u)
{
  u3_conn*  con_u = san_u->con_u;
  u3_chan*  inn_u;
  u3_cran*  ran_u;

  //  unset chan on all pending requests.
  //
  for ( ran_u = can_u->ran_u; ran_u; ran_u = ran_u->nex_u ) {
    ran_u->can_u = 0;
  }

  //  remove chan from server's connection list.
  //
  if ( san_u->can_u == can_u ) {
    san_u->can_u = (u3_chan*)can_u->mor_u.nex_u;
  }
  else {
    for ( inn_u = san_u->can_u; inn_u; inn_u = (u3_chan*)inn_u->mor_u.nex_u ) {
      if ( (u3_chan*)inn_u->mor_u.nex_u == can_u ) {
        inn_u->mor_u.nex_u = can_u->mor_u.nex_u;
        break;
      }
    }
  }
  can_u->mor_u.nex_u = NULL;

  //  send a close event to arvo and stop reading.
  //
  if ( c3y == con_u->kan_o ) {
    u3_noun wir, cad;

    wir = u3nq(c3__khan,
               u3dc("scot", c3__uv, con_u->sev_l),
               u3dc("scot", c3__ud, can_u->coq_l),
               u3_nul);
    cad = u3nc(c3__done, u3_nul);
    u3_auto_peer(
      u3_auto_plan(&con_u->car_u,
                   u3_ovum_init(0, c3__k, wir, cad)),
      0, 0, _conn_poke_bail);
  }
  u3_newt_moat_stop((u3_moat*)&can_u->mor_u, _conn_moat_free);
}

/* _conn_moor_bail(): error callback for u3_moor.
*/
static void
_conn_moor_bail(void* ptr_v, ssize_t err_i, const c3_c* err_c)
{
  u3_chan*  can_u = (u3_chan*)ptr_v;
  u3_shan*  san_u = can_u->san_u;

  if ( err_i != UV_EOF ) {
    u3l_log("conn: moor bail %zd %s\n", err_i, err_c);
  }
  _conn_close_chan(san_u, can_u);
}

/* _conn_drop_cran(): finalize/remove request from chan (does not u3z rid.)
*/
static void
_conn_drop_cran(u3_chan* can_u, u3_cran* ran_u)
{
  u3_cran* inn_u;

  //  remove from pending list, special-case for head.
  //
  if ( ran_u == can_u->ran_u ) {
    can_u->ran_u = ran_u->nex_u;
  }
  else {
    for ( inn_u = can_u->ran_u; inn_u; inn_u = inn_u->nex_u ) {
      if ( ran_u == inn_u->nex_u ) {
        inn_u->nex_u = ran_u->nex_u;
        break;
      }
    }
  }
  c3_free(ran_u);
}

/* _conn_peek_cb(): scry result handler.
*/
static void
_conn_peek_cb(void* ptr_v, u3_noun res)
{
  u3_cran* ran_u = (u3_cran*)ptr_v;
  u3_chan* can_u = ran_u->can_u;

  if ( !can_u ) {
    //  chan was closed; noop.
    //
    u3z(ran_u->rid); c3_free(ran_u);
    u3z(res); return;
  }
  _conn_send_noun(can_u, u3nt(ran_u->rid, c3__peek, res));
  _conn_drop_cran(can_u, ran_u);
}

/* _conn_ovum_bail(): bail callback on injected event.
*/
static void
_conn_ovum_bail(u3_ovum* egg_u, u3_noun lud)
{
  u3_cran* ran_u = egg_u->ptr_v;
  u3_chan* can_u = ran_u->can_u;

  u3_auto_bail_slog(egg_u, u3k(lud));
  if ( !can_u ) {
    //  chan was closed; noop.
    //
    u3z(ran_u->rid); c3_free(ran_u);
    u3z(lud); return;
  }
  _conn_send_noun(can_u, u3nt(ran_u->rid, c3__bail, lud));
  _conn_drop_cran(can_u, ran_u);
  u3_ovum_free(egg_u);
}

/* _conn_ovum_news(): lifecycle callback for injected events.
*/
static void
_conn_ovum_news(u3_ovum* egg_u, u3_ovum_news new_e)
{
  u3_cran*  ran_u = egg_u->ptr_v;
  u3_chan*  can_u = ran_u->can_u;

  if ( u3_ovum_done == new_e ||
       u3_ovum_drop == new_e )
  {
    if ( can_u ) {
      _conn_send_noun(can_u,
                      u3nt(ran_u->rid, c3__news,
                           ( u3_ovum_done == new_e
                             ? c3__done
                             : c3__drop )));
      _conn_drop_cran(can_u, ran_u);
    }
    else {
      u3z(ran_u->rid); c3_free(ran_u);
    }
  }
}

/* _conn_read_peel(): response to a %peel request, sans rid.
*/
static u3_noun
_conn_read_peel(u3_conn* con_u, u3_noun dat)
{
  u3_pier*  pir_u = con_u->car_u.pir_u;
  u3_noun   i_dat, t_dat, it_dat, tt_dat;
  u3_noun   res;

  if ( c3n == u3r_cell(dat, &i_dat, &t_dat) ) {
    res = u3_nul;
  }
  else if ( u3_nul == t_dat ) {
    //  zero-argument requests.
    //
    switch (i_dat) {
      default: {
        res = u3_nul;
      } break;
      //  command list.
      //
      case c3__help: {
        res = u3nc(
          u3_nul,
          u3i_list(
            u3nc(c3__help, u3_nul),
            u3nc(c3__info, u3_nul),
            u3nc(c3__khan, u3_nul),
            u3nc(c3__live, u3_nul),
            u3nc(c3__mass, u3_nul),
            u3nc(c3__port, u3nq(c3__ames, c3__htls, c3__http, u3_nul)),
            u3nc(c3__v, u3_nul),
            u3_none));
      } break;
      //  simple health check.
      //
      case c3__live: {
        res = u3nc(u3_nul, pir_u->liv_o);
      } break;
      //  true iff the %khan vane is live (meaning %fyrd is supported.)
      //
      case c3__khan: {
        res = u3nc(u3_nul, con_u->kan_o);
      } break;
      //  |mass output
      //
      case c3__mass: {
        //  TODO  |mass
        //
        res = u3_nul;
      } break;
      //  runtime metrics.
      //
      case c3__info: {
        res = u3nc(u3_nul, u3_pier_info(pir_u));
      } break;
      //  vere version.
      //
      case c3__v: {
        res = u3nc(u3_nul, u3i_string(URBIT_VERSION));
      } break;
    }
  }
  else if ( c3n == u3r_cell(t_dat, &it_dat, &tt_dat) ) {
    //  non-list structure, not understood.
    //
    res = u3_nul;
  }
  else if ( u3_nul == tt_dat ) {
    //  one-argument requests.
    //
    switch (i_dat) {
      default: {
        res = u3_nul;
      } break;
      case c3__port: {
        switch (it_dat) {
          default: {
            res = u3_nul;
          } break;
          case c3__ames: {
            res = u3nc(u3_nul, pir_u->por_s);
          } break;
          case c3__htls: {
            res = u3nc(u3_nul, pir_u->pes_s);
          } break;
          case c3__http: {
            res = u3nc(u3_nul, pir_u->per_s);
          } break;
        }
      } break;
    }
  }
  else {
    //  unknown request.
    //
    res = u3_nul;
  }
  u3z(dat); return res;
}

/* _conn_moor_poke(): called on message read from u3_moor.
*/
static void
_conn_moor_poke(void* ptr_v, c3_d len_d, c3_y* byt_y)
{
  u3_weak   jar;
  u3_noun   can, rid, tag, dat;
  u3_chan*  can_u = (u3_chan*)ptr_v;
  u3_conn*  con_u = can_u->san_u->con_u;
  c3_i      err_i = 0;
  c3_c*     err_c;

  jar = u3s_cue_xeno_with(con_u->sil_u, len_d, byt_y);
  if ( u3_none == jar ) {
    can_u->mor_u.bal_f(can_u, -1, "cue-none");
    return;
  }
  if ( (c3n == u3r_cell(jar, &rid, &can)) ||
       (c3n == u3r_cell(can, &tag, &dat)) ||
       (c3n == u3ud(rid)) )
  {
    err_i = -2; err_c = "jar-bad";
    goto _moor_poke_out;
  }
  else {
    u3_atom rud = u3dc("scot", c3__uv, u3k(rid));
    c3_c*   tag_c = u3r_string(tag);
    c3_c*   rid_c = u3r_string(rud);

    u3l_log("conn: %s %s\n", tag_c, rid_c);
    c3_free(tag_c);
    c3_free(rid_c);
    u3z(rud);
    switch (tag) {
      default: {
        err_i = -3; err_c = "tag-unknown";
        goto _moor_poke_out;
      } break;

      case c3__fyrd: {
        if ( c3n == con_u->kan_o ) {
          _conn_send_noun(can_u,
                          u3nt(u3k(rid), c3__fail, u3i_string("khan-miss")));
        }
        else {
          u3_noun wir = u3nc(c3__khan,
                             u3nq(u3dc("scot", c3__uv, con_u->sev_l),
                                  u3dc("scot", c3__ud, can_u->coq_l),
                                  u3dc("scot", c3__uv, u3k(rid)),
                                  u3_nul));

          u3_auto_peer(
            u3_auto_plan(&con_u->car_u,
                         u3_ovum_init(0, c3__k, wir, u3k(can))),
            0, 0, _conn_poke_bail);
        }
      } break;

      case c3__peek: {
        u3_cran*  ran_u = c3_calloc(sizeof(u3_cran));
        u3_noun   gan = u3nc(u3_nul, u3_nul);   //  `~: read from self

        ran_u->rid = u3k(rid);
        ran_u->can_u = can_u;
        ran_u->nex_u = can_u->ran_u;
        can_u->ran_u = ran_u;
        u3_pier_peek(con_u->car_u.pir_u, gan, u3k(dat), ran_u, _conn_peek_cb);
      } break;

      case c3__peel: {
        _conn_send_noun(
          can_u, u3nc(u3k(rid),
                      _conn_read_peel(con_u, u3k(dat))));
      } break;

      case c3__ovum: {
        u3_noun tar, wir, cad;

        if ( (c3n == u3r_trel(dat, &tar, &wir, &cad)) ) {
          err_i = -6; err_c = "ovum-bad";
          goto _moor_poke_out;
        }
        else {
          u3_cran* ran_u = c3_calloc(sizeof(u3_cran));

          ran_u->rid = u3k(rid);
          ran_u->can_u = can_u;
          ran_u->nex_u = can_u->ran_u;
          can_u->ran_u = ran_u;
          u3_auto_peer(
            u3_auto_plan(&con_u->car_u,
                         u3_ovum_init(0, u3k(tar), u3k(wir), u3k(cad))),
            ran_u, _conn_ovum_news, _conn_ovum_bail);
        }
      } break;

      case c3__urth: {
        switch (dat) {
          default: {
            err_i = -7; err_c = "urth-bad";
            goto _moor_poke_out;
          } break;
          case c3__meld: {
            //  ack immediately.
            //
            _conn_send_noun(can_u, u3nc(u3k(rid), c3y));
            u3_pier_meld(con_u->car_u.pir_u);
          } break;
          case c3__pack: {
            _conn_send_noun(can_u, u3nc(u3k(rid), c3y));
            u3_pier_pack(con_u->car_u.pir_u);
          } break;
          //  TODO  more %urth commands
          //  TODO  send updates, success/failure?
          //
        }
      } break;
    }
  }
_moor_poke_out:
  u3z(jar);
  if ( 0 != err_i ) {
    can_u->mor_u.bal_f(can_u, err_i, err_c);
  }
}

/* _conn_sock_cb(): socket connection callback.
*/
static void
_conn_sock_cb(uv_stream_t* sem_u, c3_i tas_i)
{
  u3_shan*  san_u = (u3_shan*)sem_u;
  u3_conn*  con_u = san_u->con_u;
  u3_chan*  can_u;
  c3_i      err_i;

  can_u = c3_calloc(sizeof(u3_chan));
  can_u->mor_u.ptr_v = can_u;
  can_u->mor_u.pok_f = _conn_moor_poke;
  can_u->mor_u.bal_f = _conn_moor_bail;
  can_u->coq_l = san_u->nex_l++;
  can_u->san_u = san_u;
  err_i = uv_timer_init(u3L, &can_u->mor_u.tim_u);
  c3_assert(!err_i);
  err_i = uv_pipe_init(u3L, &can_u->mor_u.pyp_u, 0);
  c3_assert(!err_i);
  err_i = uv_accept(sem_u, (uv_stream_t*)&can_u->mor_u.pyp_u);
  c3_assert(!err_i);
  u3_newt_read((u3_moat*)&can_u->mor_u);
  can_u->mor_u.nex_u = (u3_moor*)san_u->can_u;
  san_u->can_u = can_u;
}

/* _conn_init_sock(): initialize socket device.
*/
static void
_conn_init_sock(u3_shan* san_u)
{
  //  the full socket path is limited to about 108 characters,
  //  and we want it to be relative to the pier. save our current
  //  path, chdir to the pier, open the socket at the desired
  //  path, then chdir back. hopefully there aren't any threads.
  //
  c3_c pax_c[2048];
  c3_i err_i;

  if ( NULL == getcwd(pax_c, sizeof(pax_c)) ) {
    u3l_log("conn: getcwd: %s\n", uv_strerror(errno));
    u3_king_bail();
  }
  if ( 0 != chdir(u3_Host.dir_c) ) {
    u3l_log("conn: chdir: %s\n", uv_strerror(errno));
    u3_king_bail();
  }
  if ( 0 != unlink(URB_SOCK_PATH) && errno != ENOENT ) {
    u3l_log("conn: unlink: %s\n", uv_strerror(errno));
    goto _conn_sock_err_chdir;
  }
  if ( 0 != (err_i = uv_pipe_init(u3L, &san_u->pyp_u, 0)) ) {
    u3l_log("conn: uv_pipe_init: %s\n", uv_strerror(err_i));
    goto _conn_sock_err_chdir;
  }
  if ( 0 != (err_i = uv_pipe_bind(&san_u->pyp_u, URB_SOCK_PATH)) ) {
    u3l_log("conn: uv_pipe_bind: %s\n", uv_strerror(err_i));
    goto _conn_sock_err_chdir;
  }
  if ( 0 != (err_i = uv_listen((uv_stream_t*)&san_u->pyp_u, 0,
                               _conn_sock_cb)) ) {
    u3l_log("conn: uv_listen: %s\n", uv_strerror(err_i));
    goto _conn_sock_err_unlink;
  }
  if ( 0 != chdir(pax_c) ) {
    u3l_log("conn: chdir: %s\n", uv_strerror(errno));
    goto _conn_sock_err_close;
  }
  return;

_conn_sock_err_close:
  uv_close((uv_handle_t*)&san_u->pyp_u, _conn_close_cb);
_conn_sock_err_unlink:
  if ( 0 != unlink(URB_SOCK_PATH) ) {
    u3l_log("conn: unlink: %s\n", uv_strerror(errno));
  }
_conn_sock_err_chdir:
  if ( 0 != chdir(pax_c) ) {
    u3l_log("conn: chdir: %s\n", uv_strerror(errno));
  }
  u3_king_bail();
}

/* _conn_born_news(): initialization complete; %khan available.
*/
static void
_conn_born_news(u3_ovum* egg_u, u3_ovum_news new_e)
{
  u3_conn* con_u = (u3_conn*)egg_u->car_u;

  if ( u3_ovum_done == new_e ) {
    con_u->kan_o = c3y;
  }
}

/* _conn_born_bail(): nonessential failure; log it and keep going.
*/
static void
_conn_born_bail(u3_ovum* egg_u, u3_noun lud)
{
  u3l_log("conn: %%born failure; %%fyrd not supported\n");
  u3z(lud);
  u3_ovum_free(egg_u);
}

/* _conn_io_talk(): open socket and notify %khan that we're live.
*/
static void
_conn_io_talk(u3_auto* car_u)
{
  u3_conn* con_u = (u3_conn*)car_u;
  u3_shan* san_u;
  u3_noun  wir = u3nt(c3__khan,
                      u3dc("scot", c3__uv, con_u->sev_l),
                      u3_nul);
  u3_noun  cad = u3nc(c3__born, u3_nul);

  u3_auto_peer(
    u3_auto_plan(car_u, u3_ovum_init(0, c3__k, wir, cad)),
    0,
    _conn_born_news,
    _conn_born_bail);

  //  initialize server, opening socket.
  //
  c3_assert(!con_u->san_u);
  san_u = c3_calloc(sizeof(*san_u));
  san_u->nex_l = 1;
  san_u->con_u = con_u;
  con_u->san_u = san_u;
  _conn_init_sock(san_u);
  car_u->liv_o = c3y;
  u3l_log("conn: live on %s/%s\n", u3_Host.dir_c, URB_SOCK_PATH);
}

/* _conn_ef_handle(): handle result.
*/
static void
_conn_ef_handle(u3_conn*  con_u,
                c3_l      sev_l,
                c3_l      coq_l,
                u3_atom   rid,
                u3_noun   tag,
                u3_noun   dat)
{
  u3_chan* can_u;

  if ( 0 != (can_u = _conn_find_chan(con_u, sev_l, coq_l)) ) {
    if ( c3__avow == tag ) {
      _conn_send_noun(can_u, u3nt(u3k(rid), c3__avow, u3k(dat)));
    }
    else {
      can_u->mor_u.bal_f(can_u, -4, "handle-unknown");
      u3_king_bail();
    }
  }
  else {
    u3l_log("conn: handle-no-coq %" PRIx32 " %" PRIu32 "\n",
            sev_l, coq_l);
  }
  u3z(rid); u3z(tag); u3z(dat);
}

/* _conn_io_kick(): apply effects.
*/
static c3_o
_conn_io_kick(u3_auto* car_u, u3_noun wir, u3_noun cad)
{
  u3_conn*  con_u = (u3_conn*)car_u;
  u3_noun   tag, dat;
  c3_l      sev_l, coq_l;
  u3_weak   rid = u3_none;

  if ( (c3n == _conn_read_wire(wir, c3__khan, &sev_l, &coq_l, &rid)) ||
       (c3n == u3r_cell(cad, &tag, &dat)) ||
       (con_u->sev_l != sev_l) )
  {
    u3z(rid); u3z(cad); return c3n;
  }

  _conn_ef_handle(con_u, sev_l, coq_l, rid, u3k(tag), u3k(dat));
  u3z(cad); return c3y;
}

/* _conn_io_exit(): unlink socket, shut down connections.
*/
static void
_conn_io_exit(u3_auto* car_u)
{
  u3_conn*          con_u = (u3_conn*)car_u;
  c3_c*             pax_c = u3_Host.dir_c;
  c3_w              len_w = strlen(pax_c) + 1 + sizeof(URB_SOCK_PATH);
  c3_c*             paf_c = c3_malloc(len_w);
  c3_i              wit_i;

  wit_i = snprintf(paf_c, len_w, "%s/%s", pax_c, URB_SOCK_PATH);
  c3_assert(wit_i > 0);
  c3_assert(len_w == (c3_w)wit_i + 1);

  if ( 0 != unlink(paf_c) ) {
    if ( ENOENT != errno ) {
      u3l_log("conn: failed to unlink socket: %s\n", uv_strerror(errno));
    }
  }
  else {
    u3l_log("conn: unlinked %s\n", paf_c);
  }
  c3_free(paf_c);

  {
    u3_shan*        san_u = con_u->san_u;

    if ( san_u ) {
      while ( san_u->can_u ) {
        _conn_close_chan(san_u, san_u->can_u);
      }
      uv_close((uv_handle_t*)&san_u->pyp_u, _conn_close_cb);
    }
  }

  u3s_cue_xeno_done(con_u->sil_u);
  c3_free(con_u);
}

/* u3_conn(): initialize control plane socket.
*/
u3_auto*
u3_conn_io_init(u3_pier* pir_u)
{
  u3_conn*          con_u = c3_calloc(sizeof(*con_u));
  u3_auto*          car_u = &con_u->car_u;

  con_u->sil_u = u3s_cue_xeno_init();
  con_u->kan_o = c3n;
  car_u->nam_m = c3__conn;
  car_u->liv_o = c3n;
  car_u->io.talk_f = _conn_io_talk;
  car_u->io.kick_f = _conn_io_kick;
  car_u->io.exit_f = _conn_io_exit;

  {
    u3_noun         now;
    struct timeval  tim_u;

    gettimeofday(&tim_u, 0);
    now = u3_time_in_tv(&tim_u);
    con_u->sev_l = u3r_mug(now);
    u3z(now);
  }

  return car_u;
}

#endif  //  _WIN32
