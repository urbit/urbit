/* v/raft.c
**
** This file is in the public domain.
*/
#include <capn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <uv.h>

#include "all.h"
#include "p/raft.capnp.h"
#include "v/vere.h"


static void _raft_conn_dead(u2_rcon* ron_u);
static void _raft_remove_run(u2_rcon* ron_u);

/* _raft_readname(): parse a raft host:port peer name.
*/
static u2_bean
_raft_readname(u2_ropt* rop_u, const c3_c* str_c, c3_w siz_w)
{
  u2_rnam* nam_u = malloc(sizeof(*nam_u));
  c3_c*    col_c;
  c3_w     nam_w;

  nam_u->ron_u = 0;
  nam_u->str_c = malloc(siz_w + 1);
  strncpy(nam_u->str_c, str_c, siz_w);
  nam_u->str_c[siz_w] = '\0';
  //fprintf(stderr, "raft: peer %s\n", nam_u->str_c);

  if ( 0 == (col_c = strchr(nam_u->str_c, ':')) ) {
    fprintf(stderr, "raft: invalid name %s\n", str_c);
    return u2_no;
  }
  else {
    nam_w = col_c - nam_u->str_c + 1;
    nam_u->nam_c = malloc(nam_w + 1);
    uv_strlcpy(nam_u->nam_c, nam_u->str_c, nam_w);

    nam_u->por_c = strdup(col_c + 1);
    if ( strlen(nam_u->por_c) > 5 ) {
      fprintf(stderr, "raft: invalid port %s\n", nam_u->por_c);
      return u2_no;
    }

    nam_u->nex_u = rop_u->nam_u;
    rop_u->nam_u = nam_u;
    return u2_yes;
  }
}

/* u2_raft_readopt(): parse a string into a list of raft peers.
*/
u2_bean
u2_raft_readopt(u2_ropt* rop_u, const c3_c* arg_c)
{
  c3_c* com_c;

  while ( 0 != (com_c = strchr(arg_c, ',')) ) {
    if ( u2_no == _raft_readname(rop_u, arg_c, com_c - arg_c) ) {
      return u2_no;
    } else arg_c = com_c + 1;
  }
  return _raft_readname(rop_u, arg_c, strlen(arg_c));
}

/* _raft_alloc(): libuv-style allocator for raft.
*/
static uv_buf_t
_raft_alloc(uv_handle_t* had_u, size_t siz_i)
{
  uv_buf_t buf_u = { .base = malloc(siz_i), .len = siz_i };
  return buf_u;
}

/* _raft_election_rand(): pseudorandom component of election timeout.
*/
static c3_w
_raft_election_rand()
{
  return ((float) rand() / RAND_MAX) * 150;
}

/* _raft_promote(): actions on raft leader election.
*/
static void
_raft_promote(u2_raft* raf_u)
{
  raf_u->typ_e = u2_raty_lead;

  u2_sist_boot();
  if ( u2_no == u2_Host.ops_u.bat ) {
    u2_lo_lead(u2A);
  }
}

/* _raft_do_rest(): act on an incoming raft RPC request.
*/
static void
_raft_do_rest(u2_rcon* ron_u, struct Raft_Rest res_u)
{
  uL(fprintf(uH, "raft: rest{.tem=%d,.cid=%s,.lai=%lld,.lat=%d,.which=%d}\n",
                 res_u.tem, res_u.cid.str, res_u.lai, res_u.lat, res_u.which));
  if ( 0 == ron_u->nam_u ) {
    u2_raft* raf_u = ron_u->raf_u;
    u2_rnam* nam_u = raf_u->nam_u;

    while ( nam_u ) {
      if ( 0 == strcmp(nam_u->str_c, res_u.cid.str) ) {
        if ( nam_u->ron_u ) {
          _raft_conn_dead(nam_u->ron_u);
        }
        nam_u->ron_u = ron_u;
        ron_u->nam_u = nam_u;
        _raft_remove_run(ron_u);
        break;
      }
      else nam_u = nam_u->nex_u;
    }
  }

  if ( 0 == ron_u->nam_u ) {
    uL(fprintf(uH, "connection from unkown peer %s\n", res_u.cid.str));
    _raft_conn_dead(ron_u);
  }

  /* TODO */
}

/* _raft_do_rasp(): act on an incoming raft RPC response.
*/
static void
_raft_do_rasp(u2_rcon* ron_u, struct Raft_Rasp ras_u)
{
  if ( 0 == ron_u->nam_u ) {
    uL(fprintf(uH, "invalid connection from unknown host\n"));
    _raft_conn_dead(ron_u);
  }
  else {
    /* TODO */
  }
}

/* _raft_conn_work(): read and write requests and responses.
*/
static void
_raft_conn_work(u2_rcon* ron_u)
{
  if ( ron_u->red_t ) {
    capn_ptr rot_p = capn_root(ron_u->cap_u);

    ron_u->red_t = 0;
    if ( CAPN_NULL == rot_p.type ) {
      uL(fprintf(uH, "raft: null root\n"));
      return;
    }
    else {
      Raft_Rmsg_ptr    mes_p = {capn_getp(rot_p, 0, 1)};
      struct Raft_Rmsg mes_u;

      if ( CAPN_STRUCT != mes_p.p.type ) {
        uL(fprintf(uH, "raft: expected struct, got %d\n", mes_p.p.type));
        _raft_conn_dead(ron_u);
      }
      else {
        read_Raft_Rmsg(&mes_u, mes_p);

        if ( Raft_Rmsg_rest == mes_u.which ) {
          struct Raft_Rest res_u;

          read_Raft_Rest(&res_u, mes_u.rest);
          _raft_do_rest(ron_u, res_u);
        }
        else {
          struct Raft_Rasp ras_u;

          c3_assert(Raft_Rmsg_rasp == mes_u.which);
          read_Raft_Rasp(&ras_u, mes_u.rasp);
          _raft_do_rasp(ron_u, ras_u);
        }
      }
    }
  }

}

/* _raft_conn_read_cb(): generic connection read callback.
*/
static void
_raft_conn_read_cb(uv_stream_t* tcp_u,
                   ssize_t      siz_i,
                   uv_buf_t     buf_u)
{
  u2_rcon* ron_u = (u2_rcon*)tcp_u;

  u2_lo_open();
  {
    if ( siz_i < 0 ) {
      uv_err_t las_u = uv_last_error(u2L);

      if ( UV_EOF != las_u.code ) {
        uL(fprintf(uH, "raft: read: %s\n", uv_strerror(las_u)));
      }
      _raft_conn_dead(ron_u);
    }
    else {
      struct capn_segment* seg_u = calloc(1, sizeof(struct capn_segment));
      seg_u->data = buf_u.base;
      seg_u->len = buf_u.len;
      seg_u->cap = buf_u.len;
      seg_u->user = ron_u;

      capn_append_segment(ron_u->cap_u, seg_u);
      ron_u->red_t = 1;
      _raft_conn_work(ron_u);
    }
  }
  u2_lo_shut(u2_no);
}

/* _raft_conn_new(): allocate a new raft connection.
*/
static void
_raft_conn_new(u2_raft* raf_u)
{
  u2_rcon* ron_u = malloc(sizeof(*ron_u));

  uv_tcp_init(u2L, &ron_u->wax_u);

  if ( 0 != uv_accept((uv_stream_t*)&raf_u->wax_u,
                      (uv_stream_t*)&ron_u->wax_u) )
  {
    uL(fprintf(uH, "raft: accept: %s\n",
                   uv_strerror(uv_last_error(u2L))));

    uv_close((uv_handle_t*)&ron_u->wax_u, 0);
    free(ron_u);
  }
  else {
    uv_read_start((uv_stream_t*)&ron_u->wax_u,
                  _raft_alloc,
                  _raft_conn_read_cb);

    ron_u->cap_u = 0;
    ron_u->red_t = 0;

    ron_u->nam_u = 0;
    ron_u->raf_u = raf_u;
    ron_u->nex_u = raf_u->run_u;
    raf_u->run_u = ron_u;
  }
}

/* _raft_remove_run(): remove a connection from the list of unknowns.
*/
static void
_raft_remove_run(u2_rcon* ron_u)
{
  u2_raft* raf_u = ron_u->raf_u;

  if ( raf_u->run_u == ron_u ) {
    raf_u->run_u = ron_u->nex_u;
  }
  else {
    u2_rcon* pre_u = raf_u->run_u;

    while ( pre_u ) {
      if ( pre_u->nex_u == ron_u ) {
        pre_u->nex_u = ron_u->nex_u;
        break;
      }
      else pre_u = pre_u->nex_u;
    }
  }
}

/* _raft_conn_free(): unlink a connection and free its resources.
*/
static void
_raft_conn_free(uv_handle_t* had_u)
{
  u2_rcon* ron_u = (void*)had_u;

  if ( ron_u->nam_u ) {
    ron_u->nam_u->ron_u = 0;
  }
  else {
    _raft_remove_run(ron_u);
  }

  if ( ron_u->cap_u ) {
    capn_free(ron_u->cap_u);
    free(ron_u->cap_u);
  }
  free(ron_u);
}

/* _raft_conn_dead(): kill a connection.
*/
static void
_raft_conn_dead(u2_rcon* ron_u)
{
  uv_read_stop((uv_stream_t*)&ron_u->wax_u);
  uv_close((uv_handle_t*)&ron_u->wax_u, _raft_conn_free);
}

/* _raft_listen_cb(): generic listen callback.
*/
static void
_raft_listen_cb(uv_stream_t* str_u, c3_i sas_i)
{
  u2_raft* raf_u = (u2_raft*)str_u;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "raft: listen_cb: %s\n",
                   uv_strerror(uv_last_error(u2L))));
  }
  else {
    _raft_conn_new(raf_u);
  }
}

/* _raft_connect_cb(): generic connection callback.
*/
static void
_raft_connect_cb(uv_connect_t* con_u, c3_i sas_i)
{
  u2_rcon* ron_u = con_u->data;
  free(con_u);

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "raft: connect_cb: %s\n",
                   uv_strerror(uv_last_error(u2L))));
    _raft_conn_dead(ron_u);
  }
  else {
    _raft_conn_work(ron_u);
  }
}

/* _raft_getaddrinfo_cb(): generic getaddrinfo callback.
*/
static void
_raft_getaddrinfo_cb(uv_getaddrinfo_t* raq_u,
                     c3_i              sas_i,
                     struct addrinfo*  add_u)
{
  struct addrinfo* res_u;
  uv_connect_t*    con_u = malloc(sizeof(*con_u));
  u2_rcon*         ron_u = raq_u->data;

  con_u->data = ron_u;
  for ( res_u = add_u; res_u; res_u = res_u->ai_next ) {
    if ( 0 != uv_tcp_connect(con_u,
                             &ron_u->wax_u,
                             *(struct sockaddr_in*)res_u->ai_addr,
                             _raft_connect_cb) )
    {
      uL(fprintf(uH, "raft: getaddrinfo_cb: %s\n",
                     uv_strerror(uv_last_error(u2L))));
      uv_close((uv_handle_t*)&ron_u->wax_u, 0);
      continue;
    }
    else {
      break;                                            //  Found one
    }
  }
  uv_freeaddrinfo(add_u);
  free(raq_u);
}

/* _raft_conn_all(): ensure that we are connected to each peer.
*/
static void
_raft_conn_all(u2_raft* raf_u, void (*con_f)(u2_rcon* ron_u))
{
  u2_rnam* nam_u = raf_u->nam_u;
  u2_rcon* ron_u;

  while ( nam_u ) {
    if ( !nam_u->ron_u ) {
      struct addrinfo   hit_u;
      uv_getaddrinfo_t* raq_u = malloc(sizeof(*raq_u));

      memset(&hit_u, 0, sizeof(hit_u));
      hit_u.ai_family = AF_INET;
      hit_u.ai_socktype = SOCK_STREAM;

      ron_u = malloc(sizeof(*ron_u));
      uv_tcp_init(u2L, &ron_u->wax_u);

      raq_u->data = ron_u;

      if ( 0 != uv_getaddrinfo(u2L,
                               raq_u,
                               _raft_getaddrinfo_cb,
                               nam_u->nam_c,
                               nam_u->por_c, &hit_u) )
      {
        uL(fprintf(uH, "raft: getaddrinfo: %s\n",
                       uv_strerror(uv_last_error(u2L))));

        uv_close((uv_handle_t*)&ron_u->wax_u, 0);
        free(raq_u);
        free(ron_u);
        c3_assert(0);
      }
      else {
        ron_u->cap_u = 0;
        ron_u->red_t = 0;

        ron_u->nam_u = nam_u;
        ron_u->nex_u = 0;
        ron_u->raf_u = raf_u;
        nam_u->ron_u = ron_u;

      }
    }
    con_f(nam_u->ron_u);
    nam_u = nam_u->nex_u;
  }
}

/* _raft_send_beat(): send a heartbeat (empty AppendEntries) to peer.
*/
static void
_raft_send_beat(u2_rcon* ron_u)
{
  /* TODO */
}

/* _raft_send_revo(): send a RequestVote to peer.
*/
static void
_raft_send_revo(u2_rcon* ron_u)
{
  /* TODO */
}

/* _raft_start_election(): bump term, vote for self, solicit votes from peers.
*/
static void
_raft_start_election(u2_raft* raf_u)
{
  size_t siz_i = strlen(u2_Host.ops_u.nam_c) + 7;
  c3_i   wri_i;

  raf_u->tem_w++;
  uL(fprintf(uH, "raft: starting election [tem:%d]\n", raf_u->tem_w));

  if ( raf_u->vog_c ) {
    free(raf_u->vog_c);
  }
  raf_u->vot_w = 1;
  raf_u->vog_c = malloc(siz_i);
  wri_i = snprintf(raf_u->vog_c, siz_i, "%s:%d",
                   u2_Host.ops_u.nam_c, u2_Host.ops_u.rop_u.por_s);
  c3_assert(wri_i < siz_i);

  _raft_conn_all(raf_u, _raft_send_revo);
}

/* _raft_heartbeat(): send a heartbeat to all peers.
*/
static void
_raft_heartbeat(u2_raft* raf_u)
{
  _raft_conn_all(raf_u, _raft_send_beat);
}

/* _raft_time_cb(): generic timer callback.
**
** Called on election timeouts for non-leaders, and at heartbeat interval for
** leaders.
*/
static void
_raft_time_cb(uv_timer_t* tim_u, c3_i sas_i)
{
  u2_raft* raf_u = tim_u->data;
  //uL(fprintf(uH, "raft: time\n"));

  c3_assert(sas_i == 0);
  switch ( raf_u->typ_e ) {
    default: {
      uL(fprintf(uH, "raft: time_cb: unknown server state\n"));
      c3_assert(0);
    }
    case u2_raty_foll: {
      raf_u->typ_e = u2_raty_cand;
      // continue to cand
    }
    case u2_raty_cand: {
      sas_i = uv_timer_start(tim_u, _raft_time_cb,
                             150 + _raft_election_rand(), 0);
      c3_assert(sas_i == 0);
      _raft_start_election(raf_u);
      break;
    }
    case u2_raty_lead: {
      sas_i = uv_timer_start(tim_u, _raft_time_cb, 50, 0);
      c3_assert(sas_i == 0);
      _raft_heartbeat(raf_u);
      break;
    }
  }
}

/* _raft_foll_init(): begin, follower mode.
*/
static void
_raft_foll_init(u2_raft* raf_u)
{
  uL(fprintf(uH, "raft: starting follower\n"));

  raf_u->typ_e = u2_raty_foll;

  if ( 0 != uv_tcp_init(u2L, &raf_u->wax_u) ) {
    uL(fprintf(uH, "raft: init: %s\n", uv_strerror(uv_last_error(u2L))));
    c3_assert(0);
  }

  // Bind the listener.
  {
    struct sockaddr_in add_u;

    memset(&add_u, 0, sizeof(add_u));
    add_u.sin_family = AF_INET;
    add_u.sin_addr.s_addr = htonl(INADDR_ANY);
    add_u.sin_port = htons(u2_Host.ops_u.rop_u.por_s);

    if ( 0 != uv_tcp_bind(&raf_u->wax_u, add_u) ) {
      uL(fprintf(uH, "raft: bind: %s\n", uv_strerror(uv_last_error(u2L))));
      c3_assert(0);
    }
    else {
      if ( 0 != uv_listen((uv_stream_t*)&raf_u->wax_u, 16, _raft_listen_cb) ) {
        uL(fprintf(uH, "raft: listen: %s\n", uv_strerror(uv_last_error(u2L))));
        c3_assert(0);
      }
      else {
        uL(fprintf(uH, "raft: on TCP %d\n", u2_Host.ops_u.rop_u.por_s));
      }
    }
  }

  // Start the initial election timeout.
  uv_timer_start(&raf_u->tim_u, _raft_time_cb, _raft_election_rand(), 0);
}

/* _raft_lone_init(): begin, single-instance mode.
*/
static void
_raft_lone_init(u2_raft* raf_u)
{
  uL(fprintf(uH, "raft: single-instance mode\n"));

  _raft_promote(raf_u);
}

/* u2_raft_init(): start Raft process.
*/
void
u2_raft_init()
{
  u2_raft* raf_u = u2R;

  uv_timer_init(u2L, &raf_u->tim_u);
  raf_u->tim_u.data = raf_u;

  if ( 0 == u2_Host.ops_u.rop_u.por_s ) {
    _raft_lone_init(raf_u);
  }
  else {
    _raft_foll_init(raf_u);
  }
}

/* _raft_sure(): apply and save an input ovum and its result.
*/
static void
_raft_sure(u2_reck* rec_u, u2_noun ovo, u2_noun vir, u2_noun cor)
{
  //  Whatever worked, save it.  (XX - should be concurrent with execute.)
  //  We'd like more events that don't change the state but need work here.
  {
    u2_mug(cor);
    u2_mug(rec_u->roc);

    if ( u2_no == u2_sing(cor, rec_u->roc) ) {
      rec_u->roe = u2nc(u2nc(vir, ovo), rec_u->roe);

      u2z(rec_u->roc);
      rec_u->roc = cor;
    }
    else {
      u2z(ovo);
      rec_u->roe = u2nc(u2nc(vir, u2_nul), rec_u->roe);

      u2z(cor);
    }
  }
}

/* _raft_lame(): handle an application failure.
*/
static void
_raft_lame(u2_reck* rec_u, u2_noun ovo, u2_noun why, u2_noun tan)
{
  u2_noun bov, gon;

#if 1
  {
    c3_c* oik_c = u2_cr_string(u2h(u2t(ovo)));

    // uL(fprintf(uH, "lame: %s\n", oik_c));
    free(oik_c);
  }
#endif

  //  Formal error in a network packet generates a hole card.
  //
  //  There should be a separate path for crypto failures,
  //  to prevent timing attacks, but isn't right now.  To deal
  //  with a crypto failure, just drop the packet.
  //
  if ( (c3__exit == why) && (c3__hear == u2h(u2t(ovo))) ) {
    u2_lo_punt(2, u2_ckb_flop(u2k(tan)));

    bov = u2nc(u2k(u2h(ovo)), u2nc(c3__hole, u2k(u2t(u2t(ovo)))));
    u2z(why);
  }
  else {
    bov = u2nc(u2k(u2h(ovo)), u2nt(c3__crud, why, u2k(tan)));
    u2_hevn_at(lad) = u2_nul;
  }
  // u2_lo_show("data", u2k(u2t(u2t(ovo))));

  u2z(ovo);

  gon = u2_lo_soft(rec_u, 0, u2_reck_poke, u2k(bov));
  if ( u2_blip == u2h(gon) ) {
    _raft_sure(rec_u, bov, u2k(u2h(u2t(gon))), u2k(u2t(u2t(gon))));

    u2z(gon);
  }
  else {
    u2z(gon);
    {
      u2_noun vab = u2nc(u2k(u2h(bov)),
                         u2nc(c3__warn, u2_ci_tape("crude crash!")));
      u2_noun nog = u2_lo_soft(rec_u, 0, u2_reck_poke, u2k(vab));

      if ( u2_blip == u2h(nog) ) {
        _raft_sure(rec_u, vab, u2k(u2h(u2t(nog))), u2k(u2t(u2t(nog))));
        u2z(nog);
      }
      else {
        u2z(nog);
        u2z(vab);

        uL(fprintf(uH, "crude: all delivery failed!\n"));
      }
    }
  }
}

/* _raft_punk(): insert and apply an input ovum (unprotected).
*/
static void
_raft_punk(u2_reck* rec_u, u2_noun ovo)
{
  // c3_c* txt_c = u2_cr_string(u2h(u2t(ovo)));
  c3_w sec_w;
  // static c3_w num_w;
  u2_noun gon;

  // uL(fprintf(uH, "punk: %s: %d\n", u2_cr_string(u2h(u2t(ovo))), num_w++));

  //  XX this is wrong - the timer should be on the original hose.
  //
  if ( (c3__term == u2h(u2t(u2h(ovo)))) ||
       (c3__batz == u2h(u2t(u2h(ovo)))) ) {
    sec_w = 0;
  } else sec_w = 60;

  //  Control alarm loops.
  //
  if ( c3__wake != u2h(u2t(ovo)) ) {
    u2_Host.beh_u.run_w = 0;
  }

  gon = u2_lo_soft(rec_u, sec_w, u2_reck_poke, u2k(ovo));

  if ( u2_blip != u2h(gon) ) {
    u2_noun why = u2k(u2h(gon));
    u2_noun tan = u2k(u2t(gon));

    u2z(gon);
    _raft_lame(rec_u, ovo, why, tan);
  }
  else {
    u2_noun vir = u2k(u2h(u2t(gon)));
    u2_noun cor = u2k(u2t(u2t(gon)));
    u2_noun nug;

    u2z(gon);
    nug = u2_reck_nick(rec_u, vir, cor);

    if ( u2_blip != u2h(nug) ) {
      u2_noun why = u2k(u2h(nug));
      u2_noun tan = u2k(u2t(nug));

      u2z(nug);
      _raft_lame(rec_u, ovo, why, tan);
    }
    else {
      vir = u2k(u2h(u2t(nug)));
      cor = u2k(u2t(u2t(nug)));

      u2z(nug);
      _raft_sure(rec_u, ovo, vir, cor);
    }
  }
  // uL(fprintf(uH, "punk oot %s\n", txt_c));
}

static void
_raft_comm(u2_reck* rec_u, c3_w bid_w)
{
  u2_cart* egg_u;

  u2_lo_open();

  egg_u = rec_u->ova.egg_u;
  while ( egg_u ) {
    if ( egg_u->ent_w <= bid_w ) {
      egg_u->did = u2_yes;
      egg_u->cit = u2_yes;
    } else break;
    egg_u = egg_u->nex_u;
  }
  u2_lo_shut(u2_yes);
}

static void
_raft_comm_cb(uv_timer_t* tim_u, c3_i sas_i)
{
  u2_raft* raf_u = tim_u->data;

  _raft_comm(u2A, raf_u->ent_w);
}

static c3_w
_raft_push(u2_raft* raf_u, c3_w* bob_w, c3_w len_w)
{
  c3_assert(raf_u->typ_e == u2_raty_lead);
  c3_assert(0 != bob_w && 0 < len_w);

  if ( 0 == u2_Host.ops_u.rop_u.por_s ) {
    raf_u->ent_w = u2_sist_pack(u2A, c3__ov, bob_w, len_w);

    if ( !uv_is_active((uv_handle_t*)&raf_u->tim_u) ) {
      uv_timer_start(&raf_u->tim_u, _raft_comm_cb, 0, 0);
    }

    return raf_u->ent_w;
  }
  else {
    uL(fprintf(uH, "raft: multi-instance push\n"));
    c3_assert(0);
  }
}

/* _raft_kick_all(): kick a list of events, transferring.
*/
static void
_raft_kick_all(u2_reck* rec_u, u2_noun vir)
{
  while ( u2_nul != vir ) {
    u2_noun ovo = u2k(u2h(vir));
    u2_noun nex = u2k(u2t(vir));
    u2z(vir); vir = nex;

    u2_reck_kick(rec_u, ovo);
  }
}

/* u2_raft_work(): work in rec_u.
*/
void
u2_raft_work(u2_reck* rec_u)
{
  if ( u2R->typ_e != u2_raty_lead ) {
    c3_assert(rec_u->ova.egg_u == 0);
    if ( u2_nul != rec_u->roe ) {
      uL(fprintf(uH, "raft: dropping roe!!\n"));
      u2z(rec_u->roe);
      rec_u->roe = u2_nul;
    }
  }
  else {
    u2_cart* egg_u;
    u2_noun  ova;
    u2_noun  vir;
    u2_noun  nex;

    //  Apply effects from just-committed events, and delete finished events.
    //
    while ( rec_u->ova.egg_u ) {
      egg_u = rec_u->ova.egg_u;

      if ( u2_yes == egg_u->did ) {
        vir = egg_u->vir;

        if ( egg_u == rec_u->ova.geg_u ) {
          c3_assert(egg_u->nex_u == 0);
          rec_u->ova.geg_u = rec_u->ova.egg_u = 0;
          free(egg_u);
        }
        else {
          c3_assert(egg_u->nex_u != 0);
          rec_u->ova.egg_u = egg_u->nex_u;
          free(egg_u);
        }

        if ( u2_yes == egg_u->cit ) {
          _raft_kick_all(rec_u, vir);
        }
        else {
          //  We poked an event, but Raft failed to persist it.
          //  TODO: gracefully recover.
          uL(fprintf(uH, "vere: event executed but not persisted\n"));
          c3_assert(0);
        }
      }
      else break;
    }

    //  Poke pending events, leaving the poked events and errors on rec_u->roe.
    //
    {
      if ( 0 == u2R->lug_u.len_d ) {
        return;
      }
      ova = u2_ckb_flop(rec_u->roe);
      rec_u->roe = u2_nul;

      while ( u2_nul != ova ) {
        _raft_punk(rec_u, u2k(u2t(u2h(ova))));
        c3_assert(u2_nul == u2h(u2h(ova)));

        nex = u2k(u2t(ova));
        u2z(ova); ova = nex;
      }
    }

    //  Cartify, jam, and encrypt this batch of events. Take a number, Raft will
    //  be with you shortly.
    {
      c3_w    bid_w;
      c3_w    len_w;
      c3_w*   bob_w;
      u2_noun ron;
      u2_noun ovo;

      ova = u2_ckb_flop(rec_u->roe);
      rec_u->roe = u2_nul;

      while ( u2_nul != ova ) {
        ovo = u2k(u2t(u2h(ova)));
        vir = u2k(u2h(u2h(ova)));
        nex = u2k(u2t(ova));
        u2z(ova); ova = nex;

        if ( u2_nul != ovo ) {
          egg_u = malloc(sizeof(*egg_u));
          egg_u->nex_u = 0;
          egg_u->cit = u2_no;
          egg_u->did = u2_no;
          egg_u->vir = vir;

          ron = u2_cke_jam(u2nc(u2k(rec_u->now), ovo));
          c3_assert(rec_u->key);
          ron = u2_dc("en:crya", u2k(rec_u->key), ron);

          len_w = u2_cr_met(5, ron);
          bob_w = malloc(len_w * 4L);
          u2_cr_words(0, len_w, bob_w, ron);
          u2z(ron);

          bid_w = _raft_push(u2R, bob_w, len_w);
          egg_u->ent_w = bid_w;

          if ( 0 == rec_u->ova.geg_u ) {
            c3_assert(0 == rec_u->ova.egg_u);
            rec_u->ova.geg_u = rec_u->ova.egg_u = egg_u;
          }
          else {
            c3_assert(0 == rec_u->ova.geg_u->nex_u);
            rec_u->ova.geg_u->nex_u = egg_u;
            rec_u->ova.geg_u = egg_u;
          }
        }
        else {
          _raft_kick_all(rec_u, vir);
        }
      }
    }
  }
}
