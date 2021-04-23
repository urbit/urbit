/* vere/http.c
**
*/
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <h2o.h>

#include "all.h"
#include "vere/vere.h"

typedef struct _u3_h2o_serv {
  h2o_globalconf_t fig_u;             //  h2o global config
  h2o_context_t    ctx_u;             //  h2o ctx
  h2o_accept_ctx_t cep_u;             //  h2o accept ctx
  h2o_hostconf_t*  hos_u;             //  h2o host config
  h2o_handler_t*   han_u;             //  h2o request handler
} u3_h2o_serv;

/* u3_rsat: http request state.
*/
  typedef enum {
    u3_rsat_init = 0,                   //  initialized
    u3_rsat_plan = 1,                   //  planned
    u3_rsat_ripe = 2                    //  responded
  } u3_rsat;

/* u3_hreq: incoming http request.
*/
  typedef struct _u3_hreq {
    h2o_req_t*       rec_u;             //  h2o request
    c3_w             seq_l;             //  sequence within connection
    u3_rsat          sat_e;             //  request state
    uv_timer_t*      tim_u;             //  timeout
    void*            gen_u;             //  response generator
    struct _u3_hcon* hon_u;             //  connection backlink
    struct _u3_hreq* nex_u;             //  next in connection's list
    struct _u3_hreq* pre_u;             //  next in connection's list
  } u3_hreq;

/* u3_hcon: incoming http connection.
*/
  typedef struct _u3_hcon {
    uv_tcp_t         wax_u;             //  client stream handler
    h2o_conn_t*      con_u;             //  h2o connection
    h2o_socket_t*    sok_u;             //  h2o connection socket
    c3_w             ipf_w;             //  client ipv4
    c3_w             coq_l;             //  connection number
    c3_w             seq_l;             //  next request number
    struct _u3_http* htp_u;             //  server backlink
    struct _u3_hreq* req_u;             //  request list
    struct _u3_hcon* nex_u;             //  next in server's list
    struct _u3_hcon* pre_u;             //  next in server's list
  } u3_hcon;

/* u3_http: http server.
*/
  typedef struct _u3_http {
    uv_tcp_t         wax_u;             //  server stream handler
    void*            h2o_u;             //  libh2o configuration
    c3_w             sev_l;             //  server number
    c3_w             coq_l;             //  next connection number
    c3_s             por_s;             //  running port
    c3_o             sec;               //  logically secure
    c3_o             lop;               //  loopback-only
    c3_o             liv;               //  c3n == shutdown
    struct _u3_hcon* hon_u;             //  connection list
    struct _u3_http* nex_u;             //  next in list
    struct _u3_httd* htd_u;             //  device backpointer
  } u3_http;

/* u3_form: http config from %eyre
*/
  typedef struct _u3_form {
    c3_o             pro;               //  proxy
    c3_o             log;               //  keep access log
    c3_o             red;               //  redirect to HTTPS
    uv_buf_t         key_u;             //  PEM RSA private key
    uv_buf_t         cer_u;             //  PEM certificate chain
  } u3_form;

/* u3_hfig: general http configuration
*/
  typedef struct _u3_hfig {
    u3_form*         for_u;             //  config from %eyre
    struct _u3_hreq* seq_u;             //  open slog requests
    uv_timer_t*      sit_u;             //  slog stream heartbeat
  } u3_hfig;

/* u3_httd: general http device
*/
typedef struct _u3_httd {
  u3_auto            car_u;             //  driver
  c3_l               sev_l;             //  instance number
  u3_hfig            fig_u;             //  http configuration
  u3_http*           htp_u;             //  http servers
  SSL_CTX*           tls_u;             //  server SSL_CTX*
} u3_httd;

static void _http_serv_free(u3_http* htp_u);
static void _http_serv_start_all(u3_httd* htd_u);
static void _http_form_free(u3_httd* htd_u);

static const c3_i TCP_BACKLOG = 16;
static const c3_w HEARTBEAT_TIMEOUT = 20 * 1000;

/* _http_close_cb(): uv_close_cb that just free's handle
*/
static void
_http_close_cb(uv_handle_t* han_u)
{
  c3_free(han_u);
}

/* _http_vec_to_meth(): convert h2o_iovec_t to meth
*/
static u3_weak
_http_vec_to_meth(h2o_iovec_t vec_u)
{
  return ( 0 == strncmp(vec_u.base, "GET",     vec_u.len) ) ? u3i_string("GET") :
         ( 0 == strncmp(vec_u.base, "PUT",     vec_u.len) ) ? u3i_string("PUT")  :
         ( 0 == strncmp(vec_u.base, "POST",    vec_u.len) ) ? u3i_string("POST") :
         ( 0 == strncmp(vec_u.base, "HEAD",    vec_u.len) ) ? u3i_string("HEAD") :
         ( 0 == strncmp(vec_u.base, "CONNECT", vec_u.len) ) ? u3i_string("CONNECT") :
         ( 0 == strncmp(vec_u.base, "DELETE",  vec_u.len) ) ? u3i_string("DELETE") :
         ( 0 == strncmp(vec_u.base, "OPTIONS", vec_u.len) ) ? u3i_string("OPTIONS") :
         ( 0 == strncmp(vec_u.base, "TRACE",   vec_u.len) ) ? u3i_string("TRACE") :
         // TODO ??
         // ( 0 == strncmp(vec_u.base, "PATCH",   vec_u.len) ) ? c3__patc :
         u3_none;
}

/* _http_vec_to_atom(): convert h2o_iovec_t to atom (cord)
*/
static u3_noun
_http_vec_to_atom(h2o_iovec_t vec_u)
{
  return u3i_bytes(vec_u.len, (const c3_y*)vec_u.base);
}

/* _http_vec_to_octs(): convert h2o_iovec_t to (unit octs)
*/
static u3_noun
_http_vec_to_octs(h2o_iovec_t vec_u)
{
  if ( 0 == vec_u.len ) {
    return u3_nul;
  }

  // XX correct size_t -> atom?
  return u3nt(u3_nul, u3i_chubs(1, (const c3_d*)&vec_u.len),
                      _http_vec_to_atom(vec_u));
}

/* _cttp_bods_free(): free body structure.
*/
static void
_cttp_bods_free(u3_hbod* bod_u)
{
  while ( bod_u ) {
    u3_hbod* nex_u = bod_u->nex_u;

    c3_free(bod_u);
    bod_u = nex_u;
  }
}

/* _cttp_bod_from_octs(): translate octet-stream noun into body.
*/
static u3_hbod*
_cttp_bod_from_octs(u3_noun oct)
{
  c3_w len_w;

  if ( !_(u3a_is_cat(u3h(oct))) ) {     //  2GB max
    u3m_bail(c3__fail); return 0;
  }
  len_w = u3h(oct);

  {
    u3_hbod* bod_u = c3_malloc(1 + len_w + sizeof(*bod_u));
    bod_u->hun_y[len_w] = 0;
    bod_u->len_w = len_w;
    u3r_bytes(0, len_w, bod_u->hun_y, u3t(oct));

    bod_u->nex_u = 0;

    u3z(oct);
    return bod_u;
  }
}

/* _cttp_bods_to_vec(): translate body buffers to array of h2o_iovec_t
*/
static h2o_iovec_t*
_cttp_bods_to_vec(u3_hbod* bod_u, c3_w* tot_w)
{
  h2o_iovec_t* vec_u;
  c3_w len_w;

  {
    u3_hbod* bid_u = bod_u;
    len_w = 0;

    while( bid_u ) {
      len_w++;
      bid_u = bid_u->nex_u;
    }
  }

  vec_u = c3_malloc(sizeof(h2o_iovec_t) * len_w);
  len_w = 0;

  while( bod_u ) {
    vec_u[len_w] = h2o_iovec_init(bod_u->hun_y, bod_u->len_w);
    len_w++;
    bod_u = bod_u->nex_u;
  }

  *tot_w = len_w;

  return vec_u;
}

/* _http_heds_to_noun(): convert h2o_header_t to (list (pair @t @t))
*/
static u3_noun
_http_heds_to_noun(h2o_header_t* hed_u, c3_d hed_d)
{
  u3_noun hed = u3_nul;
  c3_d dex_d  = hed_d;

  h2o_header_t deh_u;

  while ( 0 < dex_d ) {
    deh_u = hed_u[--dex_d];
    hed = u3nc(u3nc(_http_vec_to_atom(*deh_u.name),
                    _http_vec_to_atom(deh_u.value)), hed);
  }

  return hed;
}

/* _http_heds_free(): free header linked list
*/
static void
_http_heds_free(u3_hhed* hed_u)
{
  while ( hed_u ) {
    u3_hhed* nex_u = hed_u->nex_u;

    c3_free(hed_u->nam_c);
    c3_free(hed_u->val_c);
    c3_free(hed_u);
    hed_u = nex_u;
  }
}

/* _http_hed_new(): create u3_hhed from nam/val cords
*/
static u3_hhed*
_http_hed_new(u3_atom nam, u3_atom val)
{
  c3_w     nam_w = u3r_met(3, nam);
  c3_w     val_w = u3r_met(3, val);
  u3_hhed* hed_u = c3_malloc(sizeof(*hed_u));

  hed_u->nam_c = c3_malloc(1 + nam_w);
  hed_u->val_c = c3_malloc(1 + val_w);
  hed_u->nam_c[nam_w] = 0;
  hed_u->val_c[val_w] = 0;
  hed_u->nex_u = 0;
  hed_u->nam_w = nam_w;
  hed_u->val_w = val_w;

  u3r_bytes(0, nam_w, (c3_y*)hed_u->nam_c, nam);
  u3r_bytes(0, val_w, (c3_y*)hed_u->val_c, val);

  return hed_u;
}

/* _http_heds_from_noun(): convert (list (pair @t @t)) to u3_hhed
*/
static u3_hhed*
_http_heds_from_noun(u3_noun hed)
{
  u3_noun deh = hed;
  u3_noun i_hed;

  u3_hhed* hed_u = 0;

  while ( u3_nul != hed ) {
    i_hed = u3h(hed);
    u3_hhed* nex_u = _http_hed_new(u3h(i_hed), u3t(i_hed));
    nex_u->nex_u = hed_u;

    hed_u = nex_u;
    hed = u3t(hed);
  }

  u3z(deh);
  return hed_u;
}

/* _http_req_find(): find http request in connection by sequence.
*/
static u3_hreq*
_http_req_find(u3_hcon* hon_u, c3_w seq_l)
{
  u3_hreq* req_u = hon_u->req_u;

  //  XX glories of linear search
  //
  while ( req_u ) {
    if ( seq_l == req_u->seq_l ) {
      return req_u;
    }
    req_u = req_u->nex_u;
  }
  return 0;
}

/* _http_req_link(): link http request to connection
*/
static void
_http_req_link(u3_hcon* hon_u, u3_hreq* req_u)
{
  req_u->hon_u = hon_u;
  req_u->seq_l = hon_u->seq_l++;
  req_u->nex_u = hon_u->req_u;

  if ( 0 != req_u->nex_u ) {
    req_u->nex_u->pre_u = req_u;
  }
  hon_u->req_u = req_u;
}

/* _http_req_unlink(): remove http request from connection
*/
static void
_http_req_unlink(u3_hreq* req_u)
{
  if ( 0 != req_u->pre_u ) {
    req_u->pre_u->nex_u = req_u->nex_u;

    if ( 0 != req_u->nex_u ) {
      req_u->nex_u->pre_u = req_u->pre_u;
    }
  }
  else {
    req_u->hon_u->req_u = req_u->nex_u;

    if ( 0 != req_u->nex_u ) {
      req_u->nex_u->pre_u = 0;
    }
  }
}

/* _http_seq_link(): store slog stream request in state
*/
static void
_http_seq_link(u3_hcon* hon_u, u3_hreq* req_u)
{
  u3_hfig* fig_u = &hon_u->htp_u->htd_u->fig_u;
  req_u->hon_u = hon_u;
  req_u->seq_l = hon_u->seq_l++;
  req_u->nex_u = fig_u->seq_u;

  if ( 0 != req_u->nex_u ) {
    req_u->nex_u->pre_u = req_u;
  }
  fig_u->seq_u = req_u;
}

/* _http_seq_unlink(): remove slog stream request from state
*/
static void
_http_seq_unlink(u3_hreq* req_u)
{
  u3_hfig* fig_u = &req_u->hon_u->htp_u->htd_u->fig_u;
  if ( 0 != req_u->pre_u ) {
    req_u->pre_u->nex_u = req_u->nex_u;

    if ( 0 != req_u->nex_u ) {
      req_u->nex_u->pre_u = req_u->pre_u;
    }
  }
  else {
    fig_u->seq_u = req_u->nex_u;

    if ( 0 != req_u->nex_u ) {
      req_u->nex_u->pre_u = 0;
    }
  }
}

/* _http_req_to_duct(): translate srv/con/req to duct
*/
static u3_noun
_http_req_to_duct(u3_hreq* req_u)
{
  return u3nc(u3i_string("http-server"),
              u3nq(u3dc("scot", c3__uv, req_u->hon_u->htp_u->sev_l),
                   u3dc("scot", c3__ud, req_u->hon_u->coq_l),
                   u3dc("scot", c3__ud, req_u->seq_l),
                   u3_nul));
}

/* _http_req_kill(): kill http request in %eyre.
*/
static void
_http_req_kill(u3_hreq* req_u)
{
  u3_httd* htd_u = req_u->hon_u->htp_u->htd_u;
  u3_noun wir    = _http_req_to_duct(req_u);
  u3_noun cad    = u3nc(u3i_string("cancel-request"), u3_nul);

  u3_auto_plan(&htd_u->car_u, u3_ovum_init(0, c3__e, wir, cad));
}

typedef struct _u3_hgen {
  h2o_generator_t neg_u;             // response callbacks
  c3_o            red;               // ready to send
  c3_o            dun;               // done sending
  u3_hbod*        bod_u;             // pending body
  u3_hbod*        nud_u;             // pending free
  u3_hhed*        hed_u;             // pending free
  u3_hreq*        req_u;             // originating request
} u3_hgen;

/* _http_req_close(): clean up & deallocate request
*/
static void
_http_req_close(u3_hreq* req_u)
{
  //  client canceled request before response
  //
  if ( u3_rsat_plan == req_u->sat_e ) {
    _http_req_kill(req_u);
  }

  if ( 0 != req_u->tim_u ) {
    uv_close((uv_handle_t*)req_u->tim_u, _http_close_cb);
    req_u->tim_u = 0;
  }
}

/* _http_req_done(): request finished, deallocation callback
*/
static void
_http_req_done(void* ptr_v)
{
  u3_hreq* req_u = (u3_hreq*)ptr_v;
  _http_req_close(req_u);
  _http_req_unlink(req_u);
}

/* _http_seq_done(): slog stream request finished, deallocation callback
*/
static void
_http_seq_done(void* ptr_v)
{
  u3_hreq* seq_u = (u3_hreq*)ptr_v;
  _http_req_close(seq_u);
  _http_seq_unlink(seq_u);
}

/* _http_req_timer_cb(): request timeout callback
*/
static void
_http_req_timer_cb(uv_timer_t* tim_u)
{
  u3_hreq* req_u = tim_u->data;

  if ( u3_rsat_plan == req_u->sat_e ) {
    _http_req_kill(req_u);
    req_u->sat_e = u3_rsat_ripe;

    c3_c* msg_c = "gateway timeout";
    h2o_send_error_generic(req_u->rec_u, 504, msg_c, msg_c, 0);
  }
}

/* _http_req_new(): receive standard http request.
*/
static u3_hreq*
_http_req_new(u3_hcon* hon_u, h2o_req_t* rec_u)
{
  u3_hreq* req_u = h2o_mem_alloc_shared(&rec_u->pool, sizeof(*req_u),
                                        _http_req_done);
  req_u->rec_u = rec_u;
  req_u->sat_e = u3_rsat_init;
  req_u->tim_u = 0;
  req_u->gen_u = 0;
  req_u->pre_u = 0;

  _http_req_link(hon_u, req_u);

  return req_u;
}

/* _http_seq_new(): receive slog stream http request.
*/
static u3_hreq*
_http_seq_new(u3_hcon* hon_u, h2o_req_t* rec_u)
{
  u3_hreq* req_u = h2o_mem_alloc_shared(&rec_u->pool, sizeof(*req_u),
                                        _http_seq_done);
  req_u->rec_u = rec_u;
  req_u->sat_e = u3_rsat_plan;
  req_u->tim_u = 0;
  req_u->gen_u = 0;
  req_u->pre_u = 0;

  _http_seq_link(hon_u, req_u);

  return req_u;
}

/* _http_req_dispatch(): dispatch http request to %eyre
*/
static void
_http_req_dispatch(u3_hreq* req_u, u3_noun req)
{
  c3_assert(u3_rsat_init == req_u->sat_e);
  req_u->sat_e = u3_rsat_plan;

  {
    u3_http* htp_u = req_u->hon_u->htp_u;
    u3_httd* htd_u = htp_u->htd_u;
    u3_noun wir    = _http_req_to_duct(req_u);
    u3_noun cad;

    {
      u3_noun adr = u3nc(c3__ipv4, u3i_words(1, &req_u->hon_u->ipf_w));
      //  XX loopback automatically secure too?
     //
      u3_noun dat = u3nt(htp_u->sec, adr, req);

      cad = ( c3y == req_u->hon_u->htp_u->lop )
            ? u3nc(u3i_string("request-local"), dat)
            : u3nc(u3i_string("request"), dat);
    }

    u3_auto_plan(&htd_u->car_u, u3_ovum_init(0, c3__e, wir, cad));
  }
}

/* _http_hgen_dispose(): dispose response generator and buffers
*/
static void
_http_hgen_dispose(void* ptr_v)
{
  u3_hgen* gen_u = (u3_hgen*)ptr_v;
  _http_heds_free(gen_u->hed_u);
  gen_u->hed_u = 0;
  _cttp_bods_free(gen_u->nud_u);
  gen_u->nud_u = 0;
  _cttp_bods_free(gen_u->bod_u);
  gen_u->bod_u = 0;
}

static void
_http_hgen_send(u3_hgen* gen_u)
{
  c3_assert( c3y == gen_u->red );

  u3_hreq* req_u = gen_u->req_u;
  h2o_req_t* rec_u = req_u->rec_u;

  c3_w len_w;
  h2o_iovec_t* vec_u = _cttp_bods_to_vec(gen_u->bod_u, &len_w);

  //  not ready again until _proceed
  //
  gen_u->red = c3n;

  //  stash [bod_u] to free later
  //
  _cttp_bods_free(gen_u->nud_u);
  gen_u->nud_u = gen_u->bod_u;
  gen_u->bod_u = 0;

  if ( c3n == gen_u->dun ) {
    h2o_send(rec_u, vec_u, len_w, H2O_SEND_STATE_IN_PROGRESS);
    uv_timer_start(req_u->tim_u, _http_req_timer_cb, 45 * 1000, 0);
  }
  else {
    //  close connection if shutdown pending
    //
    u3_h2o_serv* h2o_u = req_u->hon_u->htp_u->h2o_u;

    if ( 0 != h2o_u->ctx_u.shutdown_requested ) {
      rec_u->http1_is_persistent = 0;
    }

    h2o_send(rec_u, vec_u, len_w, H2O_SEND_STATE_FINAL);
  }

  c3_free(vec_u);
}

/* _http_hgen_stop(): h2o is closing an in-progress response.
*/
static void
_http_hgen_stop(h2o_generator_t* neg_u, h2o_req_t* rec_u)
{
  u3_hgen* gen_u = (u3_hgen*)neg_u;

  //  response not complete, enqueue cancel
  //
  if ( c3n == gen_u->dun ) {
    _http_req_kill(gen_u->req_u);
  }
}

/* _http_hgen_proceed(): h2o is ready for more response data.
*/
static void
_http_hgen_proceed(h2o_generator_t* neg_u, h2o_req_t* rec_u)
{
  u3_hgen* gen_u = (u3_hgen*)neg_u;
  u3_hreq* req_u = gen_u->req_u;

  // sanity check
  c3_assert( rec_u == req_u->rec_u );

  gen_u->red = c3y;

  if ( 0 != gen_u->bod_u || c3y == gen_u->dun ) {
    _http_hgen_send(gen_u);
  }
}

/* _http_start_respond(): write a [%http-response %start ...] to h2o_req_t->res
*/
static void
_http_start_respond(u3_hreq* req_u,
                    u3_noun status,
                    u3_noun headers,
                    u3_noun data,
                    u3_noun complete)
{
  // u3l_log("start");

  if ( u3_rsat_plan != req_u->sat_e ) {
    //u3l_log("duplicate response");
    return;
  }

  req_u->sat_e = u3_rsat_ripe;

  uv_timer_stop(req_u->tim_u);

  h2o_req_t* rec_u = req_u->rec_u;

  rec_u->res.status = status;
  rec_u->res.reason = (status < 200) ? "weird" :
                      (status < 300) ? "ok" :
                      (status < 400) ? "moved" :
                      (status < 500) ? "missing" :
                      "hosed";

  u3_hhed* hed_u = _http_heds_from_noun(u3k(headers));
  u3_hhed* deh_u = hed_u;

  c3_i has_len_i = 0;

  while ( 0 != hed_u ) {
    if ( 0 == strncmp(hed_u->nam_c, "content-length", 14) ) {
      has_len_i = 1;
    }
    else {
      h2o_add_header_by_str(&rec_u->pool, &rec_u->res.headers,
                            hed_u->nam_c, hed_u->nam_w, 0, 0,
                            hed_u->val_c, hed_u->val_w);
    }

    hed_u = hed_u->nex_u;
  }

  u3_hgen* gen_u = h2o_mem_alloc_shared(&rec_u->pool, sizeof(*gen_u),
                                        _http_hgen_dispose);
  gen_u->neg_u = (h2o_generator_t){ _http_hgen_proceed, _http_hgen_stop };
  gen_u->red   = c3y;
  gen_u->dun   = complete;
  gen_u->bod_u = ( u3_nul == data ) ?
                 0 : _cttp_bod_from_octs(u3k(u3t(data)));
  gen_u->nud_u = 0;
  gen_u->hed_u = deh_u;
  gen_u->req_u = req_u;

  //  if we don't explicitly set this field, h2o will send with
  //  transfer-encoding: chunked
  //
  if ( 1 == has_len_i ) {
    rec_u->res.content_length = ( 0 == gen_u->bod_u ) ?
                                0 : gen_u->bod_u->len_w;
  }

  req_u->gen_u = gen_u;

  h2o_start_response(rec_u, &gen_u->neg_u);

  _http_hgen_send(gen_u);

  u3z(status); u3z(headers); u3z(data); u3z(complete);
}

/* _http_continue_respond(): write a [%http-response %continue ...] to
 * h2o_req_t->res
*/
static void
_http_continue_respond(u3_hreq* req_u,
                       /* u3_noun status, */
                       /* u3_noun headers, */
                       u3_noun data,
                       u3_noun complete)
{
  // u3l_log("continue");

  // XX add sequence numbers for %continue effects?
  // Arvo does not (currently) guarantee effect idempotence!!

  // response has not yet been started
  if ( u3_rsat_ripe != req_u->sat_e ) {
    // u3l_log("duplicate response");
    return;
  }

  u3_hgen* gen_u = req_u->gen_u;

  uv_timer_stop(req_u->tim_u);

  // XX proposed sequence number safety check
  // if ( sequence <= gen_u->sequence ) {
  //   return;
  // }
  //
  // c3_assert( sequence == ++gen_u->sequence );

  gen_u->dun = complete;

  if ( u3_nul != data ) {
    u3_hbod* bod_u = _cttp_bod_from_octs(u3k(u3t(data)));

    if ( 0 == gen_u->bod_u ) {
      gen_u->bod_u = bod_u;
    }
    else {
      u3_hbod* pre_u = gen_u->bod_u;

      while ( 0 != pre_u->nex_u ) {
        pre_u = pre_u->nex_u;
      }

      pre_u->nex_u = bod_u;
    }
  }

  if ( c3y == gen_u->red ) {
    _http_hgen_send(gen_u);
  }

  u3z(data); u3z(complete);
}

/* _http_rec_to_httq(): convert h2o_req_t to httq
*/
static u3_weak
_http_rec_to_httq(h2o_req_t* rec_u)
{
  u3_noun med = _http_vec_to_meth(rec_u->method);

  if ( u3_none == med ) {
    return u3_none;
  }

  u3_noun url = _http_vec_to_atom(rec_u->path);
  u3_noun hed = _http_heds_to_noun(rec_u->headers.entries,
                                   rec_u->headers.size);

  // restore host header
  hed = u3nc(u3nc(u3i_string("host"),
                  _http_vec_to_atom(rec_u->authority)),
             hed);

  u3_noun bod = _http_vec_to_octs(rec_u->entity);

  return u3nq(med, url, hed, bod);
}

typedef struct _h2o_uv_sock {         //  see private st_h2o_uv_socket_t
  h2o_socket_t     sok_u;             //  socket
  uv_stream_t*     han_u;             //  client stream handler (u3_hcon)
} h2o_uv_sock;

/* _http_req_prepare(): creates u3 req from h2o req and initializes its timer
*/
static u3_hreq*
_http_req_prepare(h2o_req_t* rec_u,
                  u3_hreq* (*new_f)(u3_hcon*, h2o_req_t*))
{
  h2o_uv_sock* suv_u = (h2o_uv_sock*)rec_u->conn->
                         callbacks->get_socket(rec_u->conn);
  u3_hcon* hon_u = (u3_hcon*)suv_u->han_u;

  //  sanity check
  c3_assert( hon_u->sok_u == &suv_u->sok_u );

  u3_hreq* seq_u = new_f(hon_u, rec_u);

  seq_u->tim_u = c3_malloc(sizeof(*seq_u->tim_u));
  seq_u->tim_u->data = seq_u;
  uv_timer_init(u3L, seq_u->tim_u);
  uv_timer_start(seq_u->tim_u, _http_req_timer_cb, 600 * 1000, 0);

  return seq_u;
}

/* _http_seq_continue(): respond to slogstream request based on auth scry result
*/
static void
_http_seq_continue(void* vod_p, u3_noun nun)
{
  h2o_req_t* rec_u = vod_p;
  u3_weak    aut   = u3r_at(7, nun);

  //  if the request is authenticated properly, send slogstream/sse headers
  //
  //TODO  authentication might expire after the connection has been opened!
  //      eyre could notify us about this, or we could re-check periodically.
  //
  if ( c3y == aut ) {
    u3_hreq* req_u = _http_req_prepare(rec_u, _http_seq_new);
    u3_noun  hed   = u3nl(u3nc(u3i_string("Content-Type"),
                               u3i_string("text/event-stream")),
                          u3nc(u3i_string("Cache-Control"),
                               u3i_string("no-cache")),
                          u3nc(u3i_string("Connection"),
                               u3i_string("keep-alive")),
                          u3_none);

    _http_start_respond(req_u, 200, hed, u3_nul, c3n);
  }
  //  if the scry failed, the result is unexpected, or there is no auth,
  //  respond with the appropriate status code
  //
  else {
    //NOTE  we use req_new because we don't want to consider this a slog stream
    //      request, but this means we need to manually skip past the "in event
    //      queue" state on the hreq.
    u3_hreq* req_u = _http_req_prepare(rec_u, _http_req_new);
    req_u->sat_e = u3_rsat_plan;

    if ( c3n == aut ) {
      _http_start_respond(req_u, 403, u3_nul, u3_nul, c3y);
    }
    else if ( u3_none == aut ) {
      u3l_log("http: authentication scry failed");
      _http_start_respond(req_u, 500, u3_nul, u3_nul, c3y);
    }
    else {
      u3m_p("http: weird authentication scry result", aut);
      _http_start_respond(req_u, 500, u3_nul, u3_nul, c3y);
    }
  }

  u3z(nun);
}

/* _http_seq_accept(): handle incoming http request on slogstream endpoint
*/
static int
_http_seq_accept(h2o_handler_t* han_u, h2o_req_t* rec_u)
{
  //  try to find a cookie header
  //
  u3_weak coo = u3_none;
  {
    //TODO  http2 allows the client to put multiple 'cookie' headers
    ssize_t hin_i = h2o_find_header_by_str(&rec_u->headers, "cookie", 6, -1);
    if ( hin_i != -1 ) {
      coo = _http_vec_to_atom(rec_u->headers.entries[hin_i].value);
    }
  }

  //  if there is no cookie header, it can't possibly be authenticated
  //
  if ( u3_none == coo ) {
    u3_hreq* req_u = _http_req_prepare(rec_u, _http_req_new);
    req_u->sat_e = u3_rsat_plan;
    _http_start_respond(req_u, 403, u3_nul, u3_nul, c3y);
  }
  //  if there is a cookie, scry to see if it constitutes authentication
  //
  else {
    h2o_uv_sock* suv_u = (h2o_uv_sock*)rec_u->conn->
                           callbacks->get_socket(rec_u->conn);
    u3_hcon* hon_u = (u3_hcon*)suv_u->han_u;

    u3_noun pax = u3nq(u3i_string("authenticated"),
                       u3i_string("cookie"),
                       u3dc("scot", 't', coo),
                       u3_nul);
    u3_pier_peek_last(hon_u->htp_u->htd_u->car_u.pir_u, u3_nul, c3__ex,
                      u3_nul, pax, rec_u, _http_seq_continue);
  }

  return 0;
}

/* _http_rec_accept(); handle incoming http request from h2o.
*/
static c3_i
_http_rec_accept(h2o_handler_t* han_u, h2o_req_t* rec_u)
{
  u3_weak req = _http_rec_to_httq(rec_u);

  if ( u3_none == req ) {
    if ( (u3C.wag_w & u3o_verbose) ) {
      u3l_log("strange %.*s request", (int)rec_u->method.len,
              rec_u->method.base);
    }
    c3_c* msg_c = "bad request";
    h2o_send_error_generic(rec_u, 400, msg_c, msg_c, 0);
  }
  else {
    u3_hreq* req_u = _http_req_prepare(rec_u, _http_req_new);
    _http_req_dispatch(req_u, req);
  }

  return 0;
}

/* _http_conn_find(): find http connection in server by sequence.
*/
static u3_hcon*
_http_conn_find(u3_http *htp_u, c3_w coq_l)
{
  u3_hcon* hon_u = htp_u->hon_u;

  //  XX glories of linear search
  //
  while ( hon_u ) {
    if ( coq_l == hon_u->coq_l ) {
      return hon_u;
    }
    hon_u = hon_u->nex_u;
  }
  return 0;
}

/* _http_conn_link(): link http request to connection
*/
static void
_http_conn_link(u3_http* htp_u, u3_hcon* hon_u)
{
  hon_u->htp_u = htp_u;
  hon_u->coq_l = htp_u->coq_l++;
  hon_u->nex_u = htp_u->hon_u;

  if ( 0 != hon_u->nex_u ) {
    hon_u->nex_u->pre_u = hon_u;
  }
  htp_u->hon_u = hon_u;
}

/* _http_conn_unlink(): remove http request from connection
*/
static void
_http_conn_unlink(u3_hcon* hon_u)
{
  if ( 0 != hon_u->pre_u ) {
    hon_u->pre_u->nex_u = hon_u->nex_u;

    if ( 0 != hon_u->nex_u ) {
      hon_u->nex_u->pre_u = hon_u->pre_u;
    }
  }
  else {
    hon_u->htp_u->hon_u = hon_u->nex_u;

    if ( 0 != hon_u->nex_u ) {
      hon_u->nex_u->pre_u = 0;
    }
  }
}

/* _http_conn_free(): free http connection on close.
*/
static void
_http_conn_free(uv_handle_t* han_t)
{
  u3_hcon* hon_u = (u3_hcon*)han_t;
  u3_http* htp_u = hon_u->htp_u;
  u3_h2o_serv* h2o_u = htp_u->h2o_u;

  c3_assert( 0 == hon_u->req_u );

#if 0
  {
    c3_w len_w = 0;

    u3_hcon* noh_u = htp_u->hon_u;

    while ( 0 != noh_u ) {
      len_w++;
      noh_u = noh_u->nex_u;
    }

    u3l_log("http conn free %d of %u server %d", hon_u->coq_l, len_w, htp_u->sev_l);
  }
#endif

  _http_conn_unlink(hon_u);

#if 0
  {
    c3_w len_w = 0;

    u3_hcon* noh_u = htp_u->hon_u;

    while ( 0 != noh_u ) {
      len_w++;
      noh_u = noh_u->nex_u;
    }

    u3l_log("http conn free %u remaining", len_w);
  }
#endif

  if ( (0 == htp_u->hon_u) && (0 != h2o_u->ctx_u.shutdown_requested) ) {
#if 0
    u3l_log("http conn free %d free server %d", hon_u->coq_l, htp_u->sev_l);
#endif
    _http_serv_free(htp_u);
  }

  c3_free(hon_u);
}

/* _http_conn_new(): create and accept http connection.
*/
static u3_hcon*
_http_conn_new(u3_http* htp_u)
{
  u3_hcon* hon_u = c3_malloc(sizeof(*hon_u));
  hon_u->seq_l = 1;
  hon_u->ipf_w = 0;
  hon_u->req_u = 0;
  hon_u->sok_u = 0;
  hon_u->con_u = 0;
  hon_u->pre_u = 0;

  _http_conn_link(htp_u, hon_u);

#if 0
  u3l_log("http conn neww %d server %d", hon_u->coq_l, htp_u->sev_l);
#endif

  return hon_u;
}

/* _http_serv_find(): find http server by sequence.
*/
static u3_http*
_http_serv_find(u3_httd* htd_u, c3_l sev_l)
{
  u3_http* htp_u = htd_u->htp_u;

  //  XX glories of linear search
  //
  while ( htp_u ) {
    if ( sev_l == htp_u->sev_l ) {
      return htp_u;
    }
    htp_u = htp_u->nex_u;
  }
  return 0;
}

/* _http_serv_link(): link http server to global state.
*/
static void
_http_serv_link(u3_httd* htd_u, u3_http* htp_u)
{
  // XX link elsewhere initially, relink on start?

  if ( 0 != htd_u->htp_u ) {
    htp_u->sev_l = 1 + htd_u->htp_u->sev_l;
  }
  else {
    htp_u->sev_l = htd_u->sev_l;
  }

  htp_u->nex_u = htd_u->htp_u;
  htp_u->htd_u = htd_u;
  htd_u->htp_u = htp_u;
}

/* _http_serv_unlink(): remove http server from global state.
*/
static void
_http_serv_unlink(u3_http* htp_u)
{
  // XX link elsewhere initially, relink on start?
#if 0
  u3l_log("http serv unlink %d", htp_u->sev_l);
#endif
  u3_http* pre_u = htp_u->htd_u->htp_u;

  if ( pre_u == htp_u ) {
    pre_u = htp_u->nex_u;
  }
  else {
    //  XX glories of linear search
    //
    while ( pre_u ) {
      if ( pre_u->nex_u == htp_u ) {
        pre_u->nex_u = htp_u->nex_u;
      }
      else pre_u = pre_u->nex_u;
    }
  }
}

/* _http_h2o_context_dispose(): h2o_context_dispose, inlined and cleaned up.
*/
static void
_http_h2o_context_dispose(h2o_context_t* ctx)
{
  h2o_globalconf_t *config = ctx->globalconf;
  size_t i, j;

  for (i = 0; config->hosts[i] != NULL; ++i) {
    h2o_hostconf_t *hostconf = config->hosts[i];
    for (j = 0; j != hostconf->paths.size; ++j) {
      h2o_pathconf_t *pathconf = hostconf->paths.entries + j;
      h2o_context_dispose_pathconf_context(ctx, pathconf);
    }
    h2o_context_dispose_pathconf_context(ctx, &hostconf->fallback_path);
  }

  c3_free(ctx->_pathconfs_inited.entries);
  c3_free(ctx->_module_configs);

  h2o_timeout_dispose(ctx->loop, &ctx->zero_timeout);
  h2o_timeout_dispose(ctx->loop, &ctx->hundred_ms_timeout);
  h2o_timeout_dispose(ctx->loop, &ctx->handshake_timeout);
  h2o_timeout_dispose(ctx->loop, &ctx->http1.req_timeout);
  h2o_timeout_dispose(ctx->loop, &ctx->http2.idle_timeout);

  // NOTE: linked in http2/connection, never unlinked
  h2o_timeout_unlink(&ctx->http2._graceful_shutdown_timeout);

  h2o_timeout_dispose(ctx->loop, &ctx->http2.graceful_shutdown_timeout);
  h2o_timeout_dispose(ctx->loop, &ctx->proxy.io_timeout);
  h2o_timeout_dispose(ctx->loop, &ctx->one_sec_timeout);

  h2o_filecache_destroy(ctx->filecache);
  ctx->filecache = NULL;

  /* clear storage */
  for (i = 0; i != ctx->storage.size; ++i) {
    h2o_context_storage_item_t *item = ctx->storage.entries + i;
    if (item->dispose != NULL) {
        item->dispose(item->data);
    }
  }

  c3_free(ctx->storage.entries);

  h2o_multithread_unregister_receiver(ctx->queue, &ctx->receivers.hostinfo_getaddr);
  h2o_multithread_destroy_queue(ctx->queue);

  if (ctx->_timestamp_cache.value != NULL) {
    h2o_mem_release_shared(ctx->_timestamp_cache.value);
  }

  // NOTE: explicit uv_run removed
}

/* _http_serv_really_free(): free http server.
*/
static void
_http_serv_really_free(u3_http* htp_u)
{
  c3_assert( 0 == htp_u->hon_u );

  if ( 0 != htp_u->h2o_u ) {
    u3_h2o_serv* h2o_u = htp_u->h2o_u;

    if ( 0 != h2o_u->cep_u.ssl_ctx ) {
      SSL_CTX_free(h2o_u->cep_u.ssl_ctx);
    }

    h2o_config_dispose(&h2o_u->fig_u);

    // XX h2o_cleanup_thread if not restarting?

    c3_free(htp_u->h2o_u);
    htp_u->h2o_u = 0;
  }

  _http_serv_unlink(htp_u);
  c3_free(htp_u);
}

/* http_serv_free_cb(): timer callback for freeing http server.
*/
static void
http_serv_free_cb(uv_timer_t* tim_u)
{
  u3_http* htp_u = tim_u->data;

#if 0
  u3l_log("http serv free cb %d", htp_u->sev_l);
#endif

  _http_serv_really_free(htp_u);

  uv_close((uv_handle_t*)tim_u, _http_close_cb);
}

/* _http_serv_free(): begin to free http server.
*/
static void
_http_serv_free(u3_http* htp_u)
{
#if 0
  u3l_log("http serv free %d", htp_u->sev_l);
#endif

  c3_assert( 0 == htp_u->hon_u );

  if ( 0 == htp_u->h2o_u ) {
    _http_serv_really_free(htp_u);
  }
  else {
    u3_h2o_serv* h2o_u = htp_u->h2o_u;

    _http_h2o_context_dispose(&h2o_u->ctx_u);

    // NOTE: free deferred to allow timers to be closed
    // this is a heavy-handed workaround for the lack of
    // close callbacks in h2o_timer_t
    // it's unpredictable how many event-loop turns will
    // be required to finish closing the underlying uv_timer_t
    // and we can't free until that's done (or we have UB)
    // testing reveals 5s to be a long enough deferral
    uv_timer_t* tim_u = c3_malloc(sizeof(*tim_u));

    tim_u->data = htp_u;

    uv_timer_init(u3L, tim_u);
    uv_timer_start(tim_u, http_serv_free_cb, 5000, 0);
  }
}

/* _http_serv_close_cb(): http server uv_close callback.
*/
static void
_http_serv_close_cb(uv_handle_t* han_u)
{
  u3_http* htp_u = (u3_http*)han_u;
  u3_httd* htd_u = htp_u->htd_u;
  htp_u->liv = c3n;

  // otherwise freed by the last linked connection
  if ( 0 == htp_u->hon_u ) {
    _http_serv_free(htp_u);
  }

  // restart if all linked servers have been shutdown
  {
    htp_u = htd_u->htp_u;
    c3_o res = c3y;

    while ( 0 != htp_u ) {
      if ( c3y == htp_u->liv ) {
        res = c3n;
      }
      htp_u = htp_u->nex_u;
    }

    if ( (c3y == res) && (0 != htd_u->fig_u.for_u) ) {
      _http_serv_start_all(htd_u);
    }
  }
}

/* _http_serv_close(): close http server gracefully.
*/
static void
_http_serv_close(u3_http* htp_u)
{
  u3_h2o_serv* h2o_u = htp_u->h2o_u;
  h2o_context_request_shutdown(&h2o_u->ctx_u);

#if 0
  u3l_log("http serv close %d %p", htp_u->sev_l, &htp_u->wax_u);
#endif

  uv_close((uv_handle_t*)&htp_u->wax_u, _http_serv_close_cb);
}

/* _http_serv_new(): create new http server.
*/
static u3_http*
_http_serv_new(u3_httd* htd_u, c3_s por_s, c3_o sec, c3_o lop)
{
  u3_http* htp_u = c3_malloc(sizeof(*htp_u));

  htp_u->coq_l = 1;
  htp_u->por_s = por_s;
  htp_u->sec = sec;
  htp_u->lop = lop;
  htp_u->liv = c3y;
  htp_u->h2o_u = 0;
  htp_u->hon_u = 0;
  htp_u->nex_u = 0;

  _http_serv_link(htd_u, htp_u);

  return htp_u;
}

/* _http_serv_accept(): accept new http connection.
*/
static void
_http_serv_accept(u3_http* htp_u)
{
  u3_hcon* hon_u = _http_conn_new(htp_u);

  uv_tcp_init(u3L, &hon_u->wax_u);

  c3_i sas_i;

  if ( 0 != (sas_i = uv_accept((uv_stream_t*)&htp_u->wax_u,
                               (uv_stream_t*)&hon_u->wax_u)) ) {
    if ( (u3C.wag_w & u3o_verbose) ) {
      u3l_log("http: accept: %s", uv_strerror(sas_i));
    }

    uv_close((uv_handle_t*)&hon_u->wax_u, _http_conn_free);
    return;
  }

  hon_u->sok_u = h2o_uv_socket_create((uv_stream_t*)&hon_u->wax_u,
                                      _http_conn_free);

  h2o_accept(&((u3_h2o_serv*)htp_u->h2o_u)->cep_u, hon_u->sok_u);

  // capture h2o connection (XX fragile)
  hon_u->con_u = (h2o_conn_t*)hon_u->sok_u->data;

  struct sockaddr_in adr_u;
  h2o_socket_getpeername(hon_u->sok_u, (struct sockaddr*)&adr_u);
  hon_u->ipf_w = ( adr_u.sin_family != AF_INET ) ?
                 0 : ntohl(adr_u.sin_addr.s_addr);
}

/* _http_serv_listen_cb(): uv_connection_cb for uv_listen
*/
static void
_http_serv_listen_cb(uv_stream_t* str_u, c3_i sas_i)
{
  u3_http* htp_u = (u3_http*)str_u;

  if ( 0 != sas_i ) {
    u3l_log("http: listen_cb: %s", uv_strerror(sas_i));
  }
  else {
    _http_serv_accept(htp_u);
  }
}

/* _http_serv_init_h2o(): initialize h2o ctx and handlers for server.
*/
static u3_h2o_serv*
_http_serv_init_h2o(SSL_CTX* tls_u, c3_o log, c3_o red)
{
  u3_h2o_serv* h2o_u = c3_calloc(sizeof(*h2o_u));

  h2o_config_init(&h2o_u->fig_u);
  h2o_u->fig_u.server_name = h2o_iovec_init(
                               H2O_STRLIT("urbit/vere-" URBIT_VERSION));

  //  set maximum request size to 512 MiB
  //
  h2o_u->fig_u.max_request_entity_size = 512 * 1024 * 1024;

  // XX default pending vhost/custom-domain design
  // XX revisit the effect of specifying the port
  h2o_u->hos_u = h2o_config_register_host(&h2o_u->fig_u,
                                          h2o_iovec_init(H2O_STRLIT("default")),
                                          65535);

  h2o_u->cep_u.ctx = (h2o_context_t*)&h2o_u->ctx_u;
  h2o_u->cep_u.hosts = h2o_u->fig_u.hosts;
  h2o_u->cep_u.ssl_ctx = tls_u;

  h2o_u->han_u = h2o_create_handler(&h2o_u->hos_u->fallback_path,
                                    sizeof(*h2o_u->han_u));
  if ( c3y == red ) {
    // XX h2o_redirect_register
    h2o_u->han_u->on_req = _http_rec_accept;
  }
  else {
    h2o_u->han_u->on_req = _http_rec_accept;
  }

  //  register slog stream endpoint
  //
  h2o_pathconf_t* pac_u = h2o_config_register_path(h2o_u->hos_u, "/~_~/slog", 0);
  h2o_handler_t*  han_u = h2o_create_handler(pac_u, sizeof(*han_u));
  han_u->on_req = _http_seq_accept;

  if ( c3y == log ) {
    // XX move this to post serv_start and put the port in the name
#if 0
    c3_c* pax_c = u3_Host.dir_c;
    u3_noun now = u3dc("scot", c3__da, u3k(u3A->now));
    c3_c* now_c = u3r_string(now);
    c3_c* nam_c = ".access.log";
    c3_w len_w = 1 + strlen(pax_c) + 1 + strlen(now_c) + strlen(nam_c);

    c3_c* paf_c = c3_malloc(len_w);
    snprintf(paf_c, len_w, "%s/%s%s", pax_c, now_c, nam_c);

    h2o_access_log_filehandle_t* fil_u =
      h2o_access_log_open_handle(paf_c, 0, H2O_LOGCONF_ESCAPE_APACHE);

    h2o_access_log_register(&h2o_u->hos_u->fallback_path, fil_u);

    c3_free(paf_c);
    c3_free(now_c);
    u3z(now);
#endif
  }

  // XX h2o_compress_register

  h2o_context_init(&h2o_u->ctx_u, u3L, &h2o_u->fig_u);

  return h2o_u;
}

/* _http_serv_start(): start http server.
*/
static void
_http_serv_start(u3_http* htp_u)
{
  struct sockaddr_in adr_u;
  memset(&adr_u, 0, sizeof(adr_u));

  adr_u.sin_family = AF_INET;
  adr_u.sin_addr.s_addr = ( c3y == htp_u->lop ) ?
                          htonl(INADDR_LOOPBACK) :
                          INADDR_ANY;

  uv_tcp_init(u3L, &htp_u->wax_u);

  /*  Try ascending ports.
  */
  while ( 1 ) {
    c3_i sas_i;

    adr_u.sin_port = htons(htp_u->por_s);

    if ( 0 != (sas_i = uv_tcp_bind(&htp_u->wax_u,
                                   (const struct sockaddr*)&adr_u, 0)) ||
         0 != (sas_i = uv_listen((uv_stream_t*)&htp_u->wax_u,
                                 TCP_BACKLOG, _http_serv_listen_cb)) ) {
      if ( (UV_EADDRINUSE == sas_i) || (UV_EACCES == sas_i) ) {
        if ( (c3y == htp_u->sec) && (443 == htp_u->por_s) ) {
          htp_u->por_s = 8443;
        }
        else if ( (c3n == htp_u->sec) && (80 == htp_u->por_s) ) {
          htp_u->por_s = 8080;
        }
        else {
          htp_u->por_s++;
        }

        continue;
      }

      u3l_log("http: listen: %s", uv_strerror(sas_i));

      _http_serv_free(htp_u);
      return;
    }

    u3l_log("http: %s live on %s://localhost:%d",
            (c3y == htp_u->lop) ? "loopback" : "web interface",
            (c3y == htp_u->sec) ? "https" : "http",
            htp_u->por_s);

    break;
  }
}

static uv_buf_t
_http_wain_to_buf(u3_noun wan)
{
  c3_w len_w = u3_mcut_path(0, 0, (c3_c)10, u3k(wan));
  c3_c* buf_c = c3_malloc(1 + len_w);

  u3_mcut_path(buf_c, 0, (c3_c)10, wan);
  buf_c[len_w] = 0;

  return uv_buf_init(buf_c, len_w);
}

/* _http_init_tls: initialize OpenSSL context
*/
static SSL_CTX*
_http_init_tls(uv_buf_t key_u, uv_buf_t cer_u)
{
  // XX require 1.1.0 and use TLS_server_method()
  SSL_CTX* tls_u = SSL_CTX_new(SSLv23_server_method());
  // XX use SSL_CTX_set_max_proto_version() and SSL_CTX_set_min_proto_version()
  SSL_CTX_set_options(tls_u, SSL_OP_NO_SSLv2 |
                             SSL_OP_NO_SSLv3 |
                             // SSL_OP_NO_TLSv1 | // XX test
                             SSL_OP_NO_COMPRESSION);

  SSL_CTX_set_default_verify_paths(tls_u);
  SSL_CTX_set_session_cache_mode(tls_u, SSL_SESS_CACHE_OFF);
  SSL_CTX_set_cipher_list(tls_u,
                          "ECDH+AESGCM:DH+AESGCM:ECDH+AES256:DH+AES256:"
                          "ECDH+AES128:DH+AES:ECDH+3DES:DH+3DES:RSA+AESGCM:"
                          "RSA+AES:RSA+3DES:!aNULL:!MD5:!DSS");

  // enable ALPN for HTTP 2 support
#if 0 //H2O_USE_ALPN
  {
    SSL_CTX_set_ecdh_auto(tls_u, 1);
    h2o_ssl_register_alpn_protocols(tls_u, h2o_http2_alpn_protocols);
  }
#endif

  {
    BIO* bio_u = BIO_new_mem_buf(key_u.base, key_u.len);
    EVP_PKEY* pky_u = PEM_read_bio_PrivateKey(bio_u, 0, 0, 0);
    c3_i sas_i = SSL_CTX_use_PrivateKey(tls_u, pky_u);

    EVP_PKEY_free(pky_u);
    BIO_free(bio_u);

    if( 0 == sas_i ) {
      u3l_log("http: load private key failed:");
      FILE* fil_u = u3_term_io_hija();
      ERR_print_errors_fp(fil_u);
      u3_term_io_loja(1, fil_u);

      SSL_CTX_free(tls_u);

      return 0;
    }
  }

  {
    BIO* bio_u = BIO_new_mem_buf(cer_u.base, cer_u.len);
    X509* xer_u = PEM_read_bio_X509_AUX(bio_u, 0, 0, 0);
    c3_i sas_i = SSL_CTX_use_certificate(tls_u, xer_u);

    X509_free(xer_u);

    if( 0 == sas_i ) {
      u3l_log("http: load certificate failed:");
      FILE* fil_u = u3_term_io_hija();
      ERR_print_errors_fp(fil_u);
      u3_term_io_loja(1,fil_u);

      BIO_free(bio_u);
      SSL_CTX_free(tls_u);

      return 0;
    }

    // get any additional CA certs, ignoring errors
    while ( 0 != (xer_u = PEM_read_bio_X509(bio_u, 0, 0, 0)) ) {
      // XX require 1.0.2 or newer and use SSL_CTX_add0_chain_cert
      SSL_CTX_add_extra_chain_cert(tls_u, xer_u);
    }

    BIO_free(bio_u);
  }

  return tls_u;
}

/* _http_write_ports_file(): update .http.ports
*/
static void
_http_write_ports_file(u3_httd* htd_u, c3_c *pax_c)
{
  c3_c* nam_c = ".http.ports";
  c3_w len_w = 1 + strlen(pax_c) + 1 + strlen(nam_c);

  c3_c* paf_c = c3_malloc(len_w);
  snprintf(paf_c, len_w, "%s/%s", pax_c, nam_c);

  c3_i por_i = open(paf_c, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  c3_free(paf_c);

  u3_http* htp_u = htd_u->htp_u;

  while ( 0 != htp_u ) {
    if ( 0 < htp_u->por_s ) {
      dprintf(por_i, "%u %s %s\n", htp_u->por_s,
                     (c3y == htp_u->sec) ? "secure" : "insecure",
                     (c3y == htp_u->lop) ? "loopback" : "public");
    }

    htp_u = htp_u->nex_u;
  }

  c3_sync(por_i);
  close(por_i);
}

/* _http_release_ports_file(): remove .http.ports
*/
static void
_http_release_ports_file(c3_c *pax_c)
{
  c3_c* nam_c = ".http.ports";
  c3_w len_w = 1 + strlen(pax_c) + 1 + strlen(nam_c);

  c3_c* paf_c = c3_malloc(len_w);
  snprintf(paf_c, len_w, "%s/%s", pax_c, nam_c);

  unlink(paf_c);
  c3_free(paf_c);
}

static u3_hreq*
_http_search_req(u3_httd* htd_u,
                 c3_l     sev_l,
                 c3_l     coq_l,
                 c3_l     seq_l)
{
  u3_http* htp_u;
  u3_hcon* hon_u;
  u3_hreq* req_u;
  c3_w bug_w = u3C.wag_w & u3o_verbose;

  if ( !(htp_u = _http_serv_find(htd_u, sev_l)) ) {
    if ( bug_w ) {
      u3l_log("http: server not found: %x", sev_l);
    }
    return 0;
  }
  else if ( !(hon_u = _http_conn_find(htp_u, coq_l)) ) {
    if ( bug_w ) {
      u3l_log("http: connection not found: %x/%d", sev_l, coq_l);
    }
    return 0;
  }
  else if ( !(req_u = _http_req_find(hon_u, seq_l)) ) {
    if ( bug_w ) {
      u3l_log("http: request not found: %x/%d/%d",
              sev_l, coq_l, seq_l);
    }
    return 0;
  }

  return req_u;
}

/* _http_serv_start_all(): initialize and start servers based on saved config.
*/
static void
_http_serv_start_all(u3_httd* htd_u)
{
  u3_http* htp_u;
  c3_s por_s;

  u3_noun sec = u3_nul;
  u3_noun non = u3_none;

  u3_form* for_u = htd_u->fig_u.for_u;

  c3_assert( 0 != for_u );

  // if the SSL_CTX existed, it'll be freed with the servers
  htd_u->tls_u = 0;

  //  HTTPS server.
  if ( (0 != for_u->key_u.base) && (0 != for_u->cer_u.base) ) {
    htd_u->tls_u = _http_init_tls(for_u->key_u, for_u->cer_u);

    // Note: if tls_u is used for additional servers,
    // its reference count must be incremented with SSL_CTX_up_ref

    if ( 0 != htd_u->tls_u ) {
      por_s = ( c3y == for_u->pro ) ? 8443 : 443;
      htp_u = _http_serv_new(htd_u, por_s, c3y, c3n);
      htp_u->h2o_u = _http_serv_init_h2o(htd_u->tls_u, for_u->log, for_u->red);

      _http_serv_start(htp_u);
      sec = u3nc(u3_nul, htp_u->por_s);
    }
  }

  //  HTTP server.
  {
    por_s = ( c3y == for_u->pro ) ? 8080 : 80;
    htp_u = _http_serv_new(htd_u, por_s, c3n, c3n);
    htp_u->h2o_u = _http_serv_init_h2o(0, for_u->log, for_u->red);

    _http_serv_start(htp_u);
    non = htp_u->por_s;
  }

  //  Loopback server.
  {
    por_s = 12321;
    htp_u = _http_serv_new(htd_u, por_s, c3n, c3y);
    htp_u->h2o_u = _http_serv_init_h2o(0, for_u->log, for_u->red);

    _http_serv_start(htp_u);
  }

  //  send listening ports to %eyre
  {
    c3_assert( u3_none != non );

    //  XX remove [sen]
    //
    u3_noun wir = u3nt(u3i_string("http-server"),
                       u3dc("scot", c3__uv, htd_u->sev_l),
                       u3_nul);
    u3_noun cad = u3nt(c3__live, non, sec);

    u3_auto_plan(&htd_u->car_u, u3_ovum_init(0, c3__e, wir, cad));
  }

  _http_write_ports_file(htd_u, u3_Host.dir_c);
  _http_form_free(htd_u);
}

/* _http_serv_restart(): gracefully shutdown, then start servers.
*/
static void
_http_serv_restart(u3_httd* htd_u)
{
  u3_http* htp_u = htd_u->htp_u;

  if ( 0 == htp_u ) {
    _http_serv_start_all(htd_u);
  }
  else {
    u3l_log("http: restarting servers to apply configuration");

    while ( 0 != htp_u ) {
      if ( c3y == htp_u->liv ) {
        _http_serv_close(htp_u);
      }
      htp_u = htp_u->nex_u;
    }

    _http_release_ports_file(u3_Host.dir_c);
  }
}

/* _http_form_free(): free and unlink saved config.
*/
static void
_http_form_free(u3_httd* htd_u)
{
  u3_form* for_u = htd_u->fig_u.for_u;

  if ( 0 == for_u ) {
    return;
  }

  if ( 0 != for_u->key_u.base ) {
    c3_free(for_u->key_u.base);
  }

  if ( 0 != for_u->cer_u.base ) {
    c3_free(for_u->cer_u.base);
  }

  c3_free(for_u);
  htd_u->fig_u.for_u = 0;
}

/* u3_http_ef_form(): apply configuration, restart servers.
*/
void
u3_http_ef_form(u3_httd* htd_u, u3_noun fig)
{
  u3_noun sec, pro, log, red;

  if ( (c3n == u3r_qual(fig, &sec, &pro, &log, &red) ) ||
       // confirm sec is a valid (unit ^)
       !( u3_nul == sec || ( c3y == u3du(sec) &&
                             c3y == u3du(u3t(sec)) &&
                             u3_nul == u3h(sec) ) ) ||
       // confirm valid flags ("loobeans")
       !( c3y == pro || c3n == pro ) ||
       !( c3y == log || c3n == log ) ||
       !( c3y == red || c3n == red ) ) {
    u3l_log("http: form: invalid card");
    u3z(fig);
    return;
  }

  u3_form* for_u = c3_malloc(sizeof(*for_u));
  for_u->pro = (c3_o)pro;
  for_u->log = (c3_o)log;
  for_u->red = (c3_o)red;

  if ( u3_nul != sec ) {
    u3_noun key = u3h(u3t(sec));
    u3_noun cer = u3t(u3t(sec));

    for_u->key_u = _http_wain_to_buf(u3k(key));
    for_u->cer_u = _http_wain_to_buf(u3k(cer));
  }
  else {
    for_u->key_u = uv_buf_init(0, 0);
    for_u->cer_u = uv_buf_init(0, 0);
  }

  u3z(fig);
  _http_form_free(htd_u);

  htd_u->fig_u.for_u = for_u;

  _http_serv_restart(htd_u);

  htd_u->car_u.liv_o = c3y;
}

/* _http_io_talk(): start http I/O.
*/
static void
_http_io_talk(u3_auto* car_u)
{
  u3_httd* htd_u = (u3_httd*)car_u;

  //  XX remove [sen]
  //
  u3_noun wir = u3nt(u3i_string("http-server"),
                     u3dc("scot", c3__uv, htd_u->sev_l),
                     u3_nul);
  u3_noun cad = u3nc(c3__born, u3_nul);

  u3_auto_plan(car_u, u3_ovum_init(0, c3__e, wir, cad));

  //  XX set liv_o on done/swap?
  //
}

/* _http_ef_http_server(): dispatch an %http-server effect from %light.
*/
void
_http_ef_http_server(u3_httd* htd_u,
                     c3_l     sev_l,
                     c3_l     coq_l,
                     c3_l     seq_l,
                     u3_noun    tag,
                     u3_noun    dat)
{
  u3_hreq* req_u;

  //  sets server configuration
  //
  if ( c3y == u3r_sing_c("set-config", tag) ) {
    u3_http_ef_form(htd_u, u3k(dat));
  }
  //  responds to an open request
  //
  else if ( 0 != (req_u = _http_search_req(htd_u, sev_l, coq_l, seq_l)) ) {
    if ( c3y == u3r_sing_c("response", tag) ) {
      u3_noun response = dat;

      if ( c3y == u3r_sing_c("start", u3h(response)) ) {
        //  Separate the %start message into its components.
        //
        u3_noun response_header, data, complete;
        u3_noun status, headers;
        u3x_trel(u3t(response), &response_header, &data, &complete);
        u3x_cell(response_header, &status, &headers);

        _http_start_respond(req_u, u3k(status), u3k(headers), u3k(data),
                            u3k(complete));
      }
      else if ( c3y == u3r_sing_c("continue", u3h(response)) ) {
        //  Separate the %continue message into its components.
        //
        u3_noun data, complete;
        u3x_cell(u3t(response), &data, &complete);

        _http_continue_respond(req_u, u3k(data), u3k(complete));
      }
      else if (c3y == u3r_sing_c("cancel", u3h(response))) {
        u3l_log("http: %%cancel not handled yet");
      }
      else {
        u3l_log("http: strange response");
      }
    }
    else {
      u3l_log("http: strange response");
    }
  }

  u3z(tag);
  u3z(dat);
}

/* _http_stream_slog(): emit slog to open connections
*/
static void
_http_stream_slog(void* vop_p, c3_w pri_w, u3_noun tan)
{
  u3_httd* htd_u = (u3_httd*)vop_p;
  u3_hreq* seq_u = htd_u->fig_u.seq_u;

  //  only do the work if there are open slog streams
  //
  if ( 0 != seq_u ) {
    u3_weak data = u3_none;

    if ( c3y == u3a_is_atom(tan) ) {
      u3_noun lin = u3i_list(u3i_string("data:"),
                             u3k(tan),
                             c3_s2('\n', '\n'),
                             u3_none);
      u3_atom txt = u3qc_rap(3, lin);
      data = u3nt(u3_nul, u3r_met(3, txt), txt);
      u3z(lin);
    }
    else {
      u3_weak wol = u3_none;

      //  if we have no arvo kernel and can't evaluate nock,
      //  only send %leaf tanks
      //
      if ( 0 == u3A->roc ) {
        if ( c3__leaf == u3h(tan) ) {
          wol = u3nc(u3k(u3t(tan)), u3_nul);
        }
      }
      else {
        u3_noun blu = u3_term_get_blew(0);
        c3_l  col_l = u3h(blu);
        wol = u3dc("wash", u3nc(0, col_l), u3k(tan));
        u3z(blu);
      }

      if ( u3_none != wol ) {
        u3_noun low = wol;
        u3_noun paz = u3_nul;
        while ( u3_nul != low ) {
          u3_noun lin = u3i_list(u3i_string("data:"),
                                 u3qc_rap(3, u3h(low)),
                                 c3_s2('\n', '\n'),
                                 u3_none);
          paz = u3kb_weld(paz, lin);
          low = u3t(low);
        }
        u3_atom txt = u3qc_rap(3, paz);
        data = u3nt(u3_nul, u3r_met(3, txt), txt);
        u3z(paz);
      }

      u3z(wol);
    }

    if ( u3_none != data ) {
      while ( 0 != seq_u ) {
        _http_continue_respond(seq_u, u3k(data), c3n);
        seq_u = seq_u->nex_u;
      }
    }

    u3z(data);
  }

  u3z(tan);
}

/* _http_seq_heartbeat_cb(): send heartbeat to slog streams and restart timer
*/
static void
_http_seq_heartbeat_cb(uv_timer_t* tim_u)
{
  u3_httd* htd_u = tim_u->data;
  u3_hreq* seq_u = htd_u->fig_u.seq_u;

  if ( 0 != seq_u ) {
    u3_noun dat = u3nt(u3_nul, 1, c3_s1('\n'));
    while ( 0 != seq_u ) {
      _http_continue_respond(seq_u, u3k(dat), c3n);
      seq_u = seq_u->nex_u;
    }
    u3z(dat);
  }

  uv_timer_start(htd_u->fig_u.sit_u, _http_seq_heartbeat_cb,
                 HEARTBEAT_TIMEOUT, 0);
}

/* _reck_mole(): parse simple atomic mole.
*/
static u3_noun
_reck_mole(u3_noun  fot,
           u3_noun  san,
           c3_d*    ato_d)
{
  u3_noun uco = u3dc("slaw", fot, san);
  u3_noun p_uco, q_uco;

  if ( (c3n == u3r_cell(uco, &p_uco, &q_uco)) ||
       (u3_nul != p_uco) )
  {
    u3l_log("strange mole %s", u3r_string(san));

    u3z(fot); u3z(uco); return c3n;
  }
  else {
    *ato_d = u3r_chub(0, q_uco);

    u3z(fot); u3z(uco); return c3y;
  }
}

/* _reck_lily(): parse little atom.
*/
static u3_noun
_reck_lily(u3_noun fot, u3_noun txt, c3_l* tid_l)
{
  c3_d ato_d;

  if ( c3n == _reck_mole(fot, txt, &ato_d) ) {
    return c3n;
  } else {
    if ( ato_d >= 0x80000000ULL ) {
      return c3n;
    } else {
      *tid_l = (c3_l) ato_d;

      return c3y;
    }
  }
}

/* _http_io_kick(): apply effects.
*/
static c3_o
_http_io_kick(u3_auto* car_u, u3_noun wir, u3_noun cad)
{
  u3_httd* htd_u = (u3_httd*)car_u;

  u3_noun tag, dat, i_wir, t_wir;

  if (  (c3n == u3r_cell(wir, &i_wir, &t_wir))
     || (c3n == u3r_cell(cad, &tag, &dat))
     || (c3n == u3r_sing_c("http-server", i_wir)) )
  {
    u3z(wir); u3z(cad);
    return c3n;
  }

  //  XX this needs to be rewritten, it defers (c3n) in cases it should not
  //
  {
    u3_noun pud = t_wir;
    u3_noun p_pud, t_pud, tt_pud, q_pud, r_pud, s_pud;
    c3_l    sev_l, coq_l, seq_l;


    if ( (c3n == u3r_cell(pud, &p_pud, &t_pud)) ||
         (c3n == _reck_lily(c3__uv, u3k(p_pud), &sev_l)) )
    {
      u3z(wir); u3z(cad);
      return c3n;
    }

    if ( u3_nul == t_pud ) {
      coq_l = seq_l = 0;
    }
    else {
      if ( (c3n == u3r_cell(t_pud, &q_pud, &tt_pud)) ||
           (c3n == _reck_lily(c3__ud, u3k(q_pud), &coq_l)) )
      {
        u3z(wir); u3z(cad);
        return c3n;
      }

      if ( u3_nul == tt_pud ) {
        seq_l = 0;
      } else {
        if ( (c3n == u3r_cell(tt_pud, &r_pud, &s_pud)) ||
             (u3_nul != s_pud) ||
             (c3n == _reck_lily(c3__ud, u3k(r_pud), &seq_l)) )
        {
          u3z(wir); u3z(cad);
          return c3n;
        }
      }
    }

    _http_ef_http_server(htd_u, sev_l, coq_l, seq_l, u3k(tag), u3k(dat));
    u3z(wir); u3z(cad);
    return c3y;
  }
}

/* _http_io_exit(): shut down http.
*/
static void
_http_io_exit(u3_auto* car_u)
{
  u3_httd* htd_u = (u3_httd*)car_u;

  //  dispose of configuration to avoid restarts
  //
  _http_form_free(htd_u);

  //  close all servers
  //
  //  XX broken
  //
  // for ( u3_http* htp_u = htd_u->htp_u; htp_u; htp_u = htp_u->nex_u ) {
  //   _http_serv_close(htp_u);
  // }

  {
    u3_atom lin = u3i_string("data:urbit shutting down\n\n");
    u3_noun dat = u3nt(u3_nul, u3r_met(3, lin), lin);
    u3_hreq* seq_u = htd_u->fig_u.seq_u;
    while ( 0 != seq_u ) {
      _http_continue_respond(seq_u, u3k(dat), c3y);
      seq_u = seq_u->nex_u;
    }
    u3z(dat);
  }

  _http_release_ports_file(u3_Host.dir_c);
}

/* _http_io_info(): print status info.
*/
static void
_http_io_info(u3_auto* car_u)
{
  u3_httd* htd_u = (u3_httd*)car_u;
  c3_y sec_y = 0;
  u3_hreq* seq_u = htd_u->fig_u.seq_u;
  while ( 0 != seq_u ) {
    sec_y++;
    seq_u = seq_u->nex_u;
  }
  u3l_log("      open slogstreams: %d", sec_y);
}

/* u3_http_io_init(): initialize http I/O.
*/
u3_auto*
u3_http_io_init(u3_pier* pir_u)
{
  u3_httd* htd_u = c3_calloc(sizeof(*htd_u));

  u3_auto* car_u = &htd_u->car_u;
  car_u->nam_m = c3__http;
  car_u->liv_o = c3n;
  car_u->io.talk_f = _http_io_talk;
  car_u->io.info_f = _http_io_info;
  car_u->io.kick_f = _http_io_kick;
  car_u->io.exit_f = _http_io_exit;

  pir_u->sop_p = htd_u;
  pir_u->sog_f = _http_stream_slog;

  uv_timer_t* sit_u = c3_malloc(sizeof(*sit_u));
  sit_u->data = htd_u;
  uv_timer_init(u3L, sit_u);
  uv_timer_start(sit_u, _http_seq_heartbeat_cb, HEARTBEAT_TIMEOUT, 0);
  htd_u->fig_u.sit_u = sit_u;

  {
    u3_noun now;
    struct timeval tim_u;
    gettimeofday(&tim_u, 0);

    now = u3_time_in_tv(&tim_u);
    htd_u->sev_l = u3r_mug(now);
    u3z(now);
  }

  //  XX retry up to N?
  //
  // car_u->ev.bail_f = ...;

  return car_u;
}
