/* v/http.c
**
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>

#include "all.h"
#include "vere/vere.h"

#include "h2o.h"

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/rand.h>

// XX keep here (or in new http.h?)
typedef struct _h2o_ctx_wrap {
  h2o_context_t     cxt_u;            //  h2o ctx
  struct _u3_http*    htp_u;            //  server backlink
} h2o_ctx_wrap;

// XX rename
typedef struct _h2hed {
  struct _h2hed* nex_u;
  c3_w           nam_w;
  c3_c*          nam_c;
  c3_w           val_w;
  c3_c*          val_c;
} h2hed;

/* forward declarations
*/
static u3_noun _http_pox_to_noun(c3_w sev_l, c3_w coq_l, c3_w seq_l);
static void _http_request(u3_hreq* req_u, u3_noun recq);
static void _http_request_kill(u3_hreq* req_u);
static void _http_req_link(u3_hcon* hon_u, u3_hreq* req_u);
static void _http_req_unlink(u3_hreq* req_u);
static void _http_conn_link(u3_http* htp_u, u3_hcon* hon_u);
static void _http_conn_unlink(u3_hcon* hon_u);

static const c3_i TCP_BACKLOG = 16;

// XX put this on u3_host ?
static h2o_globalconf_t fig_u;

// XX u3_Host.tls_u ?
static SSL_CTX* tls_u = 0;

/* _http_vec_to_meth(): convert h2o_iovec_t to meth
*/
static u3_weak
_http_vec_to_meth(h2o_iovec_t vec_u)
{
  return ( 0 == strncmp(vec_u.base, "GET",     vec_u.len) ) ? c3__get  :
         ( 0 == strncmp(vec_u.base, "PUT",     vec_u.len) ) ? c3__put  :
         ( 0 == strncmp(vec_u.base, "POST",    vec_u.len) ) ? c3__post :
         ( 0 == strncmp(vec_u.base, "HEAD",    vec_u.len) ) ? c3__head :
         ( 0 == strncmp(vec_u.base, "CONNECT", vec_u.len) ) ? c3__conn :
         ( 0 == strncmp(vec_u.base, "DELETE",  vec_u.len) ) ? c3__delt :
         ( 0 == strncmp(vec_u.base, "OPTIONS", vec_u.len) ) ? c3__opts :
         ( 0 == strncmp(vec_u.base, "TRACE",   vec_u.len) ) ? c3__trac :
         // TODO ??
         // ( 0 == strncmp(vec_u.base, "PATCH",   vec_u.len) ) ? c3__patc :
         u3_none;
}

/* _http_vec_to_atom(): convert h2o_iovec_t to atom (cord)
*/
static u3_noun
_http_vec_to_atom(h2o_iovec_t vec_u)
{
  // XX portable?
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

/* _http_vec_from_octs(): convert (unit octs) to h2o_iovec_t
*/
static h2o_iovec_t*
_http_vec_from_octs(u3_noun oct)
{
  h2o_iovec_t* vec_u = c3_malloc(sizeof(*vec_u));

  if ( u3_nul == oct ) {
    vec_u->base = 0;
    vec_u->len = 0;
  }
  else {
    // XX bails
    // if ( !_(u3a_is_cat(u3t(oct))) ) {
    //   //  2GB max
    //   u3m_bail(c3__fail); return 0;
    // }

    vec_u->len = u3h(u3t(oct));
    vec_u->base = c3_malloc(vec_u->len);
    u3r_bytes(0, vec_u->len, (c3_y*)vec_u->base, u3t(u3t(oct)));
  }

  u3z(oct);
  return vec_u;
}

/* _http_heds_to_noun(): convert h2o_headers_t to (list (pair @t @t))
*/
static u3_noun
_http_heds_to_noun(h2o_headers_t* hed_u)
{
  u3_noun hed = u3_nul;
  size_t dex = hed_u->size;

  h2o_header_t deh;

  while ( 0 < dex ) {
    deh = hed_u->entries[--dex];
    hed = u3nc(u3nc(_http_vec_to_atom(*deh.name),
                    _http_vec_to_atom(deh.value)), hed);
  }

  return hed;
}

/* _http_heds_from_noun(): convert (list (pair @t @t)) to h2hed
*/
static h2hed*
_http_heds_from_noun(u3_noun hed)
{
  u3_noun deh = hed;
  h2hed* hed_u = 0;

  while ( u3_nul != hed ) {
    u3_noun nam = u3h(u3h(hed));
    u3_noun val = u3t(u3h(hed));

    h2hed* nex_u = c3_malloc(sizeof(*nex_u));
    nex_u->nam_w = u3r_met(3, nam);
    nex_u->val_w = u3r_met(3, val);
    nex_u->nam_c = c3_malloc(nex_u->nam_w);
    nex_u->val_c = c3_malloc(nex_u->val_w);

    u3r_bytes(0, nex_u->nam_w, (c3_y*)nex_u->nam_c, nam);
    u3r_bytes(0, nex_u->val_w, (c3_y*)nex_u->val_c, val);

    nex_u->nex_u = hed_u;
    hed_u = nex_u;
    hed = u3t(hed);
  }

  u3z(deh);
  return hed_u;
}

/* _http_req_to_httq(): convert h2o_req_t to httq
*/
static u3_weak
_http_req_to_httq(h2o_req_t* rec_u)
{
  u3_noun med = _http_vec_to_meth(rec_u->method);

  if ( u3_none == med ) {
    return u3_none;
  }

  u3_noun url = _http_vec_to_atom(rec_u->path);
  u3_noun hed = _http_heds_to_noun(&rec_u->headers);

  // restore host header
  hed = u3nc(u3nc(u3i_string("host"),
                  _http_vec_to_atom(rec_u->authority)),
             hed);

  u3_noun bod = _http_vec_to_octs(rec_u->entity);

  return u3nq(med, url, hed, bod);
}

/* _http_req_free(): free http request.
*/
static void
_http_req_free(u3_hreq* req_u)
{
  _http_req_unlink(req_u);
  free(req_u);
}

/* _http_req_new(): receive http request.
*/
static u3_hreq*
_http_req_new(u3_hcon* hon_u, h2o_req_t* rec_u)
{
  // unnecessary, just an example
#if 0
  h2o_ctx_wrap* ctx_u = (h2o_ctx_wrap*)rec_u->conn->ctx;
  u3_http* htp_u = ctx_u->htp_u;
#endif

  u3_hreq* req_u = c3_malloc(sizeof(*req_u));
  req_u->rec_u = rec_u;
  _http_req_link(hon_u, req_u);

  return req_u;
}

/* _http_send_response(): write httr to h2o_req_t->res and send
*/
static void
_http_send_response(u3_hreq* req_u, u3_noun sas, u3_noun hed, u3_noun bod)
{
  h2o_req_t* rec_u = req_u->rec_u;

  rec_u->res.status = sas;
  rec_u->res.reason = (sas < 200) ? "Weird" :
                      (sas < 300) ? "OK" :
                      (sas < 400) ? "Moved" :
                      (sas < 500) ? "Missing" :
                      "Hosed";

  h2hed* hed_u = _http_heds_from_noun(u3k(hed));
  h2hed* deh_u = hed_u;

  while ( 0 != hed_u ) {
    h2o_add_header_by_str(&rec_u->pool, &rec_u->res.headers,
                          hed_u->nam_c, hed_u->nam_w, 0, 0,
                          hed_u->val_c, hed_u->val_w);
    hed_u = hed_u->nex_u;
  }

  // XX free req_u on disponse (rec_u should be free'd by h2o)
  static h2o_generator_t gen_u = {NULL, NULL};
  h2o_start_response(rec_u, &gen_u);

  // TODO: put this in the generator so it runs on next turn?
  h2o_iovec_t* bod_u = _http_vec_from_octs(u3k(bod));
  h2o_send(rec_u, bod_u, 1, 1);

  _http_req_free(req_u);

  // XX allocate on &req_u->pool and skip these?
  free(bod_u->base);
  free(bod_u);
  while ( 0 != deh_u ) {
    h2hed* duh_u = deh_u;
    deh_u = deh_u->nex_u;
    free(duh_u->nam_c);
    free(duh_u->val_c);
    free(duh_u);
  }

  u3z(sas); u3z(hed); u3z(bod);
}

// for casting and retrieving h2o_socket_t; see st_h2o_http1_conn_t
typedef struct _h2o_con_http1 {
  h2o_conn_t       con_u;             //  h2o connection
  h2o_socket_t*    sok_u;             //  h2o connection socket
} h2o_con_http1;

// for casting and retrieving u3_hcon; see st_h2o_uv_socket_t
typedef struct _h2o_sok_uv {
  h2o_socket_t     sok_u;             //  h2o connection socket
  struct {
    uv_stream_t *stream;              //  client stream handler (u3_hcon)
    uv_close_cb close_cb;
  } uv;
} h2o_sok_uv;

/* _http_conn_from_req(); retrieve connection from h2o http1 request.
*/
static u3_hcon*
_http_conn_from_req(h2o_req_t* rec_u)
{
  // XX HTTP2 wat do?
  h2o_con_http1* noc_u = (h2o_con_http1*)rec_u->conn;
  h2o_sok_uv* kos_u = (h2o_sok_uv*)noc_u->sok_u;
  return (u3_hcon*)kos_u->uv.stream;
};

/* _http_handle_new_req(); handle incoming http request from h2o.
*/
static c3_i
_http_handle_new_req(h2o_handler_t* han_u, h2o_req_t* rec_u)
{
  u3_weak recq = _http_req_to_httq(rec_u);

  if ( u3_none == recq ) {
    // XX debug only
    uL(fprintf(uH, "strange request\n"));

    static h2o_generator_t gen_u = {NULL, NULL};
    rec_u->res.status = 400;
    rec_u->res.reason = "Bad Request";
    h2o_start_response(rec_u, &gen_u);
    h2o_send(rec_u, 0, 0, 1);
  }
  else {
    u3_hcon* hon_u = _http_conn_from_req(rec_u);
    u3_hreq* req_u = _http_req_new(hon_u, rec_u);
    _http_request(req_u, recq);
  }

  return 0;
}

/* _http_conn_free_early(): free http connection on failure.
*/
static void
_http_conn_free_early(uv_handle_t* han_t)
{
  u3_hcon* hon_u = (u3_hcon*)han_t;
  free(hon_u);
}

/* _http_conn_free(): free http connection on close.
*/
static void
_http_conn_free(uv_handle_t* han_t)
{
  u3_hcon* hon_u = (u3_hcon*)han_t;

  while ( 0 != hon_u->req_u ) {
    u3_hreq* req_u = hon_u->req_u;
    u3_hreq* nex_u = req_u->nex_u;

    _http_request_kill(req_u);
    _http_req_free(req_u);
    hon_u->req_u = nex_u;
  }

  _http_conn_unlink(hon_u);
  free(hon_u);
}

/* _http_conn_new(): create and accept http connection.
*/
static void
_http_conn_new(u3_http* htp_u)
{
  // TODO where?
  // u3_lo_open();

  u3_hcon* hon_u = c3_malloc(sizeof(*hon_u));
  hon_u->seq_l = 1;
  hon_u->req_u = 0;

  uv_tcp_init(u3L, &hon_u->wax_u);

  if ( 0 != uv_accept((uv_stream_t*)&htp_u->wax_u,
                      (uv_stream_t*)&hon_u->wax_u) ) {
    uv_close((uv_handle_t*)&hon_u->wax_u,
             (uv_close_cb)_http_conn_free_early);
    return;
  }

  _http_conn_link(htp_u, hon_u);

  hon_u->sok_u = h2o_uv_socket_create((uv_stream_t*)&hon_u->wax_u,
                                      (uv_close_cb)_http_conn_free);
  h2o_accept(htp_u->cep_u, hon_u->sok_u);

  // capture h2o connection (XX fragile)
  hon_u->con_u = (h2o_conn_t*)hon_u->sok_u->data;

  struct sockaddr_in adr_u;
  h2o_socket_getpeername(hon_u->sok_u, (struct sockaddr*)&adr_u);
  hon_u->ipf_w = ( adr_u.sin_family != AF_INET ) ?
                 0 : ntohl(adr_u.sin_addr.s_addr);

  // TODO where?
  // u3_lo_shut(c3y);
}

/* _http_listen_cb(): uv_connection_cb for uv_listen
*/
static void
_http_listen_cb(uv_stream_t* str_u, c3_i sas_i)
{
  u3_http* htp_u = (u3_http*)str_u;

  if ( 0 != sas_i ) {
    // XX retrieve and print error?
    uL(fprintf(uH, "http: listen_cb: ERROR %d\n", sas_i));
  }
  else {
    _http_conn_new(htp_u);
  }
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
  hon_u->req_u = req_u;
}

/* _http_req_unlink(): remove http request from connection
*/
static void
_http_req_unlink(u3_hreq* req_u)
{
  u3_hcon* hon_u = req_u->hon_u;

  if ( hon_u->req_u == req_u ) {
    hon_u->req_u = req_u->nex_u;
  }
  else {
    u3_hreq* pre_u = hon_u->req_u;

    //  XX glories of linear search
    //
    while ( pre_u ) {
      if ( pre_u->nex_u == req_u ) {
        pre_u->nex_u = req_u->nex_u;
      }
      else pre_u = pre_u->nex_u;
    }
  }
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
  htp_u->hon_u = hon_u;
}

/* _http_conn_unlink(): remove http request from connection
*/
static void
_http_conn_unlink(u3_hcon* hon_u)
{
  u3_http* htp_u = hon_u->htp_u;

  if ( htp_u->hon_u == hon_u ) {
    htp_u->hon_u = hon_u->nex_u;
  }
  else {
    u3_hcon *pre_u = htp_u->hon_u;

    //  XX glories of linear search
    //
    while ( pre_u ) {
      if ( pre_u->nex_u == hon_u ) {
        pre_u->nex_u = hon_u->nex_u;
      }
      else pre_u = pre_u->nex_u;
    }
  }
}

/* _http_serv_find(): find http server by sequence.
*/
static u3_http*
_http_serv_find(c3_l sev_l)
{
  u3_http* htp_u = u3_Host.htp_u;

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

// XX serv_link and serv_unlink

// XX rename
/* _http_pox_to_noun(): translate srv/con/req to path noun (pox).
*/
static u3_noun
_http_pox_to_noun(c3_w sev_l, c3_w coq_l, c3_w seq_l)
{
  return u3nt(u3_blip, c3__http,
              u3nq(u3dc("scot", c3_s2('u','v'), sev_l),
                   u3dc("scot", c3_s2('u','d'), coq_l),
                   u3dc("scot", c3_s2('u','d'), seq_l),
                   u3_nul));
}

/* _http_request(): dispatch http request to %eyre
*/
static void
_http_request(u3_hreq* req_u, u3_noun recq)
{
#if 1
  uL(fprintf(uH, "new request:\n"));
  u3_noun span = u3v_wish("-:!>(*{@t @t (list {p/@t q/@t}) (unit {p/@ q/@})})");
  u3m_tape(u3dc("text", span, u3k(recq)));
  uL(fprintf(uH, "\n"));
#endif

  u3_noun pox = _http_pox_to_noun(req_u->hon_u->htp_u->sev_l,
                                  req_u->hon_u->coq_l,
                                  req_u->seq_l);

  u3_noun typ = _(req_u->hon_u->htp_u->lop) ? c3__chis : c3__this;

  u3v_plan(pox, u3nq(typ,
                     req_u->hon_u->htp_u->sec,
                     u3nc(c3y, u3i_words(1, &req_u->hon_u->ipf_w)),
                     recq));
}

/* _http_request_kill(): kill http request in %eyre.
*/
static void
_http_request_kill(u3_hreq* req_u)
{
  u3_noun pox = _http_pox_to_noun(req_u->hon_u->htp_u->sev_l,
                                  req_u->hon_u->coq_l,
                                  req_u->seq_l);

  u3v_plan(pox, u3nc(c3__thud, u3_nul));
}

/* _http_respond(): attach %eyre response to open request.
*/
static void
_http_respond(c3_l sev_l, c3_l coq_l, c3_l seq_l, u3_noun rep)
{
  u3_http* htp_u;
  u3_hcon* hon_u;
  u3_hreq* req_u;

  if ( !(htp_u = _http_serv_find(sev_l)) ) {
    if ( (u3C.wag_w & u3o_verbose) ) {
      uL(fprintf(uH, "http: server not found: %x\r\n", sev_l));
    }
    u3z(rep);
    return;
  }
  if ( !(hon_u = _http_conn_find(htp_u, coq_l)) ) {
    if ( (u3C.wag_w & u3o_verbose) ) {
      uL(fprintf(uH, "http: connection not found: %x/%d\r\n", sev_l, coq_l));
    }
    u3z(rep);
    return;
  }
  if ( !(req_u = _http_req_find(hon_u, seq_l)) ) {
    if ( (u3C.wag_w & u3o_verbose) ) {
      uL(fprintf(uH, "http: request not found: %x/%d/%d\r\n", sev_l, coq_l, seq_l));
    }
    u3z(rep);
    return;
  }

  {
    u3_noun p_rep, q_rep, r_rep;

    if ( c3n == u3r_trel(rep, &p_rep, &q_rep, &r_rep) ) {
      uL(fprintf(uH, "http: strange response\n"));
    }
    else {
      _http_send_response(req_u, u3k(p_rep), u3k(q_rep), u3k(r_rep));
    }
  }

  u3z(rep);
}

/* _http_init_h2o(): initialize h2o ctx and handlers for server.
*/
static void
_http_init_h2o(u3_http* htp_u)
{
  // wrapped for server backlink (wrapper unnecessary, just an example)
  htp_u->ctx_u = c3_malloc(sizeof(h2o_ctx_wrap));
  memset(htp_u->ctx_u, 0, sizeof(h2o_ctx_wrap));
  h2o_ctx_wrap* ctx_u = (h2o_ctx_wrap*)htp_u->ctx_u;
  ctx_u->htp_u = htp_u;

  //// FAILED ATTEMPTS:
  //   can't wrap handler and link server, see pointer math in request.c call_handlers
  //   can't use htp_u->ctx_u->_module_configs (don't know why)

  htp_u->cep_u = c3_malloc(sizeof(*htp_u->cep_u));
  memset(htp_u->cep_u, 0, sizeof(*htp_u->cep_u));

  htp_u->cep_u->ctx = (h2o_context_t*)htp_u->ctx_u;
  htp_u->cep_u->hosts = fig_u.hosts;

  if ( c3y == htp_u->sec ) {
    htp_u->cep_u->ssl_ctx = tls_u;
  }

  // XX read name from server?
  htp_u->hos_u = h2o_config_register_host(&fig_u,
                                          h2o_iovec_init(H2O_STRLIT("default")),
                                          htp_u->por_w);

  // XX attach to server?
  h2o_handler_t* han_u = h2o_create_handler(&htp_u->hos_u->fallback_path, sizeof(*han_u));
  han_u->on_req = _http_handle_new_req;

  // XX handler lifecycle
  // han_u->on_context_init
  // han_u->on_context_dispose
  // han_u->dispose

  h2o_context_init((h2o_context_t*)htp_u->ctx_u, u3L, &fig_u);
}

/* _http_start(): start http server.
*/
static void
_http_start(u3_http* htp_u)
{
  struct sockaddr_in adr_u;
  memset(&adr_u, 0, sizeof(adr_u));
  adr_u.sin_family = AF_INET;

  if ( c3y == htp_u->lop ) {
    inet_pton(AF_INET, "127.0.0.1", &adr_u.sin_addr);
  }
  else {
    adr_u.sin_addr.s_addr = INADDR_ANY;
  }

  if ( c3y == htp_u->sec && 0 == tls_u ) {
    uL(fprintf(uH, "secure server error %u: no tls config\n", htp_u->sev_l));
    c3_assert(0);
  }

  uv_tcp_init(u3L, &htp_u->wax_u);

  /*  Try ascending ports.
  */
  while ( 1 ) {
    c3_i ret;

    adr_u.sin_port = htons(htp_u->por_w);

    // XX maybe don't check uv_tcp_bind ret
    if ( 0 != (ret = uv_tcp_bind(&htp_u->wax_u, (const struct sockaddr*)&adr_u, 0)) ||
         0 != (ret = uv_listen((uv_stream_t*)&htp_u->wax_u,
                               TCP_BACKLOG, _http_listen_cb)) ) {
      if ( UV_EADDRINUSE == ret ) {
        htp_u->por_w++;
        continue;
      }
      else {
        uL(fprintf(uH, "http: listen: %s\n", uv_strerror(ret)));
      }
    }

    uL(fprintf(uH, "http: live (%s, %s) on %d\n",
                   (c3y == htp_u->sec) ? "secure" : "insecure",
                   (c3y == htp_u->lop) ? "loopback" : "public",
                   htp_u->por_w));

    _http_init_h2o(htp_u);
    break;
  }
}

/* _http_init_tls: initialize OpenSSL context
*/
static SSL_CTX*
_http_init_tls()
{
  SSL_CTX* tls_u = c3_malloc(sizeof(*tls_u));

  SSL_library_init();
  SSL_load_error_strings();

  tls_u = SSL_CTX_new(TLSv1_2_server_method());

  SSL_CTX_set_options(tls_u, SSL_OP_NO_SSLv2);
  // SSL_CTX_set_verify(tls_u, SSL_VERIFY_NONE, NULL);
  SSL_CTX_set_default_verify_paths(tls_u);
  SSL_CTX_set_session_cache_mode(tls_u, SSL_SESS_CACHE_OFF);
  SSL_CTX_set_cipher_list(tls_u,
                          "ECDH+AESGCM:DH+AESGCM:ECDH+AES256:DH+AES256:"
                          "ECDH+AES128:DH+AES:ECDH+3DES:DH+3DES:RSA+AESGCM:"
                          "RSA+AES:RSA+3DES:!aNULL:!MD5:!DSS");

  c3_c pub_c[2048];
  c3_c pir_c[2048];
  c3_i ret_i;

  ret_i = snprintf(pub_c, 2048, "%s/.urb/tls/certificate.pem", u3_Host.dir_c);
  c3_assert(ret_i < 2048);
  ret_i = snprintf(pir_c, 2048, "%s/.urb/tls/private.pem", u3_Host.dir_c);
  c3_assert(ret_i < 2048);

  // TODO: SSL_CTX_use_certificate_chain_file ?
  if (SSL_CTX_use_certificate_file(tls_u, pub_c, SSL_FILETYPE_PEM) <= 0) {
    uL(fprintf(uH, "https: failed to load certificate\n"));
    // c3_assert(0);
    return 0;
  }

  if (SSL_CTX_use_PrivateKey_file(tls_u, pir_c, SSL_FILETYPE_PEM) <= 0 ) {
    uL(fprintf(uH, "https: failed to load private key\n"));
    // c3_assert(0);
    return 0;
  }

  return tls_u;
}

/* _http_write_ports_file(): update .http.ports
*/
void
_http_write_ports_file(c3_c *pax_c)
{
#if 0
  c3_i    pal_i;
  c3_c    *paf_c;
  c3_i    por_i;
  u3_http *htp_u;

  pal_i = strlen(pax_c) + 13; /* includes NUL */
  paf_c = u3a_malloc(pal_i);
  snprintf(paf_c, pal_i, "%s/%s", pax_c, ".http.ports");

  por_i = open(paf_c, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  u3a_free(paf_c);

  for ( htp_u = u3_Host.htp_u; htp_u; htp_u = htp_u->nex_u ) {
    dprintf(por_i, "%u %s %s\n", htp_u->por_w,
                   (c3y == htp_u->sec) ? "secure" : "insecure",
                   (c3y == htp_u->lop) ? "loopback" : "public");
  }

  c3_sync(por_i);
  close(por_i);
#endif
}

/* _http_release_ports_file(): remove .http.ports
*/
void
_http_release_ports_file(c3_c *pax_c)
{
#if 0
  c3_i pal_i;
  c3_c *paf_c;

  pal_i = strlen(pax_c) + 13; /* includes NUL */
  paf_c = u3a_malloc(pal_i);
  snprintf(paf_c, pal_i, "%s/%s", pax_c, ".http.ports");

  unlink(paf_c);
  u3a_free(paf_c);
#endif
}

// XX rename (is a card an effect?)
/* u3_http_ef_bake(): notify %eyre that we're live
*/
void
u3_http_ef_bake(void)
{
  u3_noun pax = u3nq(u3_blip, c3__http, u3k(u3A->sen), u3_nul);

  u3v_plan(pax, u3nc(c3__born, u3_nul));
}

/* u3_http_ef_thou(): send %thou from %eyre as http response.
*/
void
u3_http_ef_thou(c3_l     sev_l,
                c3_l     coq_l,
                c3_l     seq_l,
                u3_noun  rep)
{
  _http_respond(sev_l, coq_l, seq_l, rep);
}

/* u3_http_io_init(): initialize http I/O.
*/
void
u3_http_io_init()
{
  //  Lens port
  {
    u3_http *htp_u = c3_malloc(sizeof(*htp_u));

    htp_u->sev_l = u3A->sev_l + 2;
    htp_u->coq_l = 1;
    htp_u->por_w = 12321;
    htp_u->sec = c3n;
    htp_u->lop = c3y;

    htp_u->cep_u = 0;
    htp_u->hos_u = 0;
    htp_u->hon_u = 0;
    htp_u->nex_u = 0;

    htp_u->nex_u = u3_Host.htp_u;
    u3_Host.htp_u = htp_u;
  }

  //  Secure port.
  {
    u3_http *htp_u = c3_malloc(sizeof(*htp_u));

    htp_u->sev_l = u3A->sev_l + 1;
    htp_u->coq_l = 1;
    htp_u->por_w = 8443;
    htp_u->sec = c3y;
    htp_u->lop = c3n;

    htp_u->cep_u = 0;
    htp_u->hos_u = 0;
    htp_u->hon_u = 0;
    htp_u->nex_u = 0;

    htp_u->nex_u = u3_Host.htp_u;
    u3_Host.htp_u = htp_u;
  }

   // Insecure port.
  {
    u3_http* htp_u = c3_malloc(sizeof(*htp_u));

    htp_u->sev_l = u3A->sev_l;
    htp_u->coq_l = 1;
    htp_u->por_w = 8080;
    htp_u->sec = c3n;
    htp_u->lop = c3n;

    htp_u->cep_u = 0;
    htp_u->hos_u = 0;
    htp_u->hon_u = 0;
    htp_u->nex_u = 0;

    htp_u->nex_u = u3_Host.htp_u;
    u3_Host.htp_u = htp_u;
  }

  tls_u = _http_init_tls();

  // XX why is this here?
  u3_Host.ctp_u.coc_u = 0;
}

/* u3_http_io_talk(): start http I/O.
*/
void
u3_http_io_talk()
{
  u3_http* htp_u;

  // XX "global" per server?
  h2o_config_init(&fig_u);

  for ( htp_u = u3_Host.htp_u; htp_u; htp_u = htp_u->nex_u ) {
    _http_start(htp_u);
  }

  _http_write_ports_file(u3_Host.dir_c);
}

/* u3_http_io_poll(): poll kernel for http I/O.
*/
void
u3_http_io_poll(void)
{
}

/* u3_http_io_exit(): shut down http.
*/
void
u3_http_io_exit(void)
{
  // XX shutdown servers cleanly
  _http_release_ports_file(u3_Host.dir_c);
}
