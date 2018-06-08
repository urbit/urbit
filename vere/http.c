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
#include <openssl/ssl.h>
#include <h2o.h>
#include "all.h"
#include "vere/vere.h"

#include <picohttpparser.h>
// #include <tls.h>

static const c3_i TCP_BACKLOG = 16;

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
static h2o_iovec_t
_http_vec_from_octs(u3_noun oct)
{
  if ( u3_nul == oct ) {
    return h2o_iovec_init(0, 0);
  }

  //  2GB max
  if ( c3n == u3a_is_cat(u3h(u3t(oct))) ) {
    u3m_bail(c3__fail);
  }

  c3_w len_w  = u3h(u3t(oct));
  c3_y* buf_y = c3_malloc(1 + len_w);
  buf_y[len_w] = 0;

  u3r_bytes(0, len_w, buf_y, u3t(u3t(oct)));

  u3z(oct);
  return h2o_iovec_init(buf_y, len_w);
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

    free(hed_u->nam_c);
    free(hed_u->val_c);
    free(hed_u);
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
  u3_hreq* req_u = c3_malloc(sizeof(*req_u));
  req_u->rec_u = rec_u;
  req_u->sat_e = u3_rsat_init;
  _http_req_link(hon_u, req_u);

  return req_u;
}

/* _http_req_to_duct(): translate srv/con/req to duct
*/
static u3_noun
_http_req_to_duct(u3_hreq* req_u)
{
  return u3nt(u3_blip, c3__http,
              u3nq(u3dc("scot", c3_s2('u','v'), req_u->hon_u->htp_u->sev_l),
                   u3dc("scot", c3_s2('u','d'), req_u->hon_u->coq_l),
                   u3dc("scot", c3_s2('u','d'), req_u->seq_l),
                   u3_nul));
}

/* _http_req_kill(): kill http request in %eyre.
*/
static void
_http_req_kill(u3_hreq* req_u)
{
  u3_noun pox = _http_req_to_duct(req_u);
  u3v_plan(pox, u3nc(c3__thud, u3_nul));
}

/* _http_req_dispatch(): dispatch http request to %eyre
*/
static void
_http_req_dispatch(u3_hreq* req_u, u3_noun req)
{
  c3_assert(u3_rsat_init == req_u->sat_e);
  req_u->sat_e = u3_rsat_plan;

  u3_noun pox = _http_req_to_duct(req_u);
  u3_noun typ = _(req_u->hon_u->htp_u->lop) ? c3__chis : c3__this;

  u3v_plan(pox, u3nq(typ,
                     req_u->hon_u->htp_u->sec,
                     u3nc(c3y, u3i_words(1, &req_u->hon_u->ipf_w)),
                     req));
}

typedef struct _u3_hgen {
  h2o_generator_t neg_u;
  h2o_iovec_t     bod_u;
  u3_hreq*        req_u;
  u3_hhed*        hed_u;
} u3_hgen;

/* _http_hgen_dispose(): dispose response generator and buffers
*/
static void
_http_hgen_dispose(void* ptr_v)
{
  u3_hgen* gen_u = (u3_hgen*)ptr_v;
  _http_req_free(gen_u->req_u);
  _http_heds_free(gen_u->hed_u);
  free(gen_u->bod_u.base);
}

/* _http_req_respond(): write httr to h2o_req_t->res and send
*/
static void
_http_req_respond(u3_hreq* req_u, u3_noun sas, u3_noun hed, u3_noun bod)
{
  // XX ideally
  //c3_assert(u3_rsat_plan == req_u->sat_e);

  if ( u3_rsat_plan != req_u->sat_e ) {
    //uL(fprintf(uH, "duplicate response\n"));
    return;
  }

  req_u->sat_e = u3_rsat_ripe;

  h2o_req_t* rec_u = req_u->rec_u;

  rec_u->res.status = sas;
  rec_u->res.reason = (sas < 200) ? "weird" :
                      (sas < 300) ? "ok" :
                      (sas < 400) ? "moved" :
                      (sas < 500) ? "missing" :
                      "hosed";

  u3_hhed* hed_u = _http_heds_from_noun(u3k(hed));

  u3_hgen* gen_u = h2o_mem_alloc_shared(&rec_u->pool, sizeof(*gen_u),
                                        _http_hgen_dispose);
  gen_u->neg_u = (h2o_generator_t){0, 0};
  gen_u->req_u = req_u;
  gen_u->hed_u = hed_u;

  while ( 0 != hed_u ) {
    h2o_add_header_by_str(&rec_u->pool, &rec_u->res.headers,
                          hed_u->nam_c, hed_u->nam_w, 0, 0,
                          hed_u->val_c, hed_u->val_w);
    hed_u = hed_u->nex_u;
  }

  gen_u->bod_u = _http_vec_from_octs(u3k(bod));
  rec_u->res.content_length = gen_u->bod_u.len;

  h2o_start_response(rec_u, &gen_u->neg_u);
  h2o_send(rec_u, &gen_u->bod_u, 1, H2O_SEND_STATE_FINAL);


  u3z(sas); u3z(hed); u3z(bod);
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

/* _http_rec_fail(): fail on bad h2o_req_t
*/
static void
_http_rec_fail(h2o_req_t* rec_u, c3_i sas_i, c3_c* sas_c)
{
  static h2o_generator_t gen_u = {0, 0};
  rec_u->res.status = sas_i;
  rec_u->res.reason = sas_c;
  h2o_start_response(rec_u, &gen_u);
  h2o_send(rec_u, 0, 0, H2O_SEND_STATE_FINAL);
}

struct h2o_con_wrap {                 //  see private st_h2o_http1_conn_t
  h2o_conn_t         con_u;           //  connection
  struct {                            //  see private st_h2o_uv_socket_t
    h2o_socket_t     sok_u;           //  socket
    uv_stream_t*     han_u;           //  client stream handler (u3_hcon)
  } *suv_u;
};

/* _http_rec_accept(); handle incoming http request from h2o.
*/
static c3_i
_http_rec_accept(h2o_handler_t* han_u, h2o_req_t* rec_u)
{
  u3_weak req = _http_rec_to_httq(rec_u);

  if ( u3_none == req ) {
    if ( (u3C.wag_w & u3o_verbose) ) {
      uL(fprintf(uH, "strange %.*s request\n", (int)rec_u->method.len,
                                               rec_u->method.base));
    }
    _http_rec_fail(rec_u, 400, "bad request");
  }
  else {
    // XX HTTP2 wat do?
    struct h2o_con_wrap* noc_u = (struct h2o_con_wrap*)rec_u->conn;
    u3_hcon* hon_u = (u3_hcon*)noc_u->suv_u->han_u;

    // sanity check
    c3_assert(hon_u->sok_u == &noc_u->suv_u->sok_u);

    u3_hreq* req_u = _http_req_new(hon_u, rec_u);
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

    _http_req_kill(req_u);
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

  c3_i sas_i;

  if ( 0 != (sas_i = uv_accept((uv_stream_t*)&htp_u->wax_u,
                               (uv_stream_t*)&hon_u->wax_u)) ) {
    if ( (u3C.wag_w & u3o_verbose) ) {
      uL(fprintf(uH, "http: accept: %s\n", uv_strerror(sas_i)));
    }

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

// XX serv link/unlink/free/new

/* _http_serv_listen_cb(): uv_connection_cb for uv_listen
*/
static void
_http_serv_listen_cb(uv_stream_t* str_u, c3_i sas_i)
{
  u3_http* htp_u = (u3_http*)str_u;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "http: listen_cb: %s\n", uv_strerror(sas_i)));
  }
  else {
    _http_conn_new(htp_u);
  }
}

/* _http_serv_init_h2o(): initialize h2o ctx and handlers for server.
*/
static void
_http_serv_init_h2o(u3_http* htp_u)
{
  htp_u->fig_u = c3_calloc(sizeof(*htp_u->fig_u));
  h2o_config_init(htp_u->fig_u);
  htp_u->fig_u->server_name = h2o_iovec_init(
                                H2O_STRLIT("urbit/vere-" URBIT_VERSION));

  // XX use u3_Host.ops_u.nam_c? Or ship.urbit.org? Multiple hosts?
  // see https://github.com/urbit/urbit/issues/914
  htp_u->hos_u = h2o_config_register_host(htp_u->fig_u,
                                          h2o_iovec_init(H2O_STRLIT("default")),
                                          htp_u->por_w);

  htp_u->ctx_u = c3_calloc(sizeof(*htp_u->ctx_u));
  htp_u->cep_u = c3_calloc(sizeof(*htp_u->cep_u));
  htp_u->cep_u->ctx = (h2o_context_t*)htp_u->ctx_u;
  htp_u->cep_u->hosts = htp_u->fig_u->hosts;

  if ( c3y == htp_u->sec ) {
    htp_u->cep_u->ssl_ctx = u3_Host.tls_u;
  }

  htp_u->han_u = h2o_create_handler(&htp_u->hos_u->fallback_path,
                                    sizeof(*htp_u->han_u));
  htp_u->han_u->on_req = _http_rec_accept;

  h2o_context_init(htp_u->ctx_u, u3L, htp_u->fig_u);
}

/* _http_serv_start(): start http server.
*/
static void
_http_serv_start(u3_http* htp_u)
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

  if ( c3y == htp_u->sec && 0 == u3_Host.tls_u ) {
    uL(fprintf(uH, "http: secure server not started: .urb/tls/ not found\n"));
    htp_u->por_w = 0;
    return;
  }

  uv_tcp_init(u3L, &htp_u->wax_u);

  /*  Try ascending ports.
  */
  while ( 1 ) {
    c3_i sas_i;

    adr_u.sin_port = htons(htp_u->por_w);
    sas_i = uv_tcp_bind(&htp_u->wax_u, (const struct sockaddr*)&adr_u, 0);

    if ( 0 != sas_i ||
         0 != (sas_i = uv_listen((uv_stream_t*)&htp_u->wax_u,
                                 TCP_BACKLOG, _http_serv_listen_cb)) ) {
      if ( UV_EADDRINUSE == sas_i ) {
        htp_u->por_w++;
        continue;
      }

      uL(fprintf(uH, "http: listen: %s\n", uv_strerror(sas_i)));
      htp_u->por_w = 0;
      return;
    }

    _http_serv_init_h2o(htp_u);

    uL(fprintf(uH, "http: live (%s, %s) on %d\n",
                   (c3y == htp_u->sec) ? "secure" : "insecure",
                   (c3y == htp_u->lop) ? "loopback" : "public",
                   htp_u->por_w));
    break;
  }
}

/* _http_init_tls: initialize OpenSSL context
*/
static SSL_CTX*
_http_init_tls()
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
static void
_http_write_ports_file(c3_c *pax_c)
{
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
    if ( 0 < htp_u->por_w ) {
      dprintf(por_i, "%u %s %s\n", htp_u->por_w,
                     (c3y == htp_u->sec) ? "secure" : "insecure",
                     (c3y == htp_u->lop) ? "loopback" : "public");
    }
  }

  c3_sync(por_i);
  close(por_i);
}

/* _http_release_ports_file(): remove .http.ports
*/
static void
_http_release_ports_file(c3_c *pax_c)
{
  c3_i pal_i;
  c3_c *paf_c;

  pal_i = strlen(pax_c) + 13; /* includes NUL */
  paf_c = u3a_malloc(pal_i);
  snprintf(paf_c, pal_i, "%s/%s", pax_c, ".http.ports");

  unlink(paf_c);
  u3a_free(paf_c);
}

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
  u3_http* htp_u;
  u3_hcon* hon_u;
  u3_hreq* req_u;
  c3_w bug_w = u3C.wag_w & u3o_verbose;

  if ( !(htp_u = _http_serv_find(sev_l)) ) {
    if ( bug_w ) {
      uL(fprintf(uH, "http: server not found: %x\r\n", sev_l));
    }
  }
  else if ( !(hon_u = _http_conn_find(htp_u, coq_l)) ) {
    if ( bug_w ) {
      uL(fprintf(uH, "http: connection not found: %x/%d\r\n", sev_l, coq_l));
    }
  }
  else if ( !(req_u = _http_req_find(hon_u, seq_l)) ) {
    if ( bug_w ) {
      uL(fprintf(uH, "http: request not found: %x/%d/%d\r\n",
                 			sev_l, coq_l, seq_l));
    }
  }
  else {
    u3_noun p_rep, q_rep, r_rep;

    if ( c3n == u3r_trel(rep, &p_rep, &q_rep, &r_rep) ) {
      uL(fprintf(uH, "http: strange response\n"));
    }
    else {
      _http_req_respond(req_u, u3k(p_rep), u3k(q_rep), u3k(r_rep));
    }
  }

  u3z(rep);
}

typedef struct _u3_proxy_listener u3_proxy_listener;

static u3_proxy_listener* _proxy_listener_new(c3_s por_s, c3_o sec);
static u3_proxy_listener* _proxy_sock_start(u3_proxy_listener* lis_u);

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

  u3_Host.tls_u = _http_init_tls();
}

/* u3_http_io_talk(): start http I/O.
*/
void
u3_http_io_talk()
{
  u3_http* htp_u;

  for ( htp_u = u3_Host.htp_u; htp_u; htp_u = htp_u->nex_u ) {
    _http_serv_start(htp_u);
  }

  _http_write_ports_file(u3_Host.dir_c);

  _proxy_sock_start(_proxy_listener_new(80, c3n));
  _proxy_sock_start(_proxy_listener_new(443, c3y));
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

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

// XX rename all of these

typedef enum {
  u3_pars_good = 0,                   //  success
  u3_pars_fail = 1,                   //  failure
  u3_pars_moar = 2                    //  incomplete
} u3_proxy_pars;

/* u3_proxy_writ: struct for uv_write lifecycle management.
*/
typedef struct _u3_proxy_writ {
  uv_write_t       wri_u;
  c3_y*            buf_y;
} u3_proxy_writ;

typedef enum {
  u3_proxy_forward = 0,               //  connected to us
  u3_proxy_backward = 1               //  we connected to XX rename
} u3_proxy_type;

/* u3_proxy_conn: established proxy connection
*/
typedef struct _u3_proxy_conn {
  uv_tcp_t         don_u;             //  downstream handle
  uv_tcp_t*        upt_u;             //  upstream handle XX union of local connect and reverse listener?
  uv_buf_t         buf_u;             //  pending buffer XX support multiple
  c3_o             sec;               //  yes == https
  u3_proxy_type    typ_e;
  union {
    struct _u3_proxy_client* cli_u;   // typ_e == backward
    struct _u3_proxy_listener* lis_u; // typ_e == forward
  } src_u;
  struct _u3_proxy_conn*   nex_u;
} u3_proxy_conn;

/* u3_proxy_client: ship-specific proxy client
*/
typedef struct _u3_proxy_client {
  c3_w             ipf_w;
  c3_s             por_s;
  c3_o             sec;
  u3_atom          sip;
  c3_c*            hot_c;
  struct _u3_proxy_client* nex_u;
  // XX link connection?
} u3_proxy_client;

/* u3_proxy_reverse: ship-specific proxy listener
*/
typedef struct _u3_proxy_reverse {
  uv_tcp_t         tcp_u;
  u3_atom          sip;               //  reverse proxy for ship
  c3_s             por_s;
  struct _u3_proxy_conn* con_u;
  struct _u3_proxy_reverse* nex_u;
} u3_proxy_reverse;

/* u3_proxy_listener: general proxy listener
*/
typedef struct _u3_proxy_listener {
  uv_tcp_t         sev_u;             // server handle
  c3_s             por_s;
  c3_o             sec;               //  yes == https
  struct _u3_proxy_conn*   con_u;             // active connection list
  struct _u3_proxy_reverse* rev_u;            // active reverse listeners
  struct u3_proxy_listener* nex_u;           // next listener
} u3_proxy_listener;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

/* _proxy_alloc(): libuv buffer allocator
*/
static void
_proxy_alloc(uv_handle_t* had_u,
             size_t len_i,
             uv_buf_t* buf)
{
  // len_i is always 64k, so we're ignoring it
  // using fixed size 4K buffer for
  // XX consider h2o_buffer_t, a pool, or something XX
  void* ptr_v = c3_malloc(4096);
  *buf = uv_buf_init(ptr_v, 4096);
}

/* _proxy_write_free(): free uv_write struct and linked buffer
*/
static void
_proxy_writ_free(u3_proxy_writ* ruq_u)
{
  if ( 0 != ruq_u->buf_y ) {
    free(ruq_u->buf_y);
  }

  free(ruq_u);
}

/* _proxy_writ_new(): allocate uv_write struct and link buffer
*/
static u3_proxy_writ*
_proxy_writ_new(u3_proxy_conn* con_u, c3_y* buf_y)
{
  u3_proxy_writ* ruq_u = c3_malloc(sizeof(*ruq_u));
  ruq_u->wri_u.data = con_u;
  ruq_u->buf_y = buf_y;

  return ruq_u;
}

/* _proxy_client_free(): free proxy client
*/
static void
_proxy_client_free(u3_proxy_client* cli_u)
{
  free(cli_u);
  // XX detach from glboal state
}

/* _proxy_client_new(): allocate ship-specific proxy client
*/
static u3_proxy_client*
_proxy_client_new(u3_atom sip, c3_s por_s, c3_o sec)
{
  u3_proxy_client* cli_u = c3_malloc(sizeof(*cli_u));
  cli_u->sip = sip;
  cli_u->sec = sec;
  cli_u->por_s = por_s; // set after opened
  cli_u->nex_u = 0;

  // XX link to global state

  return cli_u;
}

/* _proxy_conn_free(): free proxy connection
*/
static void
_proxy_conn_free(u3_proxy_conn* con_u)
{
  if ( 0 != con_u->buf_u.base ) {
    free(con_u->buf_u.base);
  }

  if ( u3_proxy_backward == con_u->typ_e ) {
    _proxy_client_free(con_u->src_u.cli_u);
  }

  free(con_u);

  // XX detach from listener
}

/* _proxy_conn_close(): close both sides of proxy connection
*/
static void
_proxy_conn_close(u3_proxy_conn* con_u)
{
  if ( 0 != con_u->upt_u ) {
    uv_close((uv_handle_t*)con_u->upt_u, (uv_close_cb)free);
  }

  uv_close((uv_handle_t*)&con_u->don_u, (uv_close_cb)_proxy_conn_free);
}

/* _proxy_conn_new(): allocate proxy connection
*/
static u3_proxy_conn*
_proxy_conn_new(u3_proxy_type typ_e, void* src_u)
{
  u3_proxy_conn* con_u = c3_malloc(sizeof(*con_u));
  con_u->upt_u = 0;
  con_u->buf_u = uv_buf_init(0, 0);
  con_u->nex_u = 0;

  switch ( typ_e ) {
    default: c3_assert(0);

    case u3_proxy_forward: {
      u3_proxy_listener* lis_u = (u3_proxy_listener*)src_u;
      con_u->typ_e = typ_e;
      con_u->src_u.lis_u = lis_u;
      con_u->sec = lis_u->sec;
      break;
    }

    case u3_proxy_backward: {
      u3_proxy_client* cli_u = (u3_proxy_client*)src_u;
      con_u->typ_e = typ_e;
      con_u->src_u.cli_u = cli_u;
      con_u->sec = cli_u->sec;
      break;
    }
  }

  con_u->don_u.data = con_u;

  // XX link to global state

  return con_u;
}

/* _proxy_reverse_free(): free reverse proxy listener
*/
static void
_proxy_reverse_free(u3_proxy_reverse* rev_u)
{
  u3z(rev_u->sip);
  free(rev_u);
  // XX free buffers
  // XX detach from listener
}

/* _proxy_reverse_new(): allocate reverse proxy listener
*/
static u3_proxy_reverse*
_proxy_reverse_new(u3_proxy_conn* con_u, u3_atom sip)
{
  u3_proxy_reverse* rev_u = c3_malloc(sizeof(*rev_u));
  rev_u->tcp_u.data = rev_u;
  rev_u->con_u = con_u;
  rev_u->sip = sip;
  rev_u->por_s = 0; // set after opened
  rev_u->nex_u = 0;

  // XX link to global state

  return rev_u;
}

/* _proxy_listener_free(): free proxy listener
*/
static void
_proxy_listener_free(u3_proxy_listener* lis_u)
{
  free(lis_u);
  // XX close and free connections
  // XX close and free reverse listeners
  // XX detach from global state
}

/* _proxy_listener_new(): allocate proxy listener
*/
static u3_proxy_listener*
_proxy_listener_new(c3_s por_s, c3_o sec)
{
  u3_proxy_listener* lis_u = c3_malloc(sizeof(*lis_u));
  lis_u->sev_u.data = lis_u;
  lis_u->por_s = por_s;
  lis_u->sec = sec;
  lis_u->con_u = 0;
  lis_u->rev_u = 0;
  lis_u->nex_u = 0;
  // XX link to global state

  return lis_u;
}

/* _proxy_write_cb(): general callback for proxy uv_write
*/
static void
_proxy_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  u3_proxy_writ* ruq_u = (u3_proxy_writ*)wri_u;
  // u3_proxy_conn* con_u = ruq_u->wri_u.data;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "proxy: write: %s\n", uv_strerror(sas_i)));
    // periodically cores, already closing, broken pipe
    // _proxy_conn_close(con_u);
  }

  _proxy_writ_free(ruq_u);
}

/* _proxy_write(): write buffer to proxy stream
*/
static c3_i
_proxy_write(u3_proxy_conn* con_u, uv_stream_t* str_u, uv_buf_t buf_u)
{
  u3_proxy_writ* ruq_u = _proxy_writ_new(con_u, (c3_y*)buf_u.base);

  c3_i sas_i;
  if ( 0 != (sas_i = uv_write(&ruq_u->wri_u, str_u,
                              &buf_u, 1, _proxy_write_cb)) ) {
    uL(fprintf(uH, "proxy: write: %s\n", uv_strerror(sas_i)));
    _proxy_conn_close(con_u);
    _proxy_writ_free(ruq_u);
  }

  return sas_i;
}

/* _proxy_sock_read_downstream_cb(): read from downstream, writing to upstream
*/
static void
_proxy_sock_read_downstream_cb(uv_stream_t* don_u,
                               ssize_t      siz_w,
                               const uv_buf_t *     buf_u)
{
  u3_proxy_conn* con_u = don_u->data;

  if ( 0 > siz_w ) {
    if ( UV_EOF != siz_w ) {
      uL(fprintf(uH, "proxy: read downstream: %s\n", uv_strerror(siz_w)));
    }
    _proxy_conn_close(con_u);
  }
  else {
    _proxy_write(con_u, (uv_stream_t*)con_u->upt_u,
                 uv_buf_init(buf_u->base, siz_w));
  }
}

/* _proxy_sock_read_upstream_cb(): read from upstream, writing to downstream
*/
static void
_proxy_sock_read_upstream_cb(uv_stream_t* upt_u,
                             ssize_t      siz_w,
                             const uv_buf_t *     buf_u)
{
  u3_proxy_conn* con_u = upt_u->data;

  if ( 0 > siz_w ) {
    if ( UV_EOF != siz_w ) {
      uL(fprintf(uH, "proxy: read upstream: %s\n", uv_strerror(siz_w)));
    }
    _proxy_conn_close(con_u);
  }
  else {
    _proxy_write(con_u, (uv_stream_t*)&(con_u->don_u),
                 uv_buf_init(buf_u->base, siz_w));
  }
}

/* _proxy_fire(): send pending buf upstream, setup mutual read/write
*/
static void
_proxy_fire(u3_proxy_conn* con_u)
{
  if ( 0 != con_u->buf_u.base ) {
    uv_buf_t fub_u = con_u->buf_u;
    con_u->buf_u = uv_buf_init(0, 0);

    if ( 0 != _proxy_write(con_u, (uv_stream_t*)con_u->upt_u, fub_u) ) {
      return;
    }
  }

  // XX set cooldown timers to close these?

  uv_read_start((uv_stream_t*)&con_u->don_u, _proxy_alloc, _proxy_sock_read_downstream_cb);
  uv_read_start((uv_stream_t*)con_u->upt_u, _proxy_alloc, _proxy_sock_read_upstream_cb);
}

/* _proxy_lopc_connect_cb(): callback for loopback proxy connect
*/
static void
_proxy_lopc_connect_cb(uv_connect_t * upc_u, c3_i sas_i)
{
  u3_proxy_conn* con_u = upc_u->data;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "proxy: connect: %s\n", uv_strerror(sas_i)));
    _proxy_conn_close(con_u);
  }
  else {
    _proxy_fire(con_u);
  }

  free(upc_u);
}

/* _proxy_lopc(): connect to loopback for local proxying
*/
static void
_proxy_lopc(u3_proxy_conn* con_u)
{
  uv_tcp_t* upt_u = c3_malloc(sizeof(*upt_u));

  con_u->upt_u = upt_u;
  upt_u->data = con_u;

  uv_tcp_init(u3L, upt_u);

  struct sockaddr_in lop_u;

  memset(&lop_u, 0, sizeof(lop_u));
  lop_u.sin_family = AF_INET;
  lop_u.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

  // get the appropriate loopback port
  {
    c3_s por_s = 0;
    u3_http* htp_u;

    for ( htp_u = u3_Host.htp_u; (0 != htp_u); htp_u = htp_u->nex_u ) {
      if ( c3n == htp_u->lop && con_u->sec == htp_u->sec ) {
        por_s = htp_u->por_w;
      }
    }

    // XX fix this by refactoring proxy setup
    c3_assert( 0 != por_s );

    lop_u.sin_port = htons(por_s);
  }

  uv_connect_t* upc_u = c3_malloc(sizeof(*upc_u));
  upc_u->data = con_u;

  c3_i sas_i;

  if ( 0 != (sas_i = uv_tcp_connect(upc_u, upt_u,
                                    (const struct sockaddr*)&lop_u,
                                    _proxy_lopc_connect_cb)) ) {
    uL(fprintf(uH, "proxy: loopback: %s\n", uv_strerror(sas_i)));
    free(upc_u);
    _proxy_conn_close(con_u);
  }
}

/* _proxy_reverse_listen_cb(): accept connection on ship-specific listener
*/
static void
_proxy_reverse_listen_cb(uv_stream_t* tcp_u, c3_i sas_i)
{
  u3_proxy_reverse* rev_u = (u3_proxy_reverse*)tcp_u;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "proxy: listen_cb: %s\n", uv_strerror(sas_i)));
    _proxy_conn_close(rev_u->con_u);
  }
  else {
    uv_tcp_t* upt_u = c3_malloc(sizeof(*upt_u));

    upt_u->data = rev_u->con_u;
    rev_u->con_u->upt_u = upt_u;

    uv_tcp_init(u3L, upt_u);

    if ( 0 != (sas_i = uv_accept((uv_stream_t*)&rev_u->tcp_u, (uv_stream_t*)upt_u)) ) {
      uL(fprintf(uH, "proxy: accept: %s\n", uv_strerror(sas_i)));
      _proxy_conn_close(rev_u->con_u);
    }
    else {
      _proxy_fire(rev_u->con_u);
    }
  }

  // XX find some way to reuse
  uv_close((uv_handle_t*)&rev_u->tcp_u, (uv_close_cb)_proxy_reverse_free);
}

/* _proxy_reverse_start(): setup ship-specific listener
*/
static void
_proxy_reverse_start(u3_proxy_conn* con_u, u3_noun sip)
{
  u3_proxy_reverse* rev_u = _proxy_reverse_new(con_u, sip);

  struct sockaddr_in add_u;

  memset(&add_u, 0, sizeof(add_u));
  add_u.sin_family = AF_INET;
  add_u.sin_addr.s_addr = INADDR_ANY;
  add_u.sin_port = 0;  // first available

  uv_tcp_init(u3L, &rev_u->tcp_u);

  c3_i sas_i;

  sas_i = uv_tcp_bind(&rev_u->tcp_u, (const struct sockaddr*)&add_u, 0);

  if ( 0 != sas_i ||
       0 != (sas_i = uv_listen((uv_stream_t*)&rev_u->tcp_u,
                               TCP_BACKLOG, _proxy_reverse_listen_cb)) ) {
    uL(fprintf(uH, "proxy: listen: %s\n", uv_strerror(sas_i)));
    uv_close((uv_handle_t*)&rev_u->tcp_u, (uv_close_cb)_proxy_reverse_free);
    _proxy_conn_close(con_u);
    return;
  }

  c3_i len_i = sizeof(add_u);

  memset(&add_u, 0, sizeof(add_u));

  if ( 0 != (sas_i = uv_tcp_getsockname(&rev_u->tcp_u,
                                        (struct sockaddr*)&add_u,
                                        &len_i)) ) {
    uL(fprintf(uH, "proxy: sockname: %s\n", uv_strerror(sas_i)));
    uv_close((uv_handle_t*)&rev_u->tcp_u, (uv_close_cb)_proxy_reverse_free);
    _proxy_conn_close(con_u);
  }
  else {
    rev_u->por_s = ntohs(add_u.sin_port);

    // XX confirm duct
    u3_noun pax = u3nq(u3_blip, c3__http, c3__prox,
                       u3nc(u3k(u3A->sen), u3_nul));
    u3_noun wis = u3nq(c3__wise, u3k(sip),
                                 u3i_words(1, (c3_w*)&rev_u->por_s),
                                 u3k(con_u->sec));
    u3v_plan(pax, wis);
  }
}

/* _proxy_parse_host(): parse plaintext buffer for Host header
*/
static u3_proxy_pars
_proxy_parse_host(const uv_buf_t* buf_u, c3_c** hot_c)
{
  struct phr_header hed_u[H2O_MAX_HEADERS];
  size_t hed_t = H2O_MAX_HEADERS;

  {
    // unused
    c3_i        ver_i;
    const c3_c* met_c;
    size_t      met_t;
    const c3_c* pat_c;
    size_t      pat_t;

    size_t len_t = buf_u->len < H2O_MAX_REQLEN ? buf_u->len : H2O_MAX_REQLEN;
    // XX slowloris?
    c3_i las_i = 0;
    c3_i sas_i;

    sas_i = phr_parse_request(buf_u->base, len_t, &met_c, &met_t,
                              &pat_c, &pat_t, &ver_i, hed_u, &hed_t, las_i);

    switch ( sas_i ) {
      case -1: return u3_pars_fail;
      case -2: return u3_pars_moar;
    }
  }

  const h2o_token_t* tok_t;
  size_t i;

  for ( i = 0; i < hed_t; i++ ) {
    // XX in-place, copy first
    h2o_strtolower((c3_c*)hed_u[i].name, hed_u[i].name_len);

    if ( 0 != (tok_t = h2o_lookup_token(hed_u[i].name, hed_u[i].name_len)) ) {
      if ( tok_t->is_init_header_special && H2O_TOKEN_HOST == tok_t ) {
        c3_c* val_c;
        c3_c* por_c;

        val_c = c3_malloc(1 + hed_u[i].value_len);
        val_c[hed_u[i].value_len] = 0;
        memcpy(val_c, hed_u[i].value, hed_u[i].value_len);

        // 'truncate' by replacing port separator ':' with 0
        if ( 0 != (por_c = strchr(val_c, ':')) ) {
          por_c[0] = 0;
        }

        *hot_c = val_c;
        break;
      }
    }
  }

  return u3_pars_good;
}

/* _proxy_parse_sni(): parse clienthello buffer for SNI
*/
static u3_proxy_pars
_proxy_parse_sni(const uv_buf_t* buf_u, c3_c** hot_c)
{
  c3_i sas_i = 0; //tls_protocol->parse_packet(buf_u->base, buf_u->len, hot_c);

  if ( 0 > sas_i ) {
    switch ( sas_i ) {
      case -1: return u3_pars_moar;
      case -2: return u3_pars_good;  // SNI not present
      default: return u3_pars_fail;
    }
  }

  return u3_pars_good;
}

/* _proxy_parse_ship(): determine destination for proxied request
*/
static u3_noun
_proxy_parse_ship(c3_c* hot_c)
{
  u3_noun sip = u3_nul;
  c3_c* dom_c;

  if ( 0 == hot_c ) {
    return sip;
  }

  dom_c = strchr(hot_c, '.');

  if ( 0 == dom_c ) {
    return sip;
  }

  c3_w dif_w = dom_c - hot_c;
  c3_w dns_w = strlen(u3_Host.ops_u.dns_c);

  if ( (dns_w != strlen(hot_c) - (dif_w + 1)) ||
       (0 != strncmp(dom_c + 1, u3_Host.ops_u.dns_c, dns_w)) ) {
    return sip;
  }

  {
    c3_c* sip_c = c3_malloc(2 + dif_w);
    strncpy(sip_c + 1, hot_c, dif_w);
    sip_c[0] = '~';
    sip_c[1 + dif_w] = 0;

    sip = u3dc("slaw", 'p', u3i_string(sip_c));
    free(sip_c);

    return sip;
  }
}

/* _proxy_dest(): proxy to destination
*/
static void
_proxy_dest(u3_proxy_conn* con_u, u3_noun sip)
{
  if ( u3_nul == sip ) {
    _proxy_lopc(con_u);
  }
  else {
    u3_noun hip = u3k(u3t(sip));
    u3_noun own = u3A->own;
    c3_o our = c3n;

    while ( u3_nul != own ) {
      if ( c3y == u3r_sing(hip, u3h(own)) ) {
        our = c3y;
        break;
      }
      own = u3t(own);
    }

    if ( c3y == our ) {
      _proxy_lopc(con_u);
    }
    else {
      // XX check if (sein:title sip) == our
      // XX check will
      _proxy_reverse_start(con_u, hip);
    }
  }

  u3z(sip);
}

static void _proxy_peek_read(u3_proxy_conn* con_u);

/* _proxy_peek(): peek at proxied request for destination
*/
static void
_proxy_peek(u3_proxy_conn* con_u)
{
  c3_c* hot_c = 0;

  u3_proxy_pars sat_e = ( c3y == con_u->sec ) ?
                        _proxy_parse_sni(&con_u->buf_u, &hot_c) :
                        _proxy_parse_host(&con_u->buf_u, &hot_c);

  switch ( sat_e ) {
    default: c3_assert(0);

    case u3_pars_fail: {
      uL(fprintf(uH, "proxy: peek fail\n"));
      _proxy_conn_close(con_u);
      break;
    }

    case u3_pars_moar: {
      uL(fprintf(uH, "proxy: peek moar\n"));
      // XX count retries, fail after some n
      _proxy_peek_read(con_u);
      break;
    }

    case u3_pars_good: {
      u3_noun sip = _proxy_parse_ship(hot_c);
      _proxy_dest(con_u, sip);
      break;
    }
  }

  if ( 0 !=  hot_c ) {
    free(hot_c);
  }
}

/* _proxy_peek_read_cb(): read callback for peeking at proxied request
*/
static void
_proxy_peek_read_cb(uv_stream_t* don_u,
                    ssize_t      siz_w,
                    const uv_buf_t* buf_u)
{
  u3_proxy_conn* con_u = don_u->data;

  if ( 0 > siz_w ) {
    if ( UV_EOF != siz_w ) {
      uL(fprintf(uH, "proxy: peek: %s\n", uv_strerror(siz_w)));
    }
    _proxy_conn_close(con_u);
  }
  else {
    uv_read_stop(don_u);

    u3_lo_open();

    if ( 0 == con_u->buf_u.base ) {
      con_u->buf_u = uv_buf_init(buf_u->base, siz_w);
    }
    else {
      c3_w len_w = siz_w + con_u->buf_u.len;
      // XX c3_realloc
      void* ptr_v = realloc(con_u->buf_u.base, len_w);
      c3_assert( 0 != ptr_v );

      memcpy(ptr_v + con_u->buf_u.len, buf_u->base, siz_w);
      con_u->buf_u = uv_buf_init(ptr_v, len_w);

      free(buf_u->base);
    }

    _proxy_peek(con_u);

    u3_lo_shut(c3y);
  }
}

/* _proxy_peek_read(): start read  to peek at proxied request
*/
static void
_proxy_peek_read(u3_proxy_conn* con_u)
{
  uv_read_start((uv_stream_t*)&con_u->don_u,
                _proxy_alloc, _proxy_peek_read_cb);
}

/* _proxy_sock_new(): accept new proxied request
*/
static void
_proxy_sock_new(u3_proxy_listener* lis_u)
{
  u3_proxy_conn* con_u = _proxy_conn_new(u3_proxy_forward, lis_u);

  uv_tcp_init(u3L, &con_u->don_u);

  c3_i sas_i;
  if ( 0 != (sas_i = uv_accept((uv_stream_t*)&lis_u->sev_u,
                               (uv_stream_t*)&con_u->don_u)) ) {
    uL(fprintf(uH, "proxy: accept: %s\n", uv_strerror(sas_i)));
    _proxy_conn_close(con_u);
  }
  else {
    _proxy_peek_read(con_u);
  }
}

/* _proxy_sock_listen_cb(): listen callback for proxy listener
*/
static void
_proxy_sock_listen_cb(uv_stream_t* sev_u, c3_i sas_i)
{
  u3_proxy_listener* lis_u = (u3_proxy_listener*)sev_u;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "proxy: listen_cb: %s\n", uv_strerror(sas_i)));
  }
  else {
    _proxy_sock_new(lis_u);
  }
}

/* _proxy_sock_start(): start proxy listener
*/
static u3_proxy_listener*
_proxy_sock_start(u3_proxy_listener* lis_u)
{
  uv_tcp_init(u3L, &lis_u->sev_u);

  struct sockaddr_in add_u;

  memset(&add_u, 0, sizeof(add_u));
  add_u.sin_family = AF_INET;
  add_u.sin_addr.s_addr = INADDR_ANY;

  /*  Try ascending ports.
  */
  while ( 1 ) {
    c3_i sas_i;

    add_u.sin_port = htons(lis_u->por_s);
    sas_i = uv_tcp_bind(&lis_u->sev_u, (const struct sockaddr*)&add_u, 0);

    if ( 0 != sas_i ||
         0 != (sas_i = uv_listen((uv_stream_t*)&lis_u->sev_u,
                                 TCP_BACKLOG, _proxy_sock_listen_cb)) ) {
      if ( (UV_EADDRINUSE == sas_i) || (UV_EACCES == sas_i) ) {
        if ( (c3y == lis_u->sec) && (443 == lis_u->por_s) ) {
          lis_u->por_s = 9443;
        }
        else if ( (c3n == lis_u->sec) && (80 == lis_u->por_s) ) {
          lis_u->por_s = 9080;
        }
        else {
          lis_u->por_s++;
        }

        continue;
      }

      uL(fprintf(uH, "proxy: listen: %s\n", uv_strerror(sas_i)));
      _proxy_listener_free(lis_u);
      return 0;
    }

    uL(fprintf(uH, "proxy: live (%s) on %d\n",
                   (c3y == lis_u->sec) ? "secure" : "insecure",
                   lis_u->por_s));
    return lis_u;
  }
}

/* _proxy_reverse_connect_cb(): connection callback for client of reverse proxy listener
*/
static void
_proxy_reverse_connect_cb(uv_connect_t * upc_u, c3_i sas_i)
{
  u3_proxy_conn* con_u = upc_u->data;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "proxy: reverse connect: %s\n", uv_strerror(sas_i)));
    _proxy_conn_close(con_u);
  }
  else {
    _proxy_lopc(con_u);
  }


  free(upc_u);
}

/* _proxy_reverse_connect(): connect as client to reverse proxy listener
*/
static void
_proxy_reverse_connect(u3_proxy_client* cli_u)
{
  u3_proxy_conn* con_u = _proxy_conn_new(u3_proxy_backward, cli_u);

  uv_tcp_init(u3L, &con_u->don_u);

  struct sockaddr_in add_u;

  memset(&add_u, 0, sizeof(add_u));
  add_u.sin_family = AF_INET;
  add_u.sin_addr.s_addr = htonl(cli_u->ipf_w);
  add_u.sin_port = htons(cli_u->por_s);

  uv_connect_t* upc_u = c3_malloc(sizeof(*upc_u));
  upc_u->data = con_u;

  c3_i sas_i;

  if ( 0 != (sas_i = uv_tcp_connect(upc_u, &con_u->don_u,
                                    (const struct sockaddr*)&add_u,
                                    _proxy_reverse_connect_cb)) ) {
      uL(fprintf(uH, "proxy: reverse connect: %s\n", uv_strerror(sas_i)));
      free(upc_u);
      _proxy_conn_close(con_u);
  }
}

/* _proxy_reverse_resolve(): IP address resolution callback
*/
static void
_proxy_reverse_client_resolve_cb(uv_getaddrinfo_t* adr_u,
                                 c3_i              sas_i,
                                 struct addrinfo*  aif_u)
{
  u3_proxy_client* cli_u = adr_u->data;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "proxy: reverse resolve: %s\n", uv_strerror(sas_i)));
    _proxy_client_free(cli_u);
  }
  else {
    // XX traverse struct a la _ames_czar_cb
    cli_u->ipf_w = ntohl(((struct sockaddr_in *)aif_u->ai_addr)->sin_addr.s_addr);
    _proxy_reverse_connect(cli_u);
  }

  free(adr_u);
  uv_freeaddrinfo(aif_u);
}

/* _proxy_reverse_resolve(): resolve IP address of reverse proxy listener
*/
static void
_proxy_reverse_resolve(u3_proxy_client* cli_u)
{
  uv_getaddrinfo_t* adr_u = c3_malloc(sizeof(*adr_u));
  adr_u->data = cli_u;

  struct addrinfo hin_u;
  memset(&hin_u, 0, sizeof(struct addrinfo));

  hin_u.ai_family = PF_INET;
  hin_u.ai_socktype = SOCK_STREAM;
  hin_u.ai_protocol = IPPROTO_TCP;

  // XX set port?

  c3_i sas_i;

  if ( 0 != (sas_i = uv_getaddrinfo(u3L, adr_u, _proxy_reverse_client_resolve_cb,
                                         cli_u->hot_c, 0, &hin_u)) ) {
    _proxy_client_free(cli_u);
  }
}

/* u3_http_ef_that(): reverse proxy request notification effect
*/
void
u3_http_ef_that(u3_noun sip, u3_noun por, u3_noun sec)
{
  uL(fprintf(uH, "proxy: ef that\n"));

  // XX test types
  u3_proxy_client* cli_u = _proxy_client_new((u3_atom)sip, (c3_s)por, (c3_o)sec);

  if ( c3n == u3_Host.ops_u.net ) {
    cli_u->ipf_w = INADDR_LOOPBACK;
    _proxy_reverse_connect(cli_u);
    return;
  }

  c3_c* sip_c = u3r_string(u3dc("scot", 'p', u3k(cli_u->sip)));

  // inc to skip '~'
  cli_u->hot_c = c3_malloc(1 + strlen(sip_c) + strlen(u3_Host.ops_u.dns_c));
  // XX len_w
  snprintf(cli_u->hot_c, 256, "%s.%s", sip_c + 1, u3_Host.ops_u.dns_c);

  free(sip_c);

  _proxy_reverse_resolve(cli_u);
}
