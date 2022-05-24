//! @file client.c
//! HTTP client.

#include "vere/io/http/client.h"
#include "vere/io/http/http.h"

#include "all.h"
#include "c/list.h"
#include "vere/vere.h"
#include <openssl/ssl.h>
#include <h2o.h>

/* u3_csat: client connection state.
*/
  typedef enum {
    u3_csat_init = 0,                   //  initialized
    u3_csat_addr = 1,                   //  address resolution begun
    u3_csat_quit = 2,                   //  cancellation requested
    u3_csat_conn = 3,                   //  sync connect phase
    u3_csat_ripe = 4                    //  passed to libh2o
  } u3_csat;

/* u3_cres: response to http client.
*/
  typedef struct _u3_cres {
    c3_w             sas_w;             //  status code
    u3_noun          hed;               //  headers
    u3_hbod*         bod_u;             //  exit of body queue
    u3_hbod*         dob_u;             //  entry of body queue
  } u3_cres;

/* u3_creq: outgoing http request.
*/
  typedef struct _u3_creq {                              //!< client request
    c3_l               num_l;                            //!< request number
    h2o_http1client_t* cli_u;                            //!< h2o client
    u3_csat            sat_e;                            //!< connection state
    c3_o               sec;                              //!< yes == https
    c3_w               ipf_w;                            //!< IP
    c3_c               ipf_c[sizeof("255.255.255.255")]; //!< IP (string)
    c3_c*              hot_c;                            //!< host
    c3_s               por_s;                            //!< port
    c3_c               por_c[sizeof("65535")];           //!< port (string)
    c3_c*              met_c;                            //!< method
    c3_c*              url_c;                            //!< url
    c3_list*           hed_u;                            //!< headers
    u3_hbod*           bod_u;                            //!< body
    u3_hbod*           rub_u;                            //!< exit of send queue
    u3_hbod*           bur_u;                            //!< entry of send queue
    h2o_iovec_t*       vec_u;                            //!< send-buffer array
    u3_cres*           res_u;                            //!< nascent response
    struct _u3_creq*   nex_u;                            //!< next in list
    struct _u3_creq*   pre_u;                            //!< previous in list
    struct _u3_cttp*   ctp_u;                            //!< cttp backpointer
  } u3_creq;

/* u3_cttp: http client.
*/
  typedef struct _u3_cttp {
    u3_auto          car_u;             //  driver
    c3_l             sev_l;             //  instance number
    u3_creq*         ceq_u;             //  request list
    uv_async_t       nop_u;             //  unused handle (async close)
    h2o_timeout_t    tim_u;             //  request timeout
    h2o_http1client_ctx_t               //
                     ctx_u;             //  h2o client ctx
    void*            tls_u;             //  client SSL_CTX*
  } u3_cttp;

/* _cttp_bod_new(): create a data buffer
*/
static u3_hbod*
_cttp_bod_new(c3_w len_w, c3_c* hun_c)
{
  u3_hbod* bod_u = c3_malloc(1 + len_w + sizeof(*bod_u));
  bod_u->hun_y[len_w] = 0;
  bod_u->len_w = len_w;
  memcpy(bod_u->hun_y, (const c3_y*)hun_c, len_w);

  bod_u->nex_u = 0;
  return bod_u;
}

/* _cttp_bod_from_hed(): create a data buffer from a header
*/
static u3_hbod*
_cttp_bod_from_hed(u3_hhed* hed_u)
{
  c3_w len_w     = hed_u->nam_w + 2 + hed_u->val_w + 2;
  u3_hbod* bod_u = c3_malloc(1 + len_w + sizeof(*bod_u));
  bod_u->hun_y[len_w] = 0;

  memcpy(bod_u->hun_y, hed_u->nam_c, hed_u->nam_w);
  memcpy(bod_u->hun_y + hed_u->nam_w, ": ", 2);
  memcpy(bod_u->hun_y + hed_u->nam_w + 2, hed_u->val_c, hed_u->val_w);
  memcpy(bod_u->hun_y + hed_u->nam_w + 2 + hed_u->val_w, "\r\n", 2);

  bod_u->len_w = len_w;
  bod_u->nex_u = 0;

  return bod_u;
}

/* _cttp_bods_to_octs: translate body buffer into octet-stream noun.
*/
static u3_noun
_cttp_bods_to_octs(u3_hbod* bod_u)
{
}

/* _cttp_cres_fire_body(): attach response body buffer
*/
static void
_cttp_cres_fire_body(u3_cres* res_u, u3_hbod* bod_u)
{
  c3_assert(!bod_u->nex_u);

  if ( !(res_u->bod_u) ) {
    res_u->bod_u = res_u->dob_u = bod_u;
  }
  else {
    res_u->dob_u->nex_u = bod_u;
    res_u->dob_u = bod_u;
  }
}

/* _cttp_mcut_url(): measure/cut purl, producing relative URL.
*/
static c3_w
_cttp_mcut_url(c3_c* buf_c, c3_w len_w, u3_noun pul)
{
  u3_noun q_pul = u3h(u3t(pul));
  u3_noun r_pul = u3t(u3t(pul));

  len_w = u3_mcut_char(buf_c, len_w, '/');
  // Measure/cut path/extension.
  {
    u3_noun pok = u3k(q_pul);
    u3_noun h_pok = u3h(pok);
    u3_noun t_pok = u3t(pok);

    len_w = u3_mcut_path(buf_c, len_w, '/', u3k(t_pok));
    if ( u3_nul != h_pok ) {
      len_w = u3_mcut_char(buf_c, len_w, '.');
      len_w = u3_mcut_cord(buf_c, len_w, u3k(u3t(h_pok)));
    }
    u3z(pok);
  }

  // Measure/cut query.
  if ( u3_nul != r_pul ) {
    u3_noun quy = u3k(r_pul);
    u3_noun yuq = quy;
    c3_o  fir_o = c3y;

    while ( u3_nul != quy ) {
      if ( c3y == fir_o ) {
        len_w = u3_mcut_char(buf_c, len_w, '?');
        fir_o = c3n;
      }
      else {
        len_w = u3_mcut_char(buf_c, len_w, '&');
      }

      u3_noun i_quy, t_quy;
      u3_noun pi_quy, qi_quy;
      u3x_cell(quy, &i_quy, &t_quy);
      u3x_cell(i_quy, &pi_quy, &qi_quy);

      len_w = u3_mcut_cord(buf_c, len_w, u3k(pi_quy));
      len_w = u3_mcut_char(buf_c, len_w, '=');
      len_w = u3_mcut_cord(buf_c, len_w, u3k(qi_quy));

      quy = t_quy;
    }

    u3z(yuq);
  }

  u3z(pul);
  return len_w;
}

//! Populate an HTTP request's IP string field.
//!
//! @param[in,out] ceq_u  HTTP request.
static inline void
_cttp_creq_ip(u3_creq* ceq_u)
{
  c3_w ipf_w = ceq_u->ipf_w;
  snprintf(ceq_u->ipf_c,
           sizeof(ceq_u->ipf_c),
           "%d.%d.%d.%d",
           ipf_w >> 24,
           (ipf_w >> 16) & 0xff,
           (ipf_w >> 8) & 0xff,
           ipf_w & 0xff);
}

//! Populate an HTTP request's port string field.
//!
//! @param[in,out] ceq_u  HTTP request.
static inline void
_cttp_creq_port(u3_creq* ceq_u)
{
  c3_w por_s = ceq_u->por_s;
  snprintf(ceq_u->por_c, sizeof(ceq_u->por_c), "%hu", por_s);
}

/* _cttp_creq_url(): construct url from noun.
*/
static c3_c*
_cttp_creq_url(u3_noun pul)
{
  c3_w  len_w = _cttp_mcut_url(0, 0, u3k(pul));
  c3_c* url_c = c3_malloc(1 + len_w);

  _cttp_mcut_url(url_c, 0, pul);
  url_c[len_w] = 0;

  return url_c;
}

/* _cttp_creq_host(): construct host from noun.
*/
static c3_c*
_cttp_creq_host(u3_noun hot)
{
  c3_w  len_w = u3_mcut_host(0, 0, u3k(hot));
  c3_c* hot_c = c3_malloc(1 + len_w);

  u3_mcut_host(hot_c, 0, hot);
  hot_c[len_w] = 0;

  return hot_c;
}

/* _cttp_creq_find(): find a request by number in the client
*/
static u3_creq*
_cttp_creq_find(u3_cttp* ctp_u, c3_l num_l)
{
  u3_creq* ceq_u = ctp_u->ceq_u;

  //  XX glories of linear search
  //
  while ( ceq_u ) {
    if ( num_l == ceq_u->num_l ) {
      return ceq_u;
    }
    ceq_u = ceq_u->nex_u;
  }
  return 0;
}

/* _cttp_creq_free(): free a u3_creq.
*/
static void
_cttp_creq_free(u3_creq* ceq_u)
{
  // Unlink request from client.
  {
    u3_cttp* ctp_u = ceq_u->ctp_u;

    if ( ceq_u->pre_u ) {
      ceq_u->pre_u->nex_u = ceq_u->nex_u;

      if ( 0 != ceq_u->nex_u ) {
        ceq_u->nex_u->pre_u = ceq_u->pre_u;
      }
    }
    else {
      ctp_u->ceq_u = ceq_u->nex_u;

      if ( 0 != ceq_u->nex_u ) {
        ceq_u->nex_u->pre_u = 0;
      }
    }
  }

  u3_http_heds_free(ceq_u->hed_u);
  // Note: ceq_u->bod_u is covered here
  u3_http_bods_free(ceq_u->rub_u);

  if ( ceq_u->res_u ) {
    u3_http_bods_free(ceq_u->res_u->bod_u);
    c3_free(ceq_u->res_u);
  }

  c3_free(ceq_u->hot_c);
  c3_free(ceq_u->ipf_c);
  c3_free(ceq_u->por_c);
  c3_free(ceq_u->met_c);
  c3_free(ceq_u->url_c);
  c3_free(ceq_u->vec_u);
  c3_free(ceq_u);
}

/* _cttp_creq_new(): create a u3_creq from an +http-request
 *
 *   If we were rewriting all of this from scratch, this isn't how we'd do it.
 *
 *   We start with the (?? - JB)
 */
static u3_creq*
_cttp_creq_new(u3_cttp* ctp_u, c3_l num_l, u3_noun hes)
{
  u3_creq* ceq_u = c3_calloc(sizeof(*ceq_u));

  u3_noun method, url, headers, body;
  if (c3n == u3r_qual(hes, &method, &url, &headers, &body)) {
    u3z(hes);
    return 0;
  }

  //  parse the url out of the new style url passed to us.
  //
  u3_noun unit_pul = u3do("de-purl:html", u3k(url));

  if ( c3n == u3r_du(unit_pul) ) {
    c3_c* url_c = u3r_string(url);
    u3l_log("cttp: unable to parse url:\n    %s\n", url_c);
    c3_free(url_c);
    u3z(hes);
    return 0;
  }

  u3_noun pul = u3t(unit_pul);

  u3_noun hat = u3h(pul);      // +hart
  u3_noun sec = u3h(hat);
  u3_noun por = u3h(u3t(hat));
  u3_noun hot = u3t(u3t(hat)); // +host

  ceq_u->sat_e = u3_csat_init;
  ceq_u->num_l = num_l;
  ceq_u->sec   = sec;

  if ( c3y == u3h(hot) ) {
    ceq_u->hot_c = _cttp_creq_host(u3k(u3t(hot)));
  } else {
    ceq_u->ipf_w = u3r_word(0, u3t(hot));
    _cttp_creq_ip(ceq_u);
  }

  if ( u3_nul != por ) {
    ceq_u->por_s = u3t(por);
    _cttp_creq_port(ceq_u);
  }

  //  XX this should be checked against a whitelist
  //
  c3_assert( c3y == u3ud(method) );
  ceq_u->met_c = u3r_string(method);
  ceq_u->url_c = _cttp_creq_url(u3k(pul));

  ceq_u->hed_u = u3_http_heds_to_list(u3k(headers));

  if ( u3_nul != body ) {
    ceq_u->bod_u = u3_http_bod_from_octs(u3k(u3t(body)));
  }

  // Link request to client.
  {
    ceq_u->nex_u = ctp_u->ceq_u;

    if ( 0 != ceq_u->nex_u ) {
      ceq_u->nex_u->pre_u = ceq_u;
    }

    ceq_u->ctp_u = ctp_u;
    ctp_u->ceq_u = ceq_u;
  }

  u3z(unit_pul);
  u3z(hes);

  return ceq_u;
}

/* _cttp_creq_fire_body(): attach body to request buffers.
*/
static void
_cttp_creq_fire_body(u3_creq* ceq_u, u3_hbod *rub_u)
{
  c3_assert(!rub_u->nex_u);

  if ( !(ceq_u->rub_u) ) {
    ceq_u->rub_u = ceq_u->bur_u = rub_u;
  }
  else {
    ceq_u->bur_u->nex_u = rub_u;
    ceq_u->bur_u = rub_u;
  }
}

/* _cttp_creq_fire_str(): attach string to request buffers.
*/
static void
_cttp_creq_fire_str(u3_creq* ceq_u, c3_c* str_c)
{
  _cttp_creq_fire_body(ceq_u, _cttp_bod_new(strlen(str_c), str_c));
  c3_free(str_c);
}

/* _cttp_creq_fire(): load request data for into buffers.
*/
static void
_cttp_creq_fire(u3_creq* ceq_u)
{
  {
    c3_w  len_w = strlen(ceq_u->met_c) + 1 + strlen(ceq_u->url_c) + 12;
    c3_c* lin_c = c3_malloc(len_w);

    len_w = snprintf(lin_c, len_w, "%s %s HTTP/1.1\r\n",
                                   ceq_u->met_c,
                                   ceq_u->url_c);
    _cttp_creq_fire_str(ceq_u, lin_c);
  }

  {
    c3_c* hot_c = ceq_u->hot_c ? ceq_u->hot_c : ceq_u->ipf_c;
    c3_c* hos_c;
    c3_w  len_w;

    if ( ceq_u->por_c ) {
      len_w = 6 + strlen(hot_c) + 1 + strlen(ceq_u->por_c) + 3;
      hos_c = c3_malloc(len_w);
      len_w = snprintf(hos_c, len_w, "Host: %s:%s\r\n", hot_c, ceq_u->por_c);
    }
    else {
      len_w = 6 + strlen(hot_c) + 3;
      hos_c = c3_malloc(len_w);
      len_w = snprintf(hos_c, len_w, "Host: %s\r\n", hot_c);
    }

    _cttp_creq_fire_body(ceq_u, _cttp_bod_new(len_w, hos_c));
    c3_free(hos_c);
  }

  // Attach output headers.
  c3_lode* nod_u = c3_list_peekf(ceq_u->hed_u);
  while ( nod_u ) {
    u3_hhed* hdr_u = c3_lode_data(nod_u);
    _cttp_creq_fire_body(ceq_u, _cttp_bod_from_hed(hdr_u));
    nod_u = c3_lode_next(nod_u);
  }

  if ( !ceq_u->bod_u ) {
    _cttp_creq_fire_body(ceq_u, _cttp_bod_new(2, "\r\n"));
  }
  else {
    c3_c len_c[41];
    c3_w len_w = snprintf(len_c, 40, "Content-Length: %u\r\n\r\n",
                                     ceq_u->bod_u->len_w);

    _cttp_creq_fire_body(ceq_u, _cttp_bod_new(len_w, len_c));
    _cttp_creq_fire_body(ceq_u, ceq_u->bod_u);
  }
}

/* _cttp_creq_quit(): cancel a u3_creq
*/
static void
_cttp_creq_quit(u3_creq* ceq_u)
{
  if ( u3_csat_addr == ceq_u->sat_e ) {
    ceq_u->sat_e = u3_csat_quit;
    return;  // wait to be called again on address resolution
  }

  if ( ceq_u->cli_u ) {
    h2o_http1client_cancel(ceq_u->cli_u);
  }

  _cttp_creq_free(ceq_u);
}

static void
_cttp_http_client_receive(u3_creq* ceq_u, c3_w sas_w, u3_noun mes, u3_noun uct)
{
  u3_cttp* ctp_u = ceq_u->ctp_u;

  //  XX inject partial responses as separate events
  //
  u3_noun wir = u3nt(u3i_string("http-client"),
                     u3dc("scot", c3__uv, ctp_u->sev_l),
                     u3_nul);
  u3_noun cad = u3nt(u3i_string("receive"),
                    ceq_u->num_l,
                    u3nq(u3i_string("start"), u3nc(sas_w, mes), uct, c3y));

  u3_auto_plan(&ctp_u->car_u, u3_ovum_init(0, c3__i, wir, cad));
}

/* _cttp_creq_fail(): dispatch error response
*/
static void
_cttp_creq_fail(u3_creq* ceq_u, const c3_c* err_c)
{
  // XX anything other than a 504?
  c3_w cod_w = 504;

  u3l_log("http: fail (%d, %d): %s\r\n", ceq_u->num_l, cod_w, err_c);

  // XX include err_c as response body?
  _cttp_http_client_receive(ceq_u, cod_w, u3_nul, u3_nul);
  _cttp_creq_free(ceq_u);
}

/* _cttp_creq_respond(): dispatch response
*/
static void
_cttp_creq_respond(u3_creq* ceq_u)
{
  u3_cres* res_u = ceq_u->res_u;

  u3_noun oct = u3_nul;
  if ( res_u->bod_u ) {
    c3_w len_w = 0;
    {
      u3_hbod* bod_u = res_u->bod_u;
      while ( bod_u ) {
        len_w += bod_u->len_w;
        bod_u = bod_u->nex_u;
      }
    }

    c3_y* buf_y = c3_malloc(len_w + 1);
    buf_y[len_w] = 0;
    {
      c3_y* ptr_y = buf_y;

      u3_hbod* bod_u = res_u->bod_u;
      while ( bod_u ) {
        memcpy(ptr_y, bod_u->hun_y, bod_u->len_w);
        ptr_y += bod_u->len_w;
        bod_u = bod_u->nex_u;
      }
    }
    u3_noun cos = u3i_bytes(len_w, buf_y);
    c3_free(buf_y);
    oct = u3nc(len_w, cos);
  }

  _cttp_http_client_receive(ceq_u, res_u->sas_w, res_u->hed, oct);
  _cttp_creq_free(ceq_u);
}

// XX research: may be called with closed client?
/* _cttp_creq_on_body(): cb invoked by h2o upon receiving a response body
*/
static c3_i
_cttp_creq_on_body(h2o_http1client_t* cli_u, const c3_c* err_c)
{
  u3_creq* ceq_u = (u3_creq *)cli_u->data;

  if ( 0 != err_c && h2o_http1client_error_is_eos != err_c ) {
    _cttp_creq_fail(ceq_u, err_c);
    return -1;
  }

  h2o_buffer_t* buf_u = cli_u->sock->input;

  if ( buf_u->size ) {
    _cttp_cres_fire_body(ceq_u->res_u,
                         _cttp_bod_new(buf_u->size, buf_u->bytes));
    h2o_buffer_consume(&cli_u->sock->input, buf_u->size);
  }

  // We're using the end of stream thing here to queue event to urbit. we'll
  // need to separate this into our own timer for partial progress sends.
  if ( h2o_http1client_error_is_eos == err_c ) {
    _cttp_creq_respond(ceq_u);
  }

  return 0;
}

/* _cttp_creq_on_head(): cb invoked by h2o upon receiving response headers
*/
static h2o_http1client_body_cb
_cttp_creq_on_head(h2o_http1client_t* cli_u, const c3_c* err_c, c3_i ver_i,
                   c3_i sas_i, h2o_iovec_t sas_u, h2o_header_t* hed_u,
                   size_t hed_t, c3_i len_i)
{
  u3_creq* ceq_u = (u3_creq *)cli_u->data;

  if ( 0 != err_c && h2o_http1client_error_is_eos != err_c ) {
    _cttp_creq_fail(ceq_u, err_c);
    return 0;
  }

  ceq_u->res_u = c3_calloc(sizeof(*ceq_u->res_u));
  ceq_u->res_u->sas_w = (c3_w)sas_i;
  ceq_u->res_u->hed = u3_http_heds_to_noun(hed_u, hed_t);

  if ( h2o_http1client_error_is_eos == err_c ) {
    _cttp_creq_respond(ceq_u);
    return 0;
  }

  return _cttp_creq_on_body;
}

/* _cttp_creq_on_connect(): cb invoked by h2o upon successful connection
*/
static h2o_http1client_head_cb
_cttp_creq_on_connect(h2o_http1client_t* cli_u, const c3_c* err_c,
                      h2o_iovec_t** vec_u, size_t* vec_i, c3_i* hed_i)
{
  u3_creq* ceq_u = (u3_creq *)cli_u->data;

  if ( 0 != err_c ) {
    //  if synchronously connecting, caller will cleanup
    //
    if ( u3_csat_conn == ceq_u->sat_e ) {
      ceq_u->sat_e = u3_csat_quit;
    }
    else {
      c3_assert( u3_csat_ripe == ceq_u->sat_e );
      _cttp_creq_fail(ceq_u, err_c);
    }
    return 0;
  }

  //  serialize request (populate rub_u)
  //
  _cttp_creq_fire(ceq_u);

  {
    c3_w len_w;
    u3_http_bods_to_vec(ceq_u->rub_u, &ceq_u->vec_u, &len_w);

    *vec_i = len_w;
    *vec_u = ceq_u->vec_u;
    *hed_i = (0 == strcmp(ceq_u->met_c, "HEAD"));
  }

  return _cttp_creq_on_head;
}

/* _cttp_creq_connect(): establish connection
*/
static void
_cttp_creq_connect(u3_creq* ceq_u)
{
  c3_assert( u3_csat_conn == ceq_u->sat_e );
  c3_assert( ceq_u->ipf_c );

  //  connect by ip/port, avoiding synchronous getaddrinfo()
  //
  {
    h2o_iovec_t ipf_u = h2o_iovec_init(ceq_u->ipf_c, strlen(ceq_u->ipf_c));
    c3_t        tls_t = ( c3y == ceq_u->sec );
    c3_s        por_s = ( ceq_u->por_s )
                        ? ceq_u->por_s
                        : ( tls_t ) ? 443 : 80;

    h2o_http1client_connect(&ceq_u->cli_u, ceq_u, &ceq_u->ctp_u->ctx_u,
                            ipf_u, por_s, tls_t, _cttp_creq_on_connect);
  }

  //  connect() failed, cb invoked synchronously
  //
  if ( u3_csat_conn != ceq_u->sat_e ) {
    c3_assert( u3_csat_quit == ceq_u->sat_e );
    //  only one such failure case
    //
    _cttp_creq_fail(ceq_u, "socket create error");
  }
  else {
    ceq_u->sat_e = u3_csat_ripe;

    //  fixup hostname for TLS handshake
    //
    //    must be synchronous, after successfull connect() call
    //
    if ( ceq_u->hot_c && (c3y == ceq_u->sec) ) {
      c3_assert( ceq_u->cli_u );
      c3_free(ceq_u->cli_u->ssl.server_name);
      ceq_u->cli_u->ssl.server_name = strdup(ceq_u->hot_c);
    }
  }
}

/* _cttp_creq_resolve_cb(): cb upon IP address resolution
*/
static void
_cttp_creq_resolve_cb(uv_getaddrinfo_t* adr_u,
                      c3_i              sas_i,
                      struct addrinfo*  aif_u)
{
  u3_creq* ceq_u = adr_u->data;

  if ( u3_csat_quit == ceq_u->sat_e ) {
    _cttp_creq_quit(ceq_u);;
  }
  else if ( 0 != sas_i ) {
    _cttp_creq_fail(ceq_u, uv_strerror(sas_i));
  }
  else {
    // XX traverse struct a la _ames_czar_cb
    ceq_u->ipf_w = ntohl(((struct sockaddr_in *)aif_u->ai_addr)->sin_addr.s_addr);
    _cttp_creq_ip(ceq_u);

    ceq_u->sat_e = u3_csat_conn;
    _cttp_creq_connect(ceq_u);
  }

  c3_free(adr_u);
  uv_freeaddrinfo(aif_u);
}

/* _cttp_init_tls: initialize OpenSSL context
*/
static SSL_CTX*
_cttp_init_tls(void)
{
  // XX require 1.1.0 and use TLS_client_method()
  SSL_CTX* tls_u = SSL_CTX_new(SSLv23_client_method());
  // XX use SSL_CTX_set_max_proto_version() and SSL_CTX_set_min_proto_version()
  SSL_CTX_set_options(tls_u, SSL_OP_NO_SSLv2 |
                             SSL_OP_NO_SSLv3 |
                             // SSL_OP_NO_TLSv1 | // XX test
                             SSL_OP_NO_COMPRESSION);

  u3K.ssl_x509_f(SSL_CTX_get_cert_store(tls_u));
  SSL_CTX_set_verify(tls_u, SSL_VERIFY_PEER, 0);
  SSL_CTX_set_session_cache_mode(tls_u, SSL_SESS_CACHE_OFF);
  SSL_CTX_set_cipher_list(tls_u,
                          "ECDH+AESGCM:DH+AESGCM:ECDH+AES256:DH+AES256:"
                          "ECDH+AES128:DH+AES:ECDH+3DES:DH+3DES:RSA+AESGCM:"
                          "RSA+AES:RSA+3DES:!aNULL:!MD5:!DSS");

  return tls_u;
}

/* _cttp_ef_http_client(): send an %http-client (outgoing request) to cttp.
*/
static c3_o
_cttp_ef_http_client(u3_cttp* ctp_u, u3_noun tag, u3_noun dat)
{
  u3_creq* ceq_u;
  c3_o     ret_o;

  if ( c3y == u3r_sing_c("request", tag) ) {
    u3_noun num, req;
    c3_l  num_l;

    if (  (c3n == u3r_cell(dat, &num, &req))
       || (c3n == u3r_safe_word(num, &num_l)) )
    {
      u3l_log("cttp: strange request\n");
      ret_o = c3n;
    }
    else if ( (ceq_u = _cttp_creq_new(ctp_u, num_l, u3k(req))) ) {
      // Start a request.
      if ( ceq_u->ipf_c ) {
        ceq_u->sat_e = u3_csat_conn;
        _cttp_creq_connect(ceq_u);
      } else {
        // Resolve hostname to IP address.
        ceq_u->sat_e = u3_csat_addr;
        c3_assert(ceq_u->hot_c);

        uv_getaddrinfo_t* adr_u = c3_malloc(sizeof(*adr_u));
        adr_u->data = ceq_u;

        struct addrinfo hin_u;
        memset(&hin_u, 0, sizeof(struct addrinfo));

        hin_u.ai_family = PF_INET;
        hin_u.ai_socktype = SOCK_STREAM;
        hin_u.ai_protocol = IPPROTO_TCP;

        // XX is this necessary?
        c3_c* por_c = ceq_u->por_c ? ceq_u->por_c :
          ( c3y == ceq_u->sec ) ? "443" : "80";

        c3_i sas_i = uv_getaddrinfo(u3L,
                                    adr_u,
                                    _cttp_creq_resolve_cb,
                                    ceq_u->hot_c,
                                    por_c,
                                    &hin_u);
        if ( 0 != sas_i ) {
          _cttp_creq_fail(ceq_u, uv_strerror(sas_i));
        }
      }
      ret_o = c3y;
    }
    else {
      ret_o = c3n;
    }
  }
  else if ( c3y == u3r_sing_c("cancel-request", tag) ) {
    c3_l num_l;

    if ( c3n == u3r_safe_word(dat, &num_l) ) {
      u3l_log("cttp: strange cancel-request\n");
      ret_o = c3n;
    }
    else if ( (ceq_u =_cttp_creq_find(ctp_u, num_l)) ) {
      _cttp_creq_quit(ceq_u);
      ret_o = c3y;
    }
    else {
      //  accepted whether or not request exists
      //
      ret_o = c3y;
    }
  }
  else {
    u3l_log("cttp: strange effect (unknown type)\n");
    ret_o = c3n;
  }

  u3z(tag); u3z(dat);
  return ret_o;
}

/* _cttp_io_talk(): notify that we're live.
*/
static void
_cttp_io_talk(u3_auto* car_u)
{
  u3_cttp* ctp_u = (u3_cttp*)car_u;

  //  XX remove u3A->sen
  //
  u3_noun wir = u3nt(u3i_string("http-client"),
                     u3dc("scot", c3__uv, ctp_u->sev_l),
                     u3_nul);
  u3_noun cad = u3nc(c3__born, u3_nul);

  u3_auto_plan(car_u, u3_ovum_init(0, c3__i, wir, cad));
}

/* _cttp_io_kick(): apply effects
*/
static c3_o
_cttp_io_kick(u3_auto* car_u, u3_noun wir, u3_noun cad)
{
  u3_cttp* ctp_u = (u3_cttp*)car_u;

  u3_noun tag, dat, i_wir;
  c3_o ret_o;

  if (  (c3n == u3r_cell(wir, &i_wir, 0))
     || (c3n == u3r_cell(cad, &tag, &dat))
     || (c3n == u3r_sing_c("http-client", i_wir)) )
  {
    ret_o = c3n;
  }
  else {
    ret_o = _cttp_ef_http_client(ctp_u, u3k(tag), u3k(dat));
  }

  u3z(wir); u3z(cad);
  return ret_o;
}

/* _cttp_io_exit_cb(): free cttp.
*/
static void
_cttp_io_exit_cb(uv_handle_t* han_u)
{
  u3_cttp* ctp_u = han_u->data;

  SSL_CTX_free(ctp_u->tls_u);
  c3_free(ctp_u);
}

/* _cttp_io_exit(): shut down cttp.
*/
static void
_cttp_io_exit(u3_auto* car_u)
{
  u3_cttp* ctp_u = (u3_cttp*)car_u;

  //  close unused handle to free [ctp_u] after h2o is done
  //
  uv_close((uv_handle_t*)&ctp_u->nop_u, _cttp_io_exit_cb);

  //  cancel requests
  //
  {
    u3_creq* ceq_u = ctp_u->ceq_u;

    while ( ceq_u ) {
      _cttp_creq_quit(ceq_u);
      ceq_u = ceq_u->nex_u;
    }
  }

  h2o_timeout_dispose(u3L, &ctp_u->tim_u);
}

/* u3_cttp_io_init(): initialize http client I/O.
*/
u3_auto*
u3_cttp_io_init(u3_pier* pir_u)
{
  u3_cttp* ctp_u = c3_calloc(sizeof(*ctp_u));

  //  link to event loop
  //
  ctp_u->ctx_u.loop = u3L;

  //  unused handle for async close
  //
  uv_async_init(u3L, &ctp_u->nop_u, 0);
  ctp_u->nop_u.data = ctp_u;

  //  link to initialized request timeout
  //
  h2o_timeout_init(u3L, &ctp_u->tim_u, 300 * 1000);
  ctp_u->ctx_u.io_timeout = &ctp_u->tim_u;

  //  link to initialized tls ctx
  //
  ctp_u->tls_u = _cttp_init_tls();
  ctp_u->ctx_u.ssl_ctx = ctp_u->tls_u;

  u3_auto* car_u = &ctp_u->car_u;
  car_u->nam_m = c3__cttp;

  //  XX set in done_cb for %born
  //
  car_u->liv_o = c3y;
  car_u->io.talk_f = _cttp_io_talk;
  car_u->io.kick_f = _cttp_io_kick;
  car_u->io.exit_f = _cttp_io_exit;
  //  XX retry up to N?
  //
  // car_u->ev.bail_f = ...;

  {
    u3_noun now;
    struct timeval tim_u;
    gettimeofday(&tim_u, 0);

    now = u3_time_in_tv(&tim_u);
    ctp_u->sev_l = u3r_mug(now);
    u3z(now);
  }

  return car_u;
}
