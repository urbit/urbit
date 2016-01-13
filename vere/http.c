/* v/http.c
**
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "../outside/jhttp/http_parser.h"   // Joyent HTTP
#include "all.h"
#include "vere/vere.h"

static void _http_request(u3_hreq* req_u);
static void _http_request_dead(u3_hreq* req_u);
static void _http_conn_dead(u3_hcon *hon_u);

/* _http_alloc(): libuv buffer allocator.
*/
static void
_http_alloc(uv_handle_t* had_u,
            size_t len_i,
            uv_buf_t* buf
            )
{
  void* ptr_v = c3_malloc(len_i);
  *buf = uv_buf_init(ptr_v, len_i);
}

/* _http_bod(): create a data buffer.
*/
static u3_hbod*
_http_bod(c3_w len_w, const c3_y* hun_y)
{
  u3_hbod* bod_u = c3_malloc(len_w + sizeof(*bod_u));

  bod_u->len_w = len_w;
  memcpy(bod_u->hun_y, hun_y, len_w);

  bod_u->nex_u = 0;
  return bod_u;
}

/* _http_bud(): create a header buffer.  Not null-terminated!
*/
static u3_hbod*
_http_bud(c3_c* nam_c, c3_c* val_c)
{
  c3_w lnm_w     = strlen(nam_c);
  c3_w lvl_w     = strlen(val_c);
  c3_w len_w     = lnm_w + 2 + lvl_w + 2;
  u3_hbod* bod_u = c3_malloc(len_w + sizeof(*bod_u));

  strncpy((c3_c *)bod_u->hun_y, nam_c, lnm_w);
  strncpy((c3_c *)bod_u->hun_y + lnm_w, ": ", 2);
  strncpy((c3_c *)bod_u->hun_y + lnm_w + 2, val_c, lvl_w);
  strncpy((c3_c *)bod_u->hun_y + lnm_w + 2 + lvl_w, "\r\n", 2);

  bod_u->len_w = len_w;
  bod_u->nex_u = 0;

  return bod_u;
}

/* _http_heds_free(): free header structure.
*/
static void
_http_heds_free(u3_hhed* hed_u)
{
  while ( hed_u ) {
    u3_hhed* nex_u = hed_u->nex_u;

    if ( hed_u->nam_c ) free(hed_u->nam_c);
    if ( hed_u->val_c ) free(hed_u->val_c);

    free(hed_u);
    hed_u = nex_u;
  }
}

/* _http_bods_free(): free body structure.
*/
static void
_http_bods_free(u3_hbod* bod_u)
{
  while ( bod_u ) {
    u3_hbod* nex_u = bod_u->nex_u;

    free(bod_u);
    bod_u = nex_u;
  }
}

/* _http_req_free(): free http request.
*/
static void
_http_req_free(u3_hreq* req_u)
{
  if ( req_u ) {
    if ( req_u->url_c ) {
      free(req_u->url_c);
    }
    _http_heds_free(req_u->hed_u);
    _http_bods_free(req_u->bod_u);
    _http_bods_free(req_u->bur_u);

    free(req_u->par_u);
    free(req_u);
  }
}

/* An unusual lameness in libuv.
*/
  typedef struct {
    uv_write_t wri_u;
    c3_y*      buf_y;
  } _u3_write_t;

/* _http_write_cb(): general write callback.
*/
static void
_http_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  _u3_write_t* ruq_u = (void *)wri_u;

  if ( (u3C.wag_w & u3o_verbose) && 0 != sas_i && UV__EPIPE != sas_i ) {
    uL(fprintf(uH, "http: write: ERROR %d\n", sas_i));
    uL(fprintf(uH, "  (%s)\n", uv_strerror(sas_i)));
  }
  free(ruq_u->buf_y);
  free(ruq_u);
}

/* _http_respond_buf(): write back to http.
*/
static void
_http_respond_buf(u3_hreq* req_u, uv_buf_t buf_u)
{
  _u3_write_t* ruq_u;

  // don't respond to a dead connection
  if ( uv_is_closing((uv_handle_t*) &(req_u->hon_u->wax_u)) ) {
      free(buf_u.base);
      return;
  }

  ruq_u = (_u3_write_t*) c3_malloc(sizeof(_u3_write_t));

  ruq_u->buf_y = (c3_y*)buf_u.base;

  if ( 0 != uv_write(&ruq_u->wri_u,
                     (uv_stream_t*)&(req_u->hon_u->wax_u),
                     &buf_u, 1,
                     _http_write_cb) )
  {
    uL(fprintf(uH, "respond: ERROR\n"));
    _http_conn_dead(req_u->hon_u);
  }
}

/* _http_respond_body(): attach response body.
*/
static void
_http_send_body(u3_hreq *req_u,
                u3_hbod *rub_u)
{
  uv_buf_t buf_u;

  //  XX extra copy here due to old code.  Use hbod as base directly.
  //
  {
    c3_y* buf_y = c3_malloc(rub_u->len_w);

    memcpy(buf_y, rub_u->hun_y, rub_u->len_w);
    buf_u = uv_buf_init((c3_c*)buf_y, rub_u->len_w);
  }
  _http_respond_buf(req_u, buf_u);
}

/* _http_respond_body(): attach response body.
*/
static void
_http_respond_body(u3_hreq *req_u,
                   u3_hbod *rub_u)
{
  if ( !(req_u->rub_u) ) {
    req_u->rub_u = req_u->bur_u = rub_u;
  }
  else {
    req_u->bur_u->nex_u = rub_u;
    req_u->bur_u = rub_u;
  }
}

/* _http_respond_str(): attach output string.
*/
static void
_http_respond_str(u3_hreq*    req_u,
                  const c3_c* str_c)
{
  _http_respond_body(req_u, _http_bod(strlen(str_c), (const c3_y*)str_c));
}

/* _http_respond_headers(): attach output headers.
*/
static void
_http_respond_headers(u3_hreq* req_u,
                      u3_hhed* hed_u)
{
  while ( hed_u ) {
    _http_respond_body(req_u, _http_bud(hed_u->nam_c, hed_u->val_c));
    hed_u = hed_u->nex_u;
  }
}

/* _http_respond_request(): attach response to request, then free it.
*/
static void
_http_respond_request(u3_hreq* req_u,
                      u3_hrep* rep_u)
{
  c3_c buf_c[81];

  snprintf(buf_c, 81, "HTTP/1.1 %d %s\r\n",
                 rep_u->sas_w,
                 (rep_u->sas_w < 200) ? "Weird" :
                 (rep_u->sas_w < 300) ? "OK" :
                 (rep_u->sas_w < 400) ? "Moved" :
                 (rep_u->sas_w < 500) ? "Missing" : "Hosed");
  _http_respond_str(req_u, buf_c);

  // printf("attached response status %d\n", rep_u->sas_w);
  _http_respond_headers(req_u, rep_u->hed_u);
  _http_heds_free(rep_u->hed_u);

  //  Why is this necessary?  Why we can't send a naked error?  Waah.
  //
  if ( !rep_u->bod_u ) {
    snprintf(buf_c, 81, "HTTP error %d.\r\n", rep_u->sas_w);
    rep_u->bod_u = _http_bod(strlen(buf_c), (c3_y*) buf_c);
  }

  {
    snprintf(buf_c, 81, "content-length: %u\r\n", rep_u->bod_u->len_w);
    _http_respond_str(req_u, buf_c);

    _http_respond_str(req_u, "\r\n");
    _http_respond_body(req_u, rep_u->bod_u);
  }
  free(rep_u);

  c3_assert(c3n == req_u->end);
  req_u->end = c3y;
}

/* _http_conn_free_early(): free http connection on failure.
*/
static void
_http_conn_free_early(uv_handle_t* han_t)
{
  u3_hcon* hon_u = (void*) han_t;
  free(hon_u);
}

/* _http_conn_free(): free http connection on close.
*/
static void
_http_conn_free(uv_handle_t* han_t)
{
  u3_hcon* hon_u = (void*) han_t;

  {
    struct _u3_http *htp_u = hon_u->htp_u;

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

  while ( 0 != hon_u->req_u ) {
    u3_hreq* req_u = hon_u->req_u;
    u3_hreq* nex_u = req_u->nex_u;

    _http_request_dead(req_u);
    _http_req_free(req_u);
    hon_u->req_u = nex_u;
  }
  free(hon_u);
}

/* _http_conn_dead(): free http connection, close fd.
*/
static void
_http_conn_dead(u3_hcon *hon_u)
{
  // uL(fprintf(uH, "connection dead: %d\n", hon_u->coq_l));

  uv_read_stop((uv_stream_t*) &(hon_u->wax_u));
  uv_close((uv_handle_t*) &(hon_u->wax_u), _http_conn_free);
}

#if 0
/* _http_req_dump(): dump complete http request.
*/
static void
_http_req_dump(u3_hreq* req_u)
{
  c3_c* mets[] = { "delete", "get", "head", "post", "put", "other" };

  printf("%s %s\n", mets[req_u->met_e],
                    req_u->url_c ? req_u->url_c : "(null)");
  {
    u3_hhed* hed_u = req_u->hed_u;

    while ( hed_u ) {
      printf("%s: %s\r\n", hed_u->nam_c, hed_u->val_c);
      hed_u = hed_u->nex_u;
    }
  }
  {
    u3_hbod* bod_u = req_u->bod_u;
    c3_w bod_w = 0;

    while ( bod_u ) {
      bod_w += bod_u->len_w;
      bod_u = bod_u->nex_u;
    }
    printf("body: %d bytes\r\n", bod_w);
  }
}
#endif

/* _http_message_begin(): jhttp callback
*/
static c3_i
_http_message_begin(http_parser* par_u)
{
  return 0;
}

/* _http_more(): extend string with new data.
*/
static c3_c*
_http_more(c3_c* str_c, const c3_c* buf_c, size_t siz_i)
{
  if ( !str_c ) {
    str_c = c3_malloc(siz_i + 1);
    memcpy(str_c, buf_c, siz_i);
    str_c[siz_i] = 0;
  }
  else {
    c3_w len_w = strlen(str_c);

    str_c = realloc(str_c, len_w + siz_i + 1);
    memcpy(str_c + len_w, buf_c, siz_i);
    str_c[len_w + siz_i] = 0;
  }
  return str_c;
}

/* _http_url(): jhttp callback
*/
static c3_i
_http_url(http_parser* par_u, const c3_c* buf_c, size_t siz_i)
{
  u3_hreq *req_u = par_u->data;

  req_u->url_c = _http_more(req_u->url_c, buf_c, siz_i);
  return 0;
}

/* _http_header_field(): jhttp callback
*/
static c3_i
_http_header_field(http_parser* par_u, const c3_c* buf_c, size_t siz_i)
{
  u3_hreq *req_u = par_u->data;

  switch ( req_u->rat_e ) {
    case u3_hreq_non:
    case u3_hreq_val: {
      u3_hhed* hed_u = c3_malloc(sizeof(*hed_u));

      hed_u->nam_c = _http_more(0, buf_c, siz_i);
      hed_u->val_c = 0;
      hed_u->nex_u = req_u->hed_u;
      req_u->hed_u = hed_u;

      break;
    }
    case u3_hreq_nam: {
      req_u->hed_u->nam_c = _http_more(req_u->hed_u->nam_c, buf_c, siz_i);
      break;
    }
  }
  req_u->rat_e = u3_hreq_nam;
  return 0;
}

/* _http_header_value(): jhttp callback
*/
static c3_i
_http_header_value(http_parser* par_u, const c3_c* buf_c, size_t siz_i)
{
  u3_hreq *req_u = par_u->data;

  switch ( req_u->rat_e ) {
    case u3_hreq_non: fprintf(stderr, "http: odd value\r\n"); return 1;

    case u3_hreq_nam: {
      req_u->hed_u->val_c = _http_more(0, buf_c, siz_i);
      break;
    }
    case u3_hreq_val: {
      req_u->hed_u->val_c = _http_more(req_u->hed_u->val_c, buf_c, siz_i);
      break;
    }
  }
  req_u->rat_e = u3_hreq_val;
  return 0;
}

/* _http_headers_complete(): jhttp callback
*/
static c3_i
_http_headers_complete(http_parser* par_u)
{
  u3_hreq *req_u = par_u->data;

  if ( par_u->method >= u3_hmet_other ) {
    req_u->met_e = u3_hmet_other;
  } else req_u->met_e = par_u->method;

  return 0;
}

/* _http_body(): jhttp callback
*/
static c3_i
_http_body(http_parser* par_u, const c3_c* buf_c, size_t siz_i)
{
  u3_hreq *req_u = par_u->data;
  u3_hbod* bod_u;

  bod_u = _http_bod(siz_i, (const c3_y*)buf_c);

  if ( !(req_u->bod_u) ) {
    req_u->bod_u = req_u->dob_u = bod_u;
  }
  else {
    req_u->dob_u->nex_u = bod_u;
    req_u->dob_u = bod_u;
  }

  return 0;
}

/* _http_message_complete(): jhttp callback
*/
static c3_i
_http_message_complete(http_parser* par_u)
{
  u3_hreq* req_u = par_u->data;
  u3_hcon* hon_u = req_u->hon_u;

  c3_assert(req_u == hon_u->ruc_u);
  hon_u->ruc_u = 0;
  // _http_req_dump(req_u);

  // Queue request for response control.
  {
    if ( !hon_u->qer_u ) {
      c3_assert(!(hon_u->req_u));

      hon_u->qer_u = hon_u->req_u = req_u;
    }
    else {
      hon_u->qer_u->nex_u = req_u;
    }
  }

  // Dispatch event request.
  //
  _http_request(req_u);
  return 0;
}

/* _http_settings[]: callback array.
*/
static struct http_parser_settings _http_settings = {
  _http_message_begin,
  _http_url,
  _http_header_field,
  _http_header_value,
  _http_headers_complete,
  _http_body,
  _http_message_complete
};

/* _http_req_new(): new http request.
*/
static u3_hreq*
_http_req_new(u3_hcon* hon_u)
{
  u3_hreq* req_u = c3_malloc(sizeof(*req_u));

  req_u->hon_u = hon_u;
  req_u->seq_l = hon_u->seq_l++;

  req_u->met_e = (u3_hmet)0;
  req_u->rat_e = (u3_hrat)0;

  req_u->par_u = c3_malloc(sizeof(struct http_parser));
  http_parser_init(req_u->par_u, HTTP_REQUEST);
  ((struct http_parser *)(req_u->par_u))->data = req_u;

  {
    struct sockaddr_in adr_u;
    c3_i               len_i = sizeof(adr_u);

    uv_tcp_getpeername(&hon_u->wax_u, (struct sockaddr *)&adr_u, &len_i);
    if ( adr_u.sin_family != AF_INET ) {
      req_u->ipf_w = 0;
    }
    else req_u->ipf_w = ntohl(adr_u.sin_addr.s_addr);
  }

  req_u->liv = c3n;
  req_u->end = c3n;

  req_u->url_c = 0;

  req_u->rub_u = 0;
  req_u->bur_u = 0;

  req_u->hed_u = 0;
  req_u->bod_u = 0;
  req_u->nex_u = 0;

  return req_u;
}

/* _http_conn_read_cb(): server read callback.
*/
/*
 * `nread` (siz_w) is > 0 if there is data available, 0 if libuv is done reading for
 * now, or < 0 on error.
 *
 * The callee is responsible for closing the stream when an error happens
 * by calling uv_close(). Trying to read from the stream again is undefined.
 *
 * The callee is responsible for freeing the buffer, libuv does not reuse it.
 * The buffer may be a null buffer (where buf->base=NULL and buf->len=0) on
 * error.
 */
static void
_http_conn_read_cb(uv_stream_t* tcp_u,
                   ssize_t      siz_w,
                   const uv_buf_t *     buf_u)
{
  u3_hcon* hon_u = (u3_hcon*)(void*) tcp_u;

  u3_lo_open();
  {
    if ( siz_w == UV_EOF ) {
      _http_conn_dead(hon_u);      
    } else if ( siz_w < 0 ) {
      uL(fprintf(uH, "http: read: %s\n", uv_strerror(siz_w)));
      _http_conn_dead(hon_u);
    }
    else {
      if ( !hon_u->ruc_u ) {
        hon_u->ruc_u = _http_req_new(hon_u);
      }

      if ( siz_w != http_parser_execute(hon_u->ruc_u->par_u,
                                        &_http_settings,
                                        (c3_c*)buf_u->base,
                                        siz_w) )
      {
        uL(fprintf(uH, "http: parse error\n"));
        _http_conn_dead(hon_u);
      }
    }
    if ( buf_u->base ) {
      free(buf_u->base);
    }
  }
  u3_lo_shut(c3y);
}

/* _http_conn_new(): create http connection.
*/
static void
_http_conn_new(u3_http *htp_u)
{
  u3_hcon *hon_u = c3_malloc(sizeof(*hon_u));

  uv_tcp_init(u3L, &hon_u->wax_u);

  c3_w ret_w;
  ret_w = uv_accept((uv_stream_t*)&htp_u->wax_u,
                    (uv_stream_t*)&hon_u->wax_u);
  if (ret_w == UV_EOF)
  {
    uL(fprintf(uH, "http: accept: ERROR\n"));

    uv_close((uv_handle_t*)&hon_u->wax_u, _http_conn_free_early);
  }
  else {
    uv_read_start((uv_stream_t*)&hon_u->wax_u,
                  _http_alloc,
                  _http_conn_read_cb);

    hon_u->coq_l = htp_u->coq_l++;
    hon_u->seq_l = 1;

    hon_u->ruc_u = 0;
    hon_u->req_u = 0;
    hon_u->qer_u = 0;

    hon_u->htp_u = htp_u;
    hon_u->nex_u = htp_u->hon_u;
    htp_u->hon_u = hon_u;
  }
}

/* _http_req_find(): find http request by sequence.
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

/* _http_serv_find(): find http connection by sequence.
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

/* _http_conn_find(): find http connection by sequence.
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

/* _http_heds_to_list(): C headers to list.
*/
static u3_noun
_http_heds_to_list(u3_hhed* hed_u)
{
  if ( 0 == hed_u ) {
    return u3_nul;
  } else {
    return u3nc(u3nc(u3i_string(hed_u->nam_c),
                     hed_u->val_c ? u3i_string(hed_u->val_c) : u3_nul),
                _http_heds_to_list(hed_u->nex_u));
  }
}

/* _http_list_to_heds(): list to C headers.
*/
static u3_hhed*
_http_list_to_heds(u3_noun lix)
{
  u3_noun  yix = lix;
  u3_hhed* hed_u = 0;

  while ( 1 ) {
    if ( u3_nul == lix ) {
      break;
    }
    else {
      u3_noun  i_lix = u3h(lix);
      u3_noun  pi_lix = u3h(i_lix);
      u3_noun  qi_lix = u3t(i_lix);
      u3_noun  t_lix = u3t(lix);
      u3_hhed* nex_u = c3_malloc(sizeof(u3_hhed));

      nex_u->nam_c = u3r_string(pi_lix);
      nex_u->val_c = u3r_string(qi_lix);
      nex_u->nex_u = hed_u;

      hed_u = nex_u;
      lix = t_lix;
    }
  }
  u3z(yix);
  return hed_u;
}

/* _http_bods_to_octs: translate body into octet-stream noun.
*/
static u3_noun
_http_bods_to_octs(u3_hbod* bod_u)
{
  c3_w    len_w;
  c3_y*   buf_y;
  u3_noun cos;

  {
    u3_hbod* bid_u;

    len_w = 0;
    for ( bid_u = bod_u; bid_u; bid_u = bid_u->nex_u ) {
      len_w += bid_u->len_w;
    }
  }
  buf_y = c3_malloc(len_w);

  {
    c3_y* ptr_y = buf_y;

    while ( bod_u ) {
      memcpy(ptr_y, bod_u->hun_y, bod_u->len_w);
      ptr_y += bod_u->len_w;
      bod_u = bod_u->nex_u;
    }
  }
  cos = u3i_bytes(len_w, buf_y);
  free(buf_y);
  return u3nc(len_w, cos);
}

/* _http_octs_to_bod(): translate octet-stream noun into body.
*/
static u3_hbod*
_http_octs_to_bod(u3_noun oct)
{
  c3_w len_w;

  if ( !_(u3a_is_cat(u3h(oct))) ) {
    //  2GB max
    u3m_bail(c3__fail); return 0;
  }
  len_w = u3h(oct);

  {
    u3_hbod* bod_u = c3_malloc(len_w + sizeof(*bod_u));

    bod_u->len_w = len_w;
    u3r_bytes(0, len_w, bod_u->hun_y, u3t(oct));

    bod_u->nex_u = 0;

    u3z(oct);
    return bod_u;
  }
}

/* _http_pox_to_noun(): translate srv/con/req to path noun (pox).
*/
static u3_noun
_http_pox_to_noun(c3_w sev_l, c3_w coq_l, c3_w seq_l)
{
  return
    u3nt(
      u3_blip,
      c3__http,
      u3nq(u3dc("scot", c3_s2('u','v'), sev_l),
           u3dc("scot", c3_s2('u','d'), coq_l),
           u3dc("scot", c3_s2('u','d'), seq_l),
           u3_nul));
}

/* _http_request_to_noun(): translate http request into noun, or u3_none.
*/
static u3_noun
_http_request_to_noun(u3_hreq* req_u)
{
  u3_noun med, url, hed, bod;

  switch ( req_u->met_e ) {
    default: fprintf(stderr, "strange request\r\n"); return u3_none;
    case u3_hmet_put: { med = c3__put; break; }
    case u3_hmet_get: { med = c3__get; break; }
    case u3_hmet_head: { med = c3__head; break; }
    case u3_hmet_post: { med = c3__post; break; }
  }
  url = u3i_string(req_u->url_c);
  hed = _http_heds_to_list(req_u->hed_u);
  bod = req_u->bod_u ? u3nc(u3_nul, _http_bods_to_octs(req_u->bod_u)) : u3_nul;

  return u3nq(med, url, hed, bod);
}

/* _http_new_response(): create http response structure.
*/
static u3_hrep*
_http_new_response(c3_l sev_l, c3_l coq_l, c3_l seq_l, u3_noun rep)
{
  u3_noun p_rep, q_rep, r_rep;

  if ( c3n == u3r_trel(rep, &p_rep, &q_rep, &r_rep) ) {
    uL(fprintf(uH, "strange response\n"));
    return 0;
  }
  else {
    u3_hrep* rep_u = c3_malloc(sizeof(u3_hrep));

    rep_u->sev_l = sev_l;
    rep_u->coq_l = coq_l;
    rep_u->seq_l = seq_l;

    rep_u->sas_w = p_rep;
    rep_u->hed_u = _http_list_to_heds(u3k(q_rep));
    rep_u->bod_u = (u3_nul == r_rep) ? 0 : _http_octs_to_bod(u3k(u3t(r_rep)));

    u3z(rep); return rep_u;
  }
}

/* _http_request(): dispatch http request, returning null if async.
*/
static void
_http_request(u3_hreq* req_u)
{
  u3_noun req = _http_request_to_noun(req_u);

  if ( u3_none != req ) {
    u3_noun pox = _http_pox_to_noun(req_u->hon_u->htp_u->sev_l,
                                    req_u->hon_u->coq_l,
                                    req_u->seq_l);

    u3v_plan(pox,
               u3nq(c3__this,
                    req_u->hon_u->htp_u->sec,
                    u3nc(c3y, u3i_words(1, &req_u->ipf_w)),
                    req));
  }
}

/* _http_request_dead(): kill http request.
*/
static void
_http_request_dead(u3_hreq* req_u)
{
  u3_noun pox = _http_pox_to_noun(req_u->hon_u->htp_u->sev_l,
                                  req_u->hon_u->coq_l,
                                  req_u->seq_l);

  u3v_plan(pox, u3nc(c3__thud, u3_nul));
}

/* _http_flush(): transmit any ready data.
*/
static void
_http_flush(u3_hcon* hon_u)
{
  while ( hon_u->req_u ) {
    u3_hreq* req_u = hon_u->req_u;
    u3_hbod* rub_u = req_u->rub_u;

    if ( 0 == rub_u ) {
      if ( c3y == req_u->end ) {
        hon_u->req_u = req_u->nex_u;
        if ( 0 == hon_u->req_u ) {
          c3_assert(req_u == hon_u->qer_u);
          hon_u->qer_u = 0;
        }
        _http_req_free(req_u);
        continue;
      }
      else {
        //  We have not yet finished adding responses to this
        //  current request - so, we cannot start writing the next.
        //
        break;
      }
    }
    else {
      _http_send_body(req_u, rub_u);

      req_u->rub_u = req_u->rub_u->nex_u;
      if ( 0 == req_u->rub_u ) {
        c3_assert(rub_u == req_u->bur_u);
        req_u->bur_u = 0;
      }

      free(rub_u);
    }
  }
}

/* _http_respond(): attach http response.
*/
static void
_http_respond(u3_hrep* rep_u)
{
  u3_http* htp_u;
  u3_hcon* hon_u;
  u3_hreq* req_u;

  if ( !(htp_u = _http_serv_find(rep_u->sev_l)) ) {
    if ( (u3C.wag_w & u3o_verbose) ) {
      uL(fprintf(uH, "http: server not found: %x\r\n", rep_u->sev_l));
    }
    return;
  }
  if ( !(hon_u = _http_conn_find(htp_u, rep_u->coq_l)) ) {
    if ( (u3C.wag_w & u3o_verbose) ) {
      uL(fprintf(uH, "http: connection not found: %x/%d\r\n",
                     rep_u->sev_l,
                     rep_u->coq_l));
    }
    return;
  }
  if ( !(req_u = _http_req_find(hon_u, rep_u->seq_l)) ) {
    if ( (u3C.wag_w & u3o_verbose) ) {
      uL(fprintf(uH, "http: request not found: %x/%d/%d\r\n",
                    rep_u->sev_l,
                    rep_u->coq_l,
                    rep_u->seq_l));
    }
    return;
  }
#if 0
  uL(fprintf(uH, "http: responding: %x/%d/%d\r\n",
                  rep_u->sev_l,
                  rep_u->coq_l,
                  rep_u->seq_l));
#endif
  _http_respond_request(req_u, rep_u);

  _http_flush(hon_u);
}

void
u3_http_ef_bake(void)
{
  u3_noun pax = u3nq(u3_blip, c3__http, u3k(u3A->sen), u3_nul);

  u3v_plan(pax, u3nc(c3__born, u3_nul));
}

/* u3_http_ef_thou(): send %thou effect (incoming response) to http.
*/
void
u3_http_ef_thou(c3_l     sev_l,
                c3_l     coq_l,
                c3_l     seq_l,
                u3_noun  rep)
{
  u3_hrep* rep_u = _http_new_response(sev_l, coq_l, seq_l, rep);

  if ( !rep_u ) {
    uL(fprintf(uH, "http: response dropped\r\n"));
  }
  else _http_respond(rep_u);
}

/* _http_listen_cb(): listen callback.
*/
static void
_http_listen_cb(uv_stream_t* str_u, c3_i sas_i)
{
  u3_http* htp_u = (u3_http*)str_u;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "http: listen_cb: ERROR\n"));
  }
  else {
    _http_conn_new(htp_u);
  }
}

/* _http_start(): start http server.
*/
static void
_http_start(u3_http* htp_u)
{
  struct sockaddr_in add_u;

  uv_tcp_init(u3L, &htp_u->wax_u);

  memset(&add_u, 0, sizeof(add_u));
  add_u.sin_family = AF_INET;
  add_u.sin_addr.s_addr = INADDR_ANY;

  /*  Try ascending ports.
  */
  while ( 1 ) {
    add_u.sin_port = htons(htp_u->por_w);

    int ret;
    if ( 0 != (ret = uv_tcp_bind(&htp_u->wax_u, (const struct sockaddr*) & add_u, 0))  ) {

      if ( UV_EADDRINUSE == ret ) {
        htp_u->por_w++;
        continue;
      }
      else {
        uL(fprintf(uH, "http: bind: %s\n", uv_strerror(ret)));
      }
    }
    if ( 0 != (ret = uv_listen((uv_stream_t*)&htp_u->wax_u, 16, _http_listen_cb)) ) {
      if ( UV_EADDRINUSE == ret ) {
        htp_u->por_w++;
        continue;
      }
      else {
        uL(fprintf(uH, "http: listen: %s\n", uv_strerror(ret)));
      }
    }
#if 1
    uL(fprintf(uH, "http: live (%s) on %d\n",
                   (c3y == htp_u->sec) ? "\"secure\"" : "insecure",
                   htp_u->por_w));
#endif
    break;
  }
}

/* u3_http_io_init(): initialize http I/O.
*/
void
u3_http_io_init()
{
  //  Logically secure port.
  {
    u3_http *htp_u = c3_malloc(sizeof(*htp_u));

    htp_u->sev_l = u3A->sev_l + 1;
    htp_u->coq_l = 1;
    htp_u->por_w = 8443;
    htp_u->sec = c3y;

    htp_u->hon_u = 0;
    htp_u->nex_u = 0;

    htp_u->nex_u = u3_Host.htp_u;
    u3_Host.htp_u = htp_u;
  }

  //  Insecure port.
  //
  {
    u3_http *htp_u = c3_malloc(sizeof(*htp_u));

    htp_u->sev_l = u3A->sev_l;
    htp_u->coq_l = 1;
    htp_u->por_w = 8080;
    htp_u->sec = c3n;

    htp_u->hon_u = 0;
    htp_u->nex_u = 0;

    htp_u->nex_u = u3_Host.htp_u;
    u3_Host.htp_u = htp_u;
  }

  u3_Host.ctp_u.coc_u = 0;
}

/* u3_http_io_talk(): bring up listener.
*/
void
u3_http_io_talk()
{
  u3_http* htp_u;

  for ( htp_u = u3_Host.htp_u; htp_u; htp_u = htp_u->nex_u ) {
    _http_start(htp_u);
  }
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
}
