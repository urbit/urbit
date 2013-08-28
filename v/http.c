/* v/http.c
**
** This file is in the public domain.
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
#include "v/vere.h"

static void _http_request(u2_hreq* req_u);
static void _http_conn_dead(u2_hcon *hon_u);

/* _http_alloc(): libuv buffer allocator.
*/
static uv_buf_t
_http_alloc(uv_handle_t* had_u, size_t len_i)
{
  return uv_buf_init(malloc(len_i), len_i);
}

/* _http_bod(): create a data buffer.
*/
static u2_hbod*
_http_bod(c3_w len_w, const c3_y* hun_y)
{
  u2_hbod* bod_u = malloc(len_w + sizeof(*bod_u));

  bod_u->len_w = len_w;
  memcpy(bod_u->hun_y, hun_y, len_w);
  
  bod_u->nex_u = 0;
  return bod_u;
}

/* _http_bud(): create a header buffer.  Not null-terminated!
*/
static u2_hbod*
_http_bud(c3_c* nam_c, c3_c* val_c)
{
  c3_w lnm_w     = strlen(nam_c);
  c3_w lvl_w     = strlen(val_c);
  c3_w len_w     = lnm_w + 2 + lvl_w + 2;
  u2_hbod* bod_u = malloc(len_w + sizeof(*bod_u));

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
_http_heds_free(u2_hhed* hed_u)
{
  if ( hed_u ) {
    u2_hhed* nex_u = hed_u->nex_u;

    if ( hed_u->nam_c ) free(hed_u->nam_c);
    if ( hed_u->val_c ) free(hed_u->val_c);

    free(hed_u);
    hed_u = nex_u;
  }
}

/* _http_bods_free(): free body structure.
*/
static void
_http_bods_free(u2_hbod* bod_u)
{
  if ( bod_u ) {
    u2_hbod* nex_u = bod_u->nex_u;

    free(bod_u);
    bod_u = nex_u;
  }
}

/* _http_req_free(): free http request.
*/
static void 
_http_req_free(u2_hreq* req_u)
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
  } _u2_write_t;

/* _http_write_cb(): general write callback.
*/
static void
_http_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  _u2_write_t* ruq_u = (void *)wri_u;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "http: write: %s\n", uv_strerror(uv_last_error(u2L))));
  }
  free(ruq_u->buf_y);
  free(ruq_u);
}
 
/* _http_respond_buf(): write back to http.
*/
static void
_http_respond_buf(u2_hreq* req_u, uv_buf_t buf_u)
{
  _u2_write_t* ruq_u = (_u2_write_t*) malloc(sizeof(_u2_write_t));

  ruq_u->buf_y = (c3_y*)buf_u.base;

  if ( 0 != uv_write(&ruq_u->wri_u, 
                     (uv_stream_t*)&(req_u->hon_u->wax_u),
                     &buf_u, 1, 
                     _http_write_cb) )
  {
    uL(fprintf(uH, "respond: %s\n", uv_strerror(uv_last_error(u2L))));
    _http_conn_dead(req_u->hon_u);
  }
}
 
/* _http_respond_body(): attach response body.
*/
static void
_http_send_body(u2_hreq *req_u,
                u2_hbod *rub_u)
{
  uv_buf_t buf_u;

  //  XX extra copy here due to old code.  Use hbod as base directly.
  //
  {
    c3_y* buf_y = malloc(rub_u->len_w);

    memcpy(buf_y, rub_u->hun_y, rub_u->len_w);
    buf_u = uv_buf_init((c3_c*)buf_y, rub_u->len_w);
  }
  _http_respond_buf(req_u, buf_u);
}

/* _http_respond_body(): attach response body.
*/
static void
_http_respond_body(u2_hreq *req_u,
                   u2_hbod *rub_u)
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
_http_respond_str(u2_hreq*    req_u,
                  const c3_c* str_c)
{
  _http_respond_body(req_u, _http_bod(strlen(str_c), (const c3_y*)str_c));
}

/* _http_respond_headers(): attach output headers.
*/
static void
_http_respond_headers(u2_hreq* req_u,
                      u2_hhed* hed_u)
{
  while ( hed_u ) {
    _http_respond_body(req_u, _http_bud(hed_u->nam_c, hed_u->val_c));
    hed_u = hed_u->nex_u;
  }
}

/* _http_respond_request(): attach response to request, then free it.
*/
static void
_http_respond_request(u2_hreq* req_u,
                      u2_hrep* rep_u)
{
  c3_c buf_c[81];

  sprintf(buf_c, "HTTP/1.1 %d %s\r\n",
                 rep_u->sas_w,
                 (rep_u->sas_w == 200) ? "OK" : "Hosed");
  _http_respond_str(req_u, buf_c);

  // printf("attached response status %d\n", rep_u->sas_w);
  _http_respond_headers(req_u, rep_u->hed_u);
  _http_heds_free(rep_u->hed_u);

  //  Why is this necessary?  Why can't we send a naked error?  Waah.
  //
  if ( !rep_u->bod_u ) {
    sprintf(buf_c, "HTTP error %d.\r\n", rep_u->sas_w);
    rep_u->bod_u = _http_bod(strlen(buf_c), (c3_y*) buf_c);
  }

  {
    sprintf(buf_c, "content-length: %u\r\n", rep_u->bod_u->len_w);
    _http_respond_str(req_u, buf_c);

    _http_respond_str(req_u, "\r\n");
    _http_respond_body(req_u, rep_u->bod_u);
  }
  free(rep_u);

  c3_assert(u2_no == req_u->end);
  req_u->end = u2_yes;
}

/* _http_conn_free(): free http connection on close.
*/
static void
_http_conn_free(uv_handle_t* han_t)
{
  u2_hcon* hon_u = (void*) han_t;

  {
    struct _u2_http *htp_u = hon_u->htp_u;

    if ( htp_u->hon_u == hon_u ) {
      htp_u->hon_u = hon_u->nex_u;
    }
    else {
      u2_hcon *pre_u = htp_u->hon_u;

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
    u2_hreq* req_u = hon_u->req_u;
    u2_hreq* nex_u = req_u->nex_u;

    _http_req_free(req_u);
    hon_u->req_u = nex_u;
  }
  free(hon_u);
}

/* _http_conn_dead(): free http connection, close fd.
*/
static void
_http_conn_dead(u2_hcon *hon_u)
{ 
  uv_read_stop((uv_stream_t*) &(hon_u->wax_u));
  uv_close((uv_handle_t*) &(hon_u->wax_u), _http_conn_free);
}

#if 0
/* _http_req_dump(): dump complete http request.
*/
static void
_http_req_dump(u2_hreq* req_u)
{
  c3_c* mets[] = { "delete", "get", "head", "post", "put", "other" };

  printf("%s %s\n", mets[req_u->met_e],
                    req_u->url_c ? req_u->url_c : "(null)");
  {
    u2_hhed* hed_u = req_u->hed_u;

    while ( hed_u ) {
      printf("%s: %s\r\n", hed_u->nam_c, hed_u->val_c);
      hed_u = hed_u->nex_u;
    }
  }
  {
    u2_hbod* bod_u = req_u->bod_u;
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
    str_c = malloc(siz_i + 1);
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
  u2_hreq *req_u = par_u->data;

  req_u->url_c = _http_more(req_u->url_c, buf_c, siz_i);
  return 0;
}
 
/* _http_header_field(): jhttp callback
*/
static c3_i
_http_header_field(http_parser* par_u, const c3_c* buf_c, size_t siz_i)
{
  u2_hreq *req_u = par_u->data;

  switch ( req_u->rat_e ) {
    case u2_hreq_non: 
    case u2_hreq_val: {
      u2_hhed* hed_u = malloc(sizeof(*hed_u));

      hed_u->nam_c = _http_more(0, buf_c, siz_i);
      hed_u->val_c = 0;
      hed_u->nex_u = req_u->hed_u;
      req_u->hed_u = hed_u;

      break;
    }
    case u2_hreq_nam: {
      req_u->hed_u->nam_c = _http_more(req_u->hed_u->nam_c, buf_c, siz_i);
      break;
    }
  }
  req_u->rat_e = u2_hreq_nam;
  return 0;
}
 
/* _http_header_value(): jhttp callback
*/
static c3_i
_http_header_value(http_parser* par_u, const c3_c* buf_c, size_t siz_i)
{
  u2_hreq *req_u = par_u->data;

  switch ( req_u->rat_e ) {
    case u2_hreq_non: fprintf(stderr, "http: odd value\r\n"); return 1;

    case u2_hreq_nam: {
      req_u->hed_u->val_c = _http_more(0, buf_c, siz_i);
      break;
    }
    case u2_hreq_val: {
      req_u->hed_u->val_c = _http_more(req_u->hed_u->val_c, buf_c, siz_i);
      break;
    }
  }
  req_u->rat_e = u2_hreq_val;
  return 0;
}

/* _http_headers_complete(): jhttp callback
*/
static c3_i
_http_headers_complete(http_parser* par_u)
{
  u2_hreq *req_u = par_u->data;

  if ( par_u->method >= u2_hmet_other ) {
    req_u->met_e = u2_hmet_other;
  } else req_u->met_e = par_u->method;

  return 0;
}

/* _http_body(): jhttp callback
*/
static c3_i
_http_body(http_parser* par_u, const c3_c* buf_c, size_t siz_i)
{
  {
    u2_hreq *req_u = par_u->data;
    u2_hbod* bod_u; 
  
    bod_u = _http_bod(siz_i, (const c3_y*)buf_c);

    bod_u->nex_u = req_u->bod_u;
    req_u->bod_u = bod_u;
   
    return 0;
  }
}

/* _http_message_complete(): jhttp callback
*/
static c3_i
_http_message_complete(http_parser* par_u)
{
  u2_hreq* req_u = par_u->data;
  u2_hcon* hon_u = req_u->hon_u;

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
static u2_hreq*
_http_req_new(u2_hcon* hon_u)
{
  u2_hreq* req_u = malloc(sizeof(*req_u));

  req_u->hon_u = hon_u;
  req_u->seq_l = hon_u->seq_l++;

  req_u->met_e = (u2_hmet)0;
  req_u->rat_e = (u2_hrat)0;

  req_u->par_u = malloc(sizeof(struct http_parser));
  http_parser_init(req_u->par_u, HTTP_REQUEST);
  ((struct http_parser *)(req_u->par_u))->data = req_u;

  req_u->liv = u2_no;
  req_u->end = u2_no;

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
static void
_http_conn_read_cb(uv_stream_t* tcp_u, 
                   ssize_t      siz_i,
                   uv_buf_t     buf_u)
{
  u2_hcon* hon_u = (u2_hcon*)(void*) tcp_u;

  u2_lo_open();
  {
    if ( siz_i < 0 ) {
      uv_err_t las_u = uv_last_error(u2L);

      if ( UV_EOF != las_u.code ) {
        uL(fprintf(uH, "http: read: %s\n", uv_strerror(las_u)));
      }
      _http_conn_dead(hon_u);
    }
    else {
      if ( !hon_u->ruc_u ) {
        hon_u->ruc_u = _http_req_new(hon_u);
      }

      if ( siz_i != http_parser_execute(hon_u->ruc_u->par_u, 
                                        &_http_settings, 
                                        (c3_c*)buf_u.base,
                                        siz_i) )
      {
        uL(fprintf(uH, "http: parse error\n"));
        _http_conn_dead(hon_u);
      }
    }
    if ( buf_u.base ) {
      free(buf_u.base);
    }
  }
  u2_lo_shut(u2_yes);
}

/* _http_conn_new(): create http connection.
*/
static void
_http_conn_new(u2_http *htp_u)
{
  u2_hcon *hon_u = malloc(sizeof(*hon_u));

  uv_tcp_init(u2L, &hon_u->wax_u);

  if ( 0 != uv_accept((uv_stream_t*)&htp_u->wax_u, 
                      (uv_stream_t*)&hon_u->wax_u) )
  {
    uL(fprintf(uH, "http: accept: %s\n", 
                    uv_strerror(uv_last_error(u2L))));

    uv_close((uv_handle_t*)&hon_u->wax_u, 0);
    free(hon_u);
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
static u2_hreq*
_http_req_find(u2_hcon* hon_u, c3_w seq_l)
{
  u2_hreq* req_u = hon_u->req_u;

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

/* _http_conn_find(): find http connection by sequence.
*/
static u2_hcon*
_http_conn_find(u2_http *htp_u, c3_w coq_l)
{
  u2_hcon* hon_u = htp_u->hon_u;

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
static u2_noun
_http_heds_to_list(u2_hhed* hed_u)
{
  if ( 0 == hed_u ) {
    return u2_nul;
  } else {
    return u2nc(u2nc(u2_ci_string(hed_u->nam_c),
                     u2_ci_string(hed_u->val_c)),
                _http_heds_to_list(hed_u->nex_u));
  }
}

/* _http_list_to_heds(): list to C headers.
*/
static u2_hhed*
_http_list_to_heds(u2_noun lix)
{
  u2_noun  yix = lix;
  u2_hhed* hed_u = 0;

  while ( 1 ) {
    if ( u2_nul == lix ) {
      break;
    } 
    else {
      u2_noun  i_lix = u2h(lix);
      u2_noun  pi_lix = u2h(i_lix);
      u2_noun  qi_lix = u2t(i_lix);
      u2_noun  t_lix = u2t(lix);
      u2_hhed* nex_u = malloc(sizeof(u2_hhed));

      nex_u->nam_c = u2_cr_string(pi_lix);
      nex_u->val_c = u2_cr_string(qi_lix);
      nex_u->nex_u = hed_u;

      hed_u = nex_u;
      lix = t_lix;
    }
  }
  u2z(yix);
  return hed_u;
}

/* _http_bods_to_octs: translate body into octet-stream noun.
*/
static u2_noun
_http_bods_to_octs(u2_hbod* bod_u)
{
  c3_w    len_w;
  c3_y*   buf_y;
  u2_noun cos;

  {
    u2_hbod* bid_u;

    len_w = 0;
    for ( bid_u = bod_u; bid_u; bid_u = bid_u->nex_u ) {
      len_w += bid_u->len_w;
    }
  }
  buf_y = malloc(len_w);

  {
    c3_y* ptr_y = buf_y;

    while ( bod_u ) {
      memcpy(ptr_y, bod_u->hun_y, bod_u->len_w);
      ptr_y += bod_u->len_w;
      bod_u = bod_u->nex_u;
    }
  }
  cos = u2_ci_bytes(len_w, buf_y);
  free(buf_y);
  return u2nc(len_w, cos);
}

/* _http_octs_to_bod(): translate octet-stream noun into body.
*/
static u2_hbod*
_http_octs_to_bod(u2_noun oct)
{
  c3_w len_w;

  if ( !u2_fly_is_cat(u2h(oct)) ) {
    //  2GB max
    u2_cm_bail(c3__fail); return 0;
  }
  len_w = u2h(oct);

  {
    u2_hbod* bod_u = malloc(len_w + sizeof(*bod_u));

    bod_u->len_w = len_w;
    u2_cr_bytes(0, len_w, bod_u->hun_y, u2t(oct));
   
    bod_u->nex_u = 0;

    u2z(oct);
    return bod_u;
  }
}

/* _http_pox_to_noun(): translate srv/con/req to path noun (pox).
*/
static u2_noun
_http_pox_to_noun(c3_w sev_l, c3_w coq_l, c3_w seq_l)
{
  return 
    u2nt(
      c3__iron,
      c3__http,
      u2nq(u2_dc("scot", c3_s2('u','v'), sev_l),
           u2_dc("scot", c3_s2('u','d'), coq_l),
           u2_dc("scot", c3_s2('u','d'), seq_l),
           u2_nul));
}

/* _http_request_to_noun(): translate http request into noun, or u2_none.
*/
static u2_noun
_http_request_to_noun(u2_hreq* req_u)
{
  u2_noun med, url, hed, bod;

  switch ( req_u->met_e ) {
    default: fprintf(stderr, "strange request\r\n"); return u2_none;
    case u2_hmet_get: { med = c3__get; break; }
    case u2_hmet_post: { med = c3__post; break; }
  } 
  url = u2_ci_string(req_u->url_c);
  hed = _http_heds_to_list(req_u->hed_u);
  bod = req_u->bod_u ? u2nc(u2_nul, _http_bods_to_octs(req_u->bod_u)) : u2_nul;

  return u2nq(med, url, hed, bod);
}

/* _http_new_response(): create http response structure.
*/
static u2_hrep*
_http_new_response(c3_l coq_l, c3_l seq_l, u2_noun rep)
{
  u2_noun p_rep, q_rep, r_rep;

  if ( u2_no == u2_cr_trel(rep, &p_rep, &q_rep, &r_rep) ) {
    uL(fprintf(uH, "strange response\n"));
    return 0;
  }
  else {
    u2_hrep* rep_u = malloc(sizeof(u2_hrep));

    rep_u->coq_l = coq_l;
    rep_u->seq_l = seq_l;

    rep_u->sas_w = p_rep;
    rep_u->hed_u = _http_list_to_heds(u2k(q_rep));
    rep_u->bod_u = (u2_nul == r_rep) ? 0 : _http_octs_to_bod(u2k(u2t(r_rep)));

    u2z(rep); return rep_u;
  }
}

/* _http_request(): dispatch http request, returning null if async.
*/
static void
_http_request(u2_hreq* req_u)
{
  u2_noun req = _http_request_to_noun(req_u);

  if ( u2_none != req ) {
    u2_noun pox = _http_pox_to_noun(req_u->hon_u->htp_u->sev_l,
                                    req_u->hon_u->coq_l,
                                    req_u->seq_l); 

    u2_reck_plan(u2_Host.arv_u, pox, u2nq(c3__this, u2_yes, 0, req));
  }
}

/* _http_flush(): transmit any ready data.
*/
static void
_http_flush(u2_hcon* hon_u)
{
  while ( hon_u->req_u ) {
    u2_hreq* req_u = hon_u->req_u;
    u2_hbod* rub_u = req_u->rub_u;

    if ( 0 == rub_u ) {
      if ( u2_yes == req_u->end ) {
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
_http_respond(u2_hrep* rep_u)
{
  u2_http* htp_u = u2_Host.htp_u;
  u2_hcon* hon_u;
  u2_hreq* req_u;

  if ( !(hon_u = _http_conn_find(htp_u, rep_u->coq_l)) ) {
    uL(fprintf(uH, "http: connection not found: %d\r\n", rep_u->coq_l));
    return;
  }
  if ( !(req_u = _http_req_find(hon_u, rep_u->seq_l)) ) {
    uL(fprintf(uH, "http: request not found: %d\r\n", rep_u->seq_l));
    return;
  }
  _http_respond_request(req_u, rep_u);

  _http_flush(hon_u);
}

void
u2_http_ef_bake(void)
{
  u2_noun pax = u2nq(c3__gold, c3__http, u2k(u2A->sen), u2_nul);

  u2_reck_plan(u2A, pax, u2nc(c3__born, u2_nul));
}

/* u2_http_ef_thou(): send %thou effect to http. 
*/
void
u2_http_ef_thou(c3_l     coq_l,
                c3_l     seq_l,
                u2_noun  rep)
{
  u2_hrep* rep_u = _http_new_response(coq_l, seq_l, rep);

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
  u2_http* htp_u = (u2_http*)str_u;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "http: listen_cb: %s\n", 
                    uv_strerror(uv_last_error(u2L))));
  }
  else {
    _http_conn_new(htp_u);
  }
}

/* _http_start(): start http server.
*/
static void
_http_start(u2_http* htp_u)
{
  struct sockaddr_in add_u;

  uv_tcp_init(u2L, &htp_u->wax_u);

  memset(&add_u, 0, sizeof(add_u));
  add_u.sin_family = AF_INET;
  add_u.sin_addr.s_addr = INADDR_ANY;

  /*  Try ascending ports.
  */
  while ( 1 ) {
    add_u.sin_port = htons(htp_u->por_w);

    if ( 0 != uv_tcp_bind(&htp_u->wax_u, add_u)  ) {
      uv_err_t las_u = uv_last_error(u2L);

      if ( UV_EADDRINUSE == las_u.code ) {
        htp_u->por_w++; 
        continue;
      }
      else {
        uL(fprintf(uH, "http: bind: %s\n", uv_strerror(las_u)));
      }
    }
    if ( 0 != uv_listen((uv_stream_t*)&htp_u->wax_u, 16, _http_listen_cb) ) {
      uv_err_t las_u = uv_last_error(u2L);

      if ( UV_EADDRINUSE == las_u.code ) {
        htp_u->por_w++; 
        continue;
      }
      else {
        uL(fprintf(uH, "http: listen: %s\n", uv_strerror(las_u)));
      }
    }
    uL(fprintf(uH, "http: live on %d\r\n", htp_u->por_w));
    break;
  }
}

/* u2_http_io_init(): initialize http I/O.
*/
void 
u2_http_io_init()
{
  u2_http *htp_u = malloc(sizeof(*htp_u));

  htp_u->sev_l = u2A->sev_l;
  htp_u->coq_l = 1;
  htp_u->por_w = 8080;

  htp_u->hon_u = 0;
  htp_u->nex_u = 0;

#if 1
  _http_start(htp_u);
#endif

  htp_u->nex_u = u2_Host.htp_u;
  u2_Host.htp_u = htp_u;
}

/* u2_http_io_poll(): poll kernel for http I/O.
*/
void
u2_http_io_poll(void)
{
}

/* u2_http_io_exit(): shut down http.
*/
void
u2_http_io_exit(void)
{
}
