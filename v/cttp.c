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

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/rand.h>

#include "../outside/jhttp/http_parser.h"   // Joyent HTTP
#include "all.h"
#include "v/vere.h"

#ifdef U2_OS_osx
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#  pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

/* Forward declarations.
*/
  static void  _cttp_ccon_kick(u2_ccon* coc_u);
  static void  _cttp_ccon_cryp_hurr(u2_ccon* coc_u, c3_i rev_i);
  static void  _cttp_ccon_cryp_rout(u2_ccon* coc_u);
  static void  _cttp_ccon_fill(u2_ccon* coc_u);
  static void  _cttp_ccon_fire(u2_ccon* coc_u, u2_creq* ceq_u);
  static c3_c* _cttp_creq_url(u2_noun pul);

/* _cttp_alloc(): libuv buffer allocator.
*/
static uv_buf_t
_cttp_alloc(uv_handle_t* had_u, size_t len_i)
{
  return uv_buf_init(malloc(len_i), len_i);
}

/* _cttp_bod(): create a data buffer.
*/
static u2_hbod*
_cttp_bod(c3_w len_w, const c3_y* hun_y)
{
  u2_hbod* bod_u = malloc(len_w + sizeof(*bod_u));

  bod_u->len_w = len_w;
  memcpy(bod_u->hun_y, hun_y, len_w);

  bod_u->nex_u = 0;
  return bod_u;
}

/* _cttp_bud(): create a header buffer.  Not null-terminated!
*/
static u2_hbod*
_cttp_bud(c3_c* nam_c, c3_c* val_c)
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

/* _cttp_heds_to_list(): C headers to list.
*/
static u2_noun
_cttp_heds_to_list(u2_hhed* hed_u)
{
  if ( 0 == hed_u ) {
    return u2_nul;
  } else {
    return u2nc(u2nc(u2_ci_string(hed_u->nam_c),
                     hed_u->val_c ? u2_ci_string(hed_u->val_c) : u2_nul),
                _cttp_heds_to_list(hed_u->nex_u));
  }
}

/* _cttp_heds_free(): free header structure.
*/
static void
_cttp_heds_free(u2_hhed* hed_u)
{
  if ( hed_u ) {
    u2_hhed* nex_u = hed_u->nex_u;

    if ( hed_u->nam_c ) free(hed_u->nam_c);
    if ( hed_u->val_c ) free(hed_u->val_c);

    free(hed_u);
    hed_u = nex_u;
  }
}

/* _cttp_bods_free(): free body structure.
*/
static void
_cttp_bods_free(u2_hbod* bod_u)
{
  if ( bod_u ) {
    u2_hbod* nex_u = bod_u->nex_u;

    free(bod_u);
    bod_u = nex_u;
  }
}

/* _cttp_bods_to_octs: translate body into octet-stream noun.
*/
static u2_noun
_cttp_bods_to_octs(u2_hbod* bod_u)
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

/* _cttp_heds_list(): create headers from list.
*/
static u2_hhed*
_cttp_heds_list(u2_hhed* hed_u, u2_noun nam, u2_noun vaz)
{
  u2_noun viz = vaz;

  while ( u2_nul != viz ) {
    u2_hhed* deh_u;

    deh_u = malloc(sizeof(*deh_u));
    deh_u->nam_c = u2_cr_string(nam);
    deh_u->val_c = u2_cr_string(u2h(viz));

    deh_u->nex_u = hed_u;
    hed_u = deh_u;

    viz = u2t(viz);
  }
  u2z(nam);
  u2z(vaz);
  return hed_u;
}

/* _cttp_heds_math(): create headers from noun.
*/
static u2_hhed*
_cttp_heds_math(u2_hhed* hed_u, u2_noun mah)
{
  if ( u2_nul == mah ) {
    return hed_u;
  }
  else {
    u2_noun n_mah = u2h(mah);
    u2_noun pn_mah = u2h(n_mah);
    u2_noun qn_mah = u2t(n_mah);
    u2_noun l_mah = u2h(u2t(mah));
    u2_noun r_mah = u2t(u2t(mah));

    hed_u = _cttp_heds_list(hed_u, u2k(pn_mah), u2k(qn_mah));
    hed_u = _cttp_heds_math(hed_u, u2k(l_mah));
    hed_u = _cttp_heds_math(hed_u, u2k(r_mah));

    u2z(mah);
    return hed_u;
  }
}

/* _cttp_octs_to_bod(): translate octet-stream noun into body.
*/
static u2_hbod*
_cttp_octs_to_bod(u2_noun oct)
{
  c3_w len_w;

  if ( !u2_fly_is_cat(u2h(oct)) ) {     //  2GB max
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

/* _cttp_mcut_char(): measure/cut character.
*/
static c3_w
_cttp_mcut_char(c3_c* buf_c, c3_w len_w, c3_c chr_c)
{
  if ( buf_c ) {
    buf_c[len_w] = chr_c;
  }
  return len_w + 1;
}

/* _cttp_mcut_str(): measure/cut string.
*/
static c3_w
_cttp_mcut_str(c3_c* buf_c, c3_w len_w, const c3_c* str_c)
{
  c3_w str_w = strlen(str_c);

  if ( buf_c ) {
    strncpy(buf_c + len_w, str_c, str_w);
  }
  return (len_w + str_w);
}

/* _cttp_mcut_span(): measure/cut span.
*/
static c3_w
_cttp_mcut_span(c3_c* buf_c, c3_w len_w, u2_noun san)
{
  c3_w ten_w = u2_cr_met(3, san);

  if ( buf_c ) {
    u2_cr_bytes(0, ten_w, (c3_y *)(buf_c + len_w), san);
  }
  u2z(san);
  return (len_w + ten_w);
}

/* _cttp_mcut_path(): measure/cut span list.
*/
static c3_w
_cttp_mcut_path(c3_c* buf_c, c3_w len_w, c3_c sep_c, u2_noun pax)
{
  u2_noun axp = pax;

  while ( u2_nul != axp ) {
    u2_noun h_axp = u2h(axp);

    len_w = _cttp_mcut_span(buf_c, len_w, u2k(h_axp));
    axp = u2t(axp);

    if ( u2_nul != axp ) {
      len_w = _cttp_mcut_char(buf_c, len_w, sep_c);
    }
  }
  u2z(pax);
  return len_w;
}

/* _cttp_mcut_host(): measure/cut host.
*/
static c3_w
_cttp_mcut_host(c3_c* buf_c, c3_w len_w, u2_noun hot)
{
  if ( u2_yes == u2h(hot) ) {
    len_w = _cttp_mcut_path(buf_c, len_w, '.', u2_ckb_flop(u2k(u2t(hot))));
  }
  else {
    c3_w ipf_w = u2_cr_word(0, u2t(hot));
    c3_c ipf_c[17];

    snprintf(ipf_c, 16, "%d.%d.%d.%d", (ipf_w >> 24),
                                       ((ipf_w >> 16) & 255),
                                       ((ipf_w >> 8) & 255),
                                       (ipf_w & 255));
    len_w = _cttp_mcut_str(buf_c, len_w, ipf_c);
  }
  u2z(hot);
  return len_w;
}

#if 0
/* _cttp_mcut_pfix(): measure/cut prefix.
*/
static c3_w
_cttp_mcut_pfix(c3_c* buf_c, c3_w len_w, u2_noun hat)
{
  u2_noun p_hat = u2h(hat);
  u2_noun q_hat = u2h(u2t(hat));
  u2_noun r_hat = u2t(u2t(hat));

  if ( u2_yes == p_hat ) {
    len_w = _cttp_mcut_str(buf_c, len_w, "https://");
  } else {
    len_w = _cttp_mcut_str(buf_c, len_w, "http://");
  }
  len_w = _cttp_mcut_host(buf_c, len_w, u2k(r_hat));

  if ( u2_nul != q_hat ) {
    c3_w por_w = 0xffff & u2_cr_word(0, u2t(q_hat));
    c3_c por_c[8];

    snprintf(por_c, 7, ":%d", por_w);
    len_w = _cttp_mcut_str(buf_c, len_w, por_c);
  }
  u2z(hat);
  return len_w;
}
#endif

/* _cttp_mcut_pork(): measure/cut path/extension.
*/
static c3_w
_cttp_mcut_pork(c3_c* buf_c, c3_w len_w, u2_noun pok)
{
  u2_noun h_pok = u2h(pok);
  u2_noun t_pok = u2t(pok);

  len_w = _cttp_mcut_path(buf_c, len_w, '/', u2k(t_pok));
  if ( u2_nul != h_pok ) {
    len_w = _cttp_mcut_char(buf_c, len_w, '.');
    len_w = _cttp_mcut_span(buf_c, len_w, u2k(u2t(h_pok)));
  }
  u2z(pok);
  return len_w;
}

/* _cttp_mcut_quay(): measure/cut query.
*/
static c3_w
_cttp_mcut_quay(c3_c* buf_c, c3_w len_w, u2_noun quy)
{
  if ( u2_nul == quy ) {
    return len_w;
  }
  else {
    u2_noun n_quy = u2h(quy);
    u2_noun pn_quy = u2h(n_quy);
    u2_noun qn_quy = u2t(n_quy);
    u2_noun l_quy = u2h(u2t(quy));
    u2_noun r_quy = u2t(u2t(quy));

    len_w = _cttp_mcut_char(buf_c, len_w, '&');
    len_w = _cttp_mcut_span(buf_c, len_w, u2k(pn_quy));
    len_w = _cttp_mcut_char(buf_c, len_w, '=');
    len_w = _cttp_mcut_span(buf_c, len_w, u2k(qn_quy));

    len_w = _cttp_mcut_quay(buf_c, len_w, u2k(l_quy));
    len_w = _cttp_mcut_quay(buf_c, len_w, u2k(r_quy));
  }
  u2z(quy);
  return len_w;
}

/* _cttp_mcut_url(): measure/cut purl, producing relative URL.
*/
static c3_w
_cttp_mcut_url(c3_c* buf_c, c3_w len_w, u2_noun pul)
{
  u2_noun q_pul = u2h(u2t(pul));
  u2_noun r_pul = u2t(u2t(pul));

  // len_w = _cttp_mcut_pfix(buf_c, len_w, u2k(p_pul));
  len_w = _cttp_mcut_char(buf_c, len_w, '/');
  len_w = _cttp_mcut_pork(buf_c, len_w, u2k(q_pul));

  if ( u2_nul != r_pul ) {
    len_w = _cttp_mcut_char(buf_c, len_w, '?');
    len_w = _cttp_mcut_quay(buf_c, len_w, u2k(r_pul));
  }
  u2z(pul);
  return len_w;
}

/* _cttp_creq_url(): construct url from noun.
*/
static c3_c*
_cttp_creq_url(u2_noun pul)
{
  c3_w  len_w = _cttp_mcut_url(0, 0, u2k(pul));
  c3_c* url_c = malloc(len_w + 1);

  _cttp_mcut_url(url_c, 0, pul);
  url_c[len_w] = 0;

  return url_c;
}

/* _cttp_creq_host(): construct host from noun.
*/
static c3_c*
_cttp_creq_host(u2_noun hot)
{
  c3_w  len_w = _cttp_mcut_host(0, 0, u2k(hot));
  c3_c* hot_c = malloc(len_w + 1);

  _cttp_mcut_host(hot_c, 0, hot);
  hot_c[len_w] = 0;

  return hot_c;
}

/* _cttp_httr(): deliver http result.
*/
static void
_cttp_httr(c3_l num_l, c3_w sas_w, u2_noun mes, u2_noun uct)
{
  u2_noun htr = u2nt(sas_w, mes, uct);
  u2_noun pox = u2nt(c3__iron, c3__http, u2_nul);

  u2_reck_plan(u2_Host.arv_u, pox, u2nt(c3__they, num_l, htr));
}

/* _cttp_httr_cres(): deliver valid response.
*/
static void
_cttp_httr_cres(c3_l num_l, u2_cres* res_u)
{
  _cttp_httr
    (num_l,
     res_u->sas_w,
     _cttp_heds_to_list(res_u->hed_u),
     res_u->bod_u ? u2nc(u2_nul, _cttp_bods_to_octs(res_u->bod_u)) : u2_nul);
}

/* _cttp_httr_fail(): fail out a request by number.
*/
static void
_cttp_httr_fail(c3_l num_l, c3_c* msg_c)
{
  return _cttp_httr(num_l, 404, u2_nul, u2_nul);
}

/* _cttp_cres_free(): free a u2_cres.
*/
static void
_cttp_cres_free(u2_cres* res_u)
{
  _cttp_heds_free(res_u->hed_u);
  _cttp_bods_free(res_u->bod_u);

  free(res_u->par_u);
  free(res_u);
}

/* _cttp_creq_free(): free a u2_creq.
*/
static void
_cttp_creq_free(u2_creq* ceq_u)
{
  _cttp_heds_free(ceq_u->hed_u);
  _cttp_bods_free(ceq_u->bod_u);

  if ( ceq_u->res_u ) {
    _cttp_cres_free(ceq_u->res_u);
  }
  free(ceq_u->url_c);
  free(ceq_u);
}

/* _cttp_message_begin(): jhttp callback
*/
static c3_i
_cttp_message_begin(http_parser* par_u)
{
  return 0;
}

/* _cttp_more(): extend string with new data.
*/
static c3_c*
_cttp_more(c3_c* str_c, const c3_c* buf_c, size_t siz_i)
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

/* _cttp_url(): jhttp callback
*/
static c3_i
_cttp_url(http_parser* par_u, const c3_c* buf_c, size_t siz_i)
{
  c3_assert(!"what?");
  return 0;
}

/* _cttp_header_field(): jhttp callback
*/
static c3_i
_cttp_header_field(http_parser* par_u, const c3_c* buf_c, size_t siz_i)
{
  u2_creq* ceq_u = par_u->data;
  u2_cres* res_u = ceq_u->res_u;

  switch ( res_u->rat_e ) {
    case u2_hreq_non:
    case u2_hreq_val: {
      u2_hhed* hed_u = malloc(sizeof(*hed_u));

      hed_u->nam_c = _cttp_more(0, buf_c, siz_i);
      hed_u->val_c = 0;
      hed_u->nex_u = res_u->hed_u;
      res_u->hed_u = hed_u;

      break;
    }
    case u2_hreq_nam: {
      res_u->hed_u->nam_c = _cttp_more(res_u->hed_u->nam_c, buf_c, siz_i);
      break;
    }
  }
  res_u->rat_e = u2_hreq_nam;
  return 0;
}

/* _cttp_header_value(): jhttp callback
*/
static c3_i
_cttp_header_value(http_parser* par_u, const c3_c* buf_c, size_t siz_i)
{
  u2_creq* ceq_u = par_u->data;
  u2_cres* res_u = ceq_u->res_u;

  switch ( res_u->rat_e ) {
    case u2_hreq_non: fprintf(stderr, "http: odd value\r\n"); return 1;

    case u2_hreq_nam: {
      res_u->hed_u->val_c = _cttp_more(0, buf_c, siz_i);
      break;
    }
    case u2_hreq_val: {
      res_u->hed_u->val_c = _cttp_more(res_u->hed_u->val_c, buf_c, siz_i);
      break;
    }
  }
  res_u->rat_e = u2_hreq_val;
  return 0;
}

/* _cttp_headers_complete(): jhttp callback
*/
static c3_i
_cttp_headers_complete(http_parser* par_u)
{
  return 0;
}

/* _cttp_body(): jhttp callback
*/
static c3_i
_cttp_body(http_parser* par_u, const c3_c* buf_c, size_t siz_i)
{
  u2_creq* ceq_u = par_u->data;
  u2_cres* res_u = ceq_u->res_u;
  u2_hbod* bod_u;

  bod_u = _cttp_bod(siz_i, (const c3_y*)buf_c);

  if ( !(res_u->bod_u) ) {
    res_u->bod_u = res_u->dob_u = bod_u;
  }
  else {
    res_u->dob_u->nex_u = bod_u;
    res_u->dob_u = bod_u;
  }
  return 0;
}

/* _cttp_message_complete(): jhttp callback
*/
static c3_i
_cttp_message_complete(http_parser* par_u)
{
  u2_creq* ceq_u = par_u->data;
  u2_ccon* coc_u = ceq_u->coc_u;
  u2_cres* res_u = ceq_u->res_u;

  res_u->sas_w = par_u->status_code;

  //  Send response to the event system.
  //
  // uL(fprintf(uH, "response for %s: %d\n", ceq_u->url_c, res_u->sas_w));

  _cttp_httr_cres(ceq_u->num_l, res_u);
  _cttp_cres_free(res_u);
  ceq_u->res_u = 0;

  coc_u->ceq_u = ceq_u->nex_u;
  if ( 0 == coc_u->ceq_u ) {
    c3_assert(ceq_u == coc_u->qec_u);
    coc_u->qec_u = 0;
  }
  return 0;
}

/* _cttp_settings[]: callback array.
*/
static struct http_parser_settings _cttp_settings = {
  _cttp_message_begin,
  _cttp_url,
  _cttp_header_field,
  _cttp_header_value,
  _cttp_headers_complete,
  _cttp_body,
  _cttp_message_complete
};

/* _cttp_cres_start(): set up new cttp response for creq.
*/
static void
_cttp_cres_start(u2_creq* ceq_u)
{
  u2_cres* res_u = malloc(sizeof(*res_u));

  memset(res_u, 0, sizeof(*res_u));
  ceq_u->res_u = res_u;

  res_u->par_u = malloc(sizeof(struct http_parser));
  http_parser_init(res_u->par_u, HTTP_RESPONSE);

  ((struct http_parser *)(res_u->par_u))->data = ceq_u;
}

/* _cttp_ccon_wax(): connection from wax_u.
*/
static u2_ccon*
_cttp_ccon_wax(uv_tcp_t* wax_u)
{
  u2_ccon* coc_u = 0;

  return (u2_ccon*)(void *)
         ( ((c3_y *)(void *)wax_u) -
           (((c3_y *)(void *)&(coc_u->wax_u)) - ((c3_y *)(void *)(coc_u))) );
}

/* _cttp_ccon_cot(): connection from cot_u.
*/
static u2_ccon*
_cttp_ccon_cot(uv_connect_t* cot_u)
{
  u2_ccon* coc_u = 0;

  return (u2_ccon*)(void *)
         ( ((c3_y *)(void *)cot_u) -
           (((c3_y *)(void *)&(coc_u->cot_u)) - ((c3_y *)(void *)(coc_u))) );
}

/* _cttp_ccon_adr(): connection from adr_u.
*/
static u2_ccon*
_cttp_ccon_adr(uv_getaddrinfo_t* adr_u)
{
  u2_ccon* coc_u = 0;

  return (u2_ccon*)(void *)
         ( ((c3_y *)(void *)adr_u) -
           (((c3_y *)(void *)&(coc_u->adr_u)) - ((c3_y *)(void *)(coc_u))) );
}

/* _cttp_ccon_waste(): fail out whole connection, with message.
*/
static void
_cttp_ccon_waste(u2_ccon* coc_u, c3_c* msg_c)
{
  while ( coc_u->ceq_u ) {
    u2_creq* ceq_u = coc_u->ceq_u;

    _cttp_httr_fail(ceq_u->num_l, msg_c);
    coc_u->ceq_u = ceq_u->nex_u;
    if ( 0 == coc_u->ceq_u ) {
      c3_assert(ceq_u == coc_u->qec_u);
      coc_u->qec_u = 0;
    }
    _cttp_creq_free(ceq_u);
  }

  free(coc_u->hot_c);
  _cttp_bods_free(coc_u->rub_u);

  if ( coc_u->pre_u ) {
    coc_u->pre_u->nex_u = coc_u->nex_u;
  } else {
    u2_Host.ctp_u.coc_u = coc_u->nex_u;
  }
  if ( coc_u->nex_u ) {
    coc_u->nex_u->pre_u = coc_u->pre_u;
  }
  if ( coc_u->ssl.ssl_u ) {
      SSL_free(coc_u->ssl.ssl_u);
  }
  free(coc_u);
}

/* _cttp_ccon_reset(): reset a live connection.
*/
void
_cttp_ccon_reset(u2_ccon* coc_u)
{
  if ( coc_u->ceq_u ) {
    _cttp_bods_free(coc_u->rub_u);
    coc_u->rub_u = coc_u->bur_u = 0;

    if ( coc_u->ceq_u->res_u ) {
      _cttp_cres_free(coc_u->ceq_u->res_u);
      coc_u->ceq_u->res_u = 0;
    }
  }
  else {
    c3_assert(0 == coc_u->rub_u);
  }
}

/* _cttp_ccon_reboot(): stop appropriate I/O on failure case.
*/
static void
_cttp_ccon_reboot(u2_ccon* coc_u)
{
  switch ( coc_u->sat_e ) {
    default: c3_assert(0);

    case u2_csat_dead: {
      /*  Failed to resolve an address.  Waste it.
      */
      _cttp_ccon_waste(coc_u, "could not resolve address");
      break;
    }
    case u2_csat_addr: {
      /*  Got an address but not a connection.  Waste it.
      */
      _cttp_ccon_waste(coc_u, "connection failed");
      break;
    }
    case u2_csat_shak: {
      /*  Got a connection, but SSL failed. Waste it.
      */
      _cttp_ccon_waste(coc_u, "ssl handshake failed");
      break;
    }
    case u2_csat_cryp: {
      _cttp_ccon_waste(coc_u, "ssl fucked up");
      break;
    }
    case u2_csat_clyr: {
      /*  We had a connection but it broke.  Either there are no
      **  living requests, in which case waste; otherwise reset.
      */
      if ( 0 == coc_u->ceq_u ) {
        _cttp_ccon_waste(coc_u, 0);
      }
      else {
        /*  Clear any unsent data.
        */
        coc_u->sat_e = u2_csat_dead;
        _cttp_ccon_reset(coc_u);

        /*  Begin again.
        */
        _cttp_ccon_kick(coc_u);
      }
      break;
    }
  }
}

/* _cttp_ccon_fail_cb(): complete failure.
*/
static void
_cttp_ccon_fail_cb(uv_handle_t* wax_u)
{
  u2_ccon *coc_u = _cttp_ccon_wax((uv_tcp_t*)wax_u);

  _cttp_ccon_reboot(coc_u);
}

/* _cttp_ccon_fail(): report failure and reset connection.
*/
static void
_cttp_ccon_fail(u2_ccon* coc_u, u2_bean say)
{
  if ( u2_yes == say ) {
    uL(fprintf(uH, "cttp: %s\n", uv_strerror(uv_last_error(u2L))));
  }

  if ( coc_u->sat_e < u2_csat_shak ) {
    _cttp_ccon_reboot(coc_u);
  }
  else {
    uv_close((uv_handle_t*)&coc_u->wax_u, _cttp_ccon_fail_cb);
  }
}

/* _cttp_ccon_kick_resolve_cb(): complete address resolution.
*/
static void
_cttp_ccon_kick_resolve_cb(uv_getaddrinfo_t* adr_u,
                           c3_i              sas_i,
                           struct addrinfo*  aif_u)
{
  u2_ccon* coc_u = _cttp_ccon_adr(adr_u);

  c3_assert(u2_csat_dead == coc_u->sat_e);

  if ( 0 != sas_i ) {
    _cttp_ccon_fail(coc_u, u2_yes);
  }
  else {
    coc_u->ipf_w = ntohl(((struct sockaddr_in *)aif_u->ai_addr)->
                         sin_addr.s_addr);
    coc_u->sat_e = u2_csat_addr;

    _cttp_ccon_kick(coc_u);
  }
}

/* _cttp_ccon_kick_resolve(): start address resolution.
*/
static void
_cttp_ccon_kick_resolve(u2_ccon* coc_u)
{
  c3_c            por_c[8];
  struct addrinfo hin_u;

  c3_assert(u2_csat_dead == coc_u->sat_e);

  snprintf(por_c, 7, "%d", 65535 & coc_u->por_s);
  memset(&hin_u, 0, sizeof(struct addrinfo));
  hin_u.ai_family = PF_INET;
  hin_u.ai_socktype = SOCK_STREAM;
  hin_u.ai_protocol = IPPROTO_TCP;

  if ( 0 != uv_getaddrinfo(u2L, &coc_u->adr_u,
                                _cttp_ccon_kick_resolve_cb,
                                coc_u->hot_c, por_c, &hin_u) )
  {
    _cttp_ccon_fail(coc_u, u2_yes);
  }
}

/* _cttp_ccon_kick_connect_cb(): connect callback.
*/
static void
_cttp_ccon_kick_connect_cb(uv_connect_t* cot_u,
                           c3_i          sas_i)
{
  u2_ccon* coc_u = _cttp_ccon_cot(cot_u);

  c3_assert(u2_csat_addr == coc_u->sat_e);

  if ( 0 != sas_i ) {
    _cttp_ccon_fail(coc_u, u2_yes);
  }
  else {
    coc_u->sat_e = (u2_yes == coc_u->sec) ?
                              u2_csat_shak :
                              u2_csat_clyr;
    _cttp_ccon_kick(coc_u);
  }
}

/* _cttp_ccon_kick_connect(): start connect resolution.
*/
static void
_cttp_ccon_kick_connect(u2_ccon* coc_u)
{
  struct sockaddr_in add_u;

  c3_assert(u2_csat_addr == coc_u->sat_e);

  if ( 0 != uv_tcp_init(u2L, &coc_u->wax_u) ) {
    _cttp_ccon_fail(coc_u, u2_yes);
  }

  add_u.sin_family = AF_INET;
  add_u.sin_port = htons(coc_u->por_s);
  add_u.sin_addr.s_addr = htonl(coc_u->ipf_w);

  if ( 0 != uv_tcp_connect(&coc_u->cot_u,
                           &coc_u->wax_u,
                           add_u,
                           _cttp_ccon_kick_connect_cb) )
  {
    _cttp_ccon_fail(coc_u, u2_yes);
  }
}

/* An unusual lameness in libuv.
*/
  typedef struct {
    uv_write_t wri_u;
    u2_ccon*   coc_u;
    c3_y*      buf_y;
  } _u2_write_t;

/* _cttp_ccon_kick_write_cb(): general write callback
*/
static void
_cttp_ccon_kick_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  u2_lo_open();
  {
    _u2_write_t* ruq_u = (void *)wri_u;

    if ( 0 != sas_i ) {
      _cttp_ccon_fail(ruq_u->coc_u, u2_yes);
    }
    free(ruq_u->buf_y);
    free(ruq_u);
  }
  u2_lo_shut(u2_no);
}

/* _cttp_ccon_kick_write()
*/
static void
_cttp_ccon_kick_write_cryp(u2_ccon* coc_u)
{
  if (!SSL_is_init_finished(coc_u->ssl.ssl_u)) {
    uL(fprintf(uH, "cttp-cryp-write-nc\n"));
    return;
  }

  uL(fprintf(uH, "cttp-cryp-write\n"));

  while ( coc_u->rub_u ) {
    u2_hbod* rub_u = coc_u->rub_u;
    c3_i rev_i;

    coc_u->rub_u = coc_u->rub_u->nex_u;
    if ( 0 == coc_u->rub_u ) {
      c3_assert(rub_u == coc_u->bur_u);
      coc_u->bur_u = 0;
    }
    if ( 0 >
         (rev_i = SSL_write(coc_u->ssl.ssl_u, rub_u->hun_y, rub_u->len_w)) ) {
      uL(fprintf(uH, "kick-write: %d\n", rev_i));
      _cttp_ccon_cryp_hurr(coc_u, rev_i);
    }
  }
  _cttp_ccon_cryp_rout(coc_u);
}

/* _cttp_ccon_kick_write_buf(): transmit buffer.
*/
static void
_cttp_ccon_kick_write_buf(u2_ccon* coc_u, uv_buf_t buf_u)
{
  _u2_write_t* ruq_u = (_u2_write_t*) malloc(sizeof(_u2_write_t));

  ruq_u->coc_u = coc_u;
  ruq_u->buf_y = (c3_y*)buf_u.base;

  if ( 0 != uv_write(&ruq_u->wri_u,
                     (uv_stream_t*)&(coc_u->wax_u),
                     &buf_u, 1,
                     _cttp_ccon_kick_write_cb) )
  {
    _cttp_ccon_fail(coc_u, u2_yes);
  }
}

/* _cttp_ccon_kick_write_body(): attach response body.
*/
static void
_cttp_ccon_kick_write_body(u2_ccon* coc_u, u2_hbod *rub_u)
{
  uv_buf_t buf_u;

  //  XX extra copy here due to old code.  Use hbod as base directly.
  //
  {
    c3_y* buf_y = malloc(rub_u->len_w + 1);

    memcpy(buf_y, rub_u->hun_y, rub_u->len_w);
    buf_y[rub_u->len_w] = 0;

    // write(1, buf_y, rub_u->len_w);
    buf_u = uv_buf_init((c3_c*)buf_y, rub_u->len_w);
  }
  _cttp_ccon_kick_write_buf(coc_u, buf_u);
}

/* _cttp_ccon_kick_write()
*/
static void
_cttp_ccon_kick_write(u2_ccon* coc_u)
{
  while ( coc_u->rub_u ) {
    u2_hbod* rub_u = coc_u->rub_u;

    coc_u->rub_u = coc_u->rub_u->nex_u;
    if ( 0 == coc_u->rub_u ) {
      c3_assert(rub_u == coc_u->bur_u);
      coc_u->bur_u = 0;
    }
    _cttp_ccon_kick_write_body(coc_u, rub_u);
  }
}

/* _cttp_ccon_cryp_rout: write the SSL buffer to the network
 */
static void
_cttp_ccon_cryp_rout(u2_ccon* coc_u)
{
  uv_buf_t buf_u;
  c3_i bur_i;

  {
    c3_y* buf_y = malloc(1<<14);
    while ( 0 < (bur_i = BIO_read(coc_u->ssl.wio_u, buf_y, 1<<14)) ) {
      uL(fprintf(uH, "rout %d\n", bur_i));
      buf_u = uv_buf_init((c3_c*)buf_y, bur_i);
      _cttp_ccon_kick_write_buf(coc_u, buf_u);
    }
  }
}

/* _cttp_ccon_cryp_hurr: handle SSL errors
 */
static void
_cttp_ccon_cryp_hurr(u2_ccon* coc_u, int rev)
{
  u2_sslx* ssl = &coc_u->ssl;
  c3_i err = SSL_get_error(ssl->ssl_u, rev);

  switch ( err ) {
    default:
      _cttp_ccon_waste(coc_u, "ssl lost");
      break;
    case SSL_ERROR_NONE:
      break;
    case SSL_ERROR_WANT_WRITE: //  XX maybe bad
      uL(fprintf(uH, ("ssl-hurr want write\n")));
      break;
    case SSL_ERROR_WANT_READ:
      uL(fprintf(uH, ("ssl-hurr want read\n")));
      _cttp_ccon_cryp_rout(coc_u);
      break;
  }
}

/* _cttp_ccon_pars_shov: shove a data buffer into the parser
 */
static void
_cttp_ccon_pars_shov(u2_ccon* coc_u, void* buf_u, ssize_t siz_i)
{

  u2_creq* ceq_u = coc_u->ceq_u;

  if ( !ceq_u ) {           //  spurious input
    uL(fprintf(uH, "http: response to no request\n"));
  }
  else {
    if ( !ceq_u->res_u ) {
      _cttp_cres_start(ceq_u);
    }

    if ( siz_i != http_parser_execute(ceq_u->res_u->par_u,
                                      &_cttp_settings,
                                      (c3_c*)buf_u,
                                      siz_i) )
    {
      uL(fprintf(uH, "http: parse error\n"));
      _cttp_ccon_fail(coc_u, u2_no);
    }
  }
}

/* _cttp_ccon_cryp_pull(): pull cleartext data off of the SSL buffer
 */
static void
_cttp_ccon_cryp_pull(u2_ccon* coc_u)
{
  if ( SSL_is_init_finished(coc_u->ssl.ssl_u) ) {
    uL(fprintf(uH, "cttp-cryp-pull-dun\n"));
    static c3_c buf[1<<14];
    c3_i ruf;
    while ( 0 < (ruf = SSL_read(coc_u->ssl.ssl_u, &buf, sizeof(buf))) ) {
      uL(fprintf(uH, "shoving %d\n", ruf));
      _cttp_ccon_pars_shov(coc_u, &buf, ruf);
    }
    if ( 0 > ruf ) {
      _cttp_ccon_cryp_hurr(coc_u, ruf);
    }
    _cttp_ccon_kick_write_cryp(coc_u);
  }
  else {
    //  not connected
    uL(fprintf(uH, "cttp-cryp-pull-cun\n"));
    c3_i r = SSL_connect(coc_u->ssl.ssl_u);
    if ( 0 > r ) {
      _cttp_ccon_cryp_hurr(coc_u, r);
    }
    else {
      coc_u->sat_e = u2_csat_cryp;
      _cttp_ccon_kick(coc_u);
    }
  }
  _cttp_ccon_cryp_rout(coc_u);
}

static void
_cttp_ccon_kick_read_cryp_cb(uv_stream_t* tcp_u,
                             ssize_t      siz_i,
                             uv_buf_t     buf_u)
{
  uL(fprintf(uH, "cttp-cryp-read\n"));
  u2_ccon *coc_u = _cttp_ccon_wax((uv_tcp_t*)tcp_u);

  u2_lo_open();
  {
    if ( siz_i < 0 ) {
      uv_err_t las_u = uv_last_error(u2L);

      _cttp_ccon_fail(coc_u, (UV_EOF == las_u.code) ? u2_no : u2_yes);
    }
    else {
      u2_creq* ceq_u = coc_u->ceq_u;

      if ( !ceq_u ) {           //  spurious input
        uL(fprintf(uH, "http: response to no request\n"));
      }
      else {
        BIO_write(coc_u->ssl.rio_u, (c3_c*)buf_u.base, siz_i);
        _cttp_ccon_cryp_pull(coc_u);
      }
    }
    if ( buf_u.base ) {
      free(buf_u.base);
    }
  }
  u2_lo_shut(u2_yes);
}

/* _cttp_ccon_read_clyr_cb()
*/
static void
_cttp_ccon_kick_read_clyr_cb(uv_stream_t* tcp_u,
                             ssize_t      siz_i,
                             uv_buf_t     buf_u)
{
  u2_ccon *coc_u = _cttp_ccon_wax((uv_tcp_t*)tcp_u);

  u2_lo_open();
  {
    if ( siz_i < 0 ) {
      uv_err_t las_u = uv_last_error(u2L);

      _cttp_ccon_fail(coc_u, (UV_EOF == las_u.code) ? u2_no : u2_yes);
    }
    else {
      _cttp_ccon_pars_shov(coc_u, buf_u.base, siz_i);
    }
    if ( buf_u.base ) {
      free(buf_u.base);
    }
  }
  u2_lo_shut(u2_yes);
}

/* _cttp_ccon_kick_read_clyr(): start reading on insecure socket.
*/
static void
_cttp_ccon_kick_read_clyr(u2_ccon* coc_u)
{
  uv_read_start((uv_stream_t*)&coc_u->wax_u,
                _cttp_alloc,
                _cttp_ccon_kick_read_clyr_cb);
}

/* _cttp_ccon_kick_read_cryp(): start reading on secure socket.
*/
static void
_cttp_ccon_kick_read_cryp(u2_ccon* coc_u)
{
  uv_read_start((uv_stream_t*)&coc_u->wax_u,
                _cttp_alloc,
                _cttp_ccon_kick_read_cryp_cb);
}

/* _cttp_ccon_kick_handshake(): start ssl handshake.
*/
static void
_cttp_ccon_kick_handshake(u2_ccon* coc_u)
{
  coc_u->ssl.ssl_u = SSL_new(u2S);
  c3_assert(coc_u->ssl.ssl_u);

  coc_u->ssl.rio_u = BIO_new(BIO_s_mem());
  c3_assert(coc_u->ssl.rio_u);

  coc_u->ssl.wio_u = BIO_new(BIO_s_mem());
  c3_assert(coc_u->ssl.wio_u);

  BIO_set_nbio(coc_u->ssl.rio_u, 1);
  BIO_set_nbio(coc_u->ssl.wio_u, 1);

  SSL_set_bio(coc_u->ssl.ssl_u,
              coc_u->ssl.rio_u,
              coc_u->ssl.wio_u);

  SSL_set_connect_state(coc_u->ssl.ssl_u);
  SSL_do_handshake(coc_u->ssl.ssl_u);

  coc_u->sat_e = u2_csat_cryp;
  _cttp_ccon_kick(coc_u);
}

/* _cttp_ccon_kick(): start appropriate I/O on client connection.
*/
static void
_cttp_ccon_kick(u2_ccon* coc_u)
{
  if ( 0 == coc_u->ceq_u ) {
    return;
  }
  switch ( coc_u->sat_e ) {
    default: c3_assert(0);

    case u2_csat_dead: {
      _cttp_ccon_kick_resolve(coc_u);
      break;
    }
    case u2_csat_addr: {
      _cttp_ccon_kick_connect(coc_u);
      break;
    }
    case u2_csat_shak: {
      _cttp_ccon_kick_handshake(coc_u);
      break;
    }
    case u2_csat_cryp: {
      _cttp_ccon_fill(coc_u);
      if ( coc_u->rub_u ) {
        _cttp_ccon_kick_write_cryp(coc_u);
      }
      _cttp_ccon_kick_read_cryp(coc_u);
      _cttp_ccon_cryp_pull(coc_u);
    }
    break;
    case u2_csat_clyr: {
      _cttp_ccon_fill(coc_u);

      if ( coc_u->rub_u ) {
        _cttp_ccon_kick_write(coc_u);
      }
      _cttp_ccon_kick_read_clyr(coc_u);
      break;
    }
  }
}

/* _cttp_ccon_new(): create client connection.  Return 0 if url invalid.
*/
static u2_ccon*
_cttp_ccon_new(u2_bean sec, c3_s por_s, c3_c* hot_c)
{
  u2_ccon* coc_u = malloc(sizeof(u2_ccon));

  memset(coc_u, 0, sizeof(u2_ccon));

  coc_u->por_s = por_s;
  coc_u->hot_c = hot_c;
  coc_u->sec = sec;

  coc_u->pre_u = 0;
  coc_u->nex_u = 0;

  if ( u2_Host.ctp_u.coc_u ) {
    coc_u->nex_u = u2_Host.ctp_u.coc_u;
    u2_Host.ctp_u.coc_u->pre_u = coc_u;
  }
  u2_Host.ctp_u.coc_u = coc_u;

  return coc_u;
}

/* _cttp_ccon_find(): find existing connection for remote server.
*/
static u2_ccon*
_cttp_ccon_find(u2_bean sec, c3_s por_s, c3_c* hot_c)
{
  u2_ccon* coc_u;

  /* XX: linear search.
  */
  for ( coc_u = u2_Host.ctp_u.coc_u; coc_u; coc_u = coc_u->nex_u ) {
    if ( !strcmp(hot_c, coc_u->hot_c) && (por_s == coc_u->por_s) ) {
      return coc_u;
    }
  }
  return 0;
}

/* _cttp_ccon(): create or find persistent client connection.
*/
static u2_ccon*
_cttp_ccon(u2_bean sec, c3_s por_s, c3_c* hot_c)
{
  u2_ccon* coc_c = _cttp_ccon_find(sec, por_s, hot_c);

  if ( 0 != coc_c ) {
    free(hot_c);
    return coc_c;
  }
  else return _cttp_ccon_new(sec, por_s, hot_c);
}

/* _cttp_creq(): cttp request from noun.
*/
static u2_creq*
_cttp_creq_new(c3_l num_l, u2_noun hes)
{
  u2_creq* ceq_u = malloc(sizeof(u2_creq));
  u2_noun  pul   = u2h(hes);
  u2_noun  hat   = u2h(pul);
  u2_noun  sec   = u2h(hat);
  u2_noun  pus   = u2h(u2t(hat));
  u2_noun  hot   = u2t(u2t(hat));
  u2_noun  moh   = u2t(hes);
  u2_noun  meh   = u2h(moh);
  u2_noun  mah   = u2h(u2t(moh));
  u2_noun  moc   = u2t(u2t(moh));

  memset(ceq_u, 0, sizeof(*ceq_u));

  ceq_u->num_l = num_l;
  ceq_u->sec = sec;
  ceq_u->por_s = (u2_nul == pus) ? 80 : u2t(pus);
  ceq_u->hot_c = _cttp_creq_host(u2k(hot));  //  XX duplicate work with url
  ceq_u->url_c = _cttp_creq_url(u2k(pul));

  // uL(fprintf(uH, "requesting %s\n", ceq_u->url_c));

  switch ( meh ) {
    default: c3_assert(0);

    case c3__get: ceq_u->met_e = u2_hmet_get; break;
    case c3__post: ceq_u->met_e = u2_hmet_post; break;
  }
  ceq_u->hed_u = _cttp_heds_math(0, u2k(mah));

  if ( u2_nul == moc ) {
    ceq_u->bod_u = 0;
  } else {
    ceq_u->bod_u = _cttp_octs_to_bod(u2k(u2t(moc)));
  }
  ceq_u->nex_u = 0;

  u2z(hes);
  return ceq_u;
}

/* _cttp_ccon_fire_body(): attach body to request buffers.
*/
static void
_cttp_ccon_fire_body(u2_ccon* coc_u, u2_hbod *rub_u)
{
  if ( !(coc_u->rub_u) ) {
    coc_u->rub_u = coc_u->bur_u = rub_u;
  }
  else {
    coc_u->bur_u->nex_u = rub_u;
    coc_u->bur_u = rub_u;
  }
}

/* _cttp_ccon_fire_str(): attach string to request buffers.
*/
static void
_cttp_ccon_fire_str(u2_ccon* coc_u, const c3_c* str_c)
{
  _cttp_ccon_fire_body(coc_u, _cttp_bod(strlen(str_c), (const c3_y*)str_c));
}

/* _cttp_ccon_fire_heds(): attach output headers.
*/
static void
_cttp_ccon_fire_heds(u2_ccon* coc_u,
                     u2_hhed* hed_u)
{
  while ( hed_u ) {
    _cttp_ccon_fire_body(coc_u, _cttp_bud(hed_u->nam_c, hed_u->val_c));
    hed_u = hed_u->nex_u;
  }
}

/* _cttp_ccon_fire(): load request data for into buffers.
*/
static void
_cttp_ccon_fire(u2_ccon* coc_u, u2_creq* ceq_u)
{
  switch ( ceq_u->met_e ) {
    default: c3_assert(0);

    case u2_hmet_nop: c3_assert(0); break;            // XX
    case u2_hmet_delete: _cttp_ccon_fire_str(coc_u, "DELETE "); break;
    case u2_hmet_get: _cttp_ccon_fire_str(coc_u, "GET "); break;
    case u2_hmet_head: _cttp_ccon_fire_str(coc_u, "HEAD "); break;
    case u2_hmet_post: _cttp_ccon_fire_str(coc_u, "POST "); break;
    case u2_hmet_put: _cttp_ccon_fire_str(coc_u, "PUT "); break;
  }
  _cttp_ccon_fire_str(coc_u, ceq_u->url_c);
  _cttp_ccon_fire_str(coc_u, " HTTP/1.1\r\n");
  _cttp_ccon_fire_str(coc_u, "User-Agent: urbit/vere.0.2\r\n");
  _cttp_ccon_fire_str(coc_u, "Accept: */*\r\n");
  _cttp_ccon_fire_str(coc_u, "Connection: Keep-Alive\r\n");
  _cttp_ccon_fire_body(coc_u, _cttp_bud("Host", ceq_u->hot_c));
  _cttp_ccon_fire_heds(coc_u, ceq_u->hed_u);

  if ( !ceq_u->bod_u ) {
    _cttp_ccon_fire_str(coc_u, "\r\n");
  }
  else {
    c3_c buf_c[81];

    snprintf(buf_c, 80, "content-length: %u\r\n", ceq_u->bod_u->len_w);
    _cttp_ccon_fire_str(coc_u, buf_c);
    _cttp_ccon_fire_body(coc_u, ceq_u->bod_u);

    _cttp_ccon_fire_str(coc_u, "\r\n");
  }
}

/* _cttp_ccon_fill(): fill send pipeline as far as possible.
*/
static void
_cttp_ccon_fill(u2_ccon* coc_u)
{
  u2_creq* ceq_u = coc_u->ceq_u;
  u2_bean  fir_t = u2_yes;

  while ( ceq_u ) {
    //
    //  Fun POST handling.  To minimize the likelihood that
    //  a connection accident will disrupt a POST (it can't
    //  be utterly ruled out, because POST sucks), we ensure
    //  that there is always some request queued above the
    //  POST.  To do this, we always throw in a NOP before    XX  should
    //  the POST.  But if there is actually something real
    //  before the POST, we don't need it.
    //
    //  So before a POST, there is always a sequence of
    //  idempotent requests, or if nothing else NOT, whose
    //  completion directly triggers the POST.  This way,
    //  it's very unlikely for idling to break a POST.
    //
    //  Extend for any other non-idempotent method (XX add).
    //
    if ( (u2_no == fir_t) && (u2_hmet_nop == ceq_u->met_e) ) {
      ceq_u = ceq_u->nex_u;
      continue;
    }
    if ( (u2_no == fir_t) && (u2_hmet_post == ceq_u->met_e) ) {
      return;
    }
    fir_t = u2_no;
    _cttp_ccon_fire(coc_u, ceq_u);
    ceq_u = ceq_u->nex_u;
  }
}

/* _cttp_ccon_send(): add I/O operation.
*/
static void
_cttp_ccon_send(u2_ccon* coc_u, u2_creq* ceq_u)
{
  u2_bean nou = ((0 == coc_u->ceq_u) ? u2_yes : u2_no);

  if ( u2_yes == nou ) {
    c3_assert(0 == coc_u->qec_u);
    coc_u->ceq_u = coc_u->qec_u = ceq_u;

    _cttp_ccon_kick(coc_u);
  }
  else {
    c3_assert(0 != coc_u->qec_u);
    coc_u->qec_u->nex_u = ceq_u;
    coc_u->qec_u = ceq_u;
  }
}

/* u2_cttp_ef_thus(): send %thus effect (outgoing request) to cttp.
*/
void
u2_cttp_ef_thus(c3_l    num_l,
                u2_noun cuq)
{
  if ( u2_nul == cuq ) {
    uL(fprintf(uH, "thus: cancel?\n"));
  }
  else {
    u2_creq* ceq_u = _cttp_creq_new(num_l, u2k(u2t(cuq)));
    u2_ccon* coc_u = _cttp_ccon(ceq_u->sec, ceq_u->por_s, ceq_u->hot_c);

    ceq_u->coc_u = coc_u;
    _cttp_ccon_send(coc_u, ceq_u);
  }
  u2z(cuq);
}

/* u2_cttp_io_init(): initialize http client I/O.
*/
void
u2_cttp_io_init()
{
  c3_i rad;
  c3_y buf[4096];

  u2_Host.ctp_u.coc_u = 0;

  SSL_library_init();
  SSL_load_error_strings();

  u2_Host.ssl_u = SSL_CTX_new(TLSv1_client_method());
  SSL_CTX_set_options(u2S, SSL_OP_NO_SSLv2);
  SSL_CTX_set_verify(u2S, SSL_VERIFY_PEER, NULL);
  SSL_CTX_set_session_cache_mode(u2S, SSL_SESS_CACHE_OFF);

  // RAND_status, at least on OS X, never returns true.
  // 4096 bytes should be enough entropy for anyone, right?
  rad = open("/dev/urandom", O_RDONLY);
  if ( 4096 != read(rad, &buf, 4096) ) {
    perror("rand-seed");
    exit(1);
  }
  RAND_seed(buf, 4096);
  close(rad);
}

/* u2_cttp_io_poll(): poll kernel for cttp I/O.
*/
void
u2_cttp_io_poll(void)
{
}

/* u2_cttp_io_exit(): shut down cttp.
*/
void
u2_cttp_io_exit(void)
{
    SSL_CTX_free(u2S);
}
