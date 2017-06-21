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

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/rand.h>

#include "../outside/jhttp/http_parser.h"   // Joyent HTTP
#include "all.h"
#include "vere/vere.h"

#ifdef U3_OS_osx
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#  pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#define CTTP_NO_PIPELINE

/* Forward declarations.
*/
  static void  _cttp_ccon_kick(u3_ccon* coc_u);
  static void  _cttp_ccon_cryp_hurr(u3_ccon* coc_u, c3_i rev_i);
  static void  _cttp_ccon_cryp_rout(u3_ccon* coc_u);
  static void  _cttp_ccon_fill(u3_ccon* coc_u);
  static void  _cttp_ccon_fire(u3_ccon* coc_u, u3_creq* ceq_u);
  static void  _cttp_ccon_fail_cb(uv_handle_t* wax_u);
  static c3_c* _cttp_creq_url(u3_noun pul);

/* _cttp_alloc(): libuv buffer allocator.
*/
static void
_cttp_alloc(uv_handle_t* had_u, size_t len_i, uv_buf_t* buf )
{
  void* ptr_v = c3_malloc(len_i);

  *buf = uv_buf_init(ptr_v, len_i);
}

/* _cttp_bod(): create a data buffer.
*/
static u3_hbod*
_cttp_bod(c3_w len_w, const c3_y* hun_y)
{
  u3_hbod* bod_u = c3_malloc(len_w + sizeof(*bod_u));

  bod_u->len_w = len_w;
  memcpy(bod_u->hun_y, hun_y, len_w);

  bod_u->nex_u = 0;
  return bod_u;
}

/* _cttp_bud(): create a header buffer.  Not null-terminated!
*/
static u3_hbod*
_cttp_bud(c3_c* nam_c, c3_c* val_c)
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

/* _cttp_heds_to_list(): C headers to list.
*/
static u3_noun
_cttp_heds_to_list(u3_hhed* hed_u)
{
  if ( 0 == hed_u ) {
    return u3_nul;
  } else {
    return u3nc(u3nc(u3i_string(hed_u->nam_c),
                     hed_u->val_c ? u3i_string(hed_u->val_c) : u3_nul),
                _cttp_heds_to_list(hed_u->nex_u));
  }
}

/* _cttp_heds_free(): free header structure.
*/
static void
_cttp_heds_free(u3_hhed* hed_u)
{
  while ( hed_u ) {
    u3_hhed* nex_u = hed_u->nex_u;

    if ( hed_u->nam_c ) free(hed_u->nam_c);
    if ( hed_u->val_c ) free(hed_u->val_c);

    free(hed_u);
    hed_u = nex_u;
  }
}

/* _cttp_bods_free(): free body structure.
*/
static void
_cttp_bods_free(u3_hbod* bod_u)
{
  while ( bod_u ) {
    u3_hbod* nex_u = bod_u->nex_u;

    free(bod_u);
    bod_u = nex_u;
  }
}

/* _cttp_bods_to_octs: translate body into octet-stream noun.
*/
static u3_noun
_cttp_bods_to_octs(u3_hbod* bod_u)
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

/* _cttp_heds_list(): create headers from list.
*/
static u3_hhed*
_cttp_heds_list(u3_hhed* hed_u, u3_noun nam, u3_noun vaz)
{
  u3_noun viz = vaz;

  while ( u3_nul != viz ) {
    u3_hhed* deh_u;

    deh_u = c3_malloc(sizeof(*deh_u));
    deh_u->nam_c = u3r_string(nam);
    deh_u->val_c = u3r_string(u3h(viz));

    deh_u->nex_u = hed_u;
    hed_u = deh_u;

    viz = u3t(viz);
  }
  u3z(nam);
  u3z(vaz);
  return hed_u;
}

/* _cttp_heds_math(): create headers from noun.
*/
static u3_hhed*
_cttp_heds_math(u3_hhed* hed_u, u3_noun mah)
{
  if ( u3_nul == mah ) {
    return hed_u;
  }
  else {
    u3_noun n_mah = u3h(mah);
    u3_noun pn_mah = u3h(n_mah);
    u3_noun qn_mah = u3t(n_mah);
    u3_noun l_mah = u3h(u3t(mah));
    u3_noun r_mah = u3t(u3t(mah));

    hed_u = _cttp_heds_list(hed_u, u3k(pn_mah), u3k(qn_mah));
    hed_u = _cttp_heds_math(hed_u, u3k(l_mah));
    hed_u = _cttp_heds_math(hed_u, u3k(r_mah));

    u3z(mah);
    return hed_u;
  }
}

/* _cttp_octs_to_bod(): translate octet-stream noun into body.
*/
static u3_hbod*
_cttp_octs_to_bod(u3_noun oct)
{
  c3_w len_w;

  if ( !_(u3a_is_cat(u3h(oct))) ) {     //  2GB max
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
_cttp_mcut_span(c3_c* buf_c, c3_w len_w, u3_noun san)
{
  c3_w ten_w = u3r_met(3, san);

  if ( buf_c ) {
    u3r_bytes(0, ten_w, (c3_y *)(buf_c + len_w), san);
  }
  u3z(san);
  return (len_w + ten_w);
}

/* _cttp_mcut_path(): measure/cut span list.
*/
static c3_w
_cttp_mcut_path(c3_c* buf_c, c3_w len_w, c3_c sep_c, u3_noun pax)
{
  u3_noun axp = pax;

  while ( u3_nul != axp ) {
    u3_noun h_axp = u3h(axp);

    len_w = _cttp_mcut_span(buf_c, len_w, u3k(h_axp));
    axp = u3t(axp);

    if ( u3_nul != axp ) {
      len_w = _cttp_mcut_char(buf_c, len_w, sep_c);
    }
  }
  u3z(pax);
  return len_w;
}

/* _cttp_mcut_host(): measure/cut host.
*/
static c3_w
_cttp_mcut_host(c3_c* buf_c, c3_w len_w, u3_noun hot)
{
  if ( c3y == u3h(hot) ) {
    len_w = _cttp_mcut_path(buf_c, len_w, '.', u3kb_flop(u3k(u3t(hot))));
  }
  else {
    c3_w ipf_w = u3r_word(0, u3t(hot));
    c3_c ipf_c[17];

    snprintf(ipf_c, 16, "%d.%d.%d.%d", (ipf_w >> 24),
                                       ((ipf_w >> 16) & 255),
                                       ((ipf_w >> 8) & 255),
                                       (ipf_w & 255));
    len_w = _cttp_mcut_str(buf_c, len_w, ipf_c);
  }
  u3z(hot);
  return len_w;
}

#if 0
/* _cttp_mcut_pfix(): measure/cut prefix.
*/
static c3_w
_cttp_mcut_pfix(c3_c* buf_c, c3_w len_w, u3_noun hat)
{
  u3_noun p_hat = u3h(hat);
  u3_noun q_hat = u3h(u3t(hat));
  u3_noun r_hat = u3t(u3t(hat));

  if ( c3y == p_hat ) {
    len_w = _cttp_mcut_str(buf_c, len_w, "https://");
  } else {
    len_w = _cttp_mcut_str(buf_c, len_w, "http://");
  }
  len_w = _cttp_mcut_host(buf_c, len_w, u3k(r_hat));

  if ( u3_nul != q_hat ) {
    c3_w por_w = 0xffff & u3r_word(0, u3t(q_hat));
    c3_c por_c[8];

    snprintf(por_c, 7, ":%d", por_w);
    len_w = _cttp_mcut_str(buf_c, len_w, por_c);
  }
  u3z(hat);
  return len_w;
}
#endif

/* _cttp_mcut_pork(): measure/cut path/extension.
*/
static c3_w
_cttp_mcut_pork(c3_c* buf_c, c3_w len_w, u3_noun pok)
{
  u3_noun h_pok = u3h(pok);
  u3_noun t_pok = u3t(pok);

  len_w = _cttp_mcut_path(buf_c, len_w, '/', u3k(t_pok));
  if ( u3_nul != h_pok ) {
    len_w = _cttp_mcut_char(buf_c, len_w, '.');
    len_w = _cttp_mcut_span(buf_c, len_w, u3k(u3t(h_pok)));
  }
  u3z(pok);
  return len_w;
}

/* _cttp_mcut_quay(): measure/cut query.
*/
static c3_w
_cttp_mcut_quay(c3_c* buf_c, c3_w len_w, u3_noun quy)
{
  if ( u3_nul == quy ) {
    return len_w;
  }
  else {
    u3_noun i_quy = u3h(quy);
    u3_noun pi_quy = u3h(i_quy);
    u3_noun qi_quy = u3t(i_quy);
    u3_noun t_quy = u3t(quy);

    len_w = _cttp_mcut_char(buf_c, len_w, '&');
    len_w = _cttp_mcut_span(buf_c, len_w, u3k(pi_quy));
    len_w = _cttp_mcut_char(buf_c, len_w, '=');
    len_w = _cttp_mcut_span(buf_c, len_w, u3k(qi_quy));

    len_w = _cttp_mcut_quay(buf_c, len_w, u3k(t_quy));
  }
  u3z(quy);
  return len_w;
}

/* _cttp_mcut_url(): measure/cut purl, producing relative URL.
*/
static c3_w
_cttp_mcut_url(c3_c* buf_c, c3_w len_w, u3_noun pul)
{
  u3_noun q_pul = u3h(u3t(pul));
  u3_noun r_pul = u3t(u3t(pul));

  // len_w = _cttp_mcut_pfix(buf_c, len_w, u3k(p_pul));
  len_w = _cttp_mcut_char(buf_c, len_w, '/');
  len_w = _cttp_mcut_pork(buf_c, len_w, u3k(q_pul));

  if ( u3_nul != r_pul ) {
    len_w = _cttp_mcut_char(buf_c, len_w, '?');
    len_w = _cttp_mcut_quay(buf_c, len_w, u3k(r_pul));
  }
  u3z(pul);
  return len_w;
}

/* _cttp_creq_url(): construct url from noun.
*/
static c3_c*
_cttp_creq_url(u3_noun pul)
{
  c3_w  len_w = _cttp_mcut_url(0, 0, u3k(pul));
  c3_c* url_c = c3_malloc(len_w + 1);

  _cttp_mcut_url(url_c, 0, pul);
  url_c[len_w] = 0;

  return url_c;
}

/* _cttp_creq_host(): construct host from noun.
*/
static c3_c*
_cttp_creq_host(u3_noun hot)
{
  c3_w  len_w = _cttp_mcut_host(0, 0, u3k(hot));
  c3_c* hot_c = c3_malloc(len_w + 1);

  _cttp_mcut_host(hot_c, 0, hot);
  hot_c[len_w] = 0;

  return hot_c;
}

/* _cttp_httr(): deliver http result.
*/
static void
_cttp_httr(c3_l num_l, c3_w sas_w, u3_noun mes, u3_noun uct)
{
  u3_noun htr = u3nt(sas_w, mes, uct);
  u3_noun pox = u3nt(u3_blip, c3__http, u3_nul);

  u3v_plan(pox, u3nt(c3__they, num_l, htr));
}

/* _cttp_httr_cres(): deliver valid response.
*/
static void
_cttp_httr_cres(c3_l num_l, u3_cres* res_u)
{
  _cttp_httr
    (num_l,
     res_u->sas_w,
     _cttp_heds_to_list(res_u->hed_u),
     res_u->bod_u ? u3nc(u3_nul, _cttp_bods_to_octs(res_u->bod_u)) : u3_nul);
}

/* _cttp_httr_fail(): fail out a request by number.
*/
static void
_cttp_httr_fail(c3_l num_l, c3_w cod_w, c3_c* msg_c)
{
  if ( msg_c ) {
    fprintf(stderr, "http: fail (%d, %d): %s\r\n", num_l, cod_w, msg_c);
  } else {
    fprintf(stderr, "http: fail (%d, %d): %s\r\n", num_l, cod_w, msg_c);
  }
  return _cttp_httr(num_l, cod_w, u3_nul, u3_nul);
}

/* _cttp_cres_free(): free a u3_cres.
*/
static void
_cttp_cres_free(u3_cres* res_u)
{
  _cttp_heds_free(res_u->hed_u);
  _cttp_bods_free(res_u->bod_u);

  free(res_u->par_u);
  free(res_u);
}

/* _cttp_creq_free(): free a u3_creq.
*/
static void
_cttp_creq_free(u3_creq* ceq_u)
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
  u3_creq* ceq_u = par_u->data;
  u3_cres* res_u = ceq_u->res_u;

  switch ( res_u->rat_e ) {
    case u3_hreq_non:
    case u3_hreq_val: {
      u3_hhed* hed_u = c3_malloc(sizeof(*hed_u));

      hed_u->nam_c = _cttp_more(0, buf_c, siz_i);
      hed_u->val_c = 0;
      hed_u->nex_u = res_u->hed_u;
      res_u->hed_u = hed_u;

      break;
    }
    case u3_hreq_nam: {
      res_u->hed_u->nam_c = _cttp_more(res_u->hed_u->nam_c, buf_c, siz_i);
      break;
    }
  }
  res_u->rat_e = u3_hreq_nam;
  return 0;
}

/* _cttp_header_value(): jhttp callback
*/
static c3_i
_cttp_header_value(http_parser* par_u, const c3_c* buf_c, size_t siz_i)
{
  u3_creq* ceq_u = par_u->data;
  u3_cres* res_u = ceq_u->res_u;

  switch ( res_u->rat_e ) {
    case u3_hreq_non: fprintf(stderr, "http: odd value\r\n"); return 1;

    case u3_hreq_nam: {
      res_u->hed_u->val_c = _cttp_more(0, buf_c, siz_i);
      break;
    }
    case u3_hreq_val: {
      res_u->hed_u->val_c = _cttp_more(res_u->hed_u->val_c, buf_c, siz_i);
      break;
    }
  }
  res_u->rat_e = u3_hreq_val;
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
  u3_creq* ceq_u = par_u->data;
  u3_cres* res_u = ceq_u->res_u;
  u3_hbod* bod_u;

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
  u3_creq* ceq_u = par_u->data;
  u3_ccon* coc_u = ceq_u->coc_u;
  u3_cres* res_u = ceq_u->res_u;

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
  if ( c3y == coc_u->sec ) {
    SSL_shutdown(coc_u->ssl.ssl_u);
    _cttp_ccon_cryp_rout(coc_u);
    // uL(fprintf(uH, "cttp: close b: %p\n", coc_u));
    uv_close((uv_handle_t*)&coc_u->wax_u, _cttp_ccon_fail_cb);
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
_cttp_cres_start(u3_creq* ceq_u)
{
  u3_cres* res_u = c3_malloc(sizeof(*res_u));

  memset(res_u, 0, sizeof(*res_u));
  ceq_u->res_u = res_u;

  res_u->par_u = c3_malloc(sizeof(struct http_parser));
  http_parser_init(res_u->par_u, HTTP_RESPONSE);

  ((struct http_parser *)(res_u->par_u))->data = ceq_u;
}

/* _cttp_ccon_wax(): connection from wax_u.
*/
static u3_ccon*
_cttp_ccon_wax(uv_tcp_t* wax_u)
{
  u3_ccon* coc_u = 0;

  return (u3_ccon*)(void *)
         ( ((c3_y *)(void *)wax_u) -
           (((c3_y *)(void *)&(coc_u->wax_u)) - ((c3_y *)(void *)(coc_u))) );
}

/* _cttp_ccon_cot(): connection from cot_u.
*/
static u3_ccon*
_cttp_ccon_cot(uv_connect_t* cot_u)
{
  u3_ccon* coc_u = 0;

  return (u3_ccon*)(void *)
         ( ((c3_y *)(void *)cot_u) -
           (((c3_y *)(void *)&(coc_u->cot_u)) - ((c3_y *)(void *)(coc_u))) );
}

/* _cttp_ccon_adr(): connection from adr_u.
*/
static u3_ccon*
_cttp_ccon_adr(uv_getaddrinfo_t* adr_u)
{
  u3_ccon* coc_u = 0;

  return (u3_ccon*)(void *)
         ( ((c3_y *)(void *)adr_u) -
           (((c3_y *)(void *)&(coc_u->adr_u)) - ((c3_y *)(void *)(coc_u))) );
}

/* _cttp_ccon_waste(): fail out whole connection, with message.
*/
static void
_cttp_ccon_waste(u3_ccon* coc_u, c3_c* msg_c)
{
  while ( coc_u->ceq_u ) {
    u3_creq* ceq_u = coc_u->ceq_u;

    _cttp_httr_fail(ceq_u->num_l, 504, msg_c);
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
    u3_Host.ctp_u.coc_u = coc_u->nex_u;
  }
  if ( coc_u->nex_u ) {
    coc_u->nex_u->pre_u = coc_u->pre_u;
  }
  if ( coc_u->ssl.ssl_u ) {
    SSL_free(coc_u->ssl.ssl_u);
    coc_u->ssl.ssl_u = 0;
    coc_u->ssl.rio_u = 0;
    coc_u->ssl.wio_u = 0;
  }
  free(coc_u);
}

/* _cttp_ccon_stop(): stop appropriate I/O on failure case.
*/
static void
_cttp_ccon_stop(u3_ccon* coc_u)
{
  switch ( coc_u->sat_e ) {
    default: c3_assert(0);

    case u3_csat_dead: {
      /*  Failed to resolve an address.  Waste it.
      */
      _cttp_ccon_waste(coc_u, "could not resolve address");
      break;
    }
    case u3_csat_addr: {
      /*  Got an address but not a connection.  Waste it.
      */
      _cttp_ccon_waste(coc_u, "connection failed");
      break;
    }
    case u3_csat_crop:
    case u3_csat_sing: {
      /*  Got a connection, but SSL failed. Waste it.
      */
      _cttp_ccon_waste(coc_u, "ssl handshake failed");
      break;
    }
    case u3_csat_cryp:
    case u3_csat_clyr: {
      /*  Socket broke.  Waste it.
      */
      _cttp_ccon_waste(coc_u, 0);
    }
  }
}

/* _cttp_ccon_fail_cb(): complete failure.
*/
static void
_cttp_ccon_fail_cb(uv_handle_t* wax_u)
{
  u3_ccon *coc_u = _cttp_ccon_wax((uv_tcp_t*)wax_u);

  _cttp_ccon_stop(coc_u);
}

/* _cttp_ccon_fail(): report failure and reset connection.
*/
static void
_cttp_ccon_fail(u3_ccon* coc_u, u3_noun say)
{
  if ( c3y == say ) {
    uL(fprintf(uH, "cttp: ERROR\n"));
  }

  if ( coc_u->sat_e < u3_csat_crop ) {
    _cttp_ccon_stop(coc_u);
  } else {
    uL(fprintf(uH, "cttp: close: %p\n", coc_u));
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
  u3_ccon* coc_u = _cttp_ccon_adr(adr_u);

  c3_assert(u3_csat_dead == coc_u->sat_e);

  if ( 0 != sas_i ) {
    _cttp_ccon_fail(coc_u, c3y);
  }
  else {
    coc_u->ipf_w = ntohl(((struct sockaddr_in *)aif_u->ai_addr)->
                         sin_addr.s_addr);
    coc_u->sat_e = u3_csat_addr;

    _cttp_ccon_kick(coc_u);
  }
}

/* _cttp_ccon_kick_resolve(): start address resolution.
*/
static void
_cttp_ccon_kick_resolve(u3_ccon* coc_u)
{
  c3_c            por_c[8];
  struct addrinfo hin_u;

  c3_assert(u3_csat_dead == coc_u->sat_e);

  snprintf(por_c, 7, "%d", 65535 & coc_u->por_s);
  memset(&hin_u, 0, sizeof(struct addrinfo));
  hin_u.ai_family = PF_INET;
  hin_u.ai_socktype = SOCK_STREAM;
  hin_u.ai_protocol = IPPROTO_TCP;

  if ( 0 != uv_getaddrinfo(u3L, &coc_u->adr_u,
                                _cttp_ccon_kick_resolve_cb,
                                coc_u->hot_c, por_c, &hin_u) )
  {
    _cttp_ccon_fail(coc_u, c3y);
  }
}

/* _cttp_ccon_kick_connect_cb(): connect callback.
*/
static void
_cttp_ccon_kick_connect_cb(uv_connect_t* cot_u,
                           c3_i          sas_i)
{
  u3_ccon* coc_u = _cttp_ccon_cot(cot_u);

  c3_assert(u3_csat_addr == coc_u->sat_e);

  if ( 0 != sas_i ) {
    _cttp_ccon_fail(coc_u, c3y);
  }
  else {
    coc_u->sat_e = (c3y == coc_u->sec) ?
                              u3_csat_crop :
                              u3_csat_clyr;
    _cttp_ccon_kick(coc_u);
  }
}

/* _cttp_ccon_kick_connect(): start connect resolution.
*/
static void
_cttp_ccon_kick_connect(u3_ccon* coc_u)
{
  struct sockaddr_in add_u;

  c3_assert(u3_csat_addr == coc_u->sat_e);

  if ( 0 != uv_tcp_init(u3L, &coc_u->wax_u) ) {
    _cttp_ccon_fail(coc_u, c3y);
  }

  add_u.sin_family = AF_INET;
  add_u.sin_port = htons(coc_u->por_s);
  add_u.sin_addr.s_addr = htonl(coc_u->ipf_w);

  if ( 0 != uv_tcp_connect(&coc_u->cot_u,
                           &coc_u->wax_u,
                           (const struct sockaddr*) & add_u,
                           _cttp_ccon_kick_connect_cb) )
  {
    _cttp_ccon_fail(coc_u, c3y);
  }
}

/* An unusual lameness in libuv.
*/
  typedef struct {
    uv_write_t wri_u;
    u3_ccon*   coc_u;
    c3_y*      buf_y;
  } _u3_write_t;

/* _cttp_ccon_kick_write_cb(): general write callback
*/
static void
_cttp_ccon_kick_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  u3_lo_open();
  {
    _u3_write_t* ruq_u = (void *)wri_u;

    if ( 0 != sas_i ) {
      _cttp_ccon_fail(ruq_u->coc_u, c3y);
    }
    free(ruq_u->buf_y);
    free(ruq_u);
  }
  u3_lo_shut(c3n);
}

/* _cttp_ccon_kick_write_cryp()
*/
static void
_cttp_ccon_kick_write_cryp(u3_ccon* coc_u)
{
  if ( NULL == coc_u->ssl.ssl_u ) {
    c3_assert(!"ssl_u is null\r\n");
  }
  if ( !SSL_is_init_finished(coc_u->ssl.ssl_u)) {
    return;
  }

  while ( coc_u->rub_u ) {
    u3_hbod* rub_u = coc_u->rub_u;
    c3_i rev_i;

    coc_u->rub_u = coc_u->rub_u->nex_u;
    if ( 0 == coc_u->rub_u ) {
      c3_assert(rub_u == coc_u->bur_u);
      coc_u->bur_u = 0;
    }
    if ( 0 >
         (rev_i = SSL_write(coc_u->ssl.ssl_u, rub_u->hun_y, rub_u->len_w)) ) {
      _cttp_ccon_cryp_hurr(coc_u, rev_i);
      _cttp_ccon_cryp_rout(coc_u);
    }
  }
}

/* _cttp_ccon_kick_write_buf(): transmit buffer.
*/
static void
_cttp_ccon_kick_write_buf(u3_ccon* coc_u, uv_buf_t buf_u)
{
  _u3_write_t* ruq_u = (_u3_write_t*) c3_malloc(sizeof(_u3_write_t));

  ruq_u->coc_u = coc_u;
  ruq_u->buf_y = (c3_y*)buf_u.base;

  if ( 0 != uv_write(&ruq_u->wri_u,
                     (uv_stream_t*)&(coc_u->wax_u),
                     &buf_u, 1,
                     _cttp_ccon_kick_write_cb) )
  {
    _cttp_ccon_fail(coc_u, c3y);
  }
}

/* _cttp_ccon_kick_write_body(): attach response body.
*/
static void
_cttp_ccon_kick_write_body(u3_ccon* coc_u, u3_hbod *rub_u)
{
  uv_buf_t buf_u;

  //  XX extra copy here due to old code.  Use hbod as base directly.
  //
  {
    c3_y* buf_y = c3_malloc(rub_u->len_w + 1);

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
_cttp_ccon_kick_write(u3_ccon* coc_u)
{
  while ( coc_u->rub_u ) {
    u3_hbod* rub_u = coc_u->rub_u;

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
_cttp_ccon_cryp_rout(u3_ccon* coc_u)
{
  uv_buf_t buf_u;
  c3_i bur_i;

  {
    c3_y* buf_y = c3_malloc(1<<14);
    while ( 0 < (bur_i = BIO_read(coc_u->ssl.wio_u, buf_y, 1<<14)) ) {
      buf_u = uv_buf_init((c3_c*)buf_y, bur_i);
      _cttp_ccon_kick_write_buf(coc_u, buf_u);
    }
  }
}

/* _cttp_ccon_cryp_hurr: handle SSL errors
 */
static void
_cttp_ccon_cryp_hurr(u3_ccon* coc_u, int rev)
{
  u3_sslx* ssl = &coc_u->ssl;
  c3_i err = SSL_get_error(ssl->ssl_u, rev);

  switch ( err ) {
    default:
      _cttp_ccon_waste(coc_u, "ssl lost");
      break;
    case SSL_ERROR_NONE:
    case SSL_ERROR_ZERO_RETURN:
      break;
    case SSL_ERROR_WANT_WRITE: //  XX maybe bad
      break;
    case SSL_ERROR_WANT_READ:
      _cttp_ccon_cryp_rout(coc_u);
      break;
    case SSL_ERROR_WANT_CONNECT:
      fprintf(stderr, "cttp: want connect: %p\r\n", coc_u->ssl.ssl_u);
      break;
    case SSL_ERROR_WANT_ACCEPT:
      fprintf(stderr, "cttp: want accept: %p\r\n", coc_u->ssl.ssl_u);
      break;
    case SSL_ERROR_WANT_X509_LOOKUP:
      fprintf(stderr, "cttp: want x509 lookup: %p\r\n", coc_u->ssl.ssl_u);
      break;
    case SSL_ERROR_SYSCALL:
      fprintf(stderr, "cttp: syscall: %p\r\n", coc_u->ssl.ssl_u);
      break;
    case SSL_ERROR_SSL:
      fprintf(stderr, "cttp: error_ssl: %p\r\n", coc_u->ssl.ssl_u);
      c3_i err;
      while ( 0 != (err = ERR_get_error()) ) {
        c3_c ero[500];
        ERR_error_string_n(err, ero, 500);
        fprintf(stderr, "error code: %x\r\n%s\r\n", err, ero);
      }
      break;
  }
}

/* _cttp_ccon_pars_shov: shove a data buffer into the parser
 */
static void
_cttp_ccon_pars_shov(u3_ccon* coc_u, void* buf_u, ssize_t siz_i)
{

  u3_creq* ceq_u = coc_u->ceq_u;

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
      _cttp_ccon_fail(coc_u, c3n);
    }
  }
}

/* _cttp_ccon_cryp_pull(): pull cleartext data off of the SSL buffer
 */
static void
_cttp_ccon_cryp_pull(u3_ccon* coc_u)
{
  if ( SSL_is_init_finished(coc_u->ssl.ssl_u) ) {
    static c3_c buf[1<<14];
    c3_i ruf;
    while ( 0 < (ruf = SSL_read(coc_u->ssl.ssl_u, &buf, sizeof(buf))) ) {
      _cttp_ccon_pars_shov(coc_u, &buf, ruf);
    }
    if ( 0 >= ruf ) {
      _cttp_ccon_cryp_hurr(coc_u, ruf);
    }
  }
  else {
    //  not connected
    ERR_clear_error();
    c3_i r = SSL_connect(coc_u->ssl.ssl_u);
    if ( 0 > r ) {
      _cttp_ccon_cryp_hurr(coc_u, r);
    }
    else {
      coc_u->sat_e = u3_csat_cryp;
      _cttp_ccon_kick(coc_u);
    }
  }
  _cttp_ccon_kick_write_cryp(coc_u);
}

/* _cttp_ccon_kick_read_cryp_cb()
*/
/*
 * `nread` (siz_w) is > 0 if there is data available, UV_EOF if libuv is
 * done reading for now, or < 0 on error.
 *
 * The callee is responsible for closing the stream when an error happens
 * by calling uv_close(). Trying to read from the stream again is undefined.
 *
 * The callee is responsible for freeing the buffer, libuv does not reuse it.
 * The buffer may be a null buffer (where buf->base=NULL and buf->len=0) on
 * error.
 */

static void
_cttp_ccon_kick_read_cryp_cb(uv_stream_t* tcp_u,
                             ssize_t      siz_w,
                             const uv_buf_t *  buf_u)
{
  u3_ccon *coc_u = _cttp_ccon_wax((uv_tcp_t*)tcp_u);

  u3_lo_open();
  {
    if ( siz_w == UV_EOF ) {
#if 1
      _cttp_ccon_fail(coc_u, c3n);
#else    
      // old workaround:
      //
      // https://github.com/urbit/urbit/issues/254
      //
      uv_close((uv_handle_t*) tcp_u, NULL);
#endif
    } 
    else if ( siz_w < 0 ) {
      uL(fprintf(uH, "cttp: read 2: %s\n", uv_strerror(siz_w)));
      _cttp_ccon_fail(coc_u, c3y);
    }
    else {
      u3_creq* ceq_u = coc_u->ceq_u;

      if ( !ceq_u ) {           //  spurious input
        uL(fprintf(uH, "http: response to no request\n"));
      }
      else {
        BIO_write(coc_u->ssl.rio_u, (c3_c*)buf_u->base, siz_w);

        _cttp_ccon_cryp_pull(coc_u);
      }
    }
    if ( buf_u->base ) {
      free(buf_u->base);
    }
  }
  u3_lo_shut(c3y);
}

/* _cttp_ccon_read_clyr_cb()
*/
/*
 * `nread` (siz_w) is > 0 if there is data available, UV_EOF if libuv is
 * done reading for now, or < 0 on error.
 *
 * The callee is responsible for closing the stream when an error happens
 * by calling uv_close(). Trying to read from the stream again is undefined.
 *
 * The callee is responsible for freeing the buffer, libuv does not reuse it.
 * The buffer may be a null buffer (where buf->base=NULL and buf->len=0) on
 * error.
 */
static void
_cttp_ccon_kick_read_clyr_cb(uv_stream_t* tcp_u,
                             ssize_t      siz_w,
                             const uv_buf_t *  buf_u)
{
  u3_ccon *coc_u = _cttp_ccon_wax((uv_tcp_t*)tcp_u);

  u3_lo_open();
  {
    if ( siz_w == UV_EOF ) {
#if 1
      _cttp_ccon_fail(coc_u, c3n);
#else    
      // old workaround:
      //
      // https://github.com/urbit/urbit/issues/254
      //
      uv_close((uv_handle_t*) tcp_u, NULL);
#endif
    } else if ( siz_w < 0 ) {
      uL(fprintf(uH, "cttp: read 1: %s\n", uv_strerror(siz_w)));
      _cttp_ccon_fail(coc_u, c3y);
    }
    else {
      _cttp_ccon_pars_shov(coc_u, buf_u->base, siz_w);
    }
    if ( buf_u->base ) {
      free(buf_u->base);
    }
  }
  u3_lo_shut(c3y);
}

/* _cttp_ccon_kick_read_clyr(): start reading on insecure socket.
*/
static void
_cttp_ccon_kick_read_clyr(u3_ccon* coc_u)
{
  uv_read_start((uv_stream_t*)&coc_u->wax_u,
                _cttp_alloc,
                _cttp_ccon_kick_read_clyr_cb);
}

/* _cttp_ccon_kick_read_cryp(): start reading on secure socket.
*/
static void
_cttp_ccon_kick_read_cryp(u3_ccon* coc_u)
{
  uv_read_start((uv_stream_t*)&coc_u->wax_u,
                _cttp_alloc,
                _cttp_ccon_kick_read_cryp_cb);
}

/* _cttp_ccon_kick_handshake(): start ssl handshake.
*/
static void
_cttp_ccon_kick_handshake(u3_ccon* coc_u)
{
  coc_u->ssl.ssl_u = SSL_new(u3S);
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
  c3_i r = SSL_do_handshake(coc_u->ssl.ssl_u);
  if ( 0 > r ) {
    _cttp_ccon_cryp_hurr(coc_u, r);
  }
  else {
   coc_u->sat_e = u3_csat_cryp;
    _cttp_ccon_kick(coc_u);
  }

  coc_u->sat_e = u3_csat_sing;
  _cttp_ccon_kick(coc_u);
}

/* _cttp_ccon_kick(): start appropriate I/O on client connection.
*/
static void
_cttp_ccon_kick(u3_ccon* coc_u)
{
  if ( 0 == coc_u->ceq_u ) {
    return;
  }
  switch ( coc_u->sat_e ) {
    default: c3_assert(0);

    case u3_csat_dead: {
      _cttp_ccon_kick_resolve(coc_u);
      break;
    }
    case u3_csat_addr: {
      _cttp_ccon_kick_connect(coc_u);
      break;
    }
    case u3_csat_crop: {
      _cttp_ccon_kick_handshake(coc_u);
      break;
    }
    case u3_csat_sing: {
      _cttp_ccon_kick_read_cryp(coc_u);
      _cttp_ccon_cryp_pull(coc_u);
      break;
    }
    case u3_csat_cryp: {
      _cttp_ccon_fill(coc_u);

      if ( coc_u->rub_u ) {
        _cttp_ccon_kick_write_cryp(coc_u);
      }
      _cttp_ccon_cryp_pull(coc_u);
      break;
    }
    case u3_csat_clyr: {
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
static u3_ccon*
_cttp_ccon_new(u3_noun sec, c3_s por_s, c3_c* hot_c)
{
  u3_ccon* coc_u = c3_malloc(sizeof(u3_ccon));

  memset(coc_u, 0, sizeof(u3_ccon));

  coc_u->por_s = por_s;
  coc_u->hot_c = hot_c;
  coc_u->sec = sec;

  coc_u->pre_u = 0;
  coc_u->nex_u = 0;

  if ( u3_Host.ctp_u.coc_u ) {
    coc_u->nex_u = u3_Host.ctp_u.coc_u;
    u3_Host.ctp_u.coc_u->pre_u = coc_u;
  }
  u3_Host.ctp_u.coc_u = coc_u;

  return coc_u;
}

#ifndef CTTP_NO_PIPELINE
/* _cttp_ccon_find(): find existing connection for remote server.
*/
static u3_ccon*
_cttp_ccon_find(u3_noun sec, c3_s por_s, c3_c* hot_c)
{
  u3_ccon* coc_u;

  /* XX: linear search.
  */
  for ( coc_u = u3_Host.ctp_u.coc_u; coc_u; coc_u = coc_u->nex_u ) {
    if ( !strcmp(hot_c, coc_u->hot_c) && (por_s == coc_u->por_s) ) {
      return coc_u;
    }
  }
  return 0;
}
#endif

/* _cttp_ccon(): create or find persistent client connection.
*/
static u3_ccon*
_cttp_ccon(u3_noun sec, c3_s por_s, c3_c* hot_c)
{
#ifndef CTTP_NO_PIPELINE
  u3_ccon* coc_c = _cttp_ccon_find(sec, por_s, hot_c);

  if ( 0 != coc_c ) {
    free(hot_c);
    return coc_c;
  }
  else 
#endif
  return _cttp_ccon_new(sec, por_s, hot_c);
}

/* _cttp_creq_new(): cttp request from noun.
*/
static u3_creq*
_cttp_creq_new(c3_l num_l, u3_noun hes)
{
  u3_creq* ceq_u = c3_malloc(sizeof(u3_creq));
  u3_noun  pul   = u3h(hes);
  u3_noun  hat   = u3h(pul);
  u3_noun  sec   = u3h(hat);
  u3_noun  pus   = u3h(u3t(hat));
  u3_noun  hot   = u3t(u3t(hat));
  u3_noun  moh   = u3t(hes);
  u3_noun  meh   = u3h(moh);
  u3_noun  mah   = u3h(u3t(moh));
  u3_noun  moc   = u3t(u3t(moh));

  memset(ceq_u, 0, sizeof(*ceq_u));

  ceq_u->num_l = num_l;
  ceq_u->sec = sec;
  ceq_u->por_s = (u3_nul == pus) ?
      ( (c3y == sec) ? 443 : 80 ) : u3t(pus);
  ceq_u->hot_c = _cttp_creq_host(u3k(hot));  //  XX duplicate work with url
  ceq_u->url_c = _cttp_creq_url(u3k(pul));

  // uL(fprintf(uH, "requesting %s\n", ceq_u->url_c));

  switch ( meh ) {
    default: c3_assert(0);

    case c3__delt: ceq_u->met_e = u3_hmet_delete; break;
    case c3__get: ceq_u->met_e = u3_hmet_get; break;
    case c3__head: ceq_u->met_e = u3_hmet_head; break;
    case c3__post: ceq_u->met_e = u3_hmet_post; break;
    case c3__put: ceq_u->met_e = u3_hmet_put; break;
  }
  ceq_u->hed_u = _cttp_heds_math(0, u3k(mah));

  if ( u3_nul == moc ) {
    ceq_u->bod_u = 0;
  } else {
    ceq_u->bod_u = _cttp_octs_to_bod(u3k(u3t(moc)));
  }
  ceq_u->nex_u = 0;

  u3z(hes);
  return ceq_u;
}

/* _cttp_ccon_fire_body(): attach body to request buffers.
*/
static void
_cttp_ccon_fire_body(u3_ccon* coc_u, u3_hbod *rub_u)
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
_cttp_ccon_fire_str(u3_ccon* coc_u, const c3_c* str_c)
{
  _cttp_ccon_fire_body(coc_u, _cttp_bod(strlen(str_c), (const c3_y*)str_c));
}

/* _cttp_ccon_fire_heds(): attach output headers.
*/
static void
_cttp_ccon_fire_heds(u3_ccon* coc_u,
                     u3_hhed* hed_u)
{
  while ( hed_u ) {
    _cttp_ccon_fire_body(coc_u, _cttp_bud(hed_u->nam_c, hed_u->val_c));
    hed_u = hed_u->nex_u;
  }
}

/* _cttp_ccon_fire(): load request data for into buffers.
*/
static void
_cttp_ccon_fire(u3_ccon* coc_u, u3_creq* ceq_u)
{
  switch ( ceq_u->met_e ) {
    default: c3_assert(0);

    case u3_hmet_nop: c3_assert(0); break;            // XX
    case u3_hmet_delete: _cttp_ccon_fire_str(coc_u, "DELETE "); break;
    case u3_hmet_get: _cttp_ccon_fire_str(coc_u, "GET "); break;
    case u3_hmet_head: _cttp_ccon_fire_str(coc_u, "HEAD "); break;
    case u3_hmet_post: _cttp_ccon_fire_str(coc_u, "POST "); break;
    case u3_hmet_put: _cttp_ccon_fire_str(coc_u, "PUT "); break;
  }
  _cttp_ccon_fire_str(coc_u, ceq_u->url_c);
  _cttp_ccon_fire_str(coc_u, " HTTP/1.1\r\n");
  _cttp_ccon_fire_str(coc_u, "User-Agent: urbit/vere.0.2\r\n");
  _cttp_ccon_fire_str(coc_u, "Accept: */*\r\n");
  //  XX it's more painful than it's worth to deal with SSL+Keepalive
  if ( c3n == coc_u->sec ) {
    _cttp_ccon_fire_str(coc_u, "Connection: Keep-Alive\r\n");
  }
  _cttp_ccon_fire_body(coc_u, _cttp_bud("Host", ceq_u->hot_c));
  _cttp_ccon_fire_heds(coc_u, ceq_u->hed_u);

  if ( !ceq_u->bod_u ) {
    _cttp_ccon_fire_str(coc_u, "\r\n");
  }
  else {
    c3_c buf_c[81];

    snprintf(buf_c, 80, "content-length: %u\r\n", ceq_u->bod_u->len_w);
    _cttp_ccon_fire_str(coc_u, buf_c);
    _cttp_ccon_fire_str(coc_u, "\r\n");

    _cttp_ccon_fire_body(coc_u, ceq_u->bod_u);
  }
}

/* _cttp_ccon_fill(): fill send pipeline as far as possible.
*/
static void
_cttp_ccon_fill(u3_ccon* coc_u)
{
  u3_creq* ceq_u = coc_u->ceq_u;
  u3_noun  fir_t = c3y;

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
    if ( (c3n == fir_t) && (u3_hmet_nop == ceq_u->met_e) ) {
      ceq_u = ceq_u->nex_u;
      continue;
    }
    if ( (c3n == fir_t) && (u3_hmet_post == ceq_u->met_e) ) {
      return;
    }
    fir_t = c3n;
    _cttp_ccon_fire(coc_u, ceq_u);
    ceq_u = ceq_u->nex_u;
  }
}

/* _cttp_ccon_send(): add I/O operation.
*/
static void
_cttp_ccon_send(u3_ccon* coc_u, u3_creq* ceq_u)
{
  u3_noun nou = ((0 == coc_u->ceq_u) ? c3y : c3n);

  if ( c3y == nou ) {
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

/* u3_cttp_ef_thus(): send %thus effect (outgoing request) to cttp.
*/
void
u3_cttp_ef_thus(c3_l    num_l,
                u3_noun cuq)
{
  if ( u3_nul == cuq ) {
    uL(fprintf(uH, "thus: cancel?\n"));
  }
  else {
    u3_creq* ceq_u = _cttp_creq_new(num_l, u3k(u3t(cuq)));
    u3_ccon* coc_u = _cttp_ccon(ceq_u->sec, ceq_u->por_s, ceq_u->hot_c);

    ceq_u->coc_u = coc_u;
    _cttp_ccon_send(coc_u, ceq_u);
  }
  u3z(cuq);
}

/* u3_cttp_io_init(): initialize http client I/O.
*/
void
u3_cttp_io_init()
{
  c3_i rad;
  c3_y buf[4096];

  u3_Host.ctp_u.coc_u = 0;

  SSL_library_init();
  SSL_load_error_strings();

  u3_Host.ssl_u = SSL_CTX_new(TLSv1_client_method());
  SSL_CTX_set_options(u3S, SSL_OP_NO_SSLv2);
  SSL_CTX_set_verify(u3S, SSL_VERIFY_PEER, NULL);
  SSL_CTX_set_default_verify_paths(u3S);
  // if ( 0 == SSL_CTX_load_verify_locations(u3S,
  //             "/etc/ssl/certs/ca-certificates.crt", NULL) ) {
  //   fprintf(stderr, "\tload-error\r\n");
  // } else {
  //   fprintf(stderr, "\tload-good\r\n");
  // }

  SSL_CTX_set_session_cache_mode(u3S, SSL_SESS_CACHE_OFF);
  SSL_CTX_set_cipher_list(u3S, "ECDH+AESGCM:DH+AESGCM:ECDH+AES256:DH+AES256:"
                          "ECDH+AES128:DH+AES:ECDH+3DES:DH+3DES:RSA+AESGCM:"
                          "RSA+AES:RSA+3DES:!aNULL:!MD5:!DSS");

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

/* u3_cttp_io_poll(): poll kernel for cttp I/O.
*/
void
u3_cttp_io_poll(void)
{
}

/* u3_cttp_io_exit(): shut down cttp.
*/
void
u3_cttp_io_exit(void)
{
    SSL_CTX_free(u3S);
}
