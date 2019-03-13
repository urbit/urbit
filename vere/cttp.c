/* vere/cttp.c
**
*/
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>
#include <openssl/ssl.h>
#include <h2o.h>

#include "all.h"
#include "vere/vere.h"


// XX deduplicate with _http_vec_to_atom
/* _cttp_vec_to_atom(): convert h2o_iovec_t to atom (cord)
*/
static u3_noun
_cttp_vec_to_atom(h2o_iovec_t vec_u)
{
  return u3i_bytes(vec_u.len, (const c3_y*)vec_u.base);
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
  c3_w    len_w;
  c3_y*   buf_y;
  u3_noun cos;

  {
    u3_hbod* bid_u = bod_u;

    len_w = 0;
    while ( bid_u ) {
      len_w += bid_u->len_w;
      bid_u = bid_u->nex_u;
    }
  }
  buf_y = c3_malloc(1 + len_w);
  buf_y[len_w] = 0;

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

  if ( 0 == len_w ) {
    *tot_w = len_w;
    return 0;
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

// XX deduplicate with _http_heds_free
/* _cttp_heds_free(): free header linked list
*/
static void
_cttp_heds_free(u3_hhed* hed_u)
{
  while ( hed_u ) {
    u3_hhed* nex_u = hed_u->nex_u;

    free(hed_u->nam_c);
    free(hed_u->val_c);
    free(hed_u);
    hed_u = nex_u;
  }
}

// XX deduplicate with _http_hed_new
/* _cttp_hed_new(): create u3_hhed from nam/val cords
*/
static u3_hhed*
_cttp_hed_new(u3_atom nam, u3_atom val)
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

// XX vv similar to _http_heds_from_noun
/* _cttp_heds_math(): create headers from +math
*/
static u3_hhed*
_cttp_heds_math(u3_noun mah)
{
  u3_noun hed = u3kdi_tap(mah);
  u3_noun deh = hed;

  u3_hhed* hed_u = 0;

  while ( u3_nul != hed ) {
    u3_noun nam = u3h(u3h(hed));
    u3_noun lit = u3t(u3h(hed));

    while ( u3_nul != lit ) {
      u3_hhed* nex_u = _cttp_hed_new(nam, u3h(lit));
      nex_u->nex_u = hed_u;

      hed_u = nex_u;
      lit = u3t(lit);
    }

    hed = u3t(hed);
  }

  u3z(deh);
  return hed_u;
}

// XX deduplicate with _http_heds_to_noun
/* _cttp_heds_to_noun(): convert h2o_header_t to (list (pair @t @t))
*/
static u3_noun
_cttp_heds_to_noun(h2o_header_t* hed_u, c3_d hed_d)
{
  u3_noun hed = u3_nul;
  c3_d dex_d  = hed_d;

  h2o_header_t deh_u;

  while ( 0 < dex_d ) {
    deh_u = hed_u[--dex_d];
    hed = u3nc(u3nc(_cttp_vec_to_atom(*deh_u.name),
                    _cttp_vec_to_atom(deh_u.value)), hed);
  }

  return hed;
}

/* _cttp_cres_free(): free a u3_cres.
*/
static void
_cttp_cres_free(u3_cres* res_u)
{
  _cttp_bods_free(res_u->bod_u);
  free(res_u);
}

/* _cttp_cres_new(): create a response
*/
static void
_cttp_cres_new(u3_creq* ceq_u, c3_w sas_w)
{
  ceq_u->res_u = c3_calloc(sizeof(*ceq_u->res_u));
  ceq_u->res_u->sas_w = sas_w;
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

/* _cttp_mcut_cord(): measure/cut cord.
*/
static c3_w
_cttp_mcut_cord(c3_c* buf_c, c3_w len_w, u3_noun san)
{
  c3_w ten_w = u3r_met(3, san);

  if ( buf_c ) {
    u3r_bytes(0, ten_w, (c3_y *)(buf_c + len_w), san);
  }
  u3z(san);
  return (len_w + ten_w);
}

/* _cttp_mcut_path(): measure/cut cord list.
*/
static c3_w
_cttp_mcut_path(c3_c* buf_c, c3_w len_w, c3_c sep_c, u3_noun pax)
{
  u3_noun axp = pax;

  while ( u3_nul != axp ) {
    u3_noun h_axp = u3h(axp);

    len_w = _cttp_mcut_cord(buf_c, len_w, u3k(h_axp));
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
  len_w = _cttp_mcut_path(buf_c, len_w, '.', u3kb_flop(u3k(hot)));
  u3z(hot);
  return len_w;
}

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
    len_w = _cttp_mcut_cord(buf_c, len_w, u3k(u3t(h_pok)));
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
    len_w = _cttp_mcut_cord(buf_c, len_w, u3k(pi_quy));
    len_w = _cttp_mcut_char(buf_c, len_w, '=');
    len_w = _cttp_mcut_cord(buf_c, len_w, u3k(qi_quy));

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

  len_w = _cttp_mcut_char(buf_c, len_w, '/');
  len_w = _cttp_mcut_pork(buf_c, len_w, u3k(q_pul));

  if ( u3_nul != r_pul ) {
    len_w = _cttp_mcut_char(buf_c, len_w, '?');
    len_w = _cttp_mcut_quay(buf_c, len_w, u3k(r_pul));
  }
  u3z(pul);
  return len_w;
}

/* _cttp_creq_port(): stringify port
*/
static c3_c*
_cttp_creq_port(c3_s por_s)
{
  c3_c* por_c = c3_malloc(8);
  snprintf(por_c, 7, "%d", 0xffff & por_s);
  return por_c;
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
  c3_w  len_w = _cttp_mcut_host(0, 0, u3k(hot));
  c3_c* hot_c = c3_malloc(1 + len_w);

  _cttp_mcut_host(hot_c, 0, hot);
  hot_c[len_w] = 0;

  return hot_c;
}

/* _cttp_creq_ip(): stringify ip
*/
static c3_c*
_cttp_creq_ip(c3_w ipf_w)
{
  c3_c* ipf_c = c3_malloc(17);
  snprintf(ipf_c, 16, "%d.%d.%d.%d", (ipf_w >> 24),
                                     ((ipf_w >> 16) & 255),
                                     ((ipf_w >> 8) & 255),
                                     (ipf_w & 255));
  return ipf_c;
}

/* _cttp_creq_find(): find a request by number in the client
*/
static u3_creq*
_cttp_creq_find(c3_l num_l)
{
  u3_creq* ceq_u = u3_Host.ctp_u.ceq_u;

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

/* _cttp_creq_link(): link request to client
*/
static void
_cttp_creq_link(u3_creq* ceq_u)
{
  ceq_u->nex_u = u3_Host.ctp_u.ceq_u;

  if ( 0 != ceq_u->nex_u ) {
    ceq_u->nex_u->pre_u = ceq_u;
  }
  u3_Host.ctp_u.ceq_u = ceq_u;
}

/* _cttp_creq_unlink(): unlink request from client
*/
static void
_cttp_creq_unlink(u3_creq* ceq_u)
{
  if ( ceq_u->pre_u ) {
    ceq_u->pre_u->nex_u = ceq_u->nex_u;

    if ( 0 != ceq_u->nex_u ) {
      ceq_u->nex_u->pre_u = ceq_u->pre_u;
    }
  }
  else {
    u3_Host.ctp_u.ceq_u = ceq_u->nex_u;

    if ( 0 != ceq_u->nex_u ) {
      ceq_u->nex_u->pre_u = 0;
    }
  }
}

/* _cttp_creq_free(): free a u3_creq.
*/
static void
_cttp_creq_free(u3_creq* ceq_u)
{
  _cttp_creq_unlink(ceq_u);

  _cttp_heds_free(ceq_u->hed_u);
  // Note: ceq_u->bod_u is covered here
  _cttp_bods_free(ceq_u->rub_u);

  if ( ceq_u->res_u ) {
    _cttp_cres_free(ceq_u->res_u);
  }

  free(ceq_u->hot_c);
  free(ceq_u->por_c);
  free(ceq_u->url_c);
  free(ceq_u->vec_u);
  free(ceq_u);
}

/* _cttp_creq_new_from_request(): create a request from an +http-request 
 *
 *   If we were rewriting all of this from scratch, this isn't how we'd do it.
 *
 *   We start with the 
 */
static u3_creq*
_cttp_creq_new_from_request(c3_l num_l, u3_noun hes)
{
  u3_creq* ceq_u = c3_calloc(sizeof(*ceq_u));

  u3_noun method, url, headers, body;
  if (c3n == u3r_qual(hes, &method, &url, &headers, &body)) {
    u3z(hes);
    return 0;
  }

  // Parse the url out of the new style url passed to us.
  u3_noun unit_pul = u3do("de-purl:html", u3k(url));
  if (c3n == u3r_du(unit_pul)) {
    uL(fprintf(uH, "cttp: url parsing failed\n"));
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
    ceq_u->ipf_c = _cttp_creq_ip(ceq_u->ipf_w);
  }

  if ( u3_nul != por ) {
    ceq_u->por_s = u3t(por);
    ceq_u->por_c = _cttp_creq_port(ceq_u->por_s);
  }

  // TODO: met_m is used for lowercase only. Here, we're using it in uppercase.
  ceq_u->met_m = method;
  ceq_u->url_c = _cttp_creq_url(u3k(pul));

  // TODO: mah is a semiparsed header format, which is not what we were passed in.
  /* ceq_u->hed_u = _cttp_heds_math(u3k(mah)); */

  if ( u3_nul != body ) {
    ceq_u->bod_u = _cttp_bod_from_octs(u3k(u3t(body)));
  }

  _cttp_creq_link(ceq_u);

  u3z(unit_pul);
  u3z(hes);

  return ceq_u;
}

/* _cttp_creq_new(): create a request from a +hiss noun
*/
static u3_creq*
_cttp_creq_new(c3_l num_l, u3_noun hes)
{
  u3_creq* ceq_u = c3_calloc(sizeof(*ceq_u));

  u3_noun pul = u3h(hes);      // +purl
  u3_noun hat = u3h(pul);      // +hart
  u3_noun sec = u3h(hat);
  u3_noun por = u3h(u3t(hat));
  u3_noun hot = u3t(u3t(hat)); // +host
  u3_noun moh = u3t(hes);      // +moth
  u3_noun met = u3h(moh);      // +meth
  u3_noun mah = u3h(u3t(moh)); // +math
  u3_noun bod = u3t(u3t(moh));

  ceq_u->sat_e = u3_csat_init;
  ceq_u->num_l = num_l;
  ceq_u->sec   = sec;

  if ( c3y == u3h(hot) ) {
    ceq_u->hot_c = _cttp_creq_host(u3k(u3t(hot)));
  } else {
    ceq_u->ipf_w = u3r_word(0, u3t(hot));
    ceq_u->ipf_c = _cttp_creq_ip(ceq_u->ipf_w);
  }

  if ( u3_nul != por ) {
    ceq_u->por_s = u3t(por);
    ceq_u->por_c = _cttp_creq_port(ceq_u->por_s);
  }

  ceq_u->met_m = met;
  ceq_u->url_c = _cttp_creq_url(u3k(pul));
  ceq_u->hed_u = _cttp_heds_math(u3k(mah));

  if ( u3_nul != bod ) {
    ceq_u->bod_u = _cttp_bod_from_octs(u3k(u3t(bod)));
  }

  _cttp_creq_link(ceq_u);

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
}

/* _cttp_creq_fire_heds(): attach output headers.
*/
static void
_cttp_creq_fire_heds(u3_creq* ceq_u, u3_hhed* hed_u)
{
  while ( hed_u ) {
    _cttp_creq_fire_body(ceq_u, _cttp_bod_from_hed(hed_u));
    hed_u = hed_u->nex_u;
  }
}

/* _cttp_creq_fire(): load request data for into buffers.
*/
static void
_cttp_creq_fire(u3_creq* ceq_u)
{
  // TODO: This needs to be better. The met_m that we send now is uppercassed.
  switch ( ceq_u->met_m ) {
    default: c3_assert(0);
    case c3_s3('G', 'E', 'T'):  _cttp_creq_fire_str(ceq_u, "GET ");      break;
    case c3__get:   _cttp_creq_fire_str(ceq_u, "GET ");      break;
    case c3__put:   _cttp_creq_fire_str(ceq_u, "PUT ");      break;
    case c3__post:  _cttp_creq_fire_str(ceq_u, "POST ");     break;
    case c3__head:  _cttp_creq_fire_str(ceq_u, "HEAD ");     break;
    case c3__conn:  _cttp_creq_fire_str(ceq_u, "CONNECT ");  break;
    case c3__delt:  _cttp_creq_fire_str(ceq_u, "DELETE ");   break;
    case c3__opts:  _cttp_creq_fire_str(ceq_u, "OPTIONS ");  break;
    case c3__trac:  _cttp_creq_fire_str(ceq_u, "TRACE ");    break;
  }
  _cttp_creq_fire_str(ceq_u, ceq_u->url_c);
  _cttp_creq_fire_str(ceq_u, " HTTP/1.1\r\n");

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
    free(hos_c);
  }

  _cttp_creq_fire_heds(ceq_u, ceq_u->hed_u);

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

/* /\* _cttp_httr(): dispatch http response to %eyre */
/* *\/ */
/* static void */
/* _cttp_httr(c3_l num_l, c3_w sas_w, u3_noun mes, u3_noun uct) */
/* { */
/*   u3_noun htr = u3nt(sas_w, mes, uct); */
/*   u3_noun pox = u3nt(u3_blip, c3__http, u3_nul); */
/*   u3v_plan(pox, u3nt(c3__they, num_l, htr)); */
/* } */

static void
_cttp_http_client_receive(c3_l num_l, c3_w sas_w, u3_noun mes, u3_noun uct)
{
  // TODO: We want to eventually deal with partial responses, but I don't know
  // how to get that working right now.
  u3_noun pox = u3nq(u3_blip, u3i_string("http-client"), u3k(u3A->sen), u3_nul);

  u3v_plan(pox,
           u3nt(u3i_string("receive"),
                num_l,
                u3nq(u3i_string("start"),
                     u3nc(sas_w, mes),
                     uct,
                     c3y)));
}

/* _cttp_creq_fail(): dispatch error response
*/
static void
_cttp_creq_fail(u3_creq* ceq_u, const c3_c* err_c)
{
  // XX anything other than a 504?
  c3_w cod_w = 504;

  uL(fprintf(uH, "http: fail (%d, %d): %s\r\n", ceq_u->num_l, cod_w, err_c));

  // XX include err_c as response body?
  /* _cttp_httr(ceq_u->num_l, cod_w, u3_nul, u3_nul); */
  _cttp_http_client_receive(ceq_u->num_l, cod_w, u3_nul, u3_nul);
  _cttp_creq_free(ceq_u);
}

/* _cttp_creq_respond(): dispatch response
*/
static void
_cttp_creq_respond(u3_creq* ceq_u)
{
  u3_cres* res_u = ceq_u->res_u;

  /* _cttp_httr(ceq_u->num_l, res_u->sas_w, res_u->hed, */
  _cttp_http_client_receive(ceq_u->num_l, res_u->sas_w, res_u->hed,
             ( !res_u->bod_u ) ? u3_nul :
             u3nc(u3_nul, _cttp_bods_to_octs(res_u->bod_u)));

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

  _cttp_cres_new(ceq_u, (c3_w)sas_i);
  ceq_u->res_u->hed = _cttp_heds_to_noun(hed_u, hed_t);

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
                      h2o_iovec_t** vec_p, size_t* vec_t, c3_i* hed_i)
{
  u3_creq* ceq_u = (u3_creq *)cli_u->data;

  if ( 0 != err_c ) {
    _cttp_creq_fail(ceq_u, err_c);
    return 0;
  }

  {
    c3_w len_w;
    ceq_u->vec_u = _cttp_bods_to_vec(ceq_u->rub_u, &len_w);
    *vec_t = len_w;
    *vec_p = ceq_u->vec_u;
    *hed_i = c3__head == ceq_u->met_m;
  }

  return _cttp_creq_on_head;
}

/* _cttp_creq_connect(): establish connection
*/
static void
_cttp_creq_connect(u3_creq* ceq_u)
{
  c3_assert(u3_csat_ripe == ceq_u->sat_e);
  c3_assert(ceq_u->ipf_c);

  h2o_iovec_t ipf_u = h2o_iovec_init(ceq_u->ipf_c, strlen(ceq_u->ipf_c));
  c3_s por_s = ceq_u->por_s ? ceq_u->por_s :
               ( c3y == ceq_u->sec ) ? 443 : 80;

  // connect by IP
  h2o_http1client_connect(&ceq_u->cli_u, ceq_u, u3_Host.ctp_u.ctx_u, ipf_u,
                          por_s, c3y == ceq_u->sec, _cttp_creq_on_connect);

  // set hostname for TLS handshake
  if ( ceq_u->hot_c && c3y == ceq_u->sec ) {
    c3_w len_w  = 1 + strlen(ceq_u->hot_c);
    c3_c* hot_c = c3_malloc(len_w);
    strncpy(hot_c, ceq_u->hot_c, len_w);

    free(ceq_u->cli_u->ssl.server_name);
    ceq_u->cli_u->ssl.server_name = hot_c;
  }

  _cttp_creq_fire(ceq_u);
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
    ceq_u->ipf_c = _cttp_creq_ip(ceq_u->ipf_w);

    ceq_u->sat_e = u3_csat_ripe;
    _cttp_creq_connect(ceq_u);
  }

  free(adr_u);
  uv_freeaddrinfo(aif_u);
}

/* _cttp_creq_resolve(): resolve hostname to IP address
*/
static void
_cttp_creq_resolve(u3_creq* ceq_u)
{
  c3_assert(u3_csat_addr == ceq_u->sat_e);
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

  c3_i sas_i;

  if ( 0 != (sas_i = uv_getaddrinfo(u3L, adr_u, _cttp_creq_resolve_cb,
                                         ceq_u->hot_c, por_c, &hin_u)) ) {
    _cttp_creq_fail(ceq_u, uv_strerror(sas_i));
  }
}

/* _cttp_creq_start(): start a request
*/
static void
_cttp_creq_start(u3_creq* ceq_u)
{
  if ( ceq_u->ipf_c ) {
    ceq_u->sat_e = u3_csat_ripe;
    _cttp_creq_connect(ceq_u);
  } else {
    ceq_u->sat_e = u3_csat_addr;
    _cttp_creq_resolve(ceq_u);
  }
}

/* _cttp_init_tls: initialize OpenSSL context
*/
static SSL_CTX*
_cttp_init_tls()
{
  // XX require 1.1.0 and use TLS_client_method()
  SSL_CTX* tls_u = SSL_CTX_new(SSLv23_client_method());
  // XX use SSL_CTX_set_max_proto_version() and SSL_CTX_set_min_proto_version()
  SSL_CTX_set_options(tls_u, SSL_OP_NO_SSLv2 |
                             SSL_OP_NO_SSLv3 |
                             // SSL_OP_NO_TLSv1 | // XX test
                             SSL_OP_NO_COMPRESSION);

  SSL_CTX_set_verify(tls_u, SSL_VERIFY_PEER, 0);
  SSL_CTX_set_default_verify_paths(tls_u);
  SSL_CTX_set_session_cache_mode(tls_u, SSL_SESS_CACHE_OFF);
  SSL_CTX_set_cipher_list(tls_u,
                          "ECDH+AESGCM:DH+AESGCM:ECDH+AES256:DH+AES256:"
                          "ECDH+AES128:DH+AES:ECDH+3DES:DH+3DES:RSA+AESGCM:"
                          "RSA+AES:RSA+3DES:!aNULL:!MD5:!DSS");

  return tls_u;
}

/* _cttp_init_h2o: initialize h2o client ctx and timeout
*/
static h2o_http1client_ctx_t*
_cttp_init_h2o()
{
  h2o_timeout_t* tim_u = c3_malloc(sizeof(*tim_u));

  h2o_timeout_init(u3L, tim_u, 300 * 1000);

  h2o_http1client_ctx_t* ctx_u = c3_calloc(sizeof(*ctx_u));
  ctx_u->loop = u3L;
  ctx_u->io_timeout = tim_u;

  return ctx_u;
};

/* u3_cttp_ef_thus(): send %thus effect (outgoing request) to cttp.
*/
void
u3_cttp_ef_thus(c3_l    num_l,
                u3_noun cuq)
{
  u3_creq* ceq_u;

  if ( u3_nul == cuq ) {
    ceq_u =_cttp_creq_find(num_l);

    if ( ceq_u ) {
      _cttp_creq_quit(ceq_u);
    }
  }
  else {
    ceq_u = _cttp_creq_new(num_l, u3k(u3t(cuq)));
    _cttp_creq_start(ceq_u);
  }
  u3z(cuq);
}

/* u3_cttp_ef_http_client(): send an %http-client (outgoing request) to cttp.
*/
void
u3_cttp_ef_http_client(u3_noun fav)
{
  u3_creq* ceq_u;

  if ( c3y == u3rz_sing(u3i_string("request"), u3k(u3h(fav))) ) {
    u3_noun p_fav, q_fav;
    u3x_cell(u3t(fav), &p_fav, &q_fav);

    ceq_u = _cttp_creq_new_from_request(u3r_word(0, p_fav), u3k(q_fav));

    if ( ceq_u ) {
      _cttp_creq_start(ceq_u);
    }
    else {
      uL(fprintf(uH, "cttp: strange request (unparsable url)\n"));
    }
  }
  else if ( c3y == u3rz_sing(u3i_string("cancel-request"), u3k(u3h(fav))) ) {
    ceq_u =_cttp_creq_find(u3r_word(0, u3t(fav)));

    if ( ceq_u ) {
      _cttp_creq_quit(ceq_u);
    }
  }
  else {
    uL(fprintf(uH, "cttp: strange request (unknown type)\n"));
  }

  u3z(fav);
}

/* u3_cttp_ef_bake(): notify that we're live.
*/
void
u3_cttp_ef_bake()
{
  u3_noun pax = u3nq(u3_blip, u3i_string("http-client"), u3k(u3A->sen), u3_nul);
  u3v_plan(pax, u3nc(c3__born, u3_nul));
}

/* u3_cttp_io_init(): initialize http client I/O.
*/
void
u3_cttp_io_init()
{
  u3_Host.ctp_u.tls_u = _cttp_init_tls();
  u3_Host.ctp_u.ctx_u = _cttp_init_h2o();
  u3_Host.ctp_u.ctx_u->ssl_ctx = u3_Host.ctp_u.tls_u;
  u3_Host.ctp_u.ceq_u = 0;
}

/* u3_cttp_io_exit(): shut down cttp.
*/
void
u3_cttp_io_exit(void)
{
    SSL_CTX_free(u3_Host.ctp_u.tls_u);
    free(u3_Host.ctp_u.ctx_u->io_timeout);
    free(u3_Host.ctp_u.ctx_u);
}
