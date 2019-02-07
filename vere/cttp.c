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
#include <curl/curl.h>

#include "all.h"
#include "vere/vere.h"


typedef struct curl_ctx_s {
  uv_poll_t poll_handle;
  curl_socket_t sockfd;
} curl_ctx_t;

/* _cttp_create_curl_ctx(): initialize a curl socket and poll for data */
static curl_ctx_t* _cttp_create_curl_ctx(curl_socket_t sockfd)
{
  curl_ctx_t *context;
  context = (curl_ctx_t *) c3_malloc(sizeof(*context));
  context->sockfd = sockfd;

  uv_poll_init_socket(u3L, &context->poll_handle, sockfd);
  context->poll_handle.data = context;

  return context;
}


/* _cttp_curl_close_cb(): callback for freeing a curl context */
static void _cttp_curl_close_cb(uv_handle_t *handle)
{
  curl_ctx_t *context = (curl_ctx_t *) handle->data;
  free(context);
}

/* _cttp_destroy_curl_ctx(): close the libuv polling handle of a request */
static void _cttp_destroy_curl_ctx(curl_ctx_t *context)
{
  uv_close((uv_handle_t *) &context->poll_handle, _cttp_curl_close_cb);
}

/* _cttp_ltrim(): trim whitespace from left side of string */
c3_c *_cttp_ltrim(c3_c *str)
{
  const c3_c *seps = "\t\n\v\f\r ";
  size_t trim = strspn(str, seps);
  if ( trim > 0 ) {
    size_t len = strlen(str);
    if ( trim == len ) {
      str[0] = '\0';
    }
    else {
      memmove(str, str + trim, len + 1 - trim);
    }
  }
  return str;
}

/* _cttp_rtrim(): trim whitespace from right side of string */
c3_c *_cttp_rtrim(c3_c *str)
{
  int i = strlen(str) - 1;
  const c3_c *seps = "\t\n\v\f\r ";
  while ( i >= 0 && strchr(seps, str[i]) != NULL ) {
    str[i] = '\0';
    i--;
  }
  return str;
}

/* _cttp_trim(): trim whitespace from both sides of string */
c3_c *_cttp_trim(c3_c *str)
{
  return _cttp_ltrim(_cttp_rtrim(str));
}

/* _cttp_vec_to_atom(): convert c3_c* to atom (cord)
 * */
static u3_noun
_cttp_char_to_atom(c3_c *chr)
{
  return u3i_bytes(strlen(chr), (const c3_y*)chr);
}

/*  _cttp_hhed_to_headers(): convert ceq_u->hed_u to curl_slist struct
 *  */
static struct curl_slist*
_cttp_hhed_to_headers(u3_creq* ceq_u)
{
  u3_hhed* hed_u = ceq_u->hed_u;
  struct curl_slist *slist = NULL;

  while (hed_u) {
    c3_w len_w = hed_u-> nam_w + 2 + hed_u->val_w;
    c3_c* hed_c = c3_calloc(len_w + 1);
    snprintf(hed_c, len_w + 1, "%s: %s", hed_u->nam_c, hed_u->val_c);
    slist = curl_slist_append(slist, hed_c);
    free(hed_c);
    hed_u = hed_u->nex_u;
  }

  return slist;
}


/* _cttp_heds_to_noun(): convert hhed_u to (list (pair @t @t))
*/
static u3_noun
_cttp_heds_to_noun(u3_hhed* hed_u) {
  u3_noun hed = u3_nul;
  if ( !hed_u ) {
    return hed;
  }

  while ( hed_u ) {
    hed = u3nc(u3nc(_cttp_char_to_atom(_cttp_trim(hed_u->nam_c)),
                    _cttp_char_to_atom(_cttp_trim(hed_u->val_c))), hed);
    hed_u = hed_u->nex_u;
  }

  return hed;
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
_cttp_bod_new(c3_w len_w, void* hun_c)
{
  u3_hbod* bod_u = c3_malloc(1 + len_w + sizeof(*bod_u));
  bod_u->hun_y[len_w] = 0;
  bod_u->len_w = len_w;
  memcpy(bod_u->hun_y, (const c3_y*)hun_c, len_w);

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

/* _cttp_hed_new(): create u3_hhed from nam/val cords
*/
static u3_hhed*
_cttp_hed_new_atom(u3_atom nam, u3_atom val)
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

/* _cttp_hed_new_char(): create u3_hhed from nam/val cords
*/
static u3_hhed*
_cttp_hed_new_char(c3_c *nam, c3_c *val)
{
  u3_hhed* hed_u = c3_malloc(sizeof(*hed_u));

  hed_u->nam_c = c3_malloc(1 + strlen(nam));
  hed_u->val_c = c3_malloc(1 + strlen(val));
  hed_u->nex_u = 0;

  memcpy(hed_u->nam_c, nam, strlen(nam) + 1);
  memcpy(hed_u->val_c, val, strlen(val) + 1);
 
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
      u3_hhed* nex_u = _cttp_hed_new_atom(nam, u3h(lit));
      nex_u->nex_u = hed_u;

      hed_u = nex_u;
      lit = u3t(lit);
    }

    hed = u3t(hed);
  }

  u3z(deh);
  return hed_u;
}

/* _cttp_cres_free(): free a u3_cres.
*/
static void
_cttp_cres_free(u3_cres* res_u)
{
  _cttp_bods_free(res_u->bod_u);
  free(res_u);
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
  free(ceq_u->bod_u);

  if ( ceq_u->res_u ) {
    _cttp_cres_free(ceq_u->res_u);
  }

  curl_slist_free_all(ceq_u->lis_u);
  free(ceq_u->hot_c);
  free(ceq_u->por_c);
  free(ceq_u->url_c);
  free(ceq_u->err_c);
  free(ceq_u);
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
    ceq_u->rem_w = ceq_u->bod_u->len_w;
  }

  ceq_u->cur_u = curl_easy_init();
  ceq_u->res_u = c3_calloc(sizeof(*ceq_u->res_u));
  ceq_u->err_c = c3_calloc(CURL_ERROR_SIZE);
  ceq_u->lis_u = _cttp_hhed_to_headers(ceq_u);


  _cttp_creq_link(ceq_u);

  u3z(hes);
  return ceq_u;
}

/* _cttp_httr(): dispatch http response to %eyre
*/
static void
_cttp_httr(c3_l num_l, c3_w sas_w, u3_noun mes, u3_noun uct)
{
  u3_noun htr = u3nt(sas_w, mes, uct);
  u3_noun pox = u3nt(u3_blip, c3__http, u3_nul);

  u3_pier_plan(pox, u3nt(c3__they, num_l, htr));
}

/* _cttp_creq_fail(): dispatch error response
*/
static void
_cttp_creq_fail(u3_creq* ceq_u, c3_w cod_w, const c3_c* err_c)
{
  uL(fprintf(uH, "http: fail (%d, %d): %s\r\n", ceq_u->num_l, cod_w, err_c));

  // XX include err_c as response body?
  _cttp_httr(ceq_u->num_l, cod_w, u3_nul, u3_nul);
  _cttp_creq_free(ceq_u);
}

/* _cttp_creq_respond(): dispatch response
*/
static void
_cttp_creq_respond(u3_creq* ceq_u)
{
  u3_cres* res_u = ceq_u->res_u;
  u3_noun hed_u = _cttp_heds_to_noun(res_u->hed_u);

  _cttp_httr(ceq_u->num_l, res_u->sas_w, hed_u,
             ( !res_u->bod_u ) ? u3_nul :
             u3nc(u3_nul, _cttp_bods_to_octs(res_u->bod_u)));

  _cttp_creq_free(ceq_u);
}

static size_t
_cttp_creq_head_cb(c3_c *buffer, size_t size, size_t nitems, void *userp)
{
  size_t len = size * nitems;
  u3_creq* ceq_u = (u3_creq *)userp;

  if ( len <= 0 ) {
    return len;
  }

  c3_c *p = strchr(buffer, ':');
  if (p) {
    c3_i index = p - buffer;
    c3_c *nam = c3_calloc(index + 1);
    snprintf(nam, index + 1, "%s", buffer);
    c3_c *val = c3_calloc(strlen(buffer) - index - 1 + 1);
    snprintf(val, strlen(buffer) - index, "%s", p + 1);

    u3_hhed *hed_u = ceq_u->res_u->hed_u;
    
    if ( hed_u ) {

      while (hed_u->nex_u) {
        hed_u = hed_u->nex_u;
      }

      hed_u->nex_u = _cttp_hed_new_char(nam, val); 
    }
    else {
      ceq_u->res_u->hed_u = _cttp_hed_new_char(nam, val);
    }

    free(nam);
    free(val);
  }

  return len;
}

static size_t
_cttp_creq_body_cb(void *contents, size_t size, size_t nmemb, void *userp)
{
  size_t len = size * nmemb;
  u3_creq* ceq_u = (u3_creq *)userp;

  u3_hbod* bod_u = _cttp_bod_new(len, contents);
  _cttp_cres_fire_body(ceq_u->res_u, bod_u);

  return len;
}

static size_t
_cttp_creq_data_cb(void *dest, size_t size, size_t nmemb, void *userp)
{
  size_t len = size * nmemb;
  u3_creq* ceq_u = (u3_creq *)userp;

  if ( ceq_u->rem_w ) {
    size_t copy_this_much = ceq_u->rem_w;
    if ( copy_this_much > len ) {
      copy_this_much = len;
    }

    void *read_pointer = ceq_u->bod_u->hun_y + ceq_u->bod_u->len_w - ceq_u->rem_w;
    c3_assert(read_pointer == ceq_u->bod_u->hun_y);
    memcpy(dest, read_pointer, copy_this_much);
    ceq_u->rem_w -= copy_this_much;
    return copy_this_much;
  }

  return 0;
}
static void
_cttp_creq_get(u3_creq* ceq_u)
{
  //  no-op
}

static void
_cttp_creq_put(u3_creq* ceq_u)
{
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_PUT, 1L);
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_UPLOAD, 1L);
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_READFUNCTION, _cttp_creq_data_cb);
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_READDATA, ceq_u);
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_INFILESIZE_LARGE,
                   ceq_u->bod_u->len_w);
}

static void
_cttp_creq_post(u3_creq* ceq_u)
{
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_POST, 1L);
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_READFUNCTION, _cttp_creq_data_cb);
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_READDATA, ceq_u);
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_POSTFIELDSIZE_LARGE,
                   ceq_u->bod_u->len_w);
}

static void
_cttp_creq_delete(u3_creq* ceq_u)
{
  //  XX: there are no defined semantics for delete requests that include
  //  bodies in HTTP/1.1, so we don't send data
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_CUSTOMREQUEST, "DELETE");
}

/* _cttp_creq_connect(): establish connection
*/
static void
_cttp_creq_connect(u3_creq* ceq_u)
{
  c3_assert(ceq_u->cur_u);

  c3_c* hot_c = ceq_u->hot_c ? ceq_u->hot_c : ceq_u->ipf_c;
  c3_c* url_c = ceq_u->url_c;
  c3_c* por_c = ceq_u->por_c ? ceq_u->por_c :
                  ( c3y == ceq_u->sec ) ? "443" : "80";

  c3_w len_w = 2 + strlen(hot_c) + strlen(url_c) + strlen(por_c);
  c3_c* tot_c; 
  
  if ( c3y == ceq_u->sec ) {
    tot_c = c3_calloc(len_w + 9);
    snprintf(tot_c, len_w + 9, "https://%s:%s%s", hot_c, por_c, url_c);
  }
  else {
    tot_c = c3_calloc(len_w + 8);
    snprintf(tot_c, len_w + 8, "http://%s:%s%s", hot_c, por_c, url_c);
  }

  curl_easy_setopt(ceq_u->cur_u, CURLOPT_URL, tot_c);
  free(tot_c);

  curl_easy_setopt(ceq_u->cur_u, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_2TLS);
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_USERAGENT, "curl/7.54.0"); 
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_MAXREDIRS, 50L);
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_TCP_KEEPALIVE, 1L);
  
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_HEADERDATA, ceq_u);
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_WRITEDATA, ceq_u);
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_PRIVATE, ceq_u);

  curl_easy_setopt(ceq_u->cur_u, CURLOPT_HEADERFUNCTION, _cttp_creq_head_cb);  
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_WRITEFUNCTION, _cttp_creq_body_cb);
  curl_easy_setopt(ceq_u->cur_u, CURLOPT_HTTPHEADER, ceq_u->lis_u);

  switch ( ceq_u->met_m ) {
    default:        c3_assert(0);
    case c3__get:   _cttp_creq_get(ceq_u);      break;
    case c3__put:   _cttp_creq_put(ceq_u);      break;
    case c3__post:  _cttp_creq_post(ceq_u);     break;
    case c3__delt:  _cttp_creq_delete(ceq_u);   break;
  }

  curl_multi_add_handle(u3_Host.ctp_u.mul_u, ceq_u->cur_u);
}

static void 
_cttp_check_multi_info()
{
  CURLMsg *message;
  c3_ws pending;
  CURL *eas_u;
  c3_w code;
  CURLcode cod_w;
  u3_creq *ceq_u = NULL;

  while( (message = curl_multi_info_read(u3_Host.ctp_u.mul_u, &pending)) ) {
    switch(message->msg) {
    case CURLMSG_DONE:
      eas_u = message->easy_handle;
      cod_w = message->data.result;

      curl_easy_getinfo(eas_u, CURLINFO_RESPONSE_CODE, &code);
      curl_easy_getinfo(eas_u, CURLINFO_PRIVATE, &ceq_u);

      c3_assert(ceq_u);
      ceq_u->res_u->sas_w = (c3_w) code;
      if ( cod_w != CURLE_OK ) {
        size_t err_w = strlen(ceq_u->err_c);
        c3_c *err_c;
        if ( err_w ) {
          err_c = ceq_u->err_c;
        } else {
          err_c = (c3_c *) curl_easy_strerror(cod_w);
        }

        _cttp_creq_fail(ceq_u, code, err_c);
      } else {
        _cttp_creq_respond(ceq_u);
      }

      curl_multi_remove_handle(u3_Host.ctp_u.mul_u, eas_u);
      curl_easy_cleanup(eas_u);
      break;
    default:
      break;
    }
  }
}
 
static void 
_cttp_curl_perform(uv_poll_t *req, c3_ws status, c3_ws events)
{
  int running_handles;
  int flags = 0;
  curl_ctx_t *context;

  if ( events & UV_READABLE ) {
    flags |= CURL_CSELECT_IN;
  }
  if ( events & UV_WRITABLE ) {
    flags |= CURL_CSELECT_OUT; 
  }

  context = (curl_ctx_t *) req->data;
  curl_multi_socket_action(u3_Host.ctp_u.mul_u, context->sockfd, flags,
                 &running_handles);

  _cttp_check_multi_info();
}

static void _cttp_on_timeout(uv_timer_t *req)
{
  curl_multi_socket_action(u3_Host.ctp_u.mul_u, CURL_SOCKET_TIMEOUT, 0,
                           u3_Host.ctp_u.run_u);
  _cttp_check_multi_info();
}


static int
_cttp_handle_socket(CURL *eas_u, curl_socket_t s, int action, void *userp,
    void *socketp)
{
  curl_ctx_t *curl_ctx;
  int events = 0;

  switch ( action ) {
  case CURL_POLL_IN:
  case CURL_POLL_OUT:
  case CURL_POLL_INOUT:
    curl_ctx = socketp ?
      (curl_ctx_t *) socketp : _cttp_create_curl_ctx(s);

    curl_multi_assign(u3_Host.ctp_u.mul_u, s, (void *) curl_ctx);

    if ( action != CURL_POLL_IN ) {
      events |= UV_WRITABLE;
    }
    
    if ( action != CURL_POLL_OUT ) {
      events |= UV_READABLE;
    }

    uv_poll_start(&curl_ctx->poll_handle, events, _cttp_curl_perform);
    break;
  case CURL_POLL_REMOVE:
    if ( socketp ) {
      uv_poll_stop(&((curl_ctx_t*) socketp)->poll_handle);
      _cttp_destroy_curl_ctx((curl_ctx_t*) socketp);
      curl_multi_assign(u3_Host.ctp_u.mul_u, s, NULL);
    }
    break;
  default:
    abort();
  }

  return 0;
}

static int
_cttp_start_timer(CURLM *mul_u, long timeout_ms, void *userp)
{
  if ( timeout_ms < 0 ) {
    uv_timer_stop(&(u3_Host.ctp_u.tim_u));
  }
  else {
    if ( timeout_ms == 0 ) {
      timeout_ms = 1;
    }

    uv_timer_start(&(u3_Host.ctp_u.tim_u), _cttp_on_timeout, timeout_ms, 0);
  }

  return 0;
}

static CURLM*
_cttp_init_curl()
{
  if ( curl_global_init(CURL_GLOBAL_ALL) ) {
    c3_assert(!"Could not init curl\n");
    return NULL;
  }
  CURLM* mul_u = curl_multi_init();
  curl_multi_setopt(mul_u, CURLMOPT_SOCKETFUNCTION, _cttp_handle_socket);
  curl_multi_setopt(mul_u, CURLMOPT_TIMERFUNCTION, _cttp_start_timer);
  return mul_u;
}

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
      _cttp_creq_free(ceq_u);
    }
  }
  else {
    ceq_u = _cttp_creq_new(num_l, u3k(u3t(cuq)));
    _cttp_creq_connect(ceq_u);
  }
  u3z(cuq);
}

/* u3_cttp_io_init(): initialize http client I/O.
*/
void
u3_cttp_io_init()
{
  uv_timer_t tim_u;
  uv_timer_init(u3L, &tim_u);
  u3_Host.ctp_u.tim_u = tim_u;

  u3_Host.ctp_u.run_u = c3_calloc(sizeof(c3_i));
  *(u3_Host.ctp_u.run_u) = 0;
  u3_Host.ctp_u.mul_u = _cttp_init_curl();
  u3_Host.ctp_u.ceq_u = 0;
}

/* u3_cttp_io_exit(): shut down cttp.
*/
void
u3_cttp_io_exit(void)
{
  free(u3_Host.ctp_u.run_u);
  while (u3_Host.ctp_u.ceq_u) {
    u3_creq* ceq_u = u3_Host.ctp_u.ceq_u;
    u3_Host.ctp_u.ceq_u = u3_Host.ctp_u.ceq_u->nex_u;
    _cttp_creq_free(ceq_u);
  }
  curl_multi_cleanup(u3_Host.ctp_u.mul_u);
  curl_global_cleanup();
}
