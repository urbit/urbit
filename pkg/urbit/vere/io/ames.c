/* vere/ames.c
**
*/
#include "all.h"
#include "vere/vere.h"
#include "ur/serial.h"

/* u3_pact: outbound ames packet.
*/
  typedef struct _u3_pact {
    uv_udp_send_t    snd_u;             //  udp send request
    u3_lane          lan_u;             //  destination lane
    c3_w             len_w;             //  length in bytes
    c3_y*            hun_y;             //  packet buffer
    c3_y             imp_y;             //  galaxy number (optional)
    c3_c*            dns_c;             //  galaxy fqdn (optional)
    struct _u3_ames* sam_u;             //  ames backpointer
  } u3_pact;

/* u3_fine: fine networking
*/
  typedef struct _u3_fine {
    u3_auto           car_u;              // fine driver
    c3_y              ver_y;               //  fine protocol
    u3p(u3h_root)     sac_p;               //  scry cache hashtable
    struct _u3_ames*  sam_u;               // ames backpointer
  } u3_fine;

/* u3_ames: ames networking.
*/
  typedef struct _u3_ames {             //  packet network state
    u3_auto          car_u;             //  ames driver
    u3_fine          fin_s;             //  fine networking
    u3_pier*         pir_u;             //  pier
    union {                             //  uv udp handle
      uv_udp_t       wax_u;             //
      uv_handle_t    had_u;             //
    };                                  //
    c3_l             sev_l;             //  instance number
    ur_cue_test_t*   tes_u;             //  cue-test handle
    u3_cue_xeno*     sil_u;             //  cue handle
    c3_c*            dns_c;             //  domain XX multiple/fallback
    c3_y             ver_y;             //  protocol version
    u3p(u3h_root)    lax_p;             //  lane scry cache
    struct _u3_panc* pac_u;             //  packets pending forwards
    c3_w             imp_w[256];        //  imperial IPs
    time_t           imp_t[256];        //  imperial IP timestamps
    c3_o             imp_o[256];        //  imperial print status
    struct {                            //    config:
      c3_o           net_o;             //  can send
      c3_o           see_o;             //  can scry
      c3_o           fit_o;             //  filtering active
    } fig_u;                            //
    struct {                            //    stats:
      c3_d           dop_d;             //  drop count
      c3_d           fal_d;             //  crash count
      c3_d           saw_d;             //  successive scry failures
      c3_d           hed_d;             //  failed to read header
      c3_d           vet_d;             //  version mismatches filtered
      c3_d           mut_d;             //  invalid mugs filtered
      c3_d           bod_d;             //  failed to read body
      c3_d           foq_d;             //  forward queue size
      c3_d           fow_d;             //  forwarded count
      c3_d           fod_d;             //  forwards dropped count
    } sat_u;                            //
  } u3_ames;

/* u3_head: ames or fine packet header
*/
  typedef struct _u3_head {
    c3_o req_o;                         //  is request (fine only)
    c3_o sim_o;                         //  is ames protocol?
    c3_y ver_y;                         //  protocol version
    c3_y sac_y;                         //  sender class
    c3_y rac_y;                         //  receiver class
    c3_l mug_l;                         //  truncated mug hash of u3_body
    c3_o rel_o;                         //  relayed?
  } u3_head;
  
/* u3_prel: ames/fine packet prelude
*/
  typedef struct _u3_prel {
    c3_y  sic_y;                        //  sender life tick
    c3_y  ric_y;                        //  receiver life tick
    c3_d  sen_d[2];                     //  sender/requester
    c3_d  rec_d[2];                     //  receiver/responder
    c3_d  rog_d;                        //  origin lane (optional)
  } u3_prel;

/* TODO: request response bodies for scry */
/* u3_requ: fine packet request */
  typedef struct _u3_requ {
    u3_prel pre_u;
    c3_y    sig_y[64];                    // requester signature
    c3_w    fra_w;                        // fragment number
    c3_s    len_s;                        // path length
    c3_c* pat_c;                       // path as ascii
  } u3_requ;

  typedef struct _u3_reqp {
    u3_head hed_u;
    u3_requ req_u;
  } u3_reqp;

/* u3_resp: fine packet response */
  typedef struct _u3_resp {
    u3_prel  pre_u;
    // request: 
    c3_w    fra_w;                        // fragment number
    c3_s    len_s;                        // path length
    c3_c*   pat_c;                       // path as ascii
    // response:
    c3_y    sig_y[64];                    // host signature
    c3_w    num_w;                        // number of fragments
    c3_s    siz_s;                        // datum size
    c3_y*   dat_y;                       // datum (0 if null response)
  } u3_resp;


  /* u3_pend: generic ames or fine pending packet */
  typedef struct _u3_pend {
    uv_udp_send_t    snd_u;             //  udp send request
    c3_y   typ_y;     // 0 for ames, 1 for fine request, 2 for fine response
    c3_d   her_d[2];  // recipent mars address
    c3_c*  dns_c;     // galaxy FQDN
    c3_y*  hun_y;     // packet contents
    c3_w   len_w;     // packet contents
    u3_lane  lan_u;   // recipient lane
    c3_w   fra_w;     // fragment number
    u3_ames* sam_u;   // ames backpointer
    union {
      u3_resp* res_u;
      u3_reqp* req_u;
      u3_pact* pac_u;
    };  
  } u3_pend;

/* u3_body: ames packet body
*/
  typedef struct _u3_body {
    u3_prel pre_u;                      //  prelude
    c3_s    con_s;                        //  content size
    c3_y*   con_y;                        //  content
    c3_l    mug_l;                        //  checksum
  } u3_body;

/* u3_panc: deconstructed incoming packet
*/
  typedef struct _u3_panc {
    u3_ames*         sam_u;             //  ames backpointer
    struct _u3_panc* pre_u;             //  previous packet
    struct _u3_panc* nex_u;             //  next packet
    u3_lane          ore_u;             //  origin lane
    u3_head          hed_u;             //  header
    u3_body          bod_u;             //  body
    void*            ptr_v;             //  buffer (to free)
  } u3_panc;

/* _ames_alloc(): libuv buffer allocator.
*/
static void
_ames_alloc(uv_handle_t* had_u,
            size_t len_i,
            uv_buf_t* buf
            )
{
  //  we allocate 2K, which gives us plenty of space
  //  for a single ames packet (max size 1060 bytes)
  //
  void* ptr_v = c3_malloc(2048);
  *buf = uv_buf_init(ptr_v, 2048);
}

/* _ames_pact_free(): free packet struct.
*/
static void
_ames_pact_free(u3_pact* pac_u)
{
  c3_free(pac_u->hun_y);
  c3_free(pac_u->dns_c);
  c3_free(pac_u);
}

static void
_ames_pend_free(u3_pend* pen_u) 
{
  if(0 == pen_u->typ_y) {          // ames packet
    _ames_pact_free(pen_u->pac_u);
  } else if (1 == pen_u->typ_y) {  // fine request
    c3_free(pen_u->req_u);
    c3_free(pen_u->hun_y);
  } else {                         // fine response
    c3_free(pen_u->res_u);
    c3_free(pen_u->hun_y);
  }
  c3_free(pen_u->dns_c);
  c3_free(pen_u);
}

/* _ames_panc_free(): remove references, lose refcounts and free struct
*/
static void
_ames_panc_free(u3_panc* pac_u)
{
  if ( 0 != pac_u->nex_u ) {
    pac_u->nex_u->pre_u = pac_u->pre_u;
  }

  if ( 0 != pac_u->pre_u ) {
    pac_u->pre_u->nex_u = pac_u->nex_u;
  }
  else {
    c3_assert(pac_u == pac_u->sam_u->pac_u);
    pac_u->sam_u->pac_u = pac_u->nex_u;
  }

  c3_free(pac_u->ptr_v);
  c3_free(pac_u);
}

static void _log_prel(u3_prel* pre_u)
{
  u3l_log("-- PRELUDE --\n");
  u3l_log("sender life: %u\n", pre_u->sic_y);
  u3l_log("receiver life: %u\n", pre_u->ric_y);
  u3l_log("sender: %" PRIu64 "\n", pre_u->sen_d[0]);
  u3l_log("receiver: %" PRIu64" \n", pre_u->rec_d[0]);
  u3l_log("\n");
}

static void _log_requ(u3_requ* req_u)
{
  _log_prel(&req_u->pre_u);
  u3l_log("--- REQUEST ---\n");
  u3l_log("strlen: %u\n", req_u->len_s);
  u3l_log("path: %s\n", req_u->pat_c);
  u3l_log("frag: %u\n", req_u->fra_w);
  u3l_log("\n");
  return;
}

static void 
_log_bytes(c3_y* byt_y, c3_w len_w)
{
  int i;
  u3l_log("-- BYTES (%u) --\n", len_w);
  for(i = 0; i < len_w; i++) {
    u3l_log("%x\n", byt_y[i]);
  }
  u3l_log("\n");
}



/* _ames_sift_head(): parse packet header.
*/
static c3_o
_ames_sift_head(u3_head* hed_u, c3_y buf_y[4])
{
  c3_w hed_w = buf_y[0]
             | (buf_y[1] <<  8)
             | (buf_y[2] << 16)
             | (buf_y[3] << 24);

  //  first three bits are reserved
  //
  hed_u->req_o = (hed_w >>  2) & 0x1;
  hed_u->sim_o = (hed_w >>  3) & 0x1;
  hed_u->ver_y = (hed_w >>  4) & 0x7;
  hed_u->sac_y = (hed_w >>  7) & 0x3;
  hed_u->rac_y = (hed_w >>  9) & 0x3;
  hed_u->mug_l = (hed_w >> 11) & 0xfffff; // 20 bits
  hed_u->rel_o = (hed_w >> 31) & 0x1;

  //  reject packets that don't even claim to be ames packets
  //
  return hed_u->sim_o;
}


/* _ames_chub_bytes(): c3_y[8] to c3_d
** XX factor out, deduplicate with other conversions
*/
static inline c3_d
_ames_chub_bytes(c3_y byt_y[8])
{
  return (c3_d)byt_y[0]
       | (c3_d)byt_y[1] << 8
       | (c3_d)byt_y[2] << 16
       | (c3_d)byt_y[3] << 24
       | (c3_d)byt_y[4] << 32
       | (c3_d)byt_y[5] << 40
       | (c3_d)byt_y[6] << 48
       | (c3_d)byt_y[7] << 56;
}

/* _ames_ship_to_chubs(): pack [len_y] bytes into c3_d[2]
*/
static inline void
_ames_ship_to_chubs(c3_d sip_d[2], c3_y len_y, c3_y* buf_y)
{
  c3_y sip_y[16] = {0};
  memcpy(sip_y, buf_y, c3_min(16, len_y));

  sip_d[0] = _ames_chub_bytes(sip_y);
  sip_d[1] = _ames_chub_bytes(sip_y + 8);
}

/* _ames_chub_bytes(): c3_d to c3_y[8]
** XX factor out, deduplicate with other conversions
*/
static inline void
_ames_bytes_chub(c3_y byt_y[8], c3_d num_d)
{
  byt_y[0] = num_d & 0xff;
  byt_y[1] = (num_d >>  8) & 0xff;
  byt_y[2] = (num_d >> 16) & 0xff;
  byt_y[3] = (num_d >> 24) & 0xff;
  byt_y[4] = (num_d >> 32) & 0xff;
  byt_y[5] = (num_d >> 40) & 0xff;
  byt_y[6] = (num_d >> 48) & 0xff;
  byt_y[7] = (num_d >> 56) & 0xff;
}

/* _ames_ship_of_chubs(): unpack c3_d[2] into [len_y] bytes.
*/
static inline void
_ames_ship_of_chubs(c3_d sip_d[2], c3_y len_y, c3_y* buf_y)
{
  c3_y sip_y[16] = {0};

  _ames_bytes_chub(sip_y, sip_d[0]);
  _ames_bytes_chub(sip_y + 8, sip_d[1]);

  memcpy(buf_y, sip_y, c3_min(16, len_y));
}

/* _ames_sift_prelude(): parse prelude, returning length
*/
static c3_y
_ames_sift_prelude(u3_head* hed_u,
                   u3_prel* pre_u,
                   c3_w     len_w,
                   c3_y*    pre_y)
{
  c3_y rog_y, sen_y, rec_y, len_y;

  rog_y = ( c3y == hed_u->rel_o )? 6 : 0;
  sen_y = 2 << hed_u->sac_y;
  rec_y = 2 << hed_u->rac_y;
  len_y = rog_y + sen_y + rec_y + 1;

  pre_u->sic_y = pre_y[0] & 0xf;
  pre_u->ric_y = pre_y[0] & 0xf0;

  _ames_ship_to_chubs(pre_u->sen_d, sen_y, pre_y + 1);
  _ames_ship_to_chubs(pre_u->rec_d, rec_y, pre_y + 1 + sen_y);

  if (rog_y) {
    c3_y rag_y[8] = {0};
    memcpy(rag_y, pre_y + 1 + sen_y + rec_y, rog_y);
    pre_u->rog_d = _ames_chub_bytes(rag_y);
  }
  else {
    pre_u->rog_d = 0;
  }
  return len_y;
}



/* _fine_sift_requ(): parse request body, returning success
 * TODO: bring up to date
*/
static c3_o 
_fine_sift_requ(u3_head* hed_u,
                u3_requ* req_u,
                c3_w     len_w,
                c3_y*    req_y)
{
  c3_y pre_y = _ames_sift_prelude(hed_u, &req_u->pre_u, len_w, req_y);

  req_y += pre_y;

  c3_w rem_w = (64 + 4 + 2 + pre_y);
  if(rem_w > len_w) {
    u3l_log("not big enough\n");
    return c3n;
  }
  memcpy(req_u->sig_y, req_y, 64);
  req_y += 64;

  //_log_bytes(req_y, 4);

  req_u->fra_w = (
      (req_y[0] << 0x0)
    | (req_y[1] << 0x8)
    | (req_y[2] << 0x10)
    | (req_y[3] << 0x18));
  req_y += 4;


  req_u->len_s = c3_min(384,
      (req_y[0] << 0x0)
    | (req_y[1] << 0x8));
  req_y += 2;

  //_log_bytes(req_y, req_u->len_s);

  // TODO: strict inequality?
  /*if(rem_w + req_u->len_s != len_w) {
    u3l_log("bad format\n");
    return c3n;
  }*/

  // TODO: defend maliciusly crafted lengths?
  req_u->pat_c = c3_calloc(req_u->len_s + 1);

  memcpy(req_u->pat_c, req_y, req_u->len_s);
  req_u->pat_c[req_u->len_s] = '\0';
  return c3y;
} 

/* _fine_sift_resp(): parse response body, returning success
*/
static c3_o 
_fine_sift_resp(u3_head* hed_u,
                u3_resp* res_u,
                c3_w     len_w,
                c3_y*    res_y)
{
  c3_w rem_w = (64 + 4 + hed_u->sac_y + 4 + hed_u->rac_y + 2);
  if(rem_w > len_w) {
    u3l_log("bad size %u", len_w);
    return c3n;
  }

  c3_y pre_y = _ames_sift_prelude(hed_u, &res_u->pre_u, len_w, res_y);
  res_y += pre_y;


  res_u->fra_w = (
      (res_y[0] << 0x0)
    | (res_y[1] << 0x8)
    | (res_y[2] << 0x10)
    | (res_y[3] << 0x18));
  res_y += 4;


  res_u->len_s = c3_min(384,
      (res_y[0] << 0x0)
    | (res_y[1] << 0x8));
  res_y += 2;

  res_u->pat_c = c3_calloc(res_u->len_s + 1);
  memcpy(res_u->pat_c, res_y, res_u->len_s);

  memcpy(res_u->sig_y, res_y, 64);
  res_y += 64;

  res_u->num_w = (
      (res_y[0] << 0x0)
    | (res_y[1] << 0x8)
    | (res_y[2] << 0x10)
    | (res_y[3] << 0x18));
  res_y += 4;

  res_u->siz_s = c3_min(384,
      (res_y[0] << 0x0)
    | (res_y[1] << 0x8));
  res_y += 2;


  res_u->dat_y = c3_calloc(res_u->siz_s);
  memcpy(&res_u->dat_y, res_y, res_u->siz_s);


  // TODO: strict inequality?
  /*if(rem_w + res_u->len_s != len_w) {
    return c3n;
  }*/

  // TODO: defend maliciusly crafted lengths?
  // TODO: add null terminator?
  return c3y;
}

/* _ames_sift_body(): parse packet body.
*/
static c3_o
_ames_sift_body(u3_head* hed_u,
                u3_body* bod_u,
                c3_w     len_w,
                c3_y*    bod_y)
{
  c3_y rog_y = ( c3y == hed_u->rel_o )? 6 : 0;
  c3_y* gob_y = bod_y + rog_y;
  c3_s gob_s = len_w - rog_y;

  c3_y pre_y = _ames_sift_prelude(hed_u, &bod_u->pre_u, len_w, bod_y);

  if (pre_y >= len_w ) {
    return c3n;
  }
  else {
    bod_u->mug_l = u3r_mug_bytes(gob_y, gob_s) & 0xfffff;

    bod_u->con_y = gob_y + 4;

    return c3y;
  }
}

/* _ames_etch_head(): serialize packet header.
*/
static void
_ames_etch_head(u3_head* hed_u, c3_y buf_y[4])
{
  c3_w hed_w = ((hed_u->req_o &     0x1) <<  2)
             ^ ((hed_u->sim_o &     0x1) <<  3)
             ^ ((hed_u->ver_y &     0x7) <<  4)
             ^ ((hed_u->sac_y &     0x3) <<  7)
             ^ ((hed_u->rac_y &     0x3) <<  9)
             ^ ((hed_u->mug_l & 0xfffff) << 11)
             ^ ((hed_u->rel_o &     0x1) << 31);

  //  only version 0 currently recognized
  //
  c3_assert( 0 == hed_u->ver_y );  //  XX remove after testing

  buf_y[0] = hed_w & 0xff;
  buf_y[1] = (hed_w >>  8) & 0xff;
  buf_y[2] = (hed_w >> 16) & 0xff;
  buf_y[3] = (hed_w >> 24) & 0xff;
}

/* _ames_etch_prelude(): serialize packet prelude
*/
static c3_y
_ames_etch_prelude(u3_head* hed_u, u3_prel* pre_u, c3_y* buf_y) 
{
  c3_y  rog_y = ( c3y == hed_u->rel_o ) ? 6 : 0;           //  origin len
  c3_y  sen_y = 2 << pre_u->sic_y;
  c3_y  rec_y = 2 << pre_u->ric_y;
  c3_y  len_y = sen_y + rec_y + rog_y;


  // copy lives
  buf_y[0] = (pre_u->sic_y & 0xf) ^ ((pre_u->ric_y & 0xf) << 4);
  
  _ames_ship_of_chubs(pre_u->sen_d, sen_y, buf_y + 1);
  _ames_ship_of_chubs(pre_u->rec_d, rec_y, buf_y + 1 + sen_y);

  if ( rog_y ) {
    c3_y rag_y[8] = {0};
    _ames_bytes_chub(rag_y, pre_u->rog_d);
    memcpy(buf_y, rag_y, rog_y);
  }
  return len_y;
}

/* _fine_etch_resp(): serialise response packet
 */
static c3_w
_fine_etch_resp(u3_head* hed_u,
                u3_resp* res_u,
                c3_y**   out_y)
{
  c3_assert(0 == hed_u->req_o);
  c3_assert(0 == hed_u->sim_o);

  c3_y  sen_y = 2 << hed_u->sac_y;                         //  sender len
  c3_y  rec_y = 2 << hed_u->rac_y;                         //  receiver len
  c3_y  len_w = 80 + sen_y + rec_y + res_u->siz_s + res_u->len_s;

  c3_y* pac_y = c3_calloc(len_w);
  c3_w* pac_w = (c3_w*)pac_y;

  memcpy(pac_w, &res_u->fra_w, 4);
  *pac_w = res_u->fra_w;  // TODO: check byte order

  *(pac_y + sen_y + 4) = (0xff << 0) & res_u->len_s;
  *(pac_y + sen_y + 5) = (0xff << 1) & res_u->len_s;

  memcpy(pac_y + sen_y + 6, res_u->pat_c, res_u->len_s);

  memcpy(pac_y + sen_y + 6 + res_u->len_s, &res_u->sig_y, 64 + 4 + 4 + 2);
  
  return len_w;
}


/* _ames_etch_pack(): serialize packet header and body.
*/
static c3_w
_ames_etch_pack(u3_head* hed_u,
                u3_body* bod_u,
                c3_y**   out_y)
{
  c3_y  sen_y = 2 << hed_u->sac_y;                         //  sender len
  c3_y  rec_y = 2 << hed_u->rac_y;                         //  receiver len
  c3_y  rog_y = ( c3y == hed_u->rel_o ) ? 6 : 0;           //  origin len
  c3_w  bod_w = rog_y + 1 + sen_y + rec_y + bod_u->con_s;  //  body len
  c3_w  len_w = 4 + bod_w;                                 //  packet len
  c3_y* pac_y = c3_malloc(len_w);                          //  output buf
  c3_y* bod_y = pac_y + 4;                                 //  body cursor

  //  serialize the head
  //
  _ames_etch_head(hed_u, pac_y);

  c3_y pre_y = _ames_etch_prelude(hed_u, &bod_u->pre_u, bod_y + 4);
  c3_y* gob_y = bod_y + pre_y;                             //  after origin

  //  serialize the body
  memcpy(gob_y, bod_u->con_y, bod_u->con_s);

  *out_y = pac_y;
  return len_w;
}

/* _ames_send_cb(): send callback.
*/
static void
_ames_send_cb(uv_udp_send_t* req_u, c3_i sas_i)
{
  u3_pend* pen_u = (u3_pend*)req_u;
  u3_ames* sam_u = pen_u->sam_u;

  if (sas_i) {
    u3l_log("ames: send fail: %s\n", uv_strerror(sas_i));
    sam_u->fig_u.net_o = c3n;
  }
  else {
    sam_u->fig_u.net_o = c3y;
  }

  //_ames_pend_free(pen_u);
}

#define _fine_send _ames_send
/* _ames_send(): send buffer to address on port.
*/
static void
_ames_send(u3_pend* pen_u)
{
  u3_ames* sam_u = pen_u->sam_u;

  // TODO: prevalidation?
  /*if ( !pen_u->hun_y ) {
    // _ames_pact_free(pac_u);
    //return;
  }
  else */ {
    struct sockaddr_in add_u;

    memset(&add_u, 0, sizeof(add_u));
    add_u.sin_family = AF_INET;
    add_u.sin_addr.s_addr = htonl(pen_u->lan_u.pip_w);
    add_u.sin_port = htons(pen_u->lan_u.por_s);

    {
      uv_buf_t buf_u = uv_buf_init((c3_c*)pen_u->hun_y, pen_u->len_w);

      c3_i     sas_i = uv_udp_send(&pen_u->snd_u,
                                   &sam_u->wax_u,
                                   &buf_u, 1,
                                   (const struct sockaddr*)&add_u,
                                   _ames_send_cb);

      if ( sas_i ) {
        if ( c3y == sam_u->fig_u.net_o ) {
          u3l_log("ames: send fail: %s\n", uv_strerror(sas_i));
          sam_u->fig_u.net_o = c3n;
        }

        //_ames_pend_free(pen_u);
      }
    }
  }
}

/* u3_ames_decode_lane(): deserialize noun to lane; 0.0.0.0:0 if invalid
*/
u3_lane
u3_ames_decode_lane(u3_atom lan) {
  u3_lane lan_u;
  c3_d lan_d;

  if ( c3n == u3r_safe_chub(lan, &lan_d) || (lan_d >> 48) != 0 ) {
    return (u3_lane){0, 0};
  }

  u3z(lan);

  lan_u.pip_w = (c3_w)lan_d;
  lan_u.por_s = (c3_s)(lan_d >> 32);
  return lan_u;
}

/* u3_ames_lane_to_chub(): serialize lane to double-word
*/
c3_d
u3_ames_lane_to_chub(u3_lane lan) {
  return ((c3_d)lan.por_s << 32) ^ (c3_d)lan.pip_w;
}

/* u3_ames_encode_lane(): serialize lane to noun
*/
u3_atom
u3_ames_encode_lane(u3_lane lan) {
  // [%| p=@]
  // [%& p=@pC]
  return u3i_chub(u3_ames_lane_to_chub(lan));
}

/* _ames_lane_into_cache(): put las for who into cache, including timestamp
*/
static void
_ames_lane_into_cache(u3p(u3h_root) lax_p, u3_noun who, u3_noun las) {
  struct timeval tim_tv;
  gettimeofday(&tim_tv, 0);
  u3_noun now = u3_time_in_tv(&tim_tv);
  u3_noun val = u3nc(las, now);
  u3h_put(lax_p, who, val);
  u3z(who);
}

/* _ames_lane_from_cache(): retrieve lane for who from cache, if any & fresh
*/
static u3_weak
_ames_lane_from_cache(u3p(u3h_root) lax_p, u3_noun who) {
  u3_weak lac = u3h_git(lax_p, who);

  if ( u3_none != lac ) {
    struct timeval tim_tv;
    gettimeofday(&tim_tv, 0);
    u3_noun now = u3_time_in_tv(&tim_tv);
    u3_noun den = u3t(lac);

    //  consider entries older than 2 minutes stale, ignore them
    //
    if ( 120000 > u3_time_gap_ms(u3k(den), now) ) {
      lac = u3k(u3h(lac));
    } else {
      lac = u3_none;
    }
  }

  u3z(who);
  return lac;
}

/* _ames_serialize_packet(): u3_panc to atom, updating the origin lane if dop_o
**                           (retains pac_u)
*/
static u3_noun
_ames_serialize_packet(u3_panc* pac_u, c3_o dop_o)
{
  //  update the body's lane, if:
  //    - we're supposed to (dop_o)
  //    - it hasn't already been updated (rel_o)
  //    - sender is not a galaxy
  //
  if (  c3y == dop_o
     && c3n == pac_u->hed_u.rel_o
     && !( ( 256 > pac_u->bod_u.pre_u.sen_d[0] )
        && ( 0  == pac_u->bod_u.pre_u.sen_d[1] ) ) )
  {
    pac_u->hed_u.rel_o = c3y;
    pac_u->bod_u.pre_u.rog_d = u3_ames_lane_to_chub(pac_u->ore_u);
  }

  //  serialize the packet
  //
  //    XX serialize on stack?
  //
  {
    u3_noun pac;
    c3_y* pac_y;
    c3_w  len_w = _ames_etch_pack(&pac_u->hed_u,
                                  &pac_u->bod_u,
                                  &pac_y);
    pac = u3i_bytes(len_w, pac_y);
    c3_free(pac_y);

    return pac;
  }
}

/* _ames_czar_port(): udp port for galaxy.
*/
static c3_s
_ames_czar_port(c3_y imp_y)
{
  if ( c3n == u3_Host.ops_u.net ) {
    return 31337 + imp_y;
  }
  else {
    return 13337 + imp_y;
  }
}

/* _ames_czar_gone(): galaxy address resolution failed.
*/
static void
_ames_czar_gone(u3_pend* pen_u, time_t now)
{
  u3_ames* sam_u = pen_u->sam_u;
  c3_d imp_d = pen_u->her_d[0];

  if ( c3y == sam_u->imp_o[imp_d] ) {
    u3l_log("ames: czar at %s: not found (b)\n", pen_u->dns_c);
    sam_u->imp_o[imp_d] = c3n;
  }

  if ( (0 == sam_u->imp_w[imp_d]) ||
       (0xffffffff == sam_u->imp_w[imp_d]) )
  {
    sam_u->imp_w[imp_d] = 0xffffffff;
  }

  //  keep existing ip for 5 more minutes
  //
  sam_u->imp_t[imp_d] = now;

  _ames_pend_free(pen_u);
}

/* _ames_czar_here(): galaxy address resolution succeeded.
*/
static void
_ames_czar_here(u3_pend* pen_u, time_t now, struct sockaddr_in* add_u)
{
  u3_ames* sam_u = pen_u->sam_u;
  c3_y     imp_y = pen_u->her_d[0];
  c3_w     old_w = sam_u->imp_w[imp_y];
  c3_w     pip_w = ntohl(add_u->sin_addr.s_addr);

  if ( pip_w != old_w ) {
    u3_noun nam = u3dc("scot", c3__if, u3i_word(pip_w));
    c3_c* nam_c = u3r_string(nam);

    u3l_log("ames: czar %s: ip %s\n", pen_u->dns_c, nam_c);

    c3_free(nam_c);
    u3z(nam);
  }

  sam_u->imp_w[imp_y] = pip_w;
  sam_u->imp_t[imp_y] = now;
  sam_u->imp_o[imp_y] = c3y;

  pen_u->lan_u.pip_w = pip_w;
}

/* _ames_czar_cb(): galaxy address resolution callback.
*/
static void
_ames_czar_cb(uv_getaddrinfo_t* adr_u,
              c3_i              sas_i,
              struct addrinfo*  aif_u)
{
  {
    u3_pend*         pen_u = (u3_pend*)adr_u->data;
    struct addrinfo* rai_u = aif_u;
    time_t             now = time(0);

    while ( rai_u ) {
      if ( (AF_INET == rai_u->ai_family) ) {
        _ames_czar_here(pen_u, now, (struct sockaddr_in *)rai_u->ai_addr);
        break;
      }
      else {
        rai_u = rai_u->ai_next;
      }
    }

    if ( !rai_u ) {
      _ames_czar_gone(pen_u, now);
    }
  }

  c3_free(adr_u);
  uv_freeaddrinfo(aif_u);
}

/* _ames_czar(): galaxy address resolution.
*/
static void
_ames_czar(u3_pend* pen_u)
{
  u3_ames* sam_u = pen_u->sam_u;

  c3_d imp_d = pen_u->her_d[0];

  c3_assert( imp_d < 256 );

  pen_u->lan_u.por_s = _ames_czar_port(imp_d);

  if ( c3n == u3_Host.ops_u.net ) {
    pen_u->lan_u.pip_w = 0x7f000001;
    _ames_send(pen_u);
    return;
  }

  //  if we don't have a galaxy domain, no-op
  //
  if ( !sam_u->dns_c ) {
    u3_noun nam = u3dc("scot", 'p', pen_u->her_d[0]);
    c3_c*  nam_c = u3r_string(nam);
    u3l_log("ames: no galaxy domain for %s, no-op\r\n", nam_c);

    c3_free(nam_c);
    u3z(nam);
    return;
  }

  {
    c3_w pip_w = sam_u->imp_w[imp_d];
    time_t wen = sam_u->imp_t[imp_d];
    time_t now = time(0);

    //  backoff for 5 minutes after failed lookup
    //
    if (  ( now < wen )               //  time shenanigans!
       || (  (0xffffffff == pip_w)    //  sentinal ip address
          && ((now - wen) < 300) ) )
    {
      _ames_pend_free(pen_u);
      return;
    }
    //  cached addresses have a 5 minute TTL
    //
    else if ( (0 != pip_w) && ((now - wen) < 300) ) {
      pen_u->lan_u.pip_w = pip_w;
      _ames_send(pen_u);
      return;
    }
    else {
      c3_i sas_i;

      {
        u3_noun nam = u3dc("scot", 'p', imp_d);
        c3_c* nam_c = u3r_string(nam);

        //  NB: . separator not counted, as [nam_c] includes a ~ that we skip
        //
        pen_u->dns_c = c3_malloc(1 + strlen(nam_c) + strlen(sam_u->dns_c));
        sas_i = snprintf(pen_u->dns_c, 255, "%s.%s", nam_c + 1, sam_u->dns_c);

        c3_free(nam_c);
        u3z(nam);
      }

      if ( 255 <= sas_i ) {
        u3l_log("ames: czar: galaxy domain %s truncated\n", sam_u->dns_c);
        _ames_pend_free(pen_u);
        return;
      }

      {
        uv_getaddrinfo_t* adr_u = c3_malloc(sizeof(*adr_u));
        adr_u->data = pen_u;

        if ( 0 != (sas_i = uv_getaddrinfo(u3L, adr_u,
                                          _ames_czar_cb,
                                          pen_u->dns_c, 0, 0)) )
        {
          u3l_log("ames: %s\n", uv_strerror(sas_i));
          _ames_czar_gone(pen_u, now);
          return;
        }
      }
    }
  }
}

/* _ames_ef_send(): send packet to network (v4).
*/
static void
_ames_ef_send(u3_ames* sam_u, u3_noun lan, u3_noun pac)
{
  if ( c3n == sam_u->car_u.liv_o ) {
    u3l_log("ames: not yet live, dropping outbound\r\n");
    u3z(lan); u3z(pac);
    return;
  }

  u3_pact* pac_u = c3_calloc(sizeof(*pac_u));
  pac_u->sam_u = sam_u;
  pac_u->len_w = u3r_met(3, pac);
  pac_u->hun_y = c3_malloc(pac_u->len_w);

  u3_pend* pen_u = c3_calloc(sizeof(*pen_u));
  pen_u->typ_y = 0;
  pen_u->len_w = pac_u->len_w;
  pen_u->hun_y = pac_u->hun_y;
  pen_u->sam_u = sam_u;



  u3r_bytes(0, pac_u->len_w, pac_u->hun_y, pac);

  u3_noun tag, val;
  u3x_cell(lan, &tag, &val);
  c3_assert( (c3y == tag) || (c3n == tag) );

  //  galaxy lane; do DNS lookup and send packet
  //
  if ( c3y == tag ) {
    c3_assert( c3y == u3a_is_cat(val) );
    c3_assert( val < 256 );

    pac_u->imp_y = val;
    pen_u->her_d[0] = val;
    _ames_czar(pen_u);
  }
  //  non-galaxy lane
  //
  else {
    u3_lane lan_u = u3_ames_decode_lane(u3k(val));
    //  convert incoming localhost to outgoing localhost
    //
    lan_u.pip_w = ( 0 == lan_u.pip_w )? 0x7f000001 : lan_u.pip_w;
    //  if in local-only mode, don't send remote packets
    //
    if ( (c3n == u3_Host.ops_u.net) && (0x7f000001 != lan_u.pip_w) ) {
      _ames_pact_free(pac_u);
    }
    //  if the lane is uninterpretable, silently drop the packet
    //
    else if ( 0 == lan_u.por_s ) {
      if ( u3C.wag_w & u3o_verbose ) {
        u3l_log("ames: inscrutable lane\n");
      }
      _ames_pact_free(pac_u);
    }
    //  otherwise, mutate destination and send packet
    //
    else {
      pen_u->lan_u = lan_u;
      _ames_send(pen_u);
    }
  }
  u3z(lan); u3z(pac);
}

/* _ames_cap_queue(): cap ovum queue at 1k, dropping oldest packets.
*/
static void
_ames_cap_queue(u3_ames* sam_u)
{
  u3_ovum* egg_u = sam_u->car_u.ext_u;

  while ( egg_u && (1000 < sam_u->car_u.dep_w) ) {
    u3_ovum* nex_u = egg_u->nex_u;

    if ( c3__hear == u3h(egg_u->cad) ) {
      u3_auto_drop(&sam_u->car_u, egg_u);
      sam_u->sat_u.dop_d++;

      if ( u3C.wag_w & u3o_verbose ) {
        u3l_log("ames: packet dropped (%" PRIu64 " total)\n", sam_u->sat_u.dop_d);
      }
    }

    egg_u = nex_u;
  }

  if (  (sam_u->sat_u.dop_d && (0 == (sam_u->sat_u.dop_d % 1000)))
     && !(u3C.wag_w & u3o_verbose) )
  {
    u3l_log("ames: packet dropped (%" PRIu64 " total)\n", sam_u->sat_u.dop_d);
  }
}

/* _ames_punt_goof(): print %bail error report(s).
*/
static void
_ames_punt_goof(u3_noun lud)
{
  if ( 2 == u3qb_lent(lud) ) {
    u3_pier_punt_goof("hear", u3k(u3h(lud)));
    u3_pier_punt_goof("crud", u3k(u3h(u3t(lud))));
  }
  else {
    u3_noun dul = lud;
    c3_w len_w = 1;

    while ( u3_nul != dul ) {
      u3l_log("ames: bail %u\r\n", len_w++);
      u3_pier_punt_goof("ames", u3k(u3h(dul)));
      dul = u3t(dul);
    }
  }

  u3z(lud);
}

/* _ames_hear_bail(): handle packet failure.
*/
static void
_ames_hear_bail(u3_ovum* egg_u, u3_noun lud)
{
  u3_ames* sam_u = (u3_ames*)egg_u->car_u;
  sam_u->sat_u.fal_d++;

  if (  (u3C.wag_w & u3o_verbose)
     || (0 == (sam_u->sat_u.fal_d % 1000)) )
  {
    _ames_punt_goof(lud);
    u3l_log("ames: packet failed (%" PRIu64 " total)\n\n", sam_u->sat_u.fal_d);
  }
  else {
    u3z(lud);

    if (  0 == (sam_u->sat_u.fal_d % 1000) )  {
      u3l_log("ames: packet failed (%" PRIu64 " total)\n\n", sam_u->sat_u.fal_d);
    }
  }

  u3_ovum_free(egg_u);
}

/* _ames_put_packet(): add packet to queue, drop old packets on pressure
*/
static void
_ames_put_packet(u3_ames* sam_u,
                 u3_noun  msg,
                 u3_lane  lan_u)
{
  u3_noun wir = u3nc(c3__ames, u3_nul);
  u3_noun cad = u3nt(c3__hear, u3nc(c3n, u3_ames_encode_lane(lan_u)), msg);

  u3_auto_peer(
    u3_auto_plan(&sam_u->car_u,
                 u3_ovum_init(0, c3__a, wir, cad)),
    0, 0, _ames_hear_bail);

  _ames_cap_queue(sam_u);
}

/*  _ames_forward(): forward pac_u onto the (list lane) las, then free pac_u
*/
static void
_ames_forward(u3_panc* pac_u, u3_noun las)
{
  u3_ames* sam_u = pac_u->sam_u;

  sam_u->sat_u.fow_d++;
  if ( 0 == (sam_u->sat_u.fow_d % 1000000) ) {
    u3l_log("ames: forwarded %" PRIu64 " total\n", sam_u->sat_u.fow_d);
  }

  if ( u3C.wag_w & u3o_verbose ) {
    u3_noun sen = u3dc("scot", 'p', u3i_chubs(2, pac_u->bod_u.pre_u.sen_d));
    u3_noun rec = u3dc("scot", 'p', u3i_chubs(2, pac_u->bod_u.pre_u.rec_d));
    c3_c* sen_c = u3r_string(sen);
    c3_c* rec_c = u3r_string(rec);
    c3_y* pip_y = (c3_y*)&pac_u->ore_u.pip_w;

    u3l_log("ames: forwarding for %s to %s from %d.%d.%d.%d:%d\n",
            sen_c, rec_c,
            pip_y[0], pip_y[1], pip_y[2], pip_y[3],
            pac_u->ore_u.por_s);

    c3_free(sen_c); c3_free(rec_c);
    u3z(sen); u3z(rec);
  }

  {
    u3_noun pac = _ames_serialize_packet(pac_u, c3y);
    u3_noun tag, dat, lan, t = las;

    while ( u3_nul != t ) {
      u3x_cell(t, &lan, &t);

      //  validate lane and skip self if galaxy
      //
      if ( c3n == u3r_cell(lan, &tag, &dat) ) {
        u3l_log("ames: bogus lane\n");
        u3m_p("lan", lan);
      }
      else {
        c3_o sen_o = c3y;
        c3_d who_d[2];

        if ( c3y == tag ) {
          u3r_chubs(0, 2, who_d, dat);

          if (  (who_d[0] == sam_u->pir_u->who_d[0])
             && (who_d[1] == sam_u->pir_u->who_d[1]) )
          {
            sen_o = c3n;
            if ( u3C.wag_w & u3o_verbose ) {
              u3l_log("ames: forward skipping self\n");
            }
          }
        }

        if ( c3y == sen_o ) {
          _ames_ef_send(sam_u, u3k(lan), u3k(pac));
        }
      }
    }

    u3z(pac);
  }

  _ames_panc_free(pac_u);
  u3z(las);
}

/*  _ames_lane_scry_cb(): learn lane to forward packet on
*/
static void
_ames_lane_scry_cb(void* vod_p, u3_noun nun)
{
  u3_panc* pac_u = vod_p;
  u3_ames* sam_u = pac_u->sam_u;
  u3_weak    las = u3r_at(7, nun);

  sam_u->sat_u.foq_d--;

  //  if scry fails, remember we can't scry, and just inject the packet
  //
  if ( u3_none == las ) {
    if ( 5 < ++sam_u->sat_u.saw_d ) {
      u3l_log("ames: giving up scry\n");
      sam_u->fig_u.see_o = c3n;
    }
    _ames_put_packet(sam_u, _ames_serialize_packet(pac_u, c3n), pac_u->ore_u);
    _ames_panc_free(pac_u);
  }
  else {
    sam_u->sat_u.saw_d = 0;

    //  cache the scry result for later use
    //
    _ames_lane_into_cache(sam_u->lax_p,
                          u3i_chubs(2, pac_u->bod_u.pre_u.rec_d),
                          u3k(las));

    //  if there is no lane, drop the packet
    //
    if ( u3_nul == las ) {
      _ames_panc_free(pac_u);
    }
    //  if there is a lane, forward the packet on it
    //
    else {
      _ames_forward(pac_u, u3k(las));
    }
  }

  u3z(nun);
}

/* _ames_try_forward(): try to forward a packet for another ship.
*/
static void
_ames_try_forward(u3_ames* sam_u,
                  u3_lane* lan_u,
                  u3_head* hed_u,
                  u3_body* bod_u,
                  c3_y*    hun_y)
{
  u3_weak lac;

  //  if the recipient is a galaxy, their lane is always &+~gax
  //
  if (  (256 > bod_u->pre_u.rec_d[0])
     && (0  == bod_u->pre_u.rec_d[1]) )
  {
    lac = u3nc(c3y, (c3_y)bod_u->pre_u.rec_d[0]);
  }
  //  otherwise, try to get the lane from cache
  //
  else {
    lac = _ames_lane_from_cache(sam_u->lax_p, u3i_chubs(2, bod_u->pre_u.rec_d));

    //  if we don't know the lane, and the scry queue is full,
    //  just drop the packet
    //
    //TODO  drop oldest item in forward queue in favor of this one.
    //      ames.c doesn't/shouldn't know about the shape of scry events,
    //      so can't pluck these out of the event queue like it does in
    //      _ames_cap_queue. as such, blocked on u3_lord_peek_cancel or w/e.
    //
    if ( (u3_none == lac) && (1000 < sam_u->sat_u.foq_d) ) {
      sam_u->sat_u.fod_d++;
      if ( 0 == (sam_u->sat_u.fod_d % 10000) ) {
        u3l_log("ames: dropped %" PRIu64 " forwards total\n",
                sam_u->sat_u.fod_d);
      }

      c3_free(hun_y);
      return;
    }
    //  if we know there's no lane, drop the packet
    //
    else if ( u3_nul == lac ) {
      c3_free(hun_y);
      return;
    }
  }

  //  proceed with forwarding
  //
  {
    //  store the packet details for later processing
    //
    //    XX allocates unnecessarily when we know the lane
    //
    u3_panc* pac_u = c3_calloc(sizeof(*pac_u));
    pac_u->sam_u = sam_u;
    pac_u->hed_u = *hed_u;
    pac_u->bod_u = *bod_u;
    pac_u->ore_u = *lan_u;
    pac_u->ptr_v = hun_y;

    if ( 0 != sam_u->pac_u ) {
      pac_u->nex_u = sam_u->pac_u;
      sam_u->pac_u->pre_u = pac_u;
    }
    sam_u->pac_u = pac_u;

    //  if we already know the lane, just forward
    //
    if ( u3_none != lac ) {
      _ames_forward(pac_u, lac);
    }
    //  otherwise, there's space in the scry queue; scry the lane out of ames
    //
    else {
      sam_u->sat_u.foq_d++;
      u3_noun pax = u3nq(u3i_string("peers"),
                         u3dc("scot", 'p', u3i_chubs(2, bod_u->pre_u.rec_d)),
                         u3i_string("forward-lane"),
                         u3_nul);
      u3_pier_peek_last(sam_u->pir_u, u3_nul, c3__ax,
                        u3_nul, pax, pac_u, _ames_lane_scry_cb);
    }
  }
}

#undef AMES_SKIP
#ifdef AMES_SKIP
/* _ames_skip(): decide whether to skip this packet, for rescue
*/
static c3_o
_ames_skip(u3_body* bod_u) {
  if ( bod_u->sen_d[1] == 0 &&
       ( bod_u->sen_d[0] == 0x743a17a6
         || bod_u->sen_d[0] == 0xea99acb6
         || bod_u->sen_d[0] == 0x10100
     ) ) {
    return c3n;
  }
  else {
    return c3y;
  }
}
#endif

/* _fine_pack_scry_cb(): receive all packets for datum out of fine
 * TODO: implement
 */
static void _fine_pack_scry_cb(void* vod_p, u3_noun nun)
{

  u3_pend* pen_u = vod_p;
  u3_weak  pac = u3r_at(7, nun);
  if(pac == u3_none) {
    u3l_log("no result, bailing\n");
    return;
  }
  u3_noun num = u3i_word(pen_u->fra_w - 1);

  u3_noun fra = u3dc("snag", num, pac);

  pen_u->len_w = u3r_met(3, fra);
  pen_u->hun_y = c3_calloc(pen_u->len_w);
  
  u3r_bytes(0, pen_u->len_w, pen_u->hun_y, fra);


  _fine_send(pen_u);

  
  // 
  //  hashtable = (path, packet_num) -> cached_result;
  //  All cases, except 1st ~: put results in cache for all packets
  // ~ -> if second request, ask for notification if 2nd, drop on floor
  // [~ ~] -> send packet
  // [~ ~ *] -> send packet
  // TODO: cache logic, counting requests, evict?

  /*u3_weak lan = _ames_lane_from_cache(pen_u->sam_u->lax_p, 
                                     u3i_chubs(2, pen_u->her_d));
  if(u3_none == lan) {
    // TODO: enqueue packet for later sending, populate lane cache
  } else if(u3_nul == lan)  {
    // TODO: no lane, drop packet
  } else {
    _fine_send(pen_u);
  }
  */
}



/* _fine_hear(): hear a (potential) packet, dispatch appropriately
 */
static void _fine_hear(u3_ames* sam_u,
                       u3_lane  lan_u,
                       c3_w     len_w,
                       c3_y*    hun_y)
{
  u3_head hed_u;
  //u3_body bod_u;
  c3_assert ( c3n == _ames_sift_head(&hed_u, hun_y));

  
  if(c3n == hed_u.req_o) {
    // lookup in cache
    // (unit (unit (unit packet))
    // ~ -> miss
    // [~ ~] -> hit, unbound
    // [~ ~ ~] -> hit, empty
    // [~ ~ ~ *] -> hit, w/ data
    u3_requ req_u;

    // skip past header
    len_w -= 4;
    hun_y += 4;



    c3_assert( c3y == _fine_sift_requ(&hed_u, &req_u, len_w, hun_y));

    //_log_requ(&req_u);

    u3_noun pat = u3i_string(req_u.pat_c);

    // TODO: revive
    u3_weak cac =  u3_none; // u3h_git(sam_u->fin_s.sac_p, pat);


    if(u3_none == cac) {
      // cache miss
      //  packet scry path
      //  /packet/requested/path/elements
      //
      //  eg:
      //  /packet/gx/~zod/graph-store/5/graphs/~zod/test/message/10
      //
      u3_noun pax = u3nc(u3i_string("message"),
                        u3do("stab", pat));


      u3_reqp* rep_u = c3_calloc(sizeof(*rep_u));
      u3_pend* pen_u = c3_calloc(sizeof(*pen_u));

      pen_u->sam_u = sam_u;
      pen_u->typ_y = 2;
      pen_u->req_u = rep_u;

      memcpy(&rep_u->hed_u, &hed_u, sizeof(u3_head));
      memcpy(&rep_u->req_u, &req_u, sizeof(u3_requ));

      memcpy(&pen_u->lan_u, &lan_u, sizeof(u3_lane));
      pen_u->fra_w = req_u.fra_w;

      u3_pier_peek_last(sam_u->fin_s.car_u.pir_u, u3_nul, c3__fx, u3_nul, 
                        pax, pen_u, _fine_pack_scry_cb);
      

    } else if(u3_nul == cac) {
      // cache hit, unbound
      // do nothing, maybe report?
    } else {
      //  shape 
      //  
      u3_noun fra = u3dc("snag", u3i_word(req_u.fra_w), u3t(cac));
      //
      
      c3_w  fra_w = u3r_met(3, fra);
      c3_y* fra_y = c3_calloc(fra_w);
      
      u3r_bytes(0, fra_w, fra_y, fra);

      u3_resp* res_u = c3_calloc(sizeof(*res_u));

      c3_assert(c3y == _fine_sift_resp(&hed_u, res_u, fra_w, fra_y));
      
      memcpy(&res_u->pre_u, &req_u.pre_u, sizeof(u3_prel));

      c3_y* res_y;
      c3_w res_w = _fine_etch_resp(&hed_u, res_u, &res_y);

      // TODO: where free? maybe stack allocate instead?
      u3_pend* pen_u = c3_calloc(sizeof(*pen_u));
      pen_u->typ_y = 2;
      pen_u->res_u = res_u;
      pen_u->len_w = res_w;
      pen_u->hun_y = res_y;
      pen_u->her_d[0] = res_u->pre_u.sen_d[0];
      pen_u->her_d[1] = res_u->pre_u.sen_d[1];
      pen_u->lan_u = lan_u;
      pen_u->sam_u = sam_u;

      _fine_send(pen_u);
  
      return;
    }
  } else {
    u3_resp res_u;

    // TODO: check mug

    u3_noun wir = u3nc(c3__fine, u3_nul);
    c3_w num_w = res_u.num_w;
    c3_w fra_w = res_u.fra_w;

    u3_noun cad = u3nt(c3__purr, 
                       u3nc(c3n, u3_ames_encode_lane(lan_u)),
                       u3i_bytes(len_w, hun_y));

    u3_ovum* ovo_u = u3_ovum_init(0, c3__fine, wir, cad);
    u3_auto_plan(&sam_u->fin_s.car_u, ovo_u);
  }
}



static void _log_head(u3_head* hed_u)
{
  u3l_log("-- HEAADER --\n");
  u3l_log("is request: %u\n", hed_u->req_o);
  u3l_log("is ames: %u\n", hed_u->sim_o);
  u3l_log("protocol ames: %u\n", hed_u->ver_y);
  u3l_log("sender class: %u\n", hed_u->sac_y);
  u3l_log("recevr class: %u\n", hed_u->rac_y);
  u3l_log("\n");
}




static void
_fine_request(u3_ames* sam_u,
              u3_lane lan_u,
              u3_noun  req)
{
  u3l_log("fine: request\n");

  u3_pend* pen_u = c3_calloc(sizeof(*pen_u));
  u3_head* hed_u = c3_calloc(sizeof(*hed_u));
  u3_requ* req_u = c3_calloc(sizeof(*req_u));
  
  c3_w req_w = u3r_met(3, req);
  c3_y* req_y = c3_calloc(req_w);
  u3r_bytes(0, req_w, req_y, req);
  //_log_bytes(req_y, 4);
  pen_u->hun_y = req_y;
  pen_u->len_w = req_w;

  _ames_sift_head(hed_u, req_y);
  req_y += 4;
  req_w -= 4;

  _fine_sift_requ(hed_u, req_u, req_w, req_y);

  pen_u->typ_y = 1;
  pen_u->her_d[0] = req_u->pre_u.rec_d[0];
  pen_u->her_d[1] = req_u->pre_u.rec_d[1];
  pen_u->lan_u.pip_w = lan_u.pip_w;
  pen_u->lan_u.por_s = lan_u.por_s;

  pen_u->sam_u = sam_u;
  _ames_czar(pen_u);
}

/* _ames_hear(): parse a (potential) packet, dispatch appropriately.
*/
static void
_ames_hear(u3_ames* sam_u,
           u3_lane* lan_u,
           c3_w     len_w,
           c3_y*    hun_y)
{

  //  TODO: move from stack to heap to avoid reparsing
  u3_head hed_u;
  u3_body bod_u;

  //  XX packet filtering needs to revised for two protocol-change scenarios
  //
  //    - packets using old protocol versions from our sponsees
  //      these must be let through, and this is a transitive condition;
  //      they must also be forwarded where appropriate
  //      they can be validated, as we know their semantics
  //
  //    - packets using newer protocol versions
  //      these should probably be let through, or at least
  //      trigger printfs suggesting upgrade.
  //      they cannot be filtered, as we do not know their semantics
  //

  //  unpack header, ensuring buffer is large enough
  //
  //
  c3_o is_ames_o = _ames_sift_head(&hed_u, hun_y);
  
  if (4 > len_w) {
    sam_u->sat_u.hed_d++;
    if ( 0 == (sam_u->sat_u.hed_d % 100000) ) {
      u3l_log("ames: %" PRIu64 " dropped, failed to read header\n",
              sam_u->sat_u.hed_d);
    }

    c3_free(hun_y);
    return;
  }
  else if (c3n == is_ames_o) {
    // TODO: dispatch fine request
    _fine_hear(sam_u, *lan_u, len_w, hun_y);
    return;
  }

  //  ensure the protocol version matches ours
  //
  //    XX rethink use of [fit_o] here and elsewhere
  //
  if (  (c3y == sam_u->fig_u.fit_o)
     && (sam_u->ver_y != hed_u.ver_y) )
  {
    sam_u->sat_u.vet_d++;
    if ( 0 == (sam_u->sat_u.vet_d % 100000) ) {
      u3l_log("ames: %" PRIu64 " dropped for version mismatch\n",
              sam_u->sat_u.vet_d);
    }

    c3_free(hun_y);
    return;
  }

  {
    c3_w  bod_w = len_w - 4;
    c3_y* bod_y = hun_y + 4;

    //  unpack and validate the body
    //
    if ( (c3n == _ames_sift_body(&hed_u, &bod_u, bod_w, bod_y)) ) {
      sam_u->sat_u.bod_d++;
      if ( 0 == (sam_u->sat_u.bod_d % 100) ) {
        u3l_log("ames: %" PRIu64 " dropped, failed to read body\n",
                sam_u->sat_u.bod_d);
      }

      c3_free(hun_y);
      return;
    }

    //  ensure the mug is valid
    //
    if ( bod_u.mug_l != hed_u.mug_l ) {
      sam_u->sat_u.mut_d++;
      if ( 0 == (sam_u->sat_u.mut_d % 100000) ) {
        u3l_log("ames: %" PRIu64 " dropped for invalid mug\n",
                sam_u->sat_u.mut_d);
      }

      c3_free(hun_y);
      return;
    }
  }

  //  if we can scry,
  //  and we are not the recipient,
  //  we might want to forward statelessly
  //
  if (  0 && (c3y == sam_u->fig_u.see_o)
     && (  (bod_u.pre_u.rec_d[0] != sam_u->pir_u->who_d[0])
        || (bod_u.pre_u.rec_d[1] != sam_u->pir_u->who_d[1]) ) )
  {
    _ames_try_forward(sam_u, lan_u, &hed_u, &bod_u, hun_y);
  }
  //  otherwise, inject the packet as an event
  //
  else {
    u3_noun msg = u3i_bytes(len_w, hun_y);
    c3_free(hun_y);
#ifdef AMES_SKIP
    if (_ames_skip(&bod_u) == c3y ) {
      u3z(msg);
    }
    else {
#endif
      _ames_put_packet(sam_u, msg, *lan_u);
#ifdef AMES_SKIP
    }
#endif
  }
}

/* _ames_recv_cb(): udp message receive callback.
*/
static void
_ames_recv_cb(uv_udp_t*        wax_u,
              ssize_t          nrd_i,
              const uv_buf_t * buf_u,
              const struct sockaddr* adr_u,
              unsigned         flg_i)
{
  if ( 0 > nrd_i ) {
    if ( u3C.wag_w & u3o_verbose ) {
      u3l_log("ames: recv: fail: %s\n", uv_strerror(nrd_i));
    }
    c3_free(buf_u->base);
  }
  else if ( 0 == nrd_i ) {
    c3_free(buf_u->base);
  }
  else if ( flg_i & UV_UDP_PARTIAL ) {
    if ( u3C.wag_w & u3o_verbose ) {
      u3l_log("ames: recv: fail: message truncated\n");
    }
    c3_free(buf_u->base);
  }
  else {
    u3_ames*            sam_u = wax_u->data;
    struct sockaddr_in* add_u = (struct sockaddr_in*)adr_u;
    u3_lane             lan_u;

    lan_u.por_s = ntohs(add_u->sin_port);
    lan_u.pip_w = ntohl(add_u->sin_addr.s_addr);

    //  NB: [nrd_i] will never exceed max length from _ames_alloc()
    //
    _ames_hear(sam_u, &lan_u, (c3_w)nrd_i, (c3_y*)buf_u->base);
  }
}

/* _ames_io_start(): initialize ames I/O.
*/
static void
_ames_io_start(u3_ames* sam_u)
{
  c3_s     por_s = sam_u->pir_u->por_s;
  u3_noun    who = u3i_chubs(2, sam_u->pir_u->who_d);
  u3_noun    rac = u3do("clan:title", u3k(who));
  c3_i     ret_i;

  if ( c3__czar == rac ) {
    c3_y num_y = (c3_y)sam_u->pir_u->who_d[0];
    c3_s zar_s = _ames_czar_port(num_y);

    if ( 0 == por_s ) {
      por_s = zar_s;
    }
    else if ( por_s != zar_s ) {
      u3l_log("ames: czar: overriding port %d with -p %d\n", zar_s, por_s);
      u3l_log("ames: czar: WARNING: %d required for discoverability\n", zar_s);
    }
  }

  //  Bind and stuff.
  {
    struct sockaddr_in add_u;
    c3_i               add_i = sizeof(add_u);

    memset(&add_u, 0, sizeof(add_u));
    add_u.sin_family = AF_INET;
    add_u.sin_addr.s_addr = _(u3_Host.ops_u.net) ?
                              htonl(INADDR_ANY) :
                              htonl(INADDR_LOOPBACK);
    add_u.sin_port = htons(por_s);

    if ( (ret_i = uv_udp_bind(&sam_u->wax_u,
                              (const struct sockaddr*)&add_u, 0)) != 0 )
    {
      u3l_log("ames: bind: %s\n", uv_strerror(ret_i));

      if ( (c3__czar == rac) &&
           (UV_EADDRINUSE == ret_i) )
      {
        u3l_log("    ...perhaps you've got two copies of vere running?\n");
      }

      //  XX revise
      //
      u3_pier_bail(u3_king_stub());
    }

    uv_udp_getsockname(&sam_u->wax_u, (struct sockaddr *)&add_u, &add_i);
    c3_assert(add_u.sin_port);

    sam_u->pir_u->por_s = ntohs(add_u.sin_port);
  }

  if ( c3y == u3_Host.ops_u.net ) {
    u3l_log("ames: live on %d\n", sam_u->pir_u->por_s);
  }
  else {
    u3l_log("ames: live on %d (localhost only)\n", sam_u->pir_u->por_s);
  }

  uv_udp_recv_start(&sam_u->wax_u, _ames_alloc, _ames_recv_cb);

  sam_u->car_u.liv_o = c3y;
  u3z(rac);
  u3z(who);
}

/* _ames_ef_turf(): initialize ames I/O on domain(s).
*/
static void
_ames_ef_turf(u3_ames* sam_u, u3_noun tuf)
{
  if ( u3_nul != tuf ) {
    //  XX save all for fallback, not just first
    //
    u3_noun hot = u3k(u3h(tuf));
    c3_w  len_w = u3_mcut_host(0, 0, u3k(hot));

    sam_u->dns_c = c3_malloc(1 + len_w);
    u3_mcut_host(sam_u->dns_c, 0, hot);
    sam_u->dns_c[len_w] = 0;

    //  XX invalidate sam_u->imp_w &c ?
    //

    u3z(tuf);
  }
  else if ( (c3n == sam_u->pir_u->fak_o) && (0 == sam_u->dns_c) ) {
    u3l_log("ames: turf: no domains\n");
  }

  //  XX is this ever necessary?
  //
  if ( c3n == sam_u->car_u.liv_o ) {
    _ames_io_start(sam_u);
  }
}


/* fine_io_kick:(): receive effect from arvo
 */
static c3_o _fine_io_kick(u3_auto* car_u, u3_noun wir, u3_noun nun) {
  u3_fine* fin_u = (u3_fine*)car_u;
  u3_ames* sam_u = fin_u->sam_u;
  u3_noun hed = u3h(nun);
  //u3m_p("head: ", hed);

  if(c3__howl == hed) {
    // TODO: respond to notification of bound data 
    return c3y;
  } else if(c3__hoot == hed) {
    u3_noun lan, hot;
    u3x_cell(u3t(nun), &lan, &hot);
    u3_lane lan_u = u3_ames_decode_lane(lan);
    _fine_request(sam_u, lan_u, hot);
    return c3y;
  } else {
    return c3n;
  }
}

static void _fine_send_fail()
{
  // TODO: log some shit
}

static void _fine_send_cb()
{
  // TODO: anything to do here?
}

/* _ames_prot_scry_cb(): receive ames protocol version
*/
static void
_ames_prot_scry_cb(void* vod_p, u3_noun nun)
{
  u3_ames* sam_u = vod_p;
  u3_weak    ver = u3r_at(7, nun);

  if ( u3_none == ver ) {
    //  assume protocol version 0
    //
    sam_u->ver_y = 0;
  }
  else if ( (c3n == u3a_is_cat(ver))
         || (7 < ver) ) {
    u3m_p("ames: strange protocol", nun);
    sam_u->ver_y = 0;
  }
  else {
    sam_u->ver_y = ver;
  }

  //  XX revise: filtering should probably be disabled if
  //  we get a protocol version above the latest one we know
  //
  sam_u->fig_u.fit_o = c3y;
  u3z(nun);
}

/* _fine_prot_scry_cb(): receive fine protocol version
*/
static void
_fine_prot_scry_cb(void* vod_p, u3_noun nun)
{
  u3_ames* sam_u = vod_p;
  u3_weak    ver = u3r_at(7, nun);

  if ( u3_none == ver ) {
    //  assume protocol version 0
    //
    sam_u->fin_s.ver_y = 0;
  }
  else if ( (c3n == u3a_is_cat(ver))
         || (7 < ver) ) {
    u3m_p("fine: strange protocol", nun);
    sam_u->fin_s.ver_y = 0;
  }
  else {
    sam_u->fin_s.ver_y = ver;
  }

  u3z(nun);
}

/* _fine_io_talk(): start receiving ames traffic.
*/
static void _fine_io_talk(u3_auto* car_u)
{
  // do nothing
}



/* _ames_io_talk(): start receiving ames traffic.
*/
static void
_ames_io_talk(u3_auto* car_u)
{
  u3_ames* sam_u = (u3_ames*)car_u;
  _ames_io_start(sam_u);

  //  send born event
  //
  {
    //  XX remove [sev_l]
    //
    u3_noun wir = u3nt(c3__newt,
                       u3dc("scot", c3__uv, sam_u->sev_l),
                       u3_nul);
    u3_noun cad = u3nc(c3__born, u3_nul);

    u3_auto_plan(car_u, u3_ovum_init(0, c3__a, wir, cad));
  }
  // send fine born
  {
    u3_noun wir = u3nt(c3__newt,
                       u3dc("scot", c3__uv, sam_u->sev_l),
                       u3_nul);
    u3_noun cad = u3nc(c3__born, u3_nul);

    u3_auto_plan(&sam_u->fin_s.car_u, u3_ovum_init(0, c3__f, wir, cad));

  }


  //  TODO: scry the fine protocol out of arvo
  //
  u3_pier_peek_last(car_u->pir_u, u3_nul, c3__fx, u3_nul,
                    u3nt(u3i_string("protocol"), u3i_string("version"), u3_nul),
                    sam_u, _fine_prot_scry_cb);

  //  scry the protocol version out of arvo
  //
  //    XX this should be re-triggered periodically,
  //    or, better yet, %ames should emit a %turf
  //    (or some other reconfig) effect when it is reset.
  //
  u3_pier_peek_last(car_u->pir_u, u3_nul, c3__ax, u3_nul,
                    u3nt(u3i_string("protocol"), u3i_string("version"), u3_nul),
                    sam_u, _ames_prot_scry_cb);
}

/* _ames_kick_newt(): apply packet network outputs.
*/
static c3_o
_ames_kick_newt(u3_ames* sam_u, u3_noun tag, u3_noun dat)
{
  c3_o ret_o;

  switch ( tag ) {
    default: {
      ret_o = c3n;
    } break;

    case c3__send: {
      u3_noun lan = u3k(u3h(dat));
      u3_noun pac = u3k(u3t(dat));
      _ames_ef_send(sam_u, lan, pac);
      ret_o = c3y;
    } break;

    case c3__turf: {
      _ames_ef_turf(sam_u, u3k(dat));
      ret_o = c3y;
    } break;
  }

  u3z(tag); u3z(dat);
  return ret_o;
}

/* _ames_io_kick(): apply effects
*/
static c3_o
_ames_io_kick(u3_auto* car_u, u3_noun wir, u3_noun cad)
{
  u3_ames* sam_u = (u3_ames*)car_u;

  u3_noun tag, dat, i_wir;
  c3_o ret_o;

  if (  (c3n == u3r_cell(wir, &i_wir, 0))
     || (c3n == u3r_cell(cad, &tag, &dat)) )
  {
    ret_o = c3n;
  }
  else {
    switch ( i_wir ) {
      default: {
        ret_o = c3n;
      } break;

      //  XX should also be c3__ames
      //
      case c3__newt: {
        ret_o = _ames_kick_newt(sam_u, u3k(tag), u3k(dat));
      } break;

      //  XX obsolete
      //
      //    used to also handle %west and %woot for tcp proxy setup
      //
      case c3__ames: {
        ret_o = _( c3__init == tag);
      } break;

      //  this can return through dill due to our fscked up boot sequence
      //
      //    XX s/b obsolete, verify
      //
      case c3__term: {
        if ( c3__send != tag ) {
          ret_o = c3n;
        }
        else {
          u3l_log("kick: strange send\r\n");
          ret_o = _ames_kick_newt(sam_u, u3k(tag), u3k(dat));
        }
      } break;
    }
  }

  u3z(wir); u3z(cad);
  return ret_o;
}

/* _ames_exit_cb(): dispose resources aftr close.
*/
static void
_ames_exit_cb(uv_handle_t* had_u)
{
  u3_ames* sam_u = had_u->data;

  u3_panc* pac_u = sam_u->pac_u;
  while (0 != pac_u) {
    u3_panc* nex_u = pac_u->nex_u;
    _ames_panc_free(pac_u);
    pac_u = nex_u;
  }

  u3h_free(sam_u->lax_p);

  u3s_cue_xeno_done(sam_u->sil_u);
  ur_cue_test_done(sam_u->tes_u);

  c3_free(sam_u);
}

/* _fine_io_exit(): terminate fine I/O.
*/
static void
_fine_io_exit(u3_auto* car_u)
{
  // cleanup handled in _ames_io_exit
}

/* _ames_io_exit(): terminate ames I/O.
*/
static void
_ames_io_exit(u3_auto* car_u)
{
  u3_ames* sam_u = (u3_ames*)car_u;
  uv_close(&sam_u->had_u, _ames_exit_cb);
}

/* _fine_io_info(): print status info.
*/
static void
_fine_io_info(u3_auto* car_u)
{

}


/* _ames_io_info(): print status info.
*/
static void
_ames_io_info(u3_auto* car_u)
{
  u3_ames* sam_u = (u3_ames*)car_u;

# define FLAG(a) ( (c3y == a) ? "&" : "|" )

  u3l_log("      config:\n");
  u3l_log("        filtering: %s\n", FLAG(sam_u->fig_u.fit_o));
  u3l_log("         can send: %s\n", FLAG(sam_u->fig_u.net_o));
  u3l_log("         can scry: %s\n", FLAG(sam_u->fig_u.see_o));
  u3l_log("      counters:\n");
  u3l_log("                 dropped: %" PRIu64 "\n", sam_u->sat_u.dop_d);
  u3l_log("        forwards dropped: %" PRIu64 "\n", sam_u->sat_u.fod_d);
  u3l_log("        forwards pending: %" PRIu64 "\n", sam_u->sat_u.foq_d);
  u3l_log("               forwarded: %" PRIu64 "\n", sam_u->sat_u.fow_d);
  u3l_log("          filtered (hed): %" PRIu64 "\n", sam_u->sat_u.hed_d);
  u3l_log("          filtered (ver): %" PRIu64 "\n", sam_u->sat_u.vet_d);
  u3l_log("          filtered (mug): %" PRIu64 "\n", sam_u->sat_u.mut_d);
  u3l_log("          filtered (bod): %" PRIu64 "\n", sam_u->sat_u.bod_d);
  u3l_log("                 crashed: %" PRIu64 "\n", sam_u->sat_u.fal_d);
  u3l_log("            cached lanes: %u\n", u3h_wyt(sam_u->lax_p));
}

/* u3_ames_io_init(): initialize ames I/O.
*/
u3_auto*
u3_ames_io_init(u3_pier* pir_u, u3_auto** far_u)
{
  u3_ames* sam_u  = c3_calloc(sizeof(*sam_u));
  sam_u->pir_u    = pir_u;
  sam_u->fig_u.net_o = c3y;
  sam_u->fig_u.see_o = c3y;
  sam_u->fig_u.fit_o = c3n;

  // hashtable for scry cache
  // TODO: review size
  // 1500 bytes per packet * 100_000 = 150MB
  // 50 bytes (average) per path * 100_000 = 5MB
  sam_u->fin_s.sac_p = u3h_new_cache(100000);

  //NOTE  some numbers on memory usage for the lane cache
  //
  //    assuming we store:
  //    a (list lane) with 1 item, 1+8 + 1 + (6*2) = 22 words
  //    and a @da as timestamp,                       8 words
  //    consed together,                              6 words
  //    with worst-case (128-bit) @p keys,            8 words
  //    and an additional cell for the k-v pair,      6 words
  //    that makes for a per-entry memory use of     50 words => 200 bytes
  //
  //    the 500k entries below would take about 100mb (in the worst case, but
  //    not accounting for hashtable overhead).
  //    we could afford more, but 500k entries is more than we'll likely use
  //    in the near future.
  //
  sam_u->lax_p = u3h_new_cache(500000);

  c3_assert( !uv_udp_init(u3L, &sam_u->wax_u) );
  sam_u->wax_u.data = sam_u;

  sam_u->sil_u = u3s_cue_xeno_init();
  sam_u->tes_u = ur_cue_test_init();

  //  Disable networking for fake ships
  //
  if ( c3y == sam_u->pir_u->fak_o ) {
    u3_Host.ops_u.net = c3n;
  }

  u3_auto* car_u = &sam_u->car_u;
  car_u->nam_m = c3__ames;
  car_u->liv_o = c3n;
  car_u->io.talk_f = _ames_io_talk;
  car_u->io.info_f = _ames_io_info;
  car_u->io.kick_f = _ames_io_kick;
  car_u->io.exit_f = _ames_io_exit;

  u3_auto* fur_u = &sam_u->fin_s.car_u;
  fur_u->nam_m = c3__fine;
  fur_u->liv_o = c3y;
  fur_u->io.talk_f = _fine_io_talk;
  fur_u->io.info_f = _fine_io_info;
  fur_u->io.kick_f = _fine_io_kick;
  fur_u->io.exit_f = _fine_io_exit;
  *far_u = fur_u;

  sam_u->fin_s.sam_u = sam_u;

  {
    u3_noun now;
    struct timeval tim_u;
    gettimeofday(&tim_u, 0);

    now = u3_time_in_tv(&tim_u);
    sam_u->sev_l = u3r_mug(now);
    u3z(now);
  }

  return car_u;
}
