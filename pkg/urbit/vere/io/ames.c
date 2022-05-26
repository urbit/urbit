/* vere/ames.c
**
*/
#include "all.h"
#include "vere/vere.h"
#include "ur/serial.h"

#define FINE_FRAG 1024   //  bytes per fragment packet
#define FINE_PAGE 16384  //  packets per page

/* u3_fine: fine networking
*/
  typedef struct _u3_fine {
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
    struct _u3_panc* pan_u;             //  forwarding queue, backward
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
      c3_d           pre_d;             //  failed to read prelude
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

/* u3_keen: unsigned fine request body
*/
  typedef struct _u3_keen {
    c3_w    fra_w;                      //  fragment number
    c3_s    len_s;                      //  path length
    c3_c*   pat_c;                      //  path as ascii
  } u3_keen;

/*  u3_wail: signed fine request body
*/
  typedef struct _u3_wail {
    c3_y    sig_y[64];                  //  signature
    u3_keen ken_u;                      //  request payload
  } u3_wail;

/* u3_meow: response portion of purr packet
*/
  typedef struct _u3_meow {
    c3_y    sig_y[64];                  //  host signature
    c3_w    num_w;                      //  number of fragments
    c3_s    siz_s;                      //  datum size
    c3_y*   dat_y;                      //  datum (0 if null response)
  } u3_meow;

/* u3_purr: fine packet response
*/
  typedef struct _u3_purr {
    u3_keen ken_u;
    u3_meow mew_u;
  } u3_purr;

/* u3_body: ames packet body
*/
  typedef struct _u3_body {
    c3_s    con_s;                      //  content size
    c3_y*   con_y;                      //  content
    c3_l    mug_l;                      //  checksum
  } u3_body;

#define PACT_AMES 0  //  ames packet
#define PACT_WAIL 1  //  fine request packet
#define PACT_PURR 2  //  fine response packet

/* u3_pact: ames packet
 *
 *   Filled in piece by piece as we parse or construct it.
*/
  typedef struct _u3_pact {
    uv_udp_send_t    snd_u;             //  udp send request
    struct _u3_ames* sam_u;             //  ames backpointer
    c3_w             len_w;             //  length in bytes
    c3_y*            hun_y;             //  packet buffer
    u3_head          hed_u;             //  head of packet
    u3_prel          pre_u;             //  packet prelude
    c3_y             typ_y;             //  pointer type tag
    struct {
      u3_lane        lan_u;             //  destination/origin lane
      c3_y           imp_y;             //  galaxy (optional)
      c3_c*          dns_c;             //  galaxy fqdn (optional)
    } rut_u;
    union {
      u3_body bod_u;                    //  tagged by PACT_AMES
      u3_wail wal_u;                    //  tagged by PACT_WAIL
      u3_purr pur_u;                    //  tagged by PACT_PURR
    };
  } u3_pact;

/* u3_panc: packet queue
*/
  typedef struct _u3_panc {
    struct _u3_panc* pre_u;             //  previous packet
    struct _u3_panc* nex_u;             //  next packet
    u3_pact          pac_u;             //  this packet
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

static void
_ames_pact_free(u3_pact* pac_u)
{
  switch ( pac_u->typ_y ) {
    case PACT_AMES:
      break;

    case PACT_WAIL:
      c3_free(pac_u->wal_u.ken_u.pat_c);
      break;

    case PACT_PURR:
      c3_free(pac_u->pur_u.ken_u.pat_c);
      c3_free(pac_u->pur_u.mew_u.dat_y);
      break;

    default:
      u3l_log("ames_pact_free: bad packet type %d\n", pac_u->typ_y);
      u3_pier_bail(u3_king_stub());
  }

  c3_free(pac_u->rut_u.dns_c);
  c3_free(pac_u->hun_y);
  c3_free(pac_u);
}

/* _ames_panc_free(): remove references, lose refcounts and free struct
*/
static void
_ames_panc_free(u3_panc* pan_u)
{
  if ( 0 != pan_u->nex_u ) {
    pan_u->nex_u->pre_u = pan_u->pre_u;
  }

  if ( 0 != pan_u->pre_u ) {
    pan_u->pre_u->nex_u = pan_u->nex_u;
  }
  else {
    c3_assert(pan_u == pan_u->pac_u.sam_u->pan_u);
    pan_u->pac_u.sam_u->pan_u = pan_u->nex_u;
  }

  _ames_pact_free(&pan_u->pac_u);
  c3_free(pan_u);
}

static c3_y
_ames_prel_size(u3_head* hed_u)
{
  c3_y hed_y = 4;
  c3_y lif_y = 1;
  c3_y sen_y = 2 << hed_u->sac_y;
  c3_y rec_y = 2 << hed_u->rac_y;
  c3_y rog_y = ( c3y == hed_u->rel_o )? 6 : 0;
  return hed_y + lif_y + sen_y + rec_y + rog_y;
}

static c3_s
_ames_body_size(u3_body* bod_u)
{
  return bod_u->con_s;
}

static c3_s
_fine_keen_size(u3_keen* ken_u)
{
  return 4 + 2 + ken_u->len_s;
}

static c3_s
_fine_meow_size(u3_meow* mew_u)
{
  return 64 + 4 + 2 + mew_u->siz_s;
}

static c3_s
_fine_purr_size(u3_purr* pur_u)
{
  c3_s pur_s = _fine_keen_size(&pur_u->ken_u);
  c3_s mew_s = _fine_meow_size(&pur_u->mew_u);
  return pur_s + mew_s;
}

static c3_s
_ames_sift_short(c3_y buf_y[2])
{
  return ((buf_y[0] << 0x0) | (buf_y[1] << 0x8));
}

static c3_w
_ames_sift_word(c3_y buf_y[4])
{
  return (
      (buf_y[0] << 0x0)
    | (buf_y[1] << 0x8)
    | (buf_y[2] << 0x10)
    | (buf_y[3] << 0x18));
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

/* _ames_sift_head(): parse packet header.
*/
static void
_ames_sift_head(u3_head* hed_u, c3_y buf_y[4])
{
  c3_w hed_w = _ames_sift_word(buf_y);

  //  first three bits are reserved
  //
  hed_u->req_o = (hed_w >>  2) & 0x1;
  hed_u->sim_o = (hed_w >>  3) & 0x1;
  hed_u->ver_y = (hed_w >>  4) & 0x7;
  hed_u->sac_y = (hed_w >>  7) & 0x3;
  hed_u->rac_y = (hed_w >>  9) & 0x3;
  hed_u->mug_l = (hed_w >> 11) & 0xfffff; // 20 bits
  hed_u->rel_o = (hed_w >> 31) & 0x1;
}

/* _ames_sift_prel(): parse prelude,
*/
static void
_ames_sift_prel(u3_head* hed_u,
                u3_prel* pre_u,
                c3_y*    buf_y)
{
  c3_y sen_y, rec_y;
  c3_w cur_w = 0;

  pre_u->sic_y = buf_y[0] & 0xf;
  pre_u->ric_y = buf_y[0] & 0xf0;
  cur_w++;

  sen_y = 2 << hed_u->sac_y;
  _ames_ship_to_chubs(pre_u->sen_d, sen_y, buf_y + cur_w);
  cur_w += sen_y;

  rec_y = 2 << hed_u->rac_y;
  _ames_ship_to_chubs(pre_u->rec_d, rec_y, buf_y + cur_w);
  cur_w += rec_y;

  if ( c3y == hed_u->rel_o ) {
    c3_y rag_y[8] = {0};
    memcpy(rag_y, buf_y + cur_w, 6);
    pre_u->rog_d = _ames_chub_bytes(rag_y);
  }
  else {
    pre_u->rog_d = 0;
  }
}

static c3_o
_fine_sift_keen(u3_keen* ken_u, c3_w len_y, c3_y* buf_y)
{
  ken_u->fra_w = _ames_sift_word(buf_y);
  ken_u->len_s = _ames_sift_short(buf_y + 4);
  if ( ken_u->len_s > 384 ) {
    return c3n;
  }

  ken_u->pat_c = c3_calloc(ken_u->len_s + 1);
  memcpy(ken_u->pat_c, buf_y + 6, ken_u->len_s);
  return c3y;
}


/* _fine_sift_wail(): parse request body, returning success
*/
static c3_o
_fine_sift_wail(u3_pact* pac_u, c3_w cur_w)
{
  c3_w tot_w;
  c3_s len_s;

  if ( (cur_w + 64 + 4 + 2) > pac_u->len_w ) {
    u3l_log("fine: wail not big enough\n");
    return c3n;
  }
  memcpy(pac_u->wal_u.sig_y, pac_u->hun_y + cur_w, 64);
  cur_w += 64;

  pac_u->wal_u.ken_u.fra_w = _ames_sift_word(pac_u->hun_y + cur_w);
  cur_w += 4;

  len_s = _ames_sift_short(pac_u->hun_y + cur_w);
  pac_u->wal_u.ken_u.len_s = len_s;
  cur_w += 2;

  if ( len_s > 384 ) {
    u3l_log("ames wail len: %u, max %u\n", len_s, 384);
    return c3n;
  }

  tot_w = cur_w + len_s;
  if ( tot_w != pac_u->len_w ) {
    u3l_log("fine: wail expected total len: %u, actual %u\n",
            tot_w, pac_u->len_w);
    return c3n;
  }

  pac_u->wal_u.ken_u.pat_c = c3_calloc(len_s + 1);
  memcpy(pac_u->wal_u.ken_u.pat_c, pac_u->hun_y + cur_w, len_s);
  pac_u->wal_u.ken_u.pat_c[len_s] = '\0';
  return c3y;
}

/* _fine_sift_meow(): 
*/
static c3_o
_fine_sift_meow(u3_meow* mew_u, u3_noun mew)
{
  c3_o ret_o;
  c3_w len_w = u3r_met(3, mew);

  if ( (len_w < 68) || (len_w > FINE_FRAG + 68) )
  {
    ret_o = c3n;
  }
  else {
    c3_w cur_w = 0;

    u3r_bytes(cur_w, 64, mew_u->sig_y, mew);
    cur_w += 64;

    u3r_bytes(cur_w, 4, (c3_y*)&mew_u->num_w, mew);
    cur_w += 4;

    u3r_bytes(cur_w, 2, (c3_y*)&mew_u->siz_s, mew);
    cur_w += 2;

    mew_u->dat_y = c3_calloc(mew_u->siz_s);
    u3r_bytes(cur_w, mew_u->siz_s, mew_u->dat_y, mew);

    ret_o = c3y;
  }

  u3z(mew);
  return ret_o;
}

/* _ames_sift_body(): parse packet body.
*/
static c3_o
_ames_sift_body(u3_head* hed_u,
                u3_body* bod_u,
                c3_w     len_w,
                c3_y*    bod_y)
{
  bod_u->mug_l = u3r_mug_bytes(bod_y, len_w) & 0xfffff;
  bod_u->con_y = c3_calloc(len_w);
  memcpy(bod_u->con_y, bod_y, len_w);
  return ( bod_u->mug_l == hed_u->mug_l ) ? c3y : c3n;
}

static void
_ames_etch_short(c3_y buf_y[2], c3_s sot_s)
{
  buf_y[0] = sot_s & 0xff;
  buf_y[1] = (sot_s >>  8) & 0xff;
}

static void
_ames_etch_word(c3_y buf_y[4], c3_w wod_w)
{
  buf_y[0] = wod_w & 0xff;
  buf_y[1] = (wod_w >>  8) & 0xff;
  buf_y[2] = (wod_w >> 16) & 0xff;
  buf_y[3] = (wod_w >> 24) & 0xff;
}

/* _ames_etch_head(): serialize packet header.
*/
static void
_ames_etch_head(u3_head* hed_u, c3_y buf_y[4])
{
  //  only version 0 currently recognized
  //
  c3_assert( 0 == hed_u->ver_y );  //  XX remove after testing

  c3_w hed_w = ((hed_u->req_o &     0x1) <<  2)
             ^ ((hed_u->sim_o &     0x1) <<  3)
             ^ ((hed_u->ver_y &     0x7) <<  4)
             ^ ((hed_u->sac_y &     0x3) <<  7)
             ^ ((hed_u->rac_y &     0x3) <<  9)
             ^ ((hed_u->mug_l & 0xfffff) << 11)
             ^ ((hed_u->rel_o &     0x1) << 31);

  _ames_etch_word(buf_y, hed_w);
}

/* _ames_etch_prel(): serialize packet prelude
*/
static void
_ames_etch_prel(u3_head* hed_u, u3_prel* pre_u, c3_y* buf_y)
{
  c3_w cur_w = 0;

  buf_y[0] = (pre_u->sic_y & 0xf) ^ ((pre_u->ric_y & 0xf) << 4);
  cur_w++;

  c3_y sen_y = 2 << hed_u->sac_y;
  _ames_ship_of_chubs(pre_u->sen_d, sen_y, buf_y + cur_w);
  cur_w += sen_y;

  c3_y rec_y = 2 << hed_u->rac_y;
  _ames_ship_of_chubs(pre_u->rec_d, rec_y, buf_y + cur_w);
  cur_w += rec_y;

  c3_y  rog_y = ( c3y == hed_u->rel_o ) ? 6 : 0;           //  origin len
  if ( rog_y ) {
    c3_y rag_y[8] = {0};
    _ames_bytes_chub(rag_y, pre_u->rog_d);
    memcpy(buf_y + cur_w, rag_y, rog_y);
  }
}

static void
_fine_etch_keen(u3_keen* ken_u, c3_y* buf_y)
{
  c3_w cur_w = 0;

  _ames_etch_word(buf_y + cur_w, ken_u->fra_w);
  cur_w += 4;

  _ames_etch_short(buf_y + cur_w, ken_u->len_s);
  cur_w += 2;

  memcpy(buf_y + cur_w, ken_u->pat_c, ken_u->len_s);
}

static void
_fine_etch_meow(u3_meow* mew_u, c3_y* buf_y)
{
  c3_w cur_w = 0;

  memcpy(buf_y + cur_w, mew_u->sig_y, 64);
  cur_w += 64;

  _ames_etch_word(buf_y + cur_w, mew_u->num_w);
  cur_w += 4;

  _ames_etch_short(buf_y + cur_w, mew_u->siz_s);
  cur_w += 2;

  memcpy(buf_y + cur_w, mew_u->dat_y, mew_u->siz_s);
}

/* _fine_etch_purr(): serialise response packet
 */
static void
_fine_etch_purr(u3_purr* pur_u, c3_y* buf_y)
{
  c3_w cur_w = 0;

  _fine_etch_keen(&pur_u->ken_u, buf_y + cur_w);
  cur_w += _fine_keen_size(&pur_u->ken_u);

  _fine_etch_meow(&pur_u->mew_u, buf_y + cur_w);
}

/* _fine_etch_response(): serialize scry response packet
*/
static void
_fine_etch_response(u3_pact* pac_u)
{
  c3_w pre_w, pur_w, cur_w;

  pre_w = _ames_prel_size(&pac_u->hed_u);
  pur_w = _fine_purr_size(&pac_u->pur_u);
  pac_u->len_w = 4 + pre_w + pur_w;
  pac_u->hun_y = c3_calloc(pac_u->len_w);
  cur_w = 0;

  _ames_etch_head(&pac_u->hed_u, pac_u->hun_y + cur_w);
  cur_w += 4;

  _ames_etch_prel(&pac_u->hed_u, &pac_u->pre_u, pac_u->hun_y + cur_w);
  cur_w += pre_w;

  _fine_etch_purr(&pac_u->pur_u, pac_u->hun_y + cur_w);

// TODO: fill in mug in header
}

/* _lane_scry_path(): format scry path for retrieving a lane
*/
static inline u3_noun _lane_scry_path(u3_noun who)
{
  return u3nq(u3i_string("peers"),
              u3dc("scot", 'p', who),
              u3i_string("forward-lane"),
              u3_nul);
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

static void _log_keen(u3_keen* req_u)
{
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

/* _ames_send_cb(): send callback.
*/
static void
_ames_send_cb(uv_udp_send_t* req_u, c3_i sas_i)
{
  u3_pact* pac_u = (u3_pact*)req_u;
  u3_ames* sam_u = pac_u->sam_u;

  if (sas_i) {
    u3l_log("ames: send fail: %s\n", uv_strerror(sas_i));
    sam_u->fig_u.net_o = c3n;
  }
  else {
    sam_u->fig_u.net_o = c3y;
  }

  _ames_pact_free(pac_u);
}

#define _fine_send _ames_send
/* _ames_send(): send buffer to address on port.
*/
static void
_ames_send(u3_pact* pac_u)
{
  u3_ames* sam_u = pac_u->sam_u;

  if ( !pac_u->hun_y ) {
    _ames_pact_free(pac_u);
    return;
  }
  else {
    struct sockaddr_in add_u;

    memset(&add_u, 0, sizeof(add_u));
    add_u.sin_family = AF_INET;
    add_u.sin_addr.s_addr = htonl(pac_u->rut_u.lan_u.pip_w);
    add_u.sin_port = htons(pac_u->rut_u.lan_u.por_s);

    {
      uv_buf_t buf_u = uv_buf_init((c3_c*)pac_u->hun_y, pac_u->len_w);

      c3_i     sas_i = uv_udp_send(&pac_u->snd_u,
                                   &sam_u->wax_u,
                                   &buf_u, 1,
                                   (const struct sockaddr*)&add_u,
                                   _ames_send_cb);

      if ( sas_i ) {
        if ( c3y == sam_u->fig_u.net_o ) {
          u3l_log("ames: send fail: %s\n", uv_strerror(sas_i));
          sam_u->fig_u.net_o = c3n;
        }

        _ames_pact_free(pac_u);
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

/* _ames_serialize_packet(): u3_pac to atom, updating the origin lane if dop_o
**                           (retains pac_u)
*/
static u3_noun
_ames_serialize_packet(u3_pact* pac_u, c3_o dop_o)
{
  c3_w len_w;
  c3_y* buf_y;
  //  update the packet's "origin" lane, if:
  //    - we're supposed to (dop_o)
  //    - it hasn't already been updated (rel_o)
  //    - sender is not a galaxy
  //
  if (  c3y == dop_o
     && c3n == pac_u->hed_u.rel_o
     && !( ( 256 > pac_u->pre_u.sen_d[0] )
        && ( 0  == pac_u->pre_u.sen_d[1] ) ) )
  {
    pac_u->hed_u.rel_o = c3y;
    pac_u->pre_u.rog_d = u3_ames_lane_to_chub(pac_u->rut_u.lan_u);
  }

  //  find length of packet with added origin
  c3_w pre_w = _ames_prel_size(&pac_u->hed_u);
  c3_w bod_w = _ames_body_size(&pac_u->bod_u);
  len_w = 4 + pre_w + bod_w;

  //  allocate new packet buffer
  //
  buf_y = c3_calloc(len_w);

  //  fill in new packet
  {
    c3_w cur_w = 0;

    _ames_etch_head(&pac_u->hed_u, buf_y + cur_w);
    cur_w += 4;

    _ames_etch_prel(&pac_u->hed_u, &pac_u->pre_u, buf_y + cur_w);
    cur_w += pre_w;

    memcpy(buf_y + cur_w, pac_u->bod_u.con_y, pac_u->bod_u.con_s);
  }

  //  convert to noun
  {
    u3_noun pac = u3i_bytes(len_w, buf_y);
    c3_free(buf_y);
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
_ames_czar_gone(u3_pact* pac_u, time_t now)
{
  u3_ames* sam_u = pac_u->sam_u;
  c3_d imp_y = pac_u->rut_u.imp_y;

  if ( c3y == sam_u->imp_o[imp_y] ) {
    u3l_log("ames: czar at %s: not found (b)\n", pac_u->rut_u.dns_c);
    sam_u->imp_o[imp_y] = c3n;
  }

  if ( (0 == sam_u->imp_w[imp_y]) ||
       (0xffffffff == sam_u->imp_w[imp_y]) )
  {
    sam_u->imp_w[imp_y] = 0xffffffff;
  }

  //  keep existing ip for 5 more minutes
  //
  sam_u->imp_t[imp_y] = now;

  _ames_pact_free(pac_u);
}

/* _ames_czar_here(): galaxy address resolution succeeded.
*/
static void
_ames_czar_here(u3_pact* pac_u, time_t now, struct sockaddr_in* add_u)
{
  u3_ames* sam_u = pac_u->sam_u;
  c3_y     imp_y = pac_u->rut_u.imp_y;
  c3_w     old_w = sam_u->imp_w[imp_y];
  c3_w     pip_w = ntohl(add_u->sin_addr.s_addr);

  if ( pip_w != old_w ) {
    u3_noun nam = u3dc("scot", c3__if, u3i_word(pip_w));
    c3_c* nam_c = u3r_string(nam);

    u3l_log("ames: czar %s: ip %s\n", pac_u->rut_u.dns_c, nam_c);

    c3_free(nam_c);
    u3z(nam);
  }

  sam_u->imp_w[imp_y] = pip_w;
  sam_u->imp_t[imp_y] = now;
  sam_u->imp_o[imp_y] = c3y;

  pac_u->rut_u.lan_u.pip_w = pip_w;
}

/* _ames_czar_cb(): galaxy address resolution callback.
*/
static void
_ames_czar_cb(uv_getaddrinfo_t* adr_u,
              c3_i              sas_i,
              struct addrinfo*  aif_u)
{
  {
    u3_pact*         pac_u = (u3_pact*)adr_u->data;
    struct addrinfo* rai_u = aif_u;
    time_t             now = time(0);

    while ( rai_u ) {
      if ( (AF_INET == rai_u->ai_family) ) {
        _ames_czar_here(pac_u, now, (struct sockaddr_in *)rai_u->ai_addr);
        break;
      }
      else {
        rai_u = rai_u->ai_next;
      }
    }

    if ( !rai_u ) {
      _ames_czar_gone(pac_u, now);
    }
  }

  c3_free(adr_u);
  uv_freeaddrinfo(aif_u);
}

/* _ames_czar(): galaxy address resolution.
*/
static void
_ames_czar(u3_pact* pac_u)
{
  u3_ames* sam_u = pac_u->sam_u;

  c3_y imp_y = pac_u->rut_u.imp_y;

  pac_u->rut_u.lan_u.por_s = _ames_czar_port(imp_y);

  if ( c3n == u3_Host.ops_u.net ) {
    pac_u->rut_u.lan_u.pip_w = 0x7f000001;
    _ames_send(pac_u);
    return;
  }

  //  if we don't have a galaxy domain, no-op
  //
  if ( !sam_u->dns_c ) {
    u3_noun nam = u3dc("scot", 'p', pac_u->rut_u.imp_y);
    c3_c*  nam_c = u3r_string(nam);
    u3l_log("ames: no galaxy domain for %s, no-op\r\n", nam_c);

    c3_free(nam_c);
    u3z(nam);
    return;
  }

  {
    c3_w pip_w = sam_u->imp_w[imp_y];
    time_t wen = sam_u->imp_t[imp_y];
    time_t now = time(0);

    //  backoff for 5 minutes after failed lookup
    //
    if (  ( now < wen )               //  time shenanigans!
       || (  (0xffffffff == pip_w)    //  sentinal ip address
          && ((now - wen) < 300) ) )
    {
      _ames_pact_free(pac_u);
      return;
    }
    //  cached addresses have a 5 minute TTL
    //
    else if ( (0 != pip_w) && ((now - wen) < 300) ) {
      pac_u->rut_u.lan_u.pip_w = pip_w;
      _ames_send(pac_u);
      return;
    }
    else {
      c3_i sas_i;

      {
        u3_noun nam = u3dc("scot", 'p', imp_y);
        c3_c* nam_c = u3r_string(nam);

        //  NB: . separator not counted, as [nam_c] includes a ~ that we skip
        //
        pac_u->rut_u.dns_c =
          c3_malloc(1 + strlen(nam_c) + strlen(sam_u->dns_c));

        sas_i = 
          snprintf(pac_u->rut_u.dns_c, 255, "%s.%s", nam_c + 1, sam_u->dns_c);

        c3_free(nam_c);
        u3z(nam);
      }

      if ( 255 <= sas_i ) {
        u3l_log("ames: czar: galaxy domain %s truncated\n", sam_u->dns_c);
        _ames_pact_free(pac_u);
        return;
      }

      {
        uv_getaddrinfo_t* adr_u = c3_malloc(sizeof(*adr_u));
        adr_u->data = pac_u;

        if ( 0 != (sas_i = uv_getaddrinfo(u3L, adr_u,
                                          _ames_czar_cb,
                                          pac_u->rut_u.dns_c, 0, 0)) )
        {
          u3l_log("ames: %s\n", uv_strerror(sas_i));
          _ames_czar_gone(pac_u, now);
          return;
        }
      }
    }
  }
}

/* _fine_put_cache(): put list of packets into cache
 */
static void
_fine_put_cache(u3_ames* sam_u, u3_noun pax, c3_w lop_w, u3_noun lis)
{
  c3_w cur_w = lop_w;
  while ( lis != u3_nul ) {
    u3_noun key = u3nc(u3k(pax), u3i_word(cur_w));
    u3h_put(sam_u->fin_s.sac_p, key, u3k(u3h(lis)));

    lis = u3t(lis);
    cur_w++;
    u3z(key);
  }
}

/* _ames_ef_send(): send packet to network (v4).
 * TODO: clean up
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

  u3r_bytes(0, pac_u->len_w, pac_u->hun_y, pac);

  u3_head hed_u;
  _ames_sift_head(&hed_u, pac_u->hun_y);
  pac_u->typ_y =
    hed_u.sim_o == c3y ? PACT_AMES :
    hed_u.req_o == c3y ? PACT_WAIL : PACT_PURR;

  u3_noun tag, val;
  u3x_cell(lan, &tag, &val);
  c3_assert( (c3y == tag) || (c3n == tag) );

  //  galaxy lane; do DNS lookup and send packet
  //
  if ( c3y == tag ) {
    c3_assert( c3y == u3a_is_cat(val) );
    c3_assert( val < 256 );

    pac_u->rut_u.imp_y = val;
    _ames_czar(pac_u);
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
      pac_u->rut_u.lan_u = lan_u;
      _ames_send(pac_u);
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

/*  _ames_forward(): forward pan_u onto the (list lane) las, then free pan_u
*/
static void
_ames_forward(u3_panc* pan_u, u3_noun las)
{
  u3_pact* pac_u = &pan_u->pac_u;
  u3_ames* sam_u = pac_u->sam_u;

  sam_u->sat_u.fow_d++;
  if ( 0 == (sam_u->sat_u.fow_d % 1000000) ) {
    u3l_log("ames: forwarded %" PRIu64 " total\n", sam_u->sat_u.fow_d);
  }

  if ( u3C.wag_w & u3o_verbose ) {
    u3_noun sen = u3dc("scot", 'p', u3i_chubs(2, pac_u->pre_u.sen_d));
    u3_noun rec = u3dc("scot", 'p', u3i_chubs(2, pac_u->pre_u.rec_d));
    c3_c* sen_c = u3r_string(sen);
    c3_c* rec_c = u3r_string(rec);
    c3_y* pip_y = (c3_y*)&pac_u->rut_u.lan_u.pip_w;

    u3l_log("ames: forwarding for %s to %s from %d.%d.%d.%d:%d\n",
            sen_c, rec_c,
            pip_y[0], pip_y[1], pip_y[2], pip_y[3],
            pac_u->rut_u.lan_u.por_s);

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

  _ames_panc_free(pan_u);
  u3z(las);
}

/*  _ames_lane_scry_cb(): learn lane to forward packet on
*/
static void
_ames_lane_scry_cb(void* vod_p, u3_noun nun)
{
  u3_panc* pan_u = vod_p;
  u3_pact* pac_u = &pan_u->pac_u;
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
    _ames_put_packet(sam_u,
                     _ames_serialize_packet(&pan_u->pac_u, c3n),
                     pac_u->rut_u.lan_u);
    _ames_panc_free(pan_u);
  }
  else {
    sam_u->sat_u.saw_d = 0;

    //  cache the scry result for later use
    //
    _ames_lane_into_cache(sam_u->lax_p,
                          u3i_chubs(2, pac_u->pre_u.rec_d),
                          u3k(las));

    //  if there is no lane, drop the packet
    //
    if ( u3_nul == las ) {
      _ames_panc_free(pan_u);
    }
    //  if there is a lane, forward the packet on it
    //
    else {
      _ames_forward(pan_u, u3k(las));
    }
  }

  u3z(nun);
}

/* _ames_try_forward(): try to forward a packet for another ship.
*/
static void
_ames_try_forward(u3_panc* pan_u)
{
  u3_weak lac;
  u3_pact* pac_u = &pan_u->pac_u;
  u3_body* bod_u = &pac_u->bod_u;
  u3_ames* sam_u = pac_u->sam_u;

  //  if the recipient is a galaxy, their lane is always &+~gax
  //
  if (  (256 > pac_u->pre_u.rec_d[0])
     && (0  == pac_u->pre_u.rec_d[1]) )
  {
    lac = u3nc(c3y, (c3_y)pac_u->pre_u.rec_d[0]);
  }
  //  otherwise, try to get the lane from cache
  //
  else {
    u3_noun key = u3i_chubs(2, pac_u->pre_u.rec_d);
    lac = _ames_lane_from_cache(sam_u->lax_p, key);

    //  if we don't know the lane, and the lane scry queue is full,
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

      _ames_panc_free(pan_u);
      return;
    }
    //  if we know there's no lane, drop the packet
    //
    else if ( u3_nul == lac ) {
      _ames_panc_free(pan_u);
      return;
    }
  }

  //  proceed with forwarding
  //
  {
    //  store the packet details for later processing
    //
    if ( 0 != sam_u->pan_u ) {
      pan_u->nex_u = sam_u->pan_u;
      sam_u->pan_u->pre_u = pan_u;
    }
    sam_u->pan_u = pan_u;

    //  if we already know the lane, just forward
    //
    if ( u3_none != lac ) {
      _ames_forward(pan_u, lac);
    }
    //  otherwise, there's space in the scry queue; scry the lane out of ames
    //
    else {
      sam_u->sat_u.foq_d++;
      u3_noun pax = _lane_scry_path(u3i_chubs(2, pac_u->pre_u.rec_d));

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

/* _fine_lop(): find beginning of page containing fra_w
*/
static c3_w
_fine_lop(c3_w fra_w)
{
  return fra_w - (fra_w % FINE_PAGE) + 1;
}

/* _fine_pack_scry_cb(): receive packets for datum out of fine
 */
static void _fine_pack_scry_cb(void* vod_p, u3_noun nun)
{
  u3_pact* pac_u = vod_p;
  c3_assert( PACT_PURR == pac_u->typ_y );
  u3_ames* sam_u = pac_u->sam_u;

  u3_weak pas = u3r_at(7, nun);
  if(pas == u3_none) {
    _ames_pact_free(pac_u);

    u3z(pas);
    u3z(nun);
    return;
  }

  u3_noun pax = u3do("stab", u3i_string(pac_u->wal_u.ken_u.pat_c));
  c3_w lop_w = _fine_lop(pac_u->pur_u.ken_u.fra_w);
  _fine_put_cache(sam_u, pax, lop_w, pas);

  // find requested fragment
  u3_weak fra = u3_none;
  c3_w fra_w = lop_w;
  while ( pas != u3_nul ) {
    if ( pac_u->wal_u.ken_u.fra_w == fra_w ) {
      fra = u3k(u3h(pas));
      break;
    }
    fra_w++;
    pas = u3t(pas);
  }

  if ( fra == u3_none ) {
    u3l_log("fine: fragment number out of range\n");
    _ames_pact_free(pac_u);
  }
  else if ( c3y == _fine_sift_meow(&pac_u->pur_u.mew_u, u3k(fra)) ) {
    _fine_etch_response(pac_u);
    _fine_send(pac_u);
  }

  u3z(pas);
  u3z(nun);
  u3z(fra);
}

//  TODO: check protocol version
static void _fine_hear_response(u3_pact* pac_u, c3_w cur_w)
{
  u3_noun wir = u3nc(c3__fine, u3_nul);
  u3_noun cad = u3nt(c3__hear,
                     u3nc(c3n, u3_ames_encode_lane(pac_u->rut_u.lan_u)),
                     u3i_bytes(pac_u->len_w, pac_u->hun_y));

  u3_ovum* ovo_u = u3_ovum_init(0, c3__ames, u3k(wir), u3k(cad));
  u3_auto_plan(&pac_u->sam_u->car_u, ovo_u);

  u3z(cad);
  u3z(wir);
}

//  TODO: check protocol version
static void _fine_hear_request(u3_pact* req_u, c3_w cur_w)
{
  u3_pact* res_u;
  
  if ( c3n == _fine_sift_wail(req_u, cur_w) ) {
    _ames_pact_free(req_u);
    return;
  }

  u3_noun pux = u3i_string(req_u->wal_u.ken_u.pat_c);
  u3_noun pat = u3dc("rush", pux, u3v_wish("stap"));
  if ( u3_nul == pat ) {
    u3l_log("fine: bad request\n");
    _ames_pact_free(req_u);
    u3z(pat);
    return;
  }

  //  fill in the parts of res_u that we know from req_u
  {
    res_u = c3_calloc(sizeof(*res_u));
    res_u->sam_u = req_u->sam_u;
    res_u->typ_y = PACT_PURR;

    res_u->hed_u = (u3_head) {
      .req_o = c3n,
      .sim_o = c3n,
      .ver_y = req_u->hed_u.ver_y,
      .sac_y = req_u->hed_u.rac_y,
      .rac_y = req_u->hed_u.sac_y,
      .mug_l = 0,  //  filled in later
      .rel_o = c3n
    };

    res_u->pre_u = (u3_prel) {
      .sic_y = req_u->pre_u.ric_y,
      .ric_y = req_u->pre_u.sic_y,
      .sen_d = { req_u->pre_u.rec_d[0], req_u->pre_u.rec_d[1] },
      .rec_d = { req_u->pre_u.sen_d[0], req_u->pre_u.sen_d[1] },
      .rog_d = 0
    };

    res_u->pur_u = (u3_purr) {
      .ken_u = req_u->wal_u.ken_u,
      .mew_u = {0}  //  filled in later
    };

    _ames_pact_free(req_u);
  }

  //  if receiver is a galaxy, note that in res_u
  //
  if ( res_u->pre_u.rec_d[0] < 256
      && res_u->pre_u.rec_d[1] == 0 )
  {
     res_u->rut_u.imp_y = res_u->pre_u.rec_d[0];
  }

  //  look up request in scry cache
  //
  u3_noun key = u3nc(u3k(pat), u3i_word(res_u->pur_u.ken_u.fra_w));
  u3_weak cac = u3h_git(res_u->sam_u->fin_s.sac_p, key);
  if ( u3_none == cac ) {
    //  cache miss, scry into arvo for a page of packets
    //
    c3_w lop_w = _fine_lop(req_u->pur_u.ken_u.fra_w);
    u3_noun pax =
      u3nc(c3__fine,
      u3nq(c3__hunk, lop_w, FINE_PAGE, u3k(u3t(pat))));

    u3_pier_peek_last(res_u->sam_u->car_u.pir_u, u3_nul, c3__ax, u3_nul,
                      pax, res_u, _fine_pack_scry_cb);

  }
  //  cache hit, fill in response meow and send
  //
  else if ( c3y == _fine_sift_meow(&res_u->pur_u.mew_u, u3k(cac)) ) {
    _fine_etch_response(res_u);
    _fine_send(res_u);
  }

  u3z(key);
  u3z(pat);
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
_ames_hear_ames(u3_pact* pac_u, c3_w cur_w)
{
  //  ensure the protocol version matches ours
  //
  //    XX rethink use of [fit_o] here and elsewhere
  //
  u3_ames* sam_u = pac_u->sam_u;
  if (  (c3y == sam_u->fig_u.fit_o)
     && (sam_u->ver_y != pac_u->hed_u.ver_y) )
  {
    sam_u->sat_u.vet_d++;
    if ( 0 == (sam_u->sat_u.vet_d % 100000) ) {
      u3l_log("ames: %" PRIu64 " dropped for version mismatch\n",
              sam_u->sat_u.vet_d);
    }

    _ames_pact_free(pac_u);
    return;
  }

  //  otherwise, inject the packet as an event
  //
  else {
    u3_noun msg = u3i_bytes(pac_u->len_w, pac_u->hun_y);
#ifdef AMES_SKIP
    if (_ames_skip(&bod_u) == c3y ) {
      u3z(msg);
    }
    else {
#endif
      _ames_put_packet(sam_u, msg, pac_u->rut_u.lan_u);
#ifdef AMES_SKIP
    }
#endif
    _ames_pact_free(pac_u);
  }
}

/* _ames_hear(): parse a (potential) packet, dispatch appropriately.

  packet filtering needs to revised for two protocol-change scenarios

    - packets using old protocol versions from our sponsees
      these must be let through, and this is a transitive condition;
      they must also be forwarded where appropriate
      they can be validated, as we know their semantics

    - packets using newer protocol versions
      these should probably be let through, or at least
      trigger printfs suggesting upgrade.
      they cannot be filtered, as we do not know their semantics
*/
static void
_ames_hear(u3_ames* sam_u,
           u3_lane* lan_u,
           c3_w     len_w,
           c3_y*    hun_y)
{
  u3_panc* pan_u;
  u3_pact* pac_u;
  u3_body bod_u;
  c3_w pre_w;
  c3_w cur_w = 0;  //  cursor: how many bytes we've read from hun_y

  //  make sure packet is big enough to have a header
  if (4 > len_w) {
    sam_u->sat_u.hed_d++;
    if ( 0 == (sam_u->sat_u.hed_d % 100000) ) {
      u3l_log("ames: %" PRIu64 " dropped, failed to read header\n",
              sam_u->sat_u.hed_d);
    }

    c3_free(hun_y);
    return;
  }

  pan_u = c3_calloc(sizeof(*pan_u));
  pac_u = &pan_u->pac_u;
  pac_u->len_w = len_w;
  pac_u->hun_y = hun_y;
  pac_u->rut_u.lan_u = *lan_u;
  cur_w = 0;

  //  parse the header
  //
  _ames_sift_head(&pac_u->hed_u, hun_y);
  cur_w += 4;

  pac_u->typ_y = ( c3y == pac_u->hed_u.sim_o ) ? PACT_AMES :
                 ( c3y == pac_u->hed_u.req_o ) ? PACT_WAIL : PACT_PURR;

  //  check that packet is big enough for prelude
  //
  pre_w = _ames_prel_size(&pac_u->hed_u);
  if ( len_w < cur_w + pre_w ) {
    sam_u->sat_u.pre_d++;
    u3l_log("ames: %" PRIu64 " dropped, failed to read prelude\n",
            sam_u->sat_u.pre_d);
    _ames_pact_free(pac_u);
    return;
  }

  //  parse prelude
  //
  _ames_sift_prel(&pac_u->hed_u, &pac_u->pre_u, pac_u->hun_y + cur_w);
  cur_w += pre_w;

  //  check contents match mug in header
  //
  if ( c3n == _ames_sift_body(&pac_u->hed_u, &bod_u, 
                              pac_u->len_w - cur_w,
                              pac_u->hun_y + cur_w) )
  {
      sam_u->sat_u.mut_d++;
      if ( 0 == (sam_u->sat_u.mut_d % 100000) ) {
        u3l_log("ames: %" PRIu64 " dropped for invalid mug\n",
                sam_u->sat_u.mut_d);
      }
    _ames_pact_free(pac_u);
    return;
  }

  //  cache the lane we received this on
  {
    u3_noun her = u3i_chubs(2, pac_u->pre_u.sen_d);
    u3_noun las = u3_ames_encode_lane(*lan_u);
    _ames_lane_into_cache(sam_u->lax_p, her, las);
  }

  //  if we can scry for lanes,
  //  and we are not the recipient,
  //  we might want to forward statelessly
  //
  if (  0 && (c3y == sam_u->fig_u.see_o)
     && (  (pac_u->pre_u.rec_d[0] != sam_u->pir_u->who_d[0])
        || (pac_u->pre_u.rec_d[1] != sam_u->pir_u->who_d[1]) ) )
  {
    memcpy(&pac_u->bod_u, &bod_u, sizeof(bod_u));
    _ames_try_forward(pan_u);
  }
  else {
    //  enter protocol-specific packet handling
    //
    switch ( pac_u->typ_y ) {
      case PACT_WAIL:
        _fine_hear_request(pac_u, cur_w);
        break;

      case PACT_PURR:
        _fine_hear_response(pac_u, cur_w);
        break;

      case PACT_AMES:
        memcpy(&pac_u->bod_u, &bod_u, sizeof(bod_u));
        _ames_hear_ames(pac_u, cur_w);
        break;

      default:
        u3l_log("ames_hear: bad packet type %d\n", pac_u->typ_y);
        u3_pier_bail(u3_king_stub());
    }
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

    case c3__hoot:
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

  u3_panc* pan_u = sam_u->pan_u;
  while (0 != pan_u) {
    u3_panc* nex_u = pan_u->nex_u;
    _ames_panc_free(pan_u);
    pan_u = nex_u;
  }

  u3h_free(sam_u->lax_p);

  u3s_cue_xeno_done(sam_u->sil_u);
  ur_cue_test_done(sam_u->tes_u);

  c3_free(sam_u);
}

/* _ames_io_exit(): terminate ames I/O.
*/
static void
_ames_io_exit(u3_auto* car_u)
{
  u3_ames* sam_u = (u3_ames*)car_u;
  uv_close(&sam_u->had_u, _ames_exit_cb);
}

/* _ames_io_info(): produce status info.
*/
static u3_noun
_ames_io_info(u3_auto* car_u)
{
  u3_ames*  sam_u = (u3_ames*)car_u;

  return u3i_list(
    u3_pier_mase("filtering",        sam_u->fig_u.fit_o),
    u3_pier_mase("can-send",         sam_u->fig_u.net_o),
    u3_pier_mase("can-scry",         sam_u->fig_u.see_o),
    u3_pier_mase("dropped",          u3i_chub(sam_u->sat_u.dop_d)),
    u3_pier_mase("forwards-dropped", u3i_chub(sam_u->sat_u.fod_d)),
    u3_pier_mase("forwards-pending", u3i_chub(sam_u->sat_u.foq_d)),
    u3_pier_mase("forwarded",        u3i_chub(sam_u->sat_u.fow_d)),
    u3_pier_mase("filtered-hed",     u3i_chub(sam_u->sat_u.hed_d)),
    u3_pier_mase("filtered-ver",     u3i_chub(sam_u->sat_u.vet_d)),
    u3_pier_mase("filtered-mug",     u3i_chub(sam_u->sat_u.mut_d)),
    u3_pier_mase("filtered-bod",     u3i_chub(sam_u->sat_u.bod_d)),
    u3_pier_mase("crashed",          u3i_chub(sam_u->sat_u.fal_d)),
    u3_pier_mase("cached-lanes",     u3i_word(u3h_wyt(sam_u->lax_p))),
    u3_none);
}

/* _ames_io_slog(): print status info.
*/
static void
_ames_io_slog(u3_auto* car_u)
{
  u3_ames* sam_u = (u3_ames*)car_u;

# define FLAG(a) ( (c3y == a) ? "&" : "|" )

  //  TODO  rewrite in terms of info_f
  //
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
u3_ames_io_init(u3_pier* pir_u)
{
  u3_ames* sam_u  = c3_calloc(sizeof(*sam_u));
  sam_u->pir_u    = pir_u;
  sam_u->fig_u.net_o = c3y;
  sam_u->fig_u.see_o = c3y;
  sam_u->fig_u.fit_o = c3n;

  // hashtable for scry cache
  //
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
  car_u->io.slog_f = _ames_io_slog;
  car_u->io.kick_f = _ames_io_kick;
  car_u->io.exit_f = _ames_io_exit;

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
