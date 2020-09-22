/* vere/ames.c
**
*/
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <uv.h>
#include <errno.h>

#include "all.h"
#include "vere/vere.h"
#include "ur/serial.h"

/* u3_pact: ames packet, coming or going.
*/
  typedef struct _u3_pact {
    uv_udp_send_t    snd_u;             //  udp send request
    c3_w             pip_w;             //  target IPv4 address
    c3_s             por_s;             //  target port
    c3_w             len_w;             //  length in bytes
    c3_y*            hun_y;             //  packet buffer
    c3_y             imp_y;             //  galaxy number (optional)
    c3_c*            dns_c;             //  galaxy fqdn (optional)
    struct _u3_ames* sam_u;             //  ames backpointer
  } u3_pact;

/* u3_ames: ames networking.
*/
  typedef struct _u3_ames {             //  packet network state
    u3_auto          car_u;             //  driver
    u3_pier*         pir_u;             //  pier
    union {                             //
      uv_udp_t       wax_u;             //
      uv_handle_t    had_u;             //
    };                                  //
    ur_cue_test_t*   tes_u;             //  cue-test handle
    u3_cue_xeno*     sil_u;             //  cue handle
    c3_c*            dns_c;             //  domain XX multiple/fallback
    c3_d             dop_d;             //  drop count
    c3_d             fal_d;             //  crash count
    c3_w             imp_w[256];        //  imperial IPs
    time_t           imp_t[256];        //  imperial IP timestamps
    c3_o             imp_o[256];        //  imperial print status
    c3_o             net_o;             //  can send
    c3_o             see_o;             //  can scry
    c3_d             saw_d;             //  successive scry failures
    c3_o             fit_o;             //  filtering active
    c3_y             ver_y;             //  protocol version
    u3p(u3h_root)    lax_p;             //  lane scry cache
    c3_d             vet_d;             //  version mismatches filtered
    c3_d             mut_d;             //  invalid mugs filtered
    struct _u3_panc* pac_u;             //  packets pending forwards
    c3_d             foq_d;             //  forward queue size
    c3_d             fow_d;             //  forwarded count
    c3_d             fod_d;             //  forwards dropped count
  } u3_ames;

/* u3_head: ames packet header
*/
  typedef struct _u3_head {
    c3_y ver_y;                         //  protocol version
    c3_l mug_l;                         //  truncated mug hash of u3_body
    c3_y sac_y;                         //  sender class
    c3_y rac_y;                         //  receiver class
    c3_o enc_o;                         //  encrypted?
  } u3_head;

/* u3_body: ames packet body
*/
  typedef struct _u3_body {
    c3_d  sen_d[2];                     //  sender
    c3_d  rec_d[2];                     //  receiver
    c3_w  con_w;                        //  jam size
    c3_y* con_y;                        //  (jam [origin content])
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

/* _ames_panc_free(): remove references, lose refcounts and free struct
*/
static void
_ames_panc_free(u3_panc* pac_u) {
  if ( 0 != pac_u->nex_u ) {
    pac_u->nex_u->pre_u = pac_u->pre_u;
  }
  if ( 0 != pac_u->pre_u ) {
    pac_u->pre_u->nex_u = pac_u->nex_u;
  } else {
    c3_assert(pac_u == pac_u->sam_u->pac_u);
    pac_u->sam_u->pac_u = pac_u->nex_u;
  }

  c3_free(pac_u->bod_u.con_y);
  c3_free(pac_u);
}

/* _ames_mug_body(): truncated (20 least-significant bits) mug hash of bytes
*/
static c3_l
_ames_mug_body(c3_w len_w, c3_y* byt_y)
{
  return u3r_mug_bytes(byt_y, len_w) & 0xfffff;
}

/* _ames_send_cb(): send callback.
*/
static void
_ames_send_cb(uv_udp_send_t* req_u, c3_i sas_i)
{
  u3_pact* pac_u = (u3_pact*)req_u;
  u3_ames* sam_u = pac_u->sam_u;

  if ( sas_i && (c3y == sam_u->net_o)  ) {
    u3l_log("ames: send fail: %s\n", uv_strerror(sas_i));
    sam_u->net_o = c3n;
  }
  else {
    sam_u->net_o = c3y;
  }

  _ames_pact_free(pac_u);
}

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
    add_u.sin_addr.s_addr = htonl(pac_u->pip_w);
    add_u.sin_port = htons(pac_u->por_s);

    {
      uv_buf_t buf_u = uv_buf_init((c3_c*)pac_u->hun_y, pac_u->len_w);
      c3_i     sas_i = uv_udp_send(&pac_u->snd_u,
                                   &sam_u->wax_u,
                                   &buf_u, 1,
                                   (const struct sockaddr*)&add_u,
                                   _ames_send_cb);

      if ( sas_i ) {
        if ( c3y == sam_u->net_o ) {
          u3l_log("ames: send fail: %s\n", uv_strerror(sas_i));
          sam_u->net_o = c3n;
        }

        _ames_pact_free(pac_u);
      }
    }
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

  if ( c3y == sam_u->imp_o[pac_u->imp_y] ) {
    u3l_log("ames: czar at %s: not found (b)\n", pac_u->dns_c);
    sam_u->imp_o[pac_u->imp_y] = c3n;
  }

  if ( (0 == sam_u->imp_w[pac_u->imp_y]) ||
       (0xffffffff == sam_u->imp_w[pac_u->imp_y]) )
  {
    sam_u->imp_w[pac_u->imp_y] = 0xffffffff;
  }

  //  keep existing ip for 5 more minutes
  //
  sam_u->imp_t[pac_u->imp_y] = now;

  _ames_pact_free(pac_u);
}

/* _ames_czar_cb(): galaxy address resolution callback.
*/
static void
_ames_czar_cb(uv_getaddrinfo_t* adr_u,
              c3_i              sas_i,
              struct addrinfo*  aif_u)
{
  u3_pact* pac_u = (u3_pact*)adr_u->data;
  u3_ames* sam_u = pac_u->sam_u;
  time_t now     = time(0);

  struct addrinfo* rai_u = aif_u;

  while ( 1 ) {
    if ( !rai_u ) {
      _ames_czar_gone(pac_u, now);
      break;
    }

    if ( (AF_INET == rai_u->ai_family) ) {
      struct sockaddr_in* add_u = (struct sockaddr_in *)rai_u->ai_addr;
      c3_w old_w = sam_u->imp_w[pac_u->imp_y];

      sam_u->imp_w[pac_u->imp_y] = ntohl(add_u->sin_addr.s_addr);
      sam_u->imp_t[pac_u->imp_y] = now;
      sam_u->imp_o[pac_u->imp_y] = c3y;

#if 1
      if ( sam_u->imp_w[pac_u->imp_y] != old_w
        && sam_u->imp_w[pac_u->imp_y] != 0xffffffff ) {
        u3_noun wad = u3i_words(1, &sam_u->imp_w[pac_u->imp_y]);
        u3_noun nam = u3dc("scot", c3__if, wad);
        c3_c*   nam_c = u3r_string(nam);

        u3l_log("ames: czar %s: ip %s\n", pac_u->dns_c, nam_c);

        c3_free(nam_c); u3z(nam);
      }
#endif

      _ames_send(pac_u);
      break;
    }

    rai_u = rai_u->ai_next;
  }

  c3_free(adr_u);
  uv_freeaddrinfo(aif_u);
}

/* u3_ames_decode_lane(): deserialize noun to lane
*/
u3_lane
u3_ames_decode_lane(u3_atom lan) {
  u3_noun cud, tag, pip, por;

  cud = u3ke_cue(lan);
  u3x_trel(cud, &tag, &pip, &por);
  c3_assert( c3__ipv4 == tag );

  u3_lane lan_u;
  lan_u.pip_w = u3r_word(0, pip);

  c3_assert( _(u3a_is_cat(por)) );
  c3_assert( por < 65536 );
  lan_u.por_s = por;

  u3z(cud);
  return lan_u;
}

/* u3_ames_encode_lane(): serialize lane to jammed noun
*/
u3_atom
u3_ames_encode_lane(u3_lane lan) {
  return u3ke_jam(u3nt(c3__ipv4, u3i_words(1, &lan.pip_w), lan.por_s));
}

/* _ames_lane_from_sockaddr(): sockaddr_in to lane struct
*/
static u3_lane
_ames_lane_from_sockaddr(struct sockaddr_in* add_u)
{
  u3_lane lan_u;
  lan_u.por_s = ntohs(add_u->sin_port);
  lan_u.pip_w = ntohl(add_u->sin_addr.s_addr);
  return lan_u;
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
  c3_y sen_y = 2 << pac_u->hed_u.sac_y;
  c3_y rec_y = 2 << pac_u->hed_u.rac_y;
  c3_o nal_o = c3n;

  //  update the body's lane, if desired
  //
  if ( c3y == dop_o ) {
    //  unpack (jam [(unit lane) body])
    //
    u3_noun lon, bod;
    {
      //NOTE we checked for cue safety in _ames_recv_cb
      //
      u3_weak old = u3s_cue_xeno_with(pac_u->sam_u->sil_u,
                                      pac_u->bod_u.con_w,
                                      pac_u->bod_u.con_y);
      u3x_cell(u3x_good(old), &lon, &bod);
      u3k(lon); u3k(bod);
      u3z(old);
    }

    //  only replace the lane if it was ~
    //
    //NOTE  this sets an opaque lane even in the "sender is galaxy" case,
    //      but that doesn't matter: ames.hoon ignores origin in that case,
    //      always using the appropriate galaxy lane instead.
    //
    if ( u3_nul == lon ) {
      u3z(lon);
      lon = u3nt(u3_nul, c3n, u3_ames_encode_lane(pac_u->ore_u));
      nal_o = c3y;

      c3_free(pac_u->bod_u.con_y);
      u3_noun jam = u3ke_jam(u3nc(lon, bod));
      pac_u->bod_u.con_w = u3r_met(3, jam);
      pac_u->bod_u.con_y = c3_malloc(pac_u->bod_u.con_w);
      u3r_bytes(0, pac_u->bod_u.con_w, pac_u->bod_u.con_y, jam);
      u3z(jam);
    }
    else {
      u3z(lon); u3z(bod);
    }
  }

  //  serialize the packet
  //
  u3_noun pac;
  {
    //  start with the body
    //
    u3_body* bod_u = &pac_u->bod_u;
    c3_y*    pac_y = c3_malloc(4 + sen_y + rec_y + bod_u->con_w);
    {
      u3_atom sen = u3i_chubs(2, bod_u->sen_d);
      u3_atom rec = u3i_chubs(2, bod_u->rec_d);
      u3r_bytes(0, sen_y, pac_y + 4,         sen);
      u3r_bytes(0, rec_y, pac_y + 4 + sen_y, rec);
      u3z(sen); u3z(rec);
    }
    memcpy(pac_y + 4 + sen_y + rec_y, bod_u->con_y, bod_u->con_w);

    //  if we updated the origin lane, we need to update the mug too
    //
    if ( c3y == nal_o ) {
      pac_u->hed_u.mug_l = _ames_mug_body(sen_y + rec_y + bod_u->con_w,
                                        pac_y + 4);
    }

    //  now we can serialize the head
    //
    u3_head* hed_u = &pac_u->hed_u;
    c3_w     hed_w = hed_u->ver_y
                   | (hed_u->mug_l << 3)
                   | (hed_u->sac_y << 23)
                   | (hed_u->rac_y << 25)
                   | (hed_u->enc_o << 27);

    //  XX assumes little-endian
    //
    memcpy(pac_y, &hed_w, 4);

    pac = u3i_bytes(4 + sen_y + rec_y + bod_u->con_w, pac_y);
    c3_free(pac_y);
  }

  return pac;
}

/* _ames_czar(): galaxy address resolution.
*/
static void
_ames_czar(u3_pact* pac_u, c3_c* bos_c)
{
  u3_ames* sam_u = pac_u->sam_u;

  pac_u->por_s = _ames_czar_port(pac_u->imp_y);

  if ( c3n == u3_Host.ops_u.net ) {
    pac_u->pip_w = 0x7f000001;
    _ames_send(pac_u);
    return;
  }

  //  if we don't have a galaxy domain, no-op
  //
  if ( 0 == bos_c ) {
    u3_noun nam = u3dc("scot", 'p', pac_u->imp_y);
    c3_c*  nam_c = u3r_string(nam);
    u3l_log("ames: no galaxy domain for %s, no-op\r\n", nam_c);

    c3_free(nam_c);
    u3z(nam);
    return;
  }

  time_t now = time(0);

  // backoff
  if ( (0xffffffff == sam_u->imp_w[pac_u->imp_y]) &&
       (now - sam_u->imp_t[pac_u->imp_y]) < 300 ) {
    _ames_pact_free(pac_u);
    return;
  }

  if ( (0 == sam_u->imp_w[pac_u->imp_y]) ||
       (now - sam_u->imp_t[pac_u->imp_y]) > 300 ) { /* 5 minute TTL */
    u3_noun  nam = u3dc("scot", 'p', pac_u->imp_y);
    c3_c*  nam_c = u3r_string(nam);
    // XX remove extra byte for '~'
    pac_u->dns_c = c3_malloc(1 + strlen(bos_c) + 1 + strlen(nam_c));

    snprintf(pac_u->dns_c, 256, "%s.%s", nam_c + 1, bos_c);
    // u3l_log("czar %s, dns %s\n", nam_c, pac_u->dns_c);

    c3_free(nam_c);
    u3z(nam);

    {
      uv_getaddrinfo_t* adr_u = c3_malloc(sizeof(*adr_u));
      adr_u->data = pac_u;

      c3_i sas_i;

      if ( 0 != (sas_i = uv_getaddrinfo(u3L, adr_u,
                                        _ames_czar_cb,
                                        pac_u->dns_c, 0, 0)) ) {
        u3l_log("ames: %s\n", uv_strerror(sas_i));
        _ames_czar_gone(pac_u, now);
        return;
      }
    }
  }
  else {
    pac_u->pip_w = sam_u->imp_w[pac_u->imp_y];
    _ames_send(pac_u);
    return;
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
    _ames_czar(pac_u, sam_u->dns_c);
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
    //  otherwise, mutate destination and send packet
    //
    else {
      pac_u->pip_w = lan_u.pip_w;
      pac_u->por_s = lan_u.por_s;

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
      sam_u->dop_d++;

      if ( u3C.wag_w & u3o_verbose ) {
        u3l_log("ames: packet dropped (%" PRIu64 " total)\n", sam_u->dop_d);
      }
    }

    egg_u = nex_u;
  }

  if (  (sam_u->dop_d && (0 == (sam_u->dop_d % 1000)))
     && !(u3C.wag_w & u3o_verbose) )
  {
    u3l_log("ames: packet dropped (%" PRIu64 " total)\n", sam_u->dop_d);
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
  sam_u->fal_d++;

  if (  (u3C.wag_w & u3o_verbose)
     || (0 == (sam_u->fal_d % 1000)) )
  {
    _ames_punt_goof(lud);
    u3l_log("ames: packet failed (%" PRIu64 " total)\n\n", sam_u->fal_d);
  }
  else {
    u3z(lud);

    if (  0 == (sam_u->fal_d % 1000) )  {
      u3l_log("ames: packet failed (%" PRIu64 " total)\n\n", sam_u->fal_d);
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
  pac_u->sam_u->fow_d++;
  if ( 0 == (pac_u->sam_u->fow_d % 1000000) ) {
    u3l_log("ames: forwarded %" PRIu64 " total\n", pac_u->sam_u->fow_d);
  }

  {
    u3_noun los = las;
    u3_noun pac = _ames_serialize_packet(pac_u, c3y);
    while ( u3_nul != las ) {
      _ames_ef_send(pac_u->sam_u, u3k(u3h(las)), u3k(pac));
      las = u3t(las);
    }
    u3z(los); u3z(pac);
  }

  _ames_panc_free(pac_u);
}

/*  _ames_lane_scry_cb(): learn lane to forward packet on
*/
static void
_ames_lane_scry_cb(void* vod_p, u3_noun nun)
{
  u3_panc* pac_u = vod_p;
  u3_weak  las = u3r_at(7, nun);

  pac_u->sam_u->foq_d--;

  //  if scry fails, remember we can't scry, and just inject the packet
  //
  if ( u3_none == las ) {
    if ( 5 < ++pac_u->sam_u->saw_d ) {
      u3l_log("ames: giving up scry\n");
      pac_u->sam_u->see_o = c3n;
    }
    _ames_put_packet(pac_u->sam_u,
                     _ames_serialize_packet(pac_u, c3n),
                     pac_u->ore_u);
    _ames_panc_free(pac_u);
  }
  else {
    pac_u->sam_u->saw_d = 0;

    //  cache the scry result for later use
    //
    _ames_lane_into_cache(pac_u->sam_u->lax_p,
                          u3i_chubs(2, pac_u->bod_u.rec_d),
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

/* _ames_recv_cb(): receive callback.
*/
static void
_ames_recv_cb(uv_udp_t*        wax_u,
              ssize_t          nrd_i,
              const uv_buf_t * buf_u,
              const struct sockaddr* adr_u,
              unsigned         flg_i)
{
  u3_ames* sam_u = wax_u->data;
  c3_o     pas_o = c3y;
  c3_y*    byt_y = (c3_y*)buf_u->base;
  c3_y*    bod_y = byt_y + 4;
  u3_head  hed_u;

  //  ensure a sane message size
  //
  if ( 4 >= nrd_i ) {
    pas_o = c3n;
  }
  //  unpack the packet header
  //
  else {
    c3_w hed_w = (byt_y[0] <<  0)
               | (byt_y[1] <<  8)
               | (byt_y[2] << 16)
               | (byt_y[3] << 24);

    hed_u.ver_y = hed_w & 0x7;
    hed_u.mug_l = (hed_w >> 3) & 0xfffff; //NOTE ((1 << 20) - 1)
    hed_u.sac_y = (hed_w >> 23) & 0x3;
    hed_u.rac_y = (hed_w >> 25) & 0x3;
    hed_u.enc_o = (hed_w >> 27) & 0x1;
  }

  //  ensure the protocol version matches ours
  //
  if ( c3y == pas_o
    && (c3y == sam_u->fit_o)
    && (sam_u->ver_y != hed_u.ver_y) )
  {
    pas_o = c3n;

    sam_u->vet_d++;
    if ( 0 == (sam_u->vet_d % 100) ) {
      u3l_log("ames: %" PRIu64 " dropped for version mismatch\n", sam_u->vet_d);
    }
  }

  //  ensure the mug is valid
  //
  if ( c3y == pas_o
    && (hed_u.mug_l != _ames_mug_body(nrd_i - 4, bod_y)) )
  {
    pas_o = c3n;

    sam_u->mut_d++;
    if ( 0 == (sam_u->mut_d % 100) ) {
      u3l_log("ames: %" PRIu64 " dropped for invalid mug\n", sam_u->mut_d);
    }
  }

  //  unpack the body
  //
  c3_y  sen_y = 2 << hed_u.sac_y;
  c3_y  rec_y = 2 << hed_u.rac_y;
  c3_d  sen_d[2];
  c3_d  rec_d[2];
  c3_w  con_w = nrd_i - 4 - sen_y - rec_y;
  c3_y* con_y = NULL;

  if ( c3y == pas_o ) {
    u3_noun sen = u3i_bytes(sen_y, bod_y);
    u3_noun rec = u3i_bytes(rec_y, bod_y + sen_y);
    u3r_chubs(0, 2, rec_d, rec);
    u3r_chubs(0, 2, sen_d, sen);
    u3z(sen); u3z(rec);

    con_y = c3_malloc(con_w);
    memcpy(con_y, bod_y + sen_y + rec_y, con_w);

    //  ensure the content is cue-able
    //
    pas_o = ur_cue_test_with(sam_u->tes_u, con_w, con_y) ? c3y : c3n;
  }

  //  if we can scry,
  //  and we are not the recipient,
  //  we might want to forward statelessly
  //
  if ( c3y == pas_o
    && c3y == sam_u->see_o
    && ( (rec_d[0] != sam_u->pir_u->who_d[0])
      || (rec_d[1] != sam_u->pir_u->who_d[1]) ) )
  {
    pas_o = c3n;

    u3_weak lac;
    //  if the recipient is a galaxy, their lane is always &+~gax
    //
    if ( (rec_d[1] == 0) && (256 > rec_d[0]) ) {
      lac = u3nc(c3y, (c3_y)rec_d[0]);
    }
    //  otherwise, try to get the lane from cache
    //
    else {
      lac = _ames_lane_from_cache(sam_u->lax_p, u3i_chubs(2, rec_d));
    }

    //  if we don't know the lane, and the scry queue is full,
    //  just drop the packet
    //
    //TODO  drop oldest item in forward queue in favor of this one.
    //      ames.c doesn't/shouldn't know about the shape of scry events,
    //      so can't pluck these out of the event queue like it does in
    //      _ames_cap_queue. as such, blocked on u3_lord_peek_cancel or w/e.
    //
    if ( (u3_none == lac) && (1000 < sam_u->foq_d) ) {
      c3_free(con_y);

      sam_u->fod_d++;
      if ( 0 == (sam_u->fod_d % 10000) ) {
        u3l_log("ames: dropped %" PRIu64 " forwards total\n", sam_u->fod_d);
      }
    }
    //  if we know there's no lane, drop the packet
    //
    else if ( u3_nul == lac ) {
      c3_free(con_y);
      u3z(lac);
    }
    //  otherwise, proceed with forwarding
    //
    else {
      //  store the packet details for later processing
      //
      u3_panc* pac_u = c3_calloc(sizeof(*pac_u));
      pac_u->sam_u = sam_u;
      pac_u->hed_u = hed_u;
      pac_u->bod_u.sen_d[0] = sen_d[0];
      pac_u->bod_u.sen_d[1] = sen_d[1];
      pac_u->bod_u.rec_d[0] = rec_d[0];
      pac_u->bod_u.rec_d[1] = rec_d[1];
      pac_u->bod_u.con_w = con_w;
      pac_u->bod_u.con_y = con_y;
      pac_u->ore_u = _ames_lane_from_sockaddr((struct sockaddr_in *)adr_u);

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
        sam_u->foq_d++;
        u3_noun pax = u3nq(u3i_string("peers"),
                           u3dc("scot", 'p', u3i_chubs(2, rec_d)),
                           u3i_string("forward-lane"),
                           u3_nul);
        u3_pier_peek_last(sam_u->pir_u, u3_nul, c3__ax,
                          u3_nul, pax, pac_u, _ames_lane_scry_cb);
      }
    }
  }

  //  if we passed the filter, inject the packet
  //
  if ( c3y == pas_o ) {
    c3_free(con_y);
    u3_lane ore_u = _ames_lane_from_sockaddr((struct sockaddr_in *)adr_u);
    u3_noun msg   = u3i_bytes((c3_w)nrd_i, (c3_y*)buf_u->base);
    _ames_put_packet(sam_u, msg, ore_u);
  }

  c3_free(buf_u->base);
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

/* _ames_prot_scry_cb(): receive protocol version
*/
static void
_ames_prot_scry_cb(void* vod_p, u3_noun nun)
{
  u3_ames* sam_u = vod_p;
  u3_weak  ver   = u3r_at(7, nun);

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

  sam_u->fit_o = c3y;
  u3z(nun);
}

/* _ames_io_talk(): start receiving ames traffic.
*/
static void
_ames_io_talk(u3_auto* car_u)
{
  u3_ames* sam_u = (u3_ames*)car_u;
  _ames_io_start(sam_u);

  // send born event
  //
  {
    u3_noun wir = u3nt(c3__newt, u3k(u3A->sen), u3_nul);
    u3_noun cad = u3nc(c3__born, u3_nul);

    u3_auto_plan(car_u, u3_ovum_init(0, c3__a, wir, cad));
  }

  //  scry the protocol version out of arvo
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

/* _ames_io_exit(): terminate ames I/O.
*/
static void
_ames_io_exit(u3_auto* car_u)
{
  u3_ames* sam_u = (u3_ames*)car_u;
  uv_close(&sam_u->had_u, _ames_exit_cb);
}

/* _ames_io_info(): print status info.
*/
static void
_ames_io_info(u3_auto* car_u)
{
  u3_ames* sam_u = (u3_ames*)car_u;
  u3l_log("               dropped: %" PRIu64 "\n", sam_u->dop_d);
  u3l_log("      forwards dropped: %" PRIu64 "\n", sam_u->fod_d);
  u3l_log("      forwards pending: %" PRIu64 "\n", sam_u->foq_d);
  u3l_log("             forwarded: %" PRIu64 "\n", sam_u->fow_d);
  u3l_log("        filtered (ver): %" PRIu64 "\n", sam_u->vet_d);
  u3l_log("        filtered (mug): %" PRIu64 "\n", sam_u->mut_d);
  u3l_log("               crashed: %" PRIu64 "\n", sam_u->fal_d);
  u3l_log("          cached lanes: %u\n", u3to(u3h_root, sam_u->lax_p)->use_w);
}

/* u3_ames_io_init(): initialize ames I/O.
*/
u3_auto*
u3_ames_io_init(u3_pier* pir_u)
{
  u3_ames* sam_u  = c3_calloc(sizeof(*sam_u));
  sam_u->pir_u    = pir_u;
  sam_u->dop_d    = 0;
  sam_u->net_o    = c3y;
  sam_u->see_o    = c3y;
  sam_u->fit_o    = c3n;
  sam_u->foq_d    = 0;

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

  return car_u;
}
