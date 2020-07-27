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
    u3_auto       car_u;                //  driver
    union {                             //
      uv_udp_t    wax_u;                //
      uv_handle_t had_u;                //
    };                                  //
    c3_d          who_d[2];             //  identity
    c3_o          fak_o;                //  fake keys
    c3_s          por_s;                //  public IPv4 port
    c3_c*         dns_c;                //  domain XX multiple/fallback
    c3_d          dop_d;                //  drop count
    c3_d          fal_d;                //  crash count
    c3_w          imp_w[256];           //  imperial IPs
    time_t        imp_t[256];           //  imperial IP timestamps
    c3_o          imp_o[256];           //  imperial print status
  } u3_ames;

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

/* _ames_send_cb(): send callback.
*/
static void
_ames_send_cb(uv_udp_send_t* req_u, c3_i sas_i)
{
  u3_pact* pac_u = (u3_pact*)req_u;

  if ( 0 != sas_i ) {
    u3l_log("ames: send fail: %s\n", uv_strerror(sas_i));
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

  struct sockaddr_in add_u;

  memset(&add_u, 0, sizeof(add_u));
  add_u.sin_family = AF_INET;
  add_u.sin_addr.s_addr = htonl(pac_u->pip_w);
  add_u.sin_port = htons(pac_u->por_s);

  uv_buf_t buf_u = uv_buf_init((c3_c*)pac_u->hun_y, pac_u->len_w);
  c3_i sas_i;

  if ( 0 != (sas_i = uv_udp_send(&pac_u->snd_u,
                                 &sam_u->wax_u,
                                 &buf_u, 1,
                                 (const struct sockaddr*)&add_u,
                                 _ames_send_cb)) ) {
    u3l_log("ames: send: %s\n", uv_strerror(sas_i));
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

  //  data present, and protocol version in header matches 0
  //
  //    XX inflexible, scry version out of ames
  //
  if (  (0 < nrd_i)
     && (0 == (0x7 & *((c3_w*)buf_u->base))) )
  {
    u3_noun wir = u3nc(c3__ames, u3_nul);
    u3_noun cad;

    {
      u3_noun msg = u3i_bytes((c3_w)nrd_i, (c3_y*)buf_u->base);
      u3_noun lan;

      {
        struct sockaddr_in* add_u = (struct sockaddr_in *)adr_u;
        u3_lane             lan_u;

        lan_u.por_s = ntohs(add_u->sin_port);
        lan_u.pip_w = ntohl(add_u->sin_addr.s_addr);
        lan = u3_ames_encode_lane(lan_u);
      }

      cad = u3nt(c3__hear, u3nc(c3n, lan), msg);
    }

    u3_auto_peer(
      u3_auto_plan(&sam_u->car_u,
                   u3_ovum_init(0, c3__a, wir, cad)),
      0, 0, _ames_hear_bail);

    _ames_cap_queue(sam_u);
  }

  c3_free(buf_u->base);
}

/* _ames_io_start(): initialize ames I/O.
*/
static void
_ames_io_start(u3_ames* sam_u)
{
  c3_s     por_s = sam_u->por_s;
  u3_noun    who = u3i_chubs(2, sam_u->who_d);
  u3_noun    rac = u3do("clan:title", u3k(who));
  c3_i     ret_i;

  if ( c3__czar == rac ) {
    c3_y num_y = (c3_y)sam_u->who_d[0];
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

    sam_u->por_s = ntohs(add_u.sin_port);
  }

  if ( c3y == u3_Host.ops_u.net ) {
    u3l_log("ames: live on %d\n", sam_u->por_s);
  }
  else {
    u3l_log("ames: live on %d (localhost only)\n", sam_u->por_s);
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
  else if ( (c3n == sam_u->fak_o) && (0 == sam_u->dns_c) ) {
    u3l_log("ames: turf: no domains\n");
  }

  //  XX is this ever necessary?
  //
  if ( c3n == sam_u->car_u.liv_o ) {
    _ames_io_start(sam_u);
  }
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
  u3l_log("      dropped: %" PRIu64 "\n", sam_u->dop_d);
  u3l_log("      crashed: %" PRIu64 "\n", sam_u->fal_d);
}

/* u3_ames_io_init(): initialize ames I/O.
*/
u3_auto*
u3_ames_io_init(u3_pier* pir_u)
{
  u3_ames* sam_u  = c3_calloc(sizeof(*sam_u));
  sam_u->who_d[0] = pir_u->who_d[0];
  sam_u->who_d[1] = pir_u->who_d[1];
  sam_u->por_s    = pir_u->por_s;
  sam_u->fak_o    = pir_u->fak_o;
  sam_u->dop_d    = 0;

  c3_assert( !uv_udp_init(u3L, &sam_u->wax_u) );
  sam_u->wax_u.data = sam_u;

  //  Disable networking for fake ships
  //
  if ( c3y == sam_u->fak_o ) {
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
