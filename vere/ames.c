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
#include <ncurses/curses.h>
#include <termios.h>
#include <ncurses/term.h>

#include "all.h"
#include "vere/vere.h"

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

/* _ames_free(): contrasting free.
*/
static void
_ames_free(void* ptr_v)
{
//  uL(fprintf(uH, "free %p\n", ptr_v));
  free(ptr_v);
}

/* _ames_pact_free(): free packet struct.
*/
static void
_ames_pact_free(u3_pact* pac_u)
{
  free(pac_u->hun_y);
  free(pac_u->dns_c);
  free(pac_u);
}

/* _ames_send_cb(): send callback.
*/
static void
_ames_send_cb(uv_udp_send_t* req_u, c3_i sas_i)
{
  u3_pact* pac_u = (u3_pact*)req_u;

#if 0
  if ( 0 != sas_i ) {
    uL(fprintf(uH, "ames: send_cb: %s\n", uv_strerror(sas_i)));
  }
#endif

  _ames_pact_free(pac_u);
}

/* _ames_send(): send buffer to address on port.
*/
static void
_ames_send(u3_pact* pac_u)
{
  u3_ames* sam_u = &u3_Host.sam_u;

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
    uL(fprintf(uH, "ames: send: %s\n", uv_strerror(sas_i)));
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
  u3_ames* sam_u = &u3_Host.sam_u;

  uL(fprintf(uH, "ames: czar at %s: not found (b)\n", pac_u->dns_c));
  if ( (0 == sam_u->imp_w[pac_u->imp_y]) ||
       (0xffffffff == sam_u->imp_w[pac_u->imp_y]) ) {
    sam_u->imp_w[pac_u->imp_y] = 0xffffffff;
  } /* else keep existing ip for 5 more minutes */
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
  u3_ames* sam_u = &u3_Host.sam_u;
  u3_pact* pac_u = (u3_pact*)adr_u->data;
  time_t     now = time(0);

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

#if 1
      if ( sam_u->imp_w[pac_u->imp_y] != old_w
        && sam_u->imp_w[pac_u->imp_y] != 0xffffffff ) {
        u3_noun wad = u3i_words(1, &sam_u->imp_w[pac_u->imp_y]);
        u3_noun nam = u3dc("scot", c3__if, wad);
        c3_c*   nam_c = u3r_string(nam);

        uL(fprintf(uH, "ames: czar %s: ip %s\n", pac_u->dns_c, nam_c));

        free(nam_c); u3z(nam);
      }
#endif

      _ames_send(pac_u);
      break;
    }

    rai_u = rai_u->ai_next;
  }

  free(adr_u);
  uv_freeaddrinfo(aif_u);
}


/* _ames_czar(): galaxy address resolution.
*/
static void
_ames_czar(u3_pact* pac_u, c3_c* bos_c)
{
  u3_ames* sam_u = &u3_Host.sam_u;

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
    fprintf(stderr, "ames: no galaxy domain for %s, no-op\r\n", nam_c);

    free(nam_c);
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
    // uL(fprintf(uH, "czar %s, dns %s\n", nam_c, pac_u->dns_c));

    free(nam_c);
    u3z(nam);

    {
      uv_getaddrinfo_t* adr_u = c3_malloc(sizeof(*adr_u));
      adr_u->data = pac_u;

      c3_i sas_i;

      if ( 0 != (sas_i = uv_getaddrinfo(u3L, adr_u,
                                        _ames_czar_cb,
                                        pac_u->dns_c, 0, 0)) ) {
        uL(fprintf(uH, "ames: %s\n", uv_strerror(sas_i)));
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

/* _ames_lane_ipv4(): IPv4 address/ from lane.
*/
u3_noun
_ames_lane_ip(u3_noun lan, c3_s* por_s, c3_w* pip_w)
{
  switch ( u3h(lan) ) {
    case c3__if: {
      *por_s= (c3_s) u3h(u3t(u3t(lan)));
      *pip_w = u3r_word(0, u3t(u3t(u3t(lan))));

      return c3y;
    } break;
    case c3__is: {
      u3_noun pq_lan = u3h(u3t(u3t(lan)));

      if ( u3_nul == pq_lan ) return c3n;
      else return _ames_lane_ip(u3t(pq_lan), por_s, pip_w);
    } break;
    case c3__ix: {
      *por_s = (c3_s) u3h(u3t(u3t(lan)));
      *pip_w = u3r_word(0, u3t(u3t(u3t(lan))));

      return c3y;
    } break;
  }
  return c3n;
}

/* u3_ames_ef_bake(): notify %ames that we're live.
*/
void
u3_ames_ef_bake(void)
{
  u3_noun pax = u3nq(u3_blip, c3__newt, u3k(u3A->sen), u3_nul);

  u3v_plan(pax, u3nc(c3__barn, u3_nul));
}

/* u3_ames_ef_send(): send packet to network (v4).
*/
void
u3_ames_ef_send(u3_noun lan, u3_noun pac)
{
  if ( u3_Host.ops_u.fuz_w && ((rand() % 100) < u3_Host.ops_u.fuz_w) ) {
    u3z(lan); u3z(pac);
    return;
  }

  u3_pact* pac_u = c3_calloc(sizeof(*pac_u));

  if ( c3y == _ames_lane_ip(lan, &pac_u->por_s, &pac_u->pip_w) ) {
    pac_u->len_w = u3r_met(3, pac);
    pac_u->hun_y = c3_malloc(pac_u->len_w);

    u3r_bytes(0, pac_u->len_w, pac_u->hun_y, pac);

    if ( 0 == pac_u->pip_w ) {
      pac_u->pip_w = 0x7f000001;
      pac_u->por_s = u3_Host.sam_u.por_s;
    }

    if ( (0 == (pac_u->pip_w >> 16)) && (1 == (pac_u->pip_w >> 8)) ) {
      pac_u->imp_y = (pac_u->pip_w & 0xff);

      _ames_czar(pac_u, u3_Host.sam_u.dns_c);
    }
    else if ( (c3y == u3_Host.ops_u.net) || (0x7f000001 == pac_u->pip_w) ) {
      _ames_send(pac_u);
    }
    else {
      // networking disabled
      _ames_pact_free(pac_u);
    }
  }
  else {
    _ames_pact_free(pac_u);
  }

  u3z(lan); u3z(pac);
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
  // uL(fprintf(uH, "ames: rx %p\r\n", buf_u.base));

  if ( 0 == nrd_i ) {
    _ames_free(buf_u->base);
  }
  else {
    u3_lo_open();
    {
      u3_noun             msg   = u3i_bytes((c3_w)nrd_i, (c3_y*)buf_u->base);

      // fprintf(stderr, "ames: plan\r\n");
#if 0
      u3z(msg);
#else
      struct sockaddr_in* add_u = (struct sockaddr_in *)adr_u;
      c3_s                por_s = ntohs(add_u->sin_port);
      c3_w                pip_w = ntohl(add_u->sin_addr.s_addr);

      u3v_plan
        (u3nt(u3_blip, c3__ames, u3_nul),
         u3nt(c3__hear,
              u3nq(c3__if, u3k(u3A->now), por_s, u3i_words(1, &pip_w)),
              msg));
#endif
    }
    _ames_free(buf_u->base);
    u3_lo_shut(c3y);
  }
}

/* _ames_io_start(): initialize ames I/O.
*/
static void
_ames_io_start()
{
  u3_ames* sam_u = &u3_Host.sam_u;
  c3_s por_s     = u3_Host.ops_u.por_s;
  u3_noun rac    = u3do("clan:title", u3k(u3A->own));

  if ( c3__czar == rac ) {
    u3_noun imp = u3dc("scot", 'p', u3k(u3A->own));
    c3_c* imp_c = u3r_string(imp);
    c3_y  num_y = u3r_byte(0, u3A->own);

    por_s = _ames_czar_port(num_y);

    if ( c3y == u3_Host.ops_u.net ) {
      uL(fprintf(uH, "ames: czar: %s on %d\n", imp_c, por_s));
    }
    else {
      uL(fprintf(uH, "ames: czar: %s on %d (localhost only)\n", imp_c, por_s));
    }

    u3z(imp);
    free(imp_c);
  }

  int ret;
  if ( 0 != (ret = uv_udp_init(u3L, &u3_Host.sam_u.wax_u)) ) {
    uL(fprintf(uH, "ames: init: %s\n", uv_strerror(ret)));
    c3_assert(0);
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

    int ret;
    if ( (ret = uv_udp_bind(&sam_u->wax_u, 
                            (const struct sockaddr*) & add_u, 0)) != 0 ) {
      uL(fprintf(uH, "ames: bind: %s\n",
                     uv_strerror(ret)));
      if (UV_EADDRINUSE == ret){
        uL(fprintf(uH, 
                    "    ...perhaps you've got two copies of vere running?\n"));
      }
      u3_lo_bail();
    }

    uv_udp_getsockname(&sam_u->wax_u, (struct sockaddr *)&add_u, &add_i);
    c3_assert(add_u.sin_port);

    sam_u->por_s = ntohs(add_u.sin_port);
  }

  // uL(fprintf(uH, "ames: on localhost, UDP %d.\n", sam_u->por_s));
  uv_udp_recv_start(&sam_u->wax_u, _ames_alloc, _ames_recv_cb);

  sam_u->liv = c3y;
  u3z(rac);
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

/* u3_ames_ef_turf(): initialize ames I/O on domain(s).
*/
void
u3_ames_ef_turf(u3_noun tuf)
{
  if ( u3_nul != tuf ) {
    // XX save all for fallback, not just first
    u3_noun hot = u3k(u3h(tuf));
    c3_w  len_w = _cttp_mcut_host(0, 0, u3k(hot));

    u3_Host.sam_u.dns_c = c3_malloc(1 + len_w);
    _cttp_mcut_host(u3_Host.sam_u.dns_c, 0, hot);
    u3_Host.sam_u.dns_c[len_w] = 0;

    u3z(tuf);
  }
  else if ( (c3n == u3A->fak) && (0 == u3_Host.sam_u.dns_c) ) {
    uL(fprintf(uH, "ames: turf: no domains\n"));
  }

  if ( c3n == u3_Host.sam_u.liv ) {
    _ames_io_start();
  }
}

/* u3_ames_io_init(): initialize ames I/O.
*/
void
u3_ames_io_init()
{
  u3_ames* sam_u = &u3_Host.sam_u;
  sam_u->liv = c3n;
}

/* u3_ames_io_talk(): start receiving ames traffic.
*/
void
u3_ames_io_talk()
{
}

/* u3_ames_io_exit(): terminate ames I/O.
*/
void
u3_ames_io_exit()
{
  u3_ames* sam_u = &u3_Host.sam_u;

  if ( c3y == sam_u->liv ) {
    // XX remove had_u/wax_u union, cast and close wax_u
    uv_close(&sam_u->had_u, 0);
  }
}
