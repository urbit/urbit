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
#include <netdb.h>
#include <uv.h>
#include <errno.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "v/vere.h"

/* _ames_alloc(): libuv buffer allocator.
*/
static void
_ames_alloc(uv_handle_t* had_u, 
            size_t len_i,
            uv_buf_t* buf
            )
{
  void* ptr_v = c3_malloc(len_i);
  *buf = uv_buf_init(ptr_v, len_i);
}

/* _ames_free(): contrasting free.
*/
static void
_ames_free(void* ptr_v)
{
//  uL(fprintf(uH, "free %p\n", ptr_v));
  free(ptr_v);
}

/* _ames_czar(): quasi-static route to emperor.
*/
static c3_w
_ames_czar(c3_y imp_y, c3_s* por_s)
{
  u3_ames* sam_u = &u3_Host.sam_u;

  if ( u3_yes == u3_Host.ops_u.loh ) {
    *por_s = 31337 + imp_y;
    return 0x7f000001;
  }
  else {
    *por_s = 13337 + imp_y;

    if ( 0xffffffff == sam_u->imp_w[imp_y] ) {
      return 0;
    }
    else if ( 0 == sam_u->imp_w[imp_y] ) {
      u3_noun nam   = u3_dc("scot", 'p', imp_y);
      c3_c*   nam_c = u3_cr_string(nam);
      c3_c    dns_c[64];

      snprintf(dns_c, 64, "%s.urbit.org", nam_c + 1);
      // uL(fprintf(uH, "czar %s, dns %s\n", nam_c, dns_c));
      free(nam_c);
      u3z(nam);

      {
        struct addrinfo* air_u;

        if ( 0 != getaddrinfo(dns_c, 0, 0, &air_u) ) {
          uL(fprintf(uH, "ames: czar at %s: not found (a)\n", dns_c));
          sam_u->imp_w[imp_y] = 0xffffffff;
          return 0;
        }

        {
          struct addrinfo* rai_u = air_u;

          while ( 1 ) {
            if ( !rai_u ) {
              uL(fprintf(uH, "ames: czar at %s: not found (b)\n", dns_c));
              sam_u->imp_w[imp_y] = 0xffffffff;
              return 0;
            }
            if ( (AF_INET == rai_u->ai_family) ) {
              struct sockaddr_in* add_u = (struct sockaddr_in *)rai_u->ai_addr;

              sam_u->imp_w[imp_y] = ntohl(add_u->sin_addr.s_addr);
#if 1
              {
                u3_noun wad = u3_ci_words(1, &sam_u->imp_w[imp_y]);
                u3_noun nam = u3_dc("scot", c3__if, wad);
                c3_c*   nam_c = u3_cr_string(nam);

                uL(fprintf(uH, "ames: czar %s: ip %s\n", dns_c, nam_c));

                free(nam_c); u3z(nam);
              }
#endif
              break;
            }
            rai_u = rai_u->ai_next;
          }
        }
        freeaddrinfo(air_u);
      }
    }
    return sam_u->imp_w[imp_y];
  }
}

/* _ames_lane_ipv4(): IPv4 address/ from lane.
*/
u3_bean
_ames_lane_ip(u3_noun lan, c3_s* por_s, c3_w* pip_w)
{
  switch ( u3h(lan) ) {
    case c3__if: {
      *por_s= (c3_s) u3h(u3t(u3t(lan)));
      *pip_w = u3_cr_word(0, u3t(u3t(u3t(lan))));

      return u3_yes;
    } break;
    case c3__is: {
      u3_noun pq_lan = u3h(u3t(u3t(lan)));

      if ( u3_nul == pq_lan ) return u3_no;
      else return _ames_lane_ip(u3t(pq_lan), por_s, pip_w);
    } break;
    case c3__ix: {
      *por_s = (c3_s) u3h(u3t(u3t(lan)));
      *pip_w = u3_cr_word(0, u3t(u3t(u3t(lan))));

      return u3_yes;
    } break;
  }
  return u3_no;
}

/* An unusual lameness in libuv.
*/
  typedef struct {
    uv_udp_send_t snd_u;
    c3_y*         buf_y;
  } _u3_udp_send_t;

/* _ames_send_cb(): send callback.
*/
static void
_ames_send_cb(uv_udp_send_t* req_u, c3_i sas_i)
{
  _u3_udp_send_t* ruq_u = (void*)req_u;

#if 0
  if ( 0 != sas_i ) {
    uL(fprintf(uH, "ames: send_cb: %s\n", uv_strerror(uv_last_error(u3L))));
  }
#endif
  // fprintf(stderr, "ames: tx\r\n");
  free(ruq_u->buf_y);
  free(ruq_u);
}

void
u3_ames_ef_bake(void)
{
  u3_noun pax = u3nq(u3_blip, c3__newt, u3k(u3A->sen), u3_nul);

  u3_cv_plan(pax, u3nc(c3__barn, u3_nul));
}

/* u3_ames_ef_send(): send packet to network (v4).
*/
void
u3_ames_ef_send(u3_noun lan, u3_noun pac)
{
  u3_ames* sam_u = &u3_Host.sam_u;
  c3_s     por_s;
  c3_w     pip_w;

  if ( u3_Host.ops_u.fuz_w && ((rand() % 100) < u3_Host.ops_u.fuz_w) ) {
    u3z(pac);
    return;
  }

  if ( u3_yes == _ames_lane_ip(lan, &por_s, &pip_w) ) {
    c3_w     len_w = u3_cr_met(3, pac);
    c3_y*    buf_y = c3_malloc(len_w);

    u3_cr_bytes(0, len_w, buf_y, pac);

    if ( 0 == pip_w ) {
      pip_w = 0x7f000001;
      por_s = u3_Host.sam_u.por_s;
    }
    {
      struct sockaddr_in add_u;

      if ( (0 == (pip_w >> 16)) && (1 == (pip_w >> 8)) ) {
        c3_y imp_y = (pip_w & 0xff);

        pip_w = _ames_czar(imp_y, &por_s);
      }

      if ( 0 != pip_w ) {
        uv_buf_t        buf_u = uv_buf_init((c3_c*)buf_y, len_w);
        _u3_udp_send_t* ruq_u = c3_malloc(sizeof(_u3_udp_send_t));

        ruq_u->buf_y = buf_y;

        memset(&add_u, 0, sizeof(add_u));
        add_u.sin_family = AF_INET;
        add_u.sin_addr.s_addr = htonl(pip_w);
        add_u.sin_port = htons(por_s);

        int ret;
        if ( 0 != (ret = uv_udp_send(&ruq_u->snd_u,
                                     &sam_u->wax_u,
                                     &buf_u, 1,
                                     (const struct sockaddr*) & add_u, // IS THIS RIGHT ?!?!?
                                     _ames_send_cb)) ) {
          uL(fprintf(uH, "ames: send: %s\n", uv_strerror(ret)));
        }
        // fprintf(stderr, "ames: send\r\n");
      }
    }
  }
  u3z(lan); u3z(pac);
}

/* _ames_time_cb(): timer callback.
*/
static void
_ames_time_cb(uv_timer_t* tim_uo)
{
  u3_ames* sam_u = &u3_Host.sam_u;
  u3_lo_open();

  sam_u->law_w = time(0);
  {
    u3_cv_plan
      (u3nt(u3_blip, c3__ames, u3_nul),
       u3nc(c3__wake, u3_nul));
  }
  u3_lo_shut(u3_no);
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
      struct sockaddr_in* add_u = (struct sockaddr_in *)adr_u;
      u3_noun             msg   = u3_ci_bytes((c3_w)nrd_i, (c3_y*)buf_u->base);
      c3_s                por_s = ntohs(add_u->sin_port);
      c3_w                pip_w = ntohl(add_u->sin_addr.s_addr);

      // fprintf(stderr, "ames: plan\r\n");
      u3_cv_plan
        (u3nt(u3_blip, c3__ames, u3_nul),
         u3nt(c3__hear,
              u3nq(c3__if, u3k(u3A->now), por_s, u3_ci_words(1, &pip_w)),
              msg));
    }
    _ames_free(buf_u->base);
    u3_lo_shut(u3_yes);
  }
}

/* u3_ames_io_init(): initialize ames I/O.
*/
void
u3_ames_io_init()
{
  u3_ames* sam_u = &u3_Host.sam_u;
  c3_s por_s;

  por_s = u3_Host.ops_u.por_s;
  if ( 0 != u3_Host.ops_u.imp_c ) {
    u3_noun imp   = u3_ci_string(u3_Host.ops_u.imp_c);
    u3_noun num   = u3_dc("slaw", 'p', imp);
    c3_y    num_y;

    if ( u3_no == u3du(num) ) {
      uL(fprintf(uH, "malformed emperor: %s\n", u3_Host.ops_u.imp_c));
      exit(1);
    }
    num_y = u3_cr_byte(0, u3t(num));

    _ames_czar(num_y, &por_s);
    uL(fprintf(uH, "ames: czar: %s on %d\n", u3_Host.ops_u.imp_c, por_s));
    u3z(num);
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
    add_u.sin_addr.s_addr = htonl(INADDR_ANY);
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
      exit(1);
    }

    uv_udp_getsockname(&sam_u->wax_u, (struct sockaddr *)&add_u, &add_i);
    c3_assert(add_u.sin_port);

    sam_u->por_s = ntohs(add_u.sin_port);
  }
  //  Timer too.
  {
    uv_timer_init(u3L, &sam_u->tim_u);
  }
}

/* u3_ames_io_talk(): start receiving ames traffic.
*/
void
u3_ames_io_talk()
{
  u3_ames* sam_u = &u3_Host.sam_u;

  uL(fprintf(uH, "ames: on localhost, UDP %d.\n", sam_u->por_s));
  uv_udp_recv_start(&sam_u->wax_u, _ames_alloc, _ames_recv_cb);
}

/* u3_ames_io_exit(): terminate ames I/O.
*/
void
u3_ames_io_exit()
{
  u3_ames* sam_u = &u3_Host.sam_u;

  uv_close((uv_handle_t*)&sam_u->wax_u, 0);
}

/* u3_ames_io_poll(): update ames IO state.
*/
void
u3_ames_io_poll()
{
  u3_ames* sam_u = &u3_Host.sam_u;
  u3_noun  wen = u3_cv_keep(u3nt(u3_blip, c3__ames, u3_nul));

  if ( (u3_nul != wen) &&
       (u3_yes == u3du(wen)) &&
       (u3_yes == u3ud(u3t(wen))) )
  {
    c3_d gap_d = u3_time_gap_ms(u3k(u3A->now), u3k(u3t(wen)));
    c3_w lem_w = (time(0) - sam_u->law_w);
    c3_w lef_w = (lem_w > 32) ? 0 : (32 - lem_w);

    gap_d = c3_min(gap_d, (c3_d)(1000 * lef_w));

    if ( u3_yes == sam_u->alm ) {
      uv_timer_stop(&sam_u->tim_u);
    }
    else sam_u->alm = u3_yes;

    uv_timer_start(&sam_u->tim_u, _ames_time_cb, gap_d, 0);
  }
  else {
    if ( u3_yes == sam_u->alm ) {
      uv_timer_stop(&sam_u->tim_u);
    }
    sam_u->alm = u3_no;
  }
  u3z(wen);
}
