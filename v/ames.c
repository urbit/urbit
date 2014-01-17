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
static uv_buf_t
_ames_alloc(uv_handle_t* had_u, size_t len_i)
{
  void* ptr_v = malloc(len_i);

//  uL(fprintf(uH, "grab %p\n", ptr_v));
  return uv_buf_init(ptr_v, len_i);
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
  u2_ames* sam_u = &u2_Host.sam_u;

  if ( u2_yes == u2_Host.ops_u.loh ) {
    *por_s = 31337 + imp_y;
    return 0x7f000001;
  }
  else {
    *por_s = 13337 + imp_y;

    if ( 0xffffffff == sam_u->imp_w[imp_y] ) {
      return 0;
    }
    else if ( 0 == sam_u->imp_w[imp_y] ) {
      u2_noun nam   = u2_dc("scot", 'p', imp_y);
      c3_c*   nam_c = u2_cr_string(nam);
      c3_c    dns_c[64];

      snprintf(dns_c, 64, "%s.urbit.org", nam_c + 1);
      // uL(fprintf(uH, "czar %s, dns %s\n", nam_c, dns_c));
      free(nam_c);
      u2z(nam);

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
                u2_noun wad = u2_ci_words(1, &sam_u->imp_w[imp_y]);
                u2_noun nam = u2_dc("scot", c3__if, wad);
                c3_c*   nam_c = u2_cr_string(nam);

                uL(fprintf(uH, "ames: czar %s: ip %s\n", dns_c, nam_c));

                free(nam_c); u2z(nam);
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
u2_bean
_ames_lane_ip(u2_noun lan, c3_s* por_s, c3_w* pip_w)
{
  switch ( u2h(lan) ) {
    case c3__if: {
      *por_s= (c3_s) u2h(u2t(lan));
      *pip_w = u2_cr_word(0, u2t(u2t(lan)));

      return u2_yes;
    } break;
    case c3__is: {
      u2_noun pq_lan = u2h(u2t(u2t(lan)));

      if ( u2_nul == pq_lan ) return u2_no;
      else return _ames_lane_ip(u2t(pq_lan), por_s, pip_w);
    } break;
    case c3__ix: {
      *por_s = (c3_s) u2h(u2t(u2t(lan)));
      *pip_w = u2_cr_word(0, u2t(u2t(u2t(lan))));

      return u2_yes;
    } break;
  }
  return u2_no;
}

/* An unusual lameness in libuv.
*/
  typedef struct {
    uv_udp_send_t snd_u;
    c3_y*         buf_y;
  } _u2_udp_send_t;

/* _ames_send_cb(): send callback.
*/
static void
_ames_send_cb(uv_udp_send_t* req_u, c3_i sas_i)
{
  _u2_udp_send_t* ruq_u = (void*)req_u;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "ames: send_cb: %s\n", uv_strerror(uv_last_error(u2L))));
  }
  // fprintf(stderr, "ames: tx\r\n");
  free(ruq_u->buf_y);
  free(ruq_u);
}

/* u2_ames_ef_send(): send packet to network (v4).
*/
void
u2_ames_ef_send(u2_noun lan, u2_noun pac)
{
  u2_ames* sam_u = &u2_Host.sam_u;
  c3_s     por_s;
  c3_w     pip_w;

  if ( u2_Host.ops_u.fuz_w && ((rand() % 100) < u2_Host.ops_u.fuz_w) ) {
    u2z(pac);
    return;
  }

  if ( u2_yes == _ames_lane_ip(lan, &por_s, &pip_w) ) {
    c3_w     len_w = u2_cr_met(3, pac);
    c3_y*    buf_y = malloc(len_w);

    u2_cr_bytes(0, len_w, buf_y, pac);

    if ( 0 == pip_w ) {
      pip_w = 0x7f000001;
      por_s = u2_Host.sam_u.por_s;
    }
    {
      struct sockaddr_in add_u;

      if ( (0 == (pip_w >> 16)) && (1 == (pip_w >> 8)) ) {
        c3_y imp_y = (pip_w & 0xff);

        pip_w = _ames_czar(imp_y, &por_s);
      }

      if ( 0 != pip_w ) {
        uv_buf_t        buf_u = uv_buf_init((c3_c*)buf_y, len_w);
        _u2_udp_send_t* ruq_u = malloc(sizeof(_u2_udp_send_t));

        ruq_u->buf_y = buf_y;

        memset(&add_u, 0, sizeof(add_u));
        add_u.sin_family = AF_INET;
        add_u.sin_addr.s_addr = htonl(pip_w);
        add_u.sin_port = htons(por_s);

        if ( 0 != uv_udp_send(&ruq_u->snd_u,
                              &sam_u->wax_u,
                              &buf_u, 1,
                              add_u,
                              _ames_send_cb) ) {
          uL(fprintf(uH, "ames: send: %s\n", uv_strerror(uv_last_error(u2L))));
        }
        // fprintf(stderr, "ames: send\r\n");
      }
    }
  }
  u2z(lan); u2z(pac);
}

/* _ames_time_cb(): timer callback.
*/
static void
_ames_time_cb(uv_timer_t* tim_u, c3_i sas_i)
{
  u2_lo_open();
  {
    u2_reck_plan
      (u2A,
       u2nt(c3__gold, c3__ames, u2_nul),
       u2nc(c3__wake, u2_nul));
  }
  u2_lo_shut(u2_no);
}

/* _ames_recv_cb(): receive callback.
*/
static void
_ames_recv_cb(uv_udp_t*        wax_u,
              ssize_t          nrd_i,
              uv_buf_t         buf_u,
              struct sockaddr* adr_u,
              unsigned         flg_i)
{
  // uL(fprintf(uH, "ames: rx %p\r\n", buf_u.base));

  if ( 0 == nrd_i ) {
    _ames_free(buf_u.base);
  }
  else {
    u2_lo_open();
    {
      struct sockaddr_in* add_u = (struct sockaddr_in *)adr_u;
      u2_noun             msg   = u2_ci_bytes((c3_w)nrd_i, (c3_y*)buf_u.base);
      c3_s                por_s = ntohs(add_u->sin_port);
      c3_w                pip_w = ntohl(add_u->sin_addr.s_addr);

      // fprintf(stderr, "ames: plan\r\n");
      u2_reck_plan
        (u2A,
         u2nt(c3__gold, c3__ames, u2_nul),
         u2nt(c3__hear, u2nt(c3__if, por_s, u2_ci_words(1, &pip_w)), msg));
    }
    _ames_free(buf_u.base);
    u2_lo_shut(u2_yes);
  }
}

/* u2_ames_io_init(): initialize ames I/O.
*/
void
u2_ames_io_init()
{
  u2_ames* sam_u = &u2_Host.sam_u;
  c3_s por_s;

  por_s = u2_Host.ops_u.por_s;
  if ( 0 != u2_Host.ops_u.imp_c ) {
    u2_noun imp   = u2_ci_string(u2_Host.ops_u.imp_c);
    u2_noun num   = u2_dc("slaw", 'p', imp);
    c3_y    num_y;

    if ( u2_no == u2du(num) ) {
      uL(fprintf(uH, "malformed emperor: %s\n", u2_Host.ops_u.imp_c));
      exit(1);
    }
    num_y = u2_cr_byte(0, u2t(num));

    _ames_czar(num_y, &por_s);
    uL(fprintf(uH, "ames: czar: %s on %d\n", u2_Host.ops_u.imp_c, por_s));
    u2z(num);
  }

  if ( 0 != uv_udp_init(u2L, &u2_Host.sam_u.wax_u) ) {
    uL(fprintf(uH, "ames: init: %s\n", uv_strerror(uv_last_error(u2L))));
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

    if ( uv_udp_bind(&sam_u->wax_u, add_u, 0) != 0 ) {
      uL(fprintf(uH, "ames: bind: %s\n",
                     uv_strerror(uv_last_error(u2L))));
      c3_assert(0);
    }

    uv_udp_getsockname(&sam_u->wax_u, (struct sockaddr *)&add_u, &add_i);
    c3_assert(add_u.sin_port);

    sam_u->por_s = ntohs(add_u.sin_port);
  }

  //  Timer too.
  {
    uv_timer_init(u2L, &sam_u->tim_u);
  }
}

/* u2_ames_io_talk(): start receiving ames traffic.
*/
void
u2_ames_io_talk()
{
  u2_ames* sam_u = &u2_Host.sam_u;

  uL(fprintf(uH, "ames: on localhost, UDP %d.\n", sam_u->por_s));
  uv_udp_recv_start(&sam_u->wax_u, _ames_alloc, _ames_recv_cb);
}

/* u2_ames_io_exit(): terminate ames I/O.
*/
void
u2_ames_io_exit()
{
  u2_ames* sam_u = &u2_Host.sam_u;

  uv_close((uv_handle_t*)&sam_u->wax_u, 0);
}

/* u2_ames_io_poll(): update ames IO state.
*/
void
u2_ames_io_poll()
{
  u2_ames* sam_u = &u2_Host.sam_u;
  u2_noun  wen = u2_reck_keep(u2A, u2nt(c3__gold, c3__ames, u2_nul));

  if ( (u2_nul != wen) &&
       (u2_yes == u2du(wen)) &&
       (u2_yes == u2ud(u2t(wen))) )
  {
    c3_d gap_d = u2_time_gap_ms(u2k(u2A->now), u2k(u2t(wen)));

    if ( u2_yes == sam_u->alm ) {
      uv_timer_stop(&sam_u->tim_u);
    }
    else sam_u->alm = u2_yes;

    uv_timer_start(&sam_u->tim_u, _ames_time_cb, gap_d, 0);
  }
  else {
    if ( u2_yes == sam_u->alm ) {
      uv_timer_stop(&sam_u->tim_u);
    }
    sam_u->alm = u2_no;
  }
  u2z(wen);
}
