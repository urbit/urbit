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
#include <ev.h>
#include <errno.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "v/vere.h"

static void _lo_ames(struct ev_loop *lup_u, struct ev_io* wax_u, c3_i rev_i)
  { u2_lo_call(u2_Host.arv_u, lup_u, wax_u, c3__ames, rev_i); }

static void _lo_amat(struct ev_loop *lup_u, struct ev_timer* tim_u, c3_i rev_i)
  { u2_lo_call(u2_Host.arv_u, lup_u, tim_u, c3__ames, rev_i); }

/* _ames_czar(): quasi-static route to emperor.
*/
static c3_w
_ames_czar(u2_reck* rec_u, c3_y imp_y, c3_s* por_s)
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

      sprintf(dns_c, "%s.urbit.org", nam_c + 1);
      uL(fprintf(uH, "czar %s, dns %s\n", nam_c, dns_c));
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
              struct sockaddr_in* add_k = (struct sockaddr_in *)rai_u->ai_addr;

              sam_u->imp_w[imp_y] = ntohl(add_k->sin_addr.s_addr);
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

/* u2_ames_ef_send(): send packet to network (v4).
*/
void
u2_ames_ef_send(u2_reck* rec_u,
                u2_noun  lan,
                u2_noun  pac)
{
  u2_ames* sam_u = &u2_Host.sam_u;
  c3_w     len_w = u2_cr_met(3, pac);
  c3_s     por_s;
  c3_w     pip_w;

  if ( u2_no == _ames_lane_ip(lan, &por_s, &pip_w) ) {
    //  u2z(lan);  ? XX
    return;
  } else {
    u2_apac* pac_u = malloc(sizeof(u2_apac) + len_w);

    u2_cr_bytes(0, len_w, pac_u->hun_y, pac);
    pac_u->nex_u = 0;
    pac_u->len_w = len_w;
    pac_u->pip_w = pip_w;
    pac_u->por_s = por_s;

    if ( sam_u->tou_u ) {
      sam_u->tou_u->nex_u = pac_u;
    } else {
      c3_assert(!sam_u->out_u);
      sam_u->out_u = pac_u;
    }
    sam_u->tou_u = pac_u;
  }
}

/* u2_ames_io_init(): initialize ames I/O.
*/
void 
u2_ames_io_init(u2_reck* rec_u)
{
  u2_ames* sam_u = &u2_Host.sam_u;

  {
    //  c3_w pip_w = 0x7f000001;    //  hardbound to localhost 
    c3_s por_s;
    c3_i fid_i;

    por_s = 0;
    if ( 0 != u2_Host.ops_u.imp_c ) {
      u2_noun imp   = u2_ci_string(u2_Host.ops_u.imp_c);
      u2_noun num   = u2_dc("slaw", 'p', imp);
      c3_y    num_y;

      if ( u2_no == u2du(num) ) {
        uL(fprintf(uH, "malformed emperor: %s\n", u2_Host.ops_u.imp_c));
        exit(1);
      }
      num_y = u2_cr_byte(0, u2t(num));
        
      _ames_czar(rec_u, num_y, &por_s);
      uL(fprintf(uH, "ames: czar: %s on %d\n", u2_Host.ops_u.imp_c, por_s));
    } 

    if ( (fid_i = socket(AF_INET, SOCK_DGRAM, 0)) < 0 ) {
      perror("ames: socket");
      c3_assert(0);
    }
    {
      c3_i opt_i = 1;

      if ( -1 == setsockopt(fid_i, SOL_SOCKET, SO_REUSEADDR, 
            &opt_i, sizeof opt_i) )
      {
        perror("http: setsockopt");
        c3_assert(0);
      }
    }

    //  Bind.
    {
      struct sockaddr_in add_k;
      socklen_t          add_t = sizeof(add_k);

      memset(&add_k, 0, sizeof(add_k));
      add_k.sin_family = AF_INET;
      add_k.sin_addr.s_addr = htonl(INADDR_ANY);
      add_k.sin_port = htons(por_s);

      {
        c3_w dum_w = 0;

        while ( 1 ) {
          if ( bind(fid_i, (struct sockaddr *)&add_k, sizeof(add_k)) < 0 ) {
            if ( EADDRINUSE == errno ) {
              if ( dum_w > 4 ) {
                fprintf(stderr, "bind: address in use\r\n");
                c3_assert(0);
              }
              else {
                sleep(1);
                continue;
              }
            }
            else {
              perror("bind");
              c3_assert(0);
            }
          }
          else break;
        }
      }

      getsockname(fid_i, (struct sockaddr *)&add_k, &add_t);
      c3_assert(add_k.sin_port);
      por_s = ntohs(add_k.sin_port);

      fprintf(stderr, "ames: on localhost, UDP %d.\n", por_s);

      //  sam_u->pip_w = pip_w;
      sam_u->por_s = por_s;
      sam_u->out_u = sam_u->tou_u = 0;

      ev_io_init(&sam_u->wax_u, _lo_ames, fid_i, 0);
    }

#if 0
    //  You are here.
    {
      // u2_noun pax = u2nt(c3__lead, c3__ames, u2_nul);
      u2_noun pax = u2nt(c3__gold, c3__ames, u2_nul);   //  XX no!
      u2_noun fav = u2nt(c3__home, 
                         u2k(rec_u->our),
                         u2nt(c3__if, por_s, u2_ci_words(1, &pip_w)));

      u2_reck_plan(rec_u, pax, fav);
    }
#endif

    //  Timer too.
    {
      ev_timer_init(&sam_u->tim_u, _lo_amat, 10000.0, 0.);
      sam_u->alm = u2_no;
    }
  }
}

/* u2_ames_io_exit(): terminate ames I/O.
*/
void 
u2_ames_io_exit(u2_reck* rec_u)
{
  u2_ames* sam_u = &u2_Host.sam_u;

  close(sam_u->wax_u.fd);
}

/* u2_ames_io_spin(): start ames server(s).
*/
void
u2_ames_io_spin(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_ames* sam_u = &u2_Host.sam_u;

  ev_io_start(lup_u, &sam_u->wax_u);
  if ( u2_yes == sam_u->alm ) {
    // uL(fprintf(uH, "timer start!\n"));
    ev_timer_start(lup_u, &sam_u->tim_u);
  }
}

/* u2_ames_io_stop(): stop ames servers.
*/
void
u2_ames_io_stop(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_ames* sam_u = &u2_Host.sam_u;

  ev_io_stop(lup_u, &sam_u->wax_u);

  if ( u2_yes == sam_u->alm ) {
    ev_timer_stop(lup_u, &sam_u->tim_u);
  }
}

/* u2_ames_io_poll(): update ames IO state.
*/
void
u2_ames_io_poll(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_ames* sam_u = &u2_Host.sam_u;
  c3_i     ver_i = 0;

  ver_i |= EV_READ;   //  no constraint on reading right now
  if ( sam_u->out_u  ) {
    ver_i |= EV_WRITE;
  }
  ev_io_set(&sam_u->wax_u, sam_u->wax_u.fd, ver_i);

  {
    u2_noun wen = u2_reck_keep(rec_u, u2nt(c3__gold, c3__ames, u2_nul));
    
    if ( (u2_nul != wen) && 
         (u2_yes == u2du(wen)) &&
         (u2_yes == u2ud(u2t(wen))) )
    {
      double gap_g = u2_time_gap_double(u2k(rec_u->now), u2k(u2t(wen)));

      sam_u->alm = u2_yes;
      ev_timer_set(&sam_u->tim_u, gap_g, 0.);
    }
    else {
      sam_u->alm = u2_no;
    }
  }
}

/* u2_ames_io_fuck(): output event on connection socket.
*/
void
u2_ames_io_fuck(u2_reck*      rec_u,
                struct ev_io* wax_u)
{
  u2_ames* sam_u = &u2_Host.sam_u;

  while ( 1 ) {
    u2_apac* pac_u = sam_u->out_u;
   
    if ( 0 == pac_u ) {
      break;
    }
    else {
      if ( 0 == pac_u->pip_w ) {
        pac_u->pip_w = 0x7f000001;
        pac_u->por_s = u2_Host.sam_u.por_s;
      }
      {
        struct sockaddr_in add_k;

        if ( (0 == (pac_u->pip_w >> 16)) &&
             (1 == (pac_u->pip_w >> 8)) ) {
          c3_y imp_y = (pac_u->pip_w & 0xff);

          pac_u->pip_w = _ames_czar(rec_u, imp_y, &pac_u->por_s);
        }

        if ( 0 != pac_u->pip_w ) {
          memset(&add_k, 0, sizeof(add_k));
          add_k.sin_family = AF_INET;
          add_k.sin_addr.s_addr = htonl(pac_u->pip_w);
          add_k.sin_port = htons(pac_u->por_s);

          if ( pac_u->len_w != sendto(sam_u->wax_u.fd,
                                      pac_u->hun_y,
                                      pac_u->len_w,
                                      0,
                                      (struct sockaddr*)(void *)&add_k,
                                      sizeof(add_k)) )
          {
            if ( EAGAIN != errno ) {
              uL(fprintf(uH, "send: to %x:%d; error %s\n", 
                    pac_u->pip_w, pac_u->por_s, strerror(errno)));
            }
          } 
        }
      }

      sam_u->out_u = sam_u->out_u->nex_u;

      if ( 0 == sam_u->out_u ) {
        c3_assert(pac_u == sam_u->tou_u);
        sam_u->tou_u = 0;
      }
      free(pac_u);
    }
  }
}

/* u2_ames_io_time(): time event on ames channel.
*/
void
u2_ames_io_time(u2_reck*         rec_u,
                struct ev_timer* tim_u)
{
  u2_reck_plan
    (rec_u,
     u2nt(c3__gold, c3__ames, u2_nul),
     u2nc(c3__wake, u2_nul));
}

/* u2_ames_io_suck(): input event on listen socket.
*/
void
u2_ames_io_suck(u2_reck*      rec_u,
                struct ev_io* wax_u)
{
  c3_y               buf_y[65536];
  c3_ws              len_ws;
  struct sockaddr_in add_k;
  socklen_t          add_t;
  c3_s               por_s;
  c3_w               pip_w;

//  while ( 1 ) {

    add_t = sizeof(struct sockaddr_in);
    if ( -1 == (len_ws = recvfrom(wax_u->fd, 
                                  buf_y, 
                                  65536, 
                                  MSG_WAITALL,
                                  (struct sockaddr *)&add_k,
                                  &add_t)) )
    {
      if ( EAGAIN != errno ) {
        uL(fprintf(uH, "ames: error %d\n", errno));
      }
      return;
    }
    // por_s = ntohs(add_k.sin_port);
    // pip_w = ntohl(add_k.sin_addr.s_addr);

    por_s = ntohs(add_k.sin_port);         //??
    pip_w = ntohl(add_k.sin_addr.s_addr);  //??

    // uL(fprintf(stderr, "por_s %d, pip_w %x\n", por_s, pip_w));

    u2_reck_plan
      (rec_u,
       u2nt(c3__gold, c3__ames, u2_nul),    //  XX no!
       // u2nt(c3__lead, c3__ames, u2_nul),
       u2nt(c3__hear, 
            u2nt(c3__if, por_s, u2_ci_words(1, &pip_w)),
            u2_ci_bytes((c3_w)len_ws, buf_y)));
 // }
}
