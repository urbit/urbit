/* v/raft.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <uv.h>

#include "all.h"
#include "v/vere.h"


typedef struct {
  c3_w             tem_w;                               //  Log entry term
  c3_w             typ_w;                               //  Entry type, %ra|%ov
  c3_w             len_w;                               //  Length of blob
  c3_w*            bob_w;                               //  Blob
} u2_rent;

typedef struct _u2_rmsg {
  c3_w             ver_w;                               //  version
  c3_d             len_d;                               //  Words in message
  c3_w             tem_w;                               //  Current term
  c3_w             typ_w;                               //  %apen|%revo|%rasp
  union {
    struct {
      c3_w         suc_w;                               //  Request successful
    } rasp;
    struct {
      c3_d         lai_d;                               //  Last log index
      c3_w         lat_w;                               //  Last log term
      c3_w         nam_w;                               //  Name world length
      c3_c*        nam_c;                               //  Requestor name
      union {
        struct {
          c3_d     cit_d;                               //  Leader commitIndex
          c3_d     ent_d;                               //  Number of entries
          u2_rent* ent_u;                               //  Entries
        } apen;
      };
    } rest;
  };
} u2_rmsg;


static ssize_t _raft_rmsg_read(const u2_rbuf* buf_u, u2_rmsg* msg_u);
static void _raft_rmsg_send(uv_stream_t* sem_u, const u2_rmsg* msg_u);
static void _raft_rmsg_free(u2_rmsg* msg_u);
static void _raft_conn_dead(u2_rcon* ron_u);
static u2_bean _raft_remove_run(u2_rcon* ron_u);
static void _raft_send_rasp(u2_rcon* ron_u, c3_t suc_t);
static void _raft_rreq_free(u2_rreq* req_u);
static void _raft_time_cb(uv_timer_t* tim_u, c3_i sas_i);

/* _raft_readname(): parse a raft host:port peer name.
*/
static u2_bean
_raft_readname(u2_ropt* rop_u, const c3_c* str_c, c3_w siz_w)
{
  u2_rnam* nam_u = malloc(sizeof(*nam_u));
  c3_c*    col_c;
  c3_w     nam_w;

  nam_u->ron_u = 0;
  nam_u->str_c = malloc(siz_w + 1);
  strncpy(nam_u->str_c, str_c, siz_w);
  nam_u->str_c[siz_w] = '\0';
  //fprintf(stderr, "raft: peer %s\n", nam_u->str_c);

  if ( 0 == (col_c = strchr(nam_u->str_c, ':')) ) {
    fprintf(stderr, "raft: invalid name %s\n", str_c);
    return u2_no;
  }
  else {
    nam_w = col_c - nam_u->str_c + 1;
    nam_u->nam_c = malloc(nam_w + 1);
    uv_strlcpy(nam_u->nam_c, nam_u->str_c, nam_w);

    nam_u->por_c = strdup(col_c + 1);
    if ( strlen(nam_u->por_c) > 5 ) {
      fprintf(stderr, "raft: invalid port %s\n", nam_u->por_c);
      return u2_no;
    }

    nam_u->nex_u = rop_u->nam_u;
    rop_u->nam_u = nam_u;
    return u2_yes;
  }
}

/* u2_raft_readopt(): parse a string into a list of raft peers.
*/
u2_bean
u2_raft_readopt(u2_ropt* rop_u, const c3_c* arg_c)
{
  c3_c* com_c;

  while ( 0 != (com_c = strchr(arg_c, ',')) ) {
    if ( u2_no == _raft_readname(rop_u, arg_c, com_c - arg_c) ) {
      return u2_no;
    } else arg_c = com_c + 1;
  }
  return _raft_readname(rop_u, arg_c, strlen(arg_c));
}

/* _raft_alloc(): libuv-style allocator for raft.
*/
static uv_buf_t
_raft_alloc(uv_handle_t* had_u, size_t siz_i)
{
  uv_buf_t buf_u = { .base = malloc(siz_i), .len = siz_i };
  return buf_u;
}

/* _raft_election_rand(): pseudorandom component of election timeout.
*/
static c3_w
_raft_election_rand()
{
  return ((float) rand() / RAND_MAX) * 150;
}

/* _raft_promote(): actions on raft leader election.
*/
static void
_raft_promote(u2_raft* raf_u)
{
  if ( u2_raty_lead == raf_u->typ_e ) {
    uL(fprintf(uH, "raft: double promote; ignoring\n"));
  }
  else {
    c3_i sas_i;

    uL(fprintf(uH, "raft: promoting to leader\n"));
    if ( 1 == raf_u->pop_w ) {
      raf_u->typ_e = u2_raty_lead;
    }
    else {
      c3_assert(u2_raty_cand == raf_u->typ_e);
      sas_i = uv_timer_stop(&raf_u->tim_u);
      c3_assert(0 == sas_i);

      raf_u->typ_e = u2_raty_lead;

      sas_i = uv_timer_start(&raf_u->tim_u, _raft_time_cb, 50, 0);
      c3_assert(0 == sas_i);
    }
  }

  /* TODO */
  if ( 1 == raf_u->pop_w ) {
    u2_sist_boot();
    if ( u2_no == u2_Host.ops_u.bat ) {
      u2_lo_lead(u2A);
    }
  }
}

/* _raft_demote(): demote to follower.
*/
static void
_raft_demote(u2_raft* raf_u)
{
  if ( u2_raty_lead == raf_u->typ_e ) {
    uL(fprintf(uH, "raft: demoting leader\n"));
    /* TODO just start dropping events */
    exit(1);
  }
  else {
    c3_assert(u2_raty_cand == raf_u->typ_e);
    uL(fprintf(uH, "raft: demoting to follower\n"));
    raf_u->vog_c = 0;
    raf_u->vot_w = 0;
    raf_u->typ_e = u2_raty_foll;
  }
}

/* _raft_update_term(): note an updated term.
*/
static void
_raft_update_term(u2_raft* raf_u, c3_w tem_w)
{
  if ( raf_u->tem_w < tem_w ) {
    uL(fprintf(uH, "raft: got term from network: %d\n", tem_w));
    raf_u->tem_w = tem_w;
    c3_assert(raf_u->typ_e != u2_raty_none);
    if ( raf_u->typ_e == u2_raty_foll ) {
      c3_assert(0 == raf_u->vot_w);
    } else _raft_demote(raf_u);
  }
}

/* _raft_rest_name(): update conn name from incoming request.
**
** If this connection already has a name, make sure the passed name
** matches.  Otherwise, try to associate it with a name, killing old
** connections to that name.
*/
static void
_raft_rest_name(u2_rcon* ron_u, const c3_c* nam_c)
{
  if ( 0 != ron_u->nam_u ) {
    if ( 0 != strcmp(ron_u->nam_u->str_c, nam_c) ) {
      uL(fprintf(uH, "raft: names disagree o:%s n:%s\n",
                     ron_u->nam_u->str_c, nam_c));
      _raft_conn_dead(ron_u);
    }
  }
  else {
    u2_raft* raf_u = ron_u->raf_u;
    u2_rnam* nam_u = raf_u->nam_u;

    while ( nam_u ) {
      if ( 0 == strcmp(nam_u->str_c, nam_c) ) {
        if ( nam_u->ron_u ) {
          c3_assert(nam_u->ron_u != ron_u);
          uL(fprintf(uH, "raft: closing existing conn to %s\n", nam_u->str_c));
          _raft_conn_dead(nam_u->ron_u);
        }
        nam_u->ron_u = ron_u;
        ron_u->nam_u = nam_u;
        _raft_remove_run(ron_u);
        break;
      }
      else nam_u = nam_u->nex_u;
    }
    if ( 0 == ron_u->nam_u ) {
      uL(fprintf(uH, "connection from unkown peer %s\n", nam_c));
      _raft_conn_dead(ron_u);
    }
  }
}

/* _raft_do_rest(): effects of an incoming request.
*/
static void
_raft_do_rest(u2_rcon* ron_u, const u2_rmsg* msg_u)
{
  u2_raft* raf_u = ron_u->raf_u;
  c3_i     sas_i;

  sas_i = uv_timer_stop(&raf_u->tim_u);
  c3_assert(0 == sas_i);
  sas_i = uv_timer_start(&raf_u->tim_u, &_raft_time_cb,
                         150 + _raft_election_rand(), 0);
  c3_assert(0 == sas_i);

  if ( msg_u->tem_w > raf_u->tem_w ) {
    uL(fprintf(uH, "raft: got term %d from network\n", msg_u->tem_w));
    raf_u->tem_w = msg_u->tem_w;
    raf_u->vog_c = 0;
    raf_u->vot_w = 0;
  }
  _raft_rest_name(ron_u, msg_u->rest.nam_c);
}

/* _raft_do_apen(): Handle incoming AppendEntries.
*/
static void
_raft_do_apen(u2_rcon* ron_u, const u2_rmsg* msg_u)
{
  c3_assert(c3__apen == msg_u->typ_w);
  _raft_do_rest(ron_u, msg_u);
  /* TODO respond */
}

/* _raft_apen_done(): process AppendEntries response.
*/
static void
_raft_apen_done(u2_rreq* req_u, c3_w suc_w)
{
  /* TODO */
}

/* _raft_do_revo(): Handle incoming RequestVote.
*/
static void
_raft_do_revo(u2_rcon* ron_u, const u2_rmsg* msg_u)
{
  u2_raft* raf_u = ron_u->raf_u;

  c3_assert(c3__revo == msg_u->typ_w);
  _raft_do_rest(ron_u, msg_u);

  c3_assert(0 != ron_u->nam_u);
  if ( msg_u->tem_w >= raf_u->tem_w                     &&
       (0 == raf_u->vog_c                             ||
        0 == strcmp(raf_u->vog_c, ron_u->nam_u->str_c)) &&
       (raf_u->lat_w < msg_u->rest.lat_w              ||
        (raf_u->lat_w == msg_u->rest.lat_w          &&
         raf_u->ent_w <= msg_u->rest.lai_d)) )
  {
    _raft_send_rasp(ron_u, 1);
  }
  else _raft_send_rasp(ron_u, 0);
}

/* _raft_revo_done(): process RequestVote response.
*/
static void
_raft_revo_done(u2_rreq* req_u, c3_w suc_w)
{
  u2_rcon* ron_u = req_u->ron_u;
  u2_raft* raf_u = ron_u->raf_u;

  if ( suc_w ) {
    raf_u->vot_w++;
  }
  if ( raf_u->vot_w > raf_u->pop_w / 2 ) {
    uL(fprintf(uH, "raft: got majority of %d for term %d\n",
                   raf_u->vot_w, raf_u->tem_w));
    _raft_promote(raf_u);
  }
}

/* _raft_do_rasp(): act on an incoming raft RPC response.
*/
static void
_raft_do_rasp(u2_rcon* ron_u, u2_rmsg* msg_u)
{
  u2_raft* raf_u = ron_u->raf_u;

  c3_assert(c3__rasp == msg_u->typ_w);
  if ( 0 == ron_u->nam_u ) {
    uL(fprintf(uH, "raft: invalid connection from unknown host\n"));
    _raft_conn_dead(ron_u);
  }
  else {
    u2_rreq* req_u = ron_u->out_u;

    uL(fprintf(uH, "raft: got response from %s\n", ron_u->nam_u->str_c));
    if ( !req_u ) {
      uL(fprintf(uH, "raft: no request found\n"));
      _raft_conn_dead(ron_u);
    }
    else {
      switch ( req_u->msg_u->typ_w ) {
        default: {
          uL(fprintf(uH, "raft: bogus request type %x?!\n",
                         req_u->msg_u->typ_w));
          c3_assert(0);
        }
        case c3__apen: {
          _raft_apen_done(req_u, msg_u->rasp.suc_w);
          break;
        }
        case c3__revo: {
          _raft_revo_done(req_u, msg_u->rasp.suc_w);
          break;
        }
      }

      _raft_update_term(raf_u, msg_u->tem_w);

      ron_u->out_u = req_u->nex_u;
      if ( 0 == req_u->nex_u ) {
        c3_assert(req_u == ron_u->tou_u);
        ron_u->tou_u = 0;
      }
      _raft_rreq_free(req_u);
    }
  }
}

/* _raft_rmsg_read(): read a u2_rmsg from a buffer.
**
** Returns <0 on parse failure.
** Returns 0 on partial data.
** Returns bytes read on successful read.
**
** If successful, caller must eventually call _raft_free_rmsg() on msg_u.
*/
static ssize_t
_raft_rmsg_read(const u2_rbuf* buf_u, u2_rmsg* msg_u)
{
  ssize_t red_i = 0;
  c3_d    ben_d;

  if ( buf_u->len_w < sizeof(c3_w) + sizeof(c3_d) ) {
    return 0;
  }
  memcpy(&msg_u->ver_w, buf_u->buf_y + red_i, sizeof(c3_w));
  red_i += sizeof(c3_w);
  if ( msg_u->ver_w != u2_cr_mug('a') ) {
    uL(fprintf(uH, "raft: versions don't match: %x %x\n",
                   msg_u->ver_w, u2_cr_mug('a')));
    return -1;
  }

  memcpy(&msg_u->len_d, buf_u->buf_y + red_i, sizeof(c3_d));
  red_i += sizeof(c3_d);

  if ( msg_u->len_d < 4 ) {
    uL(fprintf(uH, "raft: length too short (a) %lld\n", msg_u->len_d));
    return -1;
  }

  ben_d = 4ULL * msg_u->len_d;

  if ( buf_u->len_w < ben_d ) {
    return 0;
  }

  if ( ben_d < red_i + 2 * sizeof(c3_w) ) {
    uL(fprintf(uH, "raft: length too short (b) %lld\n", msg_u->len_d));
    return -1;
  }
  memcpy(&msg_u->tem_w, buf_u->buf_y + red_i, sizeof(c3_w));
  red_i += sizeof(c3_w);
  memcpy(&msg_u->typ_w, buf_u->buf_y + red_i, sizeof(c3_w));
  red_i += sizeof(c3_w);

  switch ( msg_u->typ_w ) {
    default: {
      uL(fprintf(uH, "raft: unknown msg type %x\n", msg_u->typ_w));
      return -1;
    }
    case c3__rasp: {
      if ( ben_d < red_i + sizeof(c3_w) ) {
        uL(fprintf(uH, "raft: length too short (c) %lld\n", msg_u->len_d));
        return -1;
      }
      memcpy(&msg_u->rasp.suc_w, buf_u->buf_y + red_i, sizeof(c3_w));
      red_i += sizeof(c3_w);
      break;
    }
    case c3__apen: case c3__revo: {
      if ( ben_d < red_i + sizeof(c3_d) + 2 * sizeof(c3_w) ) {
        uL(fprintf(uH, "raft: length too short (d) %lld\n", msg_u->len_d));
        return -1;
      }
      memcpy(&msg_u->rest.lai_d, buf_u->buf_y + red_i, sizeof(c3_d));
      red_i += sizeof(c3_d);
      memcpy(&msg_u->rest.lat_w, buf_u->buf_y + red_i, sizeof(c3_w));
      red_i += sizeof(c3_w);
      memcpy(&msg_u->rest.nam_w, buf_u->buf_y + red_i, sizeof(c3_w));
      red_i += sizeof(c3_w);

      if ( ben_d < red_i + 4 * msg_u->rest.nam_w ) {
        uL(fprintf(uH, "raft: length too short (e) %lld\n", msg_u->len_d));
        return -1;
      }
      msg_u->rest.nam_c = malloc(4 * msg_u->rest.nam_w);
      uv_strlcpy(msg_u->rest.nam_c, (const char*)(buf_u->buf_y + red_i),
                 4 * msg_u->rest.nam_w);
      red_i += 4 * msg_u->rest.nam_w;
      break;
    }
  }

  if ( c3__apen == msg_u->typ_w ) {
    if ( ben_d < red_i + 2 * sizeof(c3_d) ) {
      uL(fprintf(uH, "raft: length too short (f) %lld\n", msg_u->len_d));
      red_i = -1;
      goto fail;
    }
    memcpy(&msg_u->rest.apen.cit_d, buf_u->buf_y + red_i, sizeof(c3_d));
    red_i += sizeof(c3_d);
    memcpy(&msg_u->rest.apen.ent_d, buf_u->buf_y + red_i, sizeof(c3_d));
    red_i += sizeof(c3_d);

    msg_u->rest.apen.ent_u = calloc(
        1, msg_u->rest.apen.ent_d * sizeof(u2_rent));
    {
      c3_d     i_d;
      u2_rent* ent_u = msg_u->rest.apen.ent_u;

      for ( i_d = 0; i_d < msg_u->rest.apen.ent_d; i_d++ ) {
        if ( ben_d < red_i + 3 * sizeof(c3_w) ) {
          uL(fprintf(uH, "raft: length too short (g) %lld\n", msg_u->len_d));
          red_i = -1;
          goto fail;
        }
        memcpy(&ent_u[i_d].tem_w, buf_u->buf_y + red_i, sizeof(c3_w));
        red_i += sizeof(c3_w);
        memcpy(&ent_u[i_d].typ_w, buf_u->buf_y + red_i, sizeof(c3_w));
        red_i += sizeof(c3_w);
        memcpy(&ent_u[i_d].len_w, buf_u->buf_y + red_i, sizeof(c3_w));
        red_i += sizeof(c3_w);
        if ( ben_d < red_i + ent_u[i_d].len_w ) {
          uL(fprintf(uH, "raft: length too short (h) %lld\n", msg_u->len_d));
          red_i = -1;
          goto fail;
        }
        ent_u[i_d].bob_w = malloc(ent_u[i_d].len_w);
        memcpy(ent_u[i_d].bob_w, buf_u->buf_y + red_i, ent_u[i_d].len_w);
        red_i += ent_u[i_d].len_w;
      }
    }
  }

  if ( red_i != ben_d ) {
    uL(fprintf(uH, "raft: sizes don't match r:%ld w:%llu\n", red_i, ben_d));
    red_i = -1;
    goto fail;
  }

out:
  return red_i;
fail:
  _raft_rmsg_free(msg_u);
  goto out;
}

struct _u2_write_t {
  uv_write_t wri_u;
  c3_y*      buf_y;
};

static void
_raft_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  struct _u2_write_t* req_u = (struct _u2_write_t*)wri_u;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "raft: write_cb: %s\n",
                   uv_strerror(uv_last_error(u2L))));
    _raft_conn_dead((u2_rcon*)wri_u->handle);
  }
  free(req_u->buf_y);
  free(req_u);
}

static void
_raft_bytes_send(uv_stream_t* sem_u, const void* ptr_v, size_t siz_w)
{
  struct _u2_write_t* req_u = malloc(sizeof(*req_u));
  uv_buf_t            buf_u;

  req_u->buf_y = malloc(siz_w);
  memcpy(req_u->buf_y, ptr_v, siz_w);
  buf_u.base = (char*)req_u->buf_y;
  buf_u.len = siz_w;
  uv_write(&req_u->wri_u, sem_u, &buf_u, 1, _raft_write_cb);
}

/* _raft_rmsg_send(): send a u2_rmsg over the wire.
*/
static void
_raft_rmsg_send(uv_stream_t* sem_u, const u2_rmsg* msg_u)
{
  c3_d len_d = sizeof(c3_d) + 3 * sizeof(c3_w);

  _raft_bytes_send(sem_u, &msg_u->ver_w, sizeof(c3_w));
  _raft_bytes_send(sem_u, &msg_u->len_d, sizeof(c3_d));
  _raft_bytes_send(sem_u, &msg_u->tem_w, sizeof(c3_w));
  _raft_bytes_send(sem_u, &msg_u->typ_w, sizeof(c3_w));
  switch ( msg_u->typ_w ) {
    default: {
      uL(fprintf(uH, "raft: send: unknown message type\n"));
      c3_assert(0);
    }
    case c3__rasp: {
      len_d += sizeof(c3_w);
      _raft_bytes_send(sem_u, &msg_u->rasp.suc_w, sizeof(c3_w));
      break;
    }
    case c3__apen: case c3__revo: {
      len_d += sizeof(c3_d) + 2 * sizeof(c3_w) + 4 * msg_u->rest.nam_w;
      _raft_bytes_send(sem_u, &msg_u->rest.lai_d, sizeof(c3_d));
      _raft_bytes_send(sem_u, &msg_u->rest.lat_w, sizeof(c3_w));
      _raft_bytes_send(sem_u, &msg_u->rest.nam_w, sizeof(c3_w));
      _raft_bytes_send(sem_u, msg_u->rest.nam_c, 4 * msg_u->rest.nam_w);
      break;
    }
  }
  if ( c3__apen == msg_u->typ_w ) {
    c3_d     i_d;
    u2_rent* ent_u = msg_u->rest.apen.ent_u;

    len_d += 2 * sizeof(c3_d);
    _raft_bytes_send(sem_u, &msg_u->rest.apen.cit_d, sizeof(c3_d));
    _raft_bytes_send(sem_u, &msg_u->rest.apen.ent_d, sizeof(c3_d));
    for ( i_d = 0; i_d < msg_u->rest.apen.ent_d; i_d++ ) {
      len_d += 3 * sizeof(c3_w) + ent_u[i_d].len_w;
      _raft_bytes_send(sem_u, &ent_u[i_d].tem_w, sizeof(c3_w));
      _raft_bytes_send(sem_u, &ent_u[i_d].typ_w, sizeof(c3_w));
      _raft_bytes_send(sem_u, &ent_u[i_d].len_w, sizeof(c3_w));
      _raft_bytes_send(sem_u, ent_u[i_d].bob_w, ent_u[i_d].len_w);
    }
  }

  //uL(fprintf(uH, "raft: sent %llu (%llu) [%x]\n", len_d, msg_u->len_d, msg_u->typ_w));
}

static void
_raft_rmsg_free(u2_rmsg* msg_u) {
  if ( c3__apen == msg_u->typ_w && msg_u->rest.apen.ent_u ) {
    c3_d i_d;

    for ( i_d = 0; i_d < msg_u->rest.apen.ent_d; i_d++ ) {
      free(msg_u->rest.apen.ent_u[i_d].bob_w);
    }
    free(msg_u->rest.apen.ent_u);
    msg_u->rest.apen.ent_u = 0;
  }
  if ( c3__apen == msg_u->typ_w || c3__revo == msg_u->typ_w ) {
    free(msg_u->rest.nam_c);
    msg_u->rest.nam_c = 0;
  }
}

/* _raft_conn_work(): read and write requests and responses.
*/
static void
_raft_conn_work(u2_rcon* ron_u)
{
  if ( u2_yes == ron_u->red ) {
    c3_assert(ron_u->red_u);
    ron_u->red = u2_no;
    while (1) {
      u2_rmsg msg_u;
      ssize_t ret_i = _raft_rmsg_read(ron_u->red_u, &msg_u);

      if ( ret_i < 0 ) {
        if ( ron_u->nam_u ) {
          uL(fprintf(uH, "raft: conn_work: error reading from %s\n",
                         ron_u->nam_u->str_c));
        }
        else {
          uL(fprintf(uH, "raft: conn_work: error reading\n"));
        }
        _raft_conn_dead(ron_u);
        break;
      }
      else if ( ret_i == 0 ) {
        break;
      }
      else {
        if ( 4 * msg_u.len_d != ret_i ) {
          uL(fprintf(uH, "raft: conn_work: lengths don't match\n"));
          c3_assert(0);
        }
        else {
          c3_assert(ron_u->red_u->len_w >= ret_i);
          memmove(ron_u->red_u->buf_y,
                  ron_u->red_u->buf_y + ret_i,
                  ron_u->red_u->len_w - ret_i);
          ron_u->red_u->len_w -= ret_i;

          switch ( msg_u.typ_w ) {
            default: {
              uL(fprintf(uH, "raft: work: unknown message type %x\n",
                             msg_u.typ_w));
              break;
            }
            case c3__apen: {
              _raft_do_apen(ron_u, &msg_u);
              break;
            }
            case c3__revo: {
              _raft_do_revo(ron_u, &msg_u);
              break;
            }
            case c3__rasp: {
              _raft_do_rasp(ron_u, &msg_u);
              break;
            }
          }
          /* TODO move msg_u onto rreq, free it there */
          _raft_rmsg_free(&msg_u);
        }
      }
    }
  }
}

/* _raft_conn_grow(): append buffer to raft read state.
*/
static void
_raft_conn_grow(u2_rcon* ron_u, c3_y* buf_y, ssize_t siz_i)
{
  u2_rbuf* red_u = ron_u->red_u;

  c3_assert(siz_i > 0);
  if ( !red_u ) {
    red_u = malloc(sizeof(*red_u) + siz_i);
    red_u->len_w = 0;
    red_u->cap_w = siz_i;
  }

  if ( red_u->cap_w < red_u->len_w + siz_i ) {
    c3_w cap_w = c3_max(2 * red_u->cap_w,
                        red_u->len_w + siz_i);

    red_u = realloc(red_u, sizeof(*red_u) + cap_w);
    red_u->cap_w = cap_w;
  }

  memcpy(red_u->buf_y + red_u->len_w, buf_y, siz_i);
  red_u->len_w += siz_i;

  ron_u->red_u = red_u;
}

/* _raft_conn_read_cb(): generic connection read callback.
*/
static void
_raft_conn_read_cb(uv_stream_t* tcp_u,
                   ssize_t      siz_i,
                   uv_buf_t     buf_u)
{
  u2_rcon* ron_u = (u2_rcon*)tcp_u;

  u2_lo_open();
  {
    if ( siz_i < 0 ) {
      uv_err_t las_u = uv_last_error(u2L);

      if ( UV_EOF != las_u.code ) {
        uL(fprintf(uH, "raft: read: %s\n", uv_strerror(las_u)));
      }
      _raft_conn_dead(ron_u);
    }
    else if ( siz_i == 0 ) {
      //  do nothing
    }
    else {
      _raft_conn_grow(ron_u, (c3_y*)buf_u.base, siz_i);
      ron_u->red = u2_yes;
      _raft_conn_work(ron_u);
    }
  }
  free(buf_u.base);
  u2_lo_shut(u2_no);
}

/* _raft_conn_new(): allocate a new raft connection.
*/
static u2_rcon*
_raft_conn_new(u2_raft* raf_u)
{
  u2_rcon* ron_u = malloc(sizeof(*ron_u));

  uv_tcp_init(u2L, &ron_u->wax_u);
  ron_u->red_u = 0;
  ron_u->out_u = ron_u->tou_u = 0;
  ron_u->red_u = 0;
  ron_u->red = u2_no;
  ron_u->nam_u = 0;
  ron_u->raf_u = raf_u;
  ron_u->nex_u = 0;

  return ron_u;
}

/* _raft_remove_run(): remove a connection from the list of unknowns.
*/
static u2_bean
_raft_remove_run(u2_rcon* ron_u)
{
  u2_raft* raf_u = ron_u->raf_u;
  u2_bean  suc = u2_no;

  if ( raf_u->run_u == ron_u ) {
    raf_u->run_u = ron_u->nex_u;
    suc = u2_yes;
  }
  else {
    u2_rcon* pre_u = raf_u->run_u;

    while ( pre_u ) {
      if ( pre_u->nex_u == ron_u ) {
        pre_u->nex_u = ron_u->nex_u;
        suc = u2_yes;
        break;
      }
      else pre_u = pre_u->nex_u;
    }
  }

  return suc;
}

static u2_rreq*
_raft_rreq_new(u2_rcon* ron_u)
{
  u2_rreq* req_u = malloc(sizeof(*req_u));

  req_u->msg_u = malloc(sizeof(*req_u->msg_u));
  req_u->nex_u = 0;
  req_u->ron_u = ron_u;
  if ( ron_u->tou_u ) {
    c3_assert(ron_u->out_u);
    ron_u->tou_u->nex_u = req_u;
    ron_u->tou_u = req_u;
  }
  else {
    c3_assert(0 == ron_u->out_u);
    ron_u->tou_u = ron_u->out_u = req_u;
  }
  return req_u;
}

static void
_raft_rreq_free(u2_rreq* req_u)
{
  _raft_rmsg_free(req_u->msg_u);
  free(req_u->msg_u);   //  XX
  free(req_u);
}

/* _raft_conn_free(): unlink a connection and free its resources.
*/
static void
_raft_conn_free(uv_handle_t* had_u)
{
  u2_rcon* ron_u = (void*)had_u;

  //uL(fprintf(uH, "raft: conn_free %p\n", ron_u));
  {
    u2_rreq* req_u = ron_u->out_u;

    if ( 0 == req_u ) {
      c3_assert(0 == ron_u->tou_u);
    }
    else {
      while ( req_u ) {
        if ( 0 == req_u->nex_u ) {
          c3_assert(req_u == ron_u->tou_u);
        }
        ron_u->out_u = req_u->nex_u;
        _raft_rreq_free(req_u);
        req_u = ron_u->out_u;
      }
    }
  }
  free(ron_u->red_u);
  free(ron_u);
}

/* _raft_conn_dead(): kill a connection.
*/
static void
_raft_conn_dead(u2_rcon* ron_u)
{
  //uL(fprintf(uH, "raft: conn_dead %p\n", ron_u));
  uv_read_stop((uv_stream_t*)&ron_u->wax_u);
  if ( ron_u->nam_u ) {
    c3_assert(u2_no == _raft_remove_run(ron_u));
    c3_assert(ron_u->nam_u->ron_u == ron_u);
    ron_u->nam_u->ron_u = 0;
  }
  else {
    u2_bean suc = _raft_remove_run(ron_u);
    c3_assert(u2_yes == suc);
  }
  uv_close((uv_handle_t*)&ron_u->wax_u, _raft_conn_free);
}

/* _raft_listen_cb(): generic listen callback.
*/
static void
_raft_listen_cb(uv_stream_t* str_u, c3_i sas_i)
{
  u2_raft* raf_u = (u2_raft*)str_u;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "raft: listen_cb: %s\n",
                   uv_strerror(uv_last_error(u2L))));
  }
  else {
    u2_rcon* ron_u = _raft_conn_new(raf_u);

    if ( 0 != uv_accept((uv_stream_t*)&raf_u->wax_u,
                        (uv_stream_t*)&ron_u->wax_u) )
    {
      uL(fprintf(uH, "raft: accept: %s\n",
                     uv_strerror(uv_last_error(u2L))));

      uv_close((uv_handle_t*)&ron_u->wax_u, 0);
      free(ron_u);
    }
    else {
      uv_read_start((uv_stream_t*)&ron_u->wax_u,
                    _raft_alloc,
                    _raft_conn_read_cb);

      ron_u->nex_u = raf_u->run_u;
      raf_u->run_u = ron_u;
    }
  }
}

/* _raft_connect_cb(): generic connection callback.
*/
static void
_raft_connect_cb(uv_connect_t* con_u, c3_i sas_i)
{
  u2_rcon* ron_u = con_u->data;
  free(con_u);

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "raft: connect_cb: %s\n",
                   uv_strerror(uv_last_error(u2L))));
    _raft_conn_dead(ron_u);
  }
  else {
    uv_read_start((uv_stream_t*)&ron_u->wax_u,
                  _raft_alloc,
                  _raft_conn_read_cb);

    _raft_conn_work(ron_u);
  }
}

/* _raft_getaddrinfo_cb(): generic getaddrinfo callback.
*/
static void
_raft_getaddrinfo_cb(uv_getaddrinfo_t* raq_u,
                     c3_i              sas_i,
                     struct addrinfo*  add_u)
{
  struct addrinfo* res_u;
  uv_connect_t*    con_u = malloc(sizeof(*con_u));
  u2_rcon*         ron_u = raq_u->data;

  //uL(fprintf(uH, "getaddrinfo_cb %s\n", ron_u->nam_u->nam_c));

  con_u->data = ron_u;
  for ( res_u = add_u; res_u; res_u = res_u->ai_next ) {
    if ( 0 != uv_tcp_connect(con_u,
                             &ron_u->wax_u,
                             *(struct sockaddr_in*)res_u->ai_addr,
                             _raft_connect_cb) )
    {
      uL(fprintf(uH, "raft: getaddrinfo_cb: %s\n",
                     uv_strerror(uv_last_error(u2L))));
      uv_close((uv_handle_t*)&ron_u->wax_u, 0);
      continue;
    }
    else {
      c3_c  add_c[17] = {'\0'};

      uv_ip4_name((struct sockaddr_in*)res_u->ai_addr, add_c, 16);
      uL(fprintf(uH, "raft: conn %s\n", add_c));
      break;                                            //  Found one
    }
  }
  if ( !res_u ) {
    uL(fprintf(uH, "raft: getaddrinfo_cb: no address matched\n"));
    _raft_conn_free((uv_handle_t*)&ron_u->wax_u);
    free(con_u);
  }
  uv_freeaddrinfo(add_u);
  free(raq_u);
}

/* _raft_conn_all(): ensure that we are connected to each peer.
*/
static void
_raft_conn_all(u2_raft* raf_u, void (*con_f)(u2_rcon* ron_u))
{
  u2_rnam* nam_u = raf_u->nam_u;
  u2_rcon* ron_u;

  while ( nam_u ) {
    if ( !nam_u->ron_u ) {
      struct addrinfo   hit_u;
      uv_getaddrinfo_t* raq_u = malloc(sizeof(*raq_u));

      //uL(fprintf(uH, "raft: new conn to %s (%s)\n",
      //               nam_u->nam_c, nam_u->por_c));

      memset(&hit_u, 0, sizeof(hit_u));
      hit_u.ai_family = AF_INET;
      hit_u.ai_socktype = SOCK_STREAM;
      hit_u.ai_protocol = IPPROTO_TCP;

      ron_u = _raft_conn_new(raf_u);

      raq_u->data = ron_u;

      if ( 0 != uv_getaddrinfo(u2L,
                               raq_u,
                               _raft_getaddrinfo_cb,
                               nam_u->nam_c,
                               nam_u->por_c,
                               &hit_u) )
      {
        uL(fprintf(uH, "raft: getaddrinfo: %s\n",
                       uv_strerror(uv_last_error(u2L))));

        uv_close((uv_handle_t*)&ron_u->wax_u, 0);
        free(raq_u);
        free(ron_u);
        c3_assert(0);
      }
      else {
        ron_u->nam_u = nam_u;
        nam_u->ron_u = ron_u;
      }
    }
    else {
      //uL(fprintf(uH, "raft: existing connection %p for %s\n",
      //               nam_u->ron_u, nam_u->str_c));
    }
    con_f(nam_u->ron_u);
    nam_u = nam_u->nex_u;
  }
}

/* _raft_send_rasp(): send a response to a peer.
*/
static void
_raft_send_rasp(u2_rcon* ron_u, c3_t suc_t)
{
  u2_raft* raf_u = ron_u->raf_u;
  u2_rmsg  msg_u;
  msg_u.ver_w = u2_cr_mug('a');
  msg_u.len_d = 6;
  msg_u.tem_w = raf_u->tem_w;
  msg_u.typ_w = c3__rasp;
  msg_u.rasp.suc_w = suc_t;

  _raft_rmsg_send((uv_stream_t*)&ron_u->wax_u, &msg_u);
}

/* _raft_send_beat(): send a heartbeat (empty AppendEntries) to peer.
*/
static void
_raft_send_beat(u2_rcon* ron_u)
{
  u2_rreq* req_u = _raft_rreq_new(ron_u);
  u2_rmsg* msg_u = req_u->msg_u;
  u2_raft* raf_u = ron_u->raf_u;

  c3_assert(ron_u->nam_u);
  msg_u->ver_w = u2_cr_mug('a');
  msg_u->tem_w = raf_u->tem_w;
  msg_u->typ_w = c3__apen;
  msg_u->rest.lai_d = 0;   //  XX
  msg_u->rest.lat_w = 0;   //  XX
  msg_u->rest.nam_w = 1 + strlen(raf_u->str_c) / 4;
  msg_u->rest.nam_c = calloc(1, 4 * msg_u->rest.nam_w);
  uv_strlcpy(msg_u->rest.nam_c, raf_u->str_c, 4 * msg_u->rest.nam_w);
  msg_u->rest.apen.cit_d = 0;   //  XX
  msg_u->rest.apen.ent_d = 0;
  msg_u->rest.apen.ent_u = 0;
  msg_u->len_d = 13 + msg_u->rest.nam_w;

  _raft_rmsg_send((uv_stream_t*)&ron_u->wax_u, msg_u);
}

/* _raft_send_revo(): send a RequestVote to peer.
*/
static void
_raft_send_revo(u2_rcon* ron_u)
{
  u2_rreq* req_u = _raft_rreq_new(ron_u);
  u2_rmsg* msg_u = req_u->msg_u;
  u2_raft* raf_u = ron_u->raf_u;

  c3_assert(ron_u->nam_u);
  msg_u->ver_w = u2_cr_mug('a');
  msg_u->tem_w = raf_u->tem_w;
  msg_u->typ_w = c3__revo;
  msg_u->rest.lai_d = 0;   //  XX
  msg_u->rest.lat_w = 0;   //  XX
  msg_u->rest.nam_w = 1 + strlen(raf_u->str_c) / 4;
  msg_u->rest.nam_c = malloc(4 * msg_u->rest.nam_w);
  uv_strlcpy(msg_u->rest.nam_c, raf_u->str_c, 4 * msg_u->rest.nam_w);
  msg_u->len_d = 9 + msg_u->rest.nam_w;

  _raft_rmsg_send((uv_stream_t*)&ron_u->wax_u, msg_u);
}

/* _raft_start_election(): bump term, vote for self, solicit votes from peers.
*/
static void
_raft_start_election(u2_raft* raf_u)
{
  raf_u->tem_w++;
  uL(fprintf(uH, "raft: starting election [tem:%d]\n", raf_u->tem_w));

  raf_u->vot_w = 1;
  raf_u->vog_c = raf_u->str_c;

  _raft_conn_all(raf_u, _raft_send_revo);
}

/* _raft_heartbeat(): send a heartbeat to all peers.
*/
static void
_raft_heartbeat(u2_raft* raf_u)
{
  _raft_conn_all(raf_u, _raft_send_beat);
}

/* _raft_time_cb(): generic timer callback.
**
** Called on election timeouts for non-leaders, and at heartbeat interval for
** leaders.
*/
static void
_raft_time_cb(uv_timer_t* tim_u, c3_i sas_i)
{
  u2_raft* raf_u = tim_u->data;
  //uL(fprintf(uH, "raft: time\n"));

  c3_assert(sas_i == 0);
  switch ( raf_u->typ_e ) {
    default: {
      uL(fprintf(uH, "raft: time_cb: unknown server state\n"));
      c3_assert(0);
    }
    case u2_raty_foll: {
      raf_u->typ_e = u2_raty_cand;
      // continue to cand
    }
    case u2_raty_cand: {
      sas_i = uv_timer_start(tim_u, _raft_time_cb,
                             150 + _raft_election_rand(), 0);
      c3_assert(sas_i == 0);
      _raft_start_election(raf_u);
      break;
    }
    case u2_raty_lead: {
      sas_i = uv_timer_start(tim_u, _raft_time_cb, 50, 0);
      c3_assert(sas_i == 0);
      _raft_heartbeat(raf_u);
      break;
    }
  }
}

/* _raft_foll_init(): begin, follower mode.
*/
static void
_raft_foll_init(u2_raft* raf_u)
{
  uL(fprintf(uH, "raft: starting follower\n"));

  raf_u->typ_e = u2_raty_foll;

  if ( 0 != uv_tcp_init(u2L, &raf_u->wax_u) ) {
    uL(fprintf(uH, "raft: init: %s\n", uv_strerror(uv_last_error(u2L))));
    c3_assert(0);
  }

  // Bind the listener.
  {
    struct sockaddr_in add_u = uv_ip4_addr(
        "0.0.0.0", u2_Host.ops_u.rop_u.por_s);

    if ( 0 != uv_tcp_bind(&raf_u->wax_u, add_u) ) {
      uL(fprintf(uH, "raft: bind: %s\n", uv_strerror(uv_last_error(u2L))));
      c3_assert(0);
    }
    else {
      if ( 0 != uv_listen((uv_stream_t*)&raf_u->wax_u, 16, _raft_listen_cb) ) {
        uL(fprintf(uH, "raft: listen: %s\n", uv_strerror(uv_last_error(u2L))));
        c3_assert(0);
      }
      else {
        uL(fprintf(uH, "raft: on TCP %d\n", u2_Host.ops_u.rop_u.por_s));
      }
    }
  }

  // Start the initial election timeout.
  uv_timer_start(&raf_u->tim_u, _raft_time_cb, _raft_election_rand(), 0);
}

/* _raft_lone_init(): begin, single-instance mode.
*/
static void
_raft_lone_init(u2_raft* raf_u)
{
  uL(fprintf(uH, "raft: single-instance mode\n"));

  _raft_promote(raf_u);
}

/* u2_raft_init(): start Raft process.
*/
void
u2_raft_init()
{
  u2_raft* raf_u = u2R;
  c3_i     wri_i, siz_i;

  raf_u->nam_u = u2_Host.ops_u.rop_u.nam_u;

  {
    u2_rnam* nam_u = raf_u->nam_u;

    raf_u->pop_w = 1;
    while ( nam_u ) {
      raf_u->pop_w++;
      nam_u = nam_u->nex_u;
    }
  }

  siz_i = strlen(u2_Host.ops_u.nam_c) + strlen(":65536") + 1;
  raf_u->str_c = malloc(siz_i);
  wri_i = snprintf(raf_u->str_c, siz_i, "%s:%d",
                   u2_Host.ops_u.nam_c, u2_Host.ops_u.rop_u.por_s);
  c3_assert(wri_i < siz_i);

  uv_timer_init(u2L, &raf_u->tim_u);
  raf_u->tim_u.data = raf_u;

  if ( 0 == u2_Host.ops_u.rop_u.por_s ) {
    _raft_lone_init(raf_u);
  }
  else {
    _raft_foll_init(raf_u);
  }
}

/* _raft_sure(): apply and save an input ovum and its result.
*/
static void
_raft_sure(u2_reck* rec_u, u2_noun ovo, u2_noun vir, u2_noun cor)
{
  //  Whatever worked, save it.  (XX - should be concurrent with execute.)
  //  We'd like more events that don't change the state but need work here.
  {
    u2_mug(cor);
    u2_mug(rec_u->roc);

    if ( u2_no == u2_sing(cor, rec_u->roc) ) {
      rec_u->roe = u2nc(u2nc(vir, ovo), rec_u->roe);

      u2z(rec_u->roc);
      rec_u->roc = cor;
    }
    else {
      u2z(ovo);
      rec_u->roe = u2nc(u2nc(vir, u2_nul), rec_u->roe);

      u2z(cor);
    }
  }
}

/* _raft_lame(): handle an application failure.
*/
static void
_raft_lame(u2_reck* rec_u, u2_noun ovo, u2_noun why, u2_noun tan)
{
  u2_noun bov, gon;

#if 1
  {
    c3_c* oik_c = u2_cr_string(u2h(u2t(ovo)));

    // uL(fprintf(uH, "lame: %s\n", oik_c));
    free(oik_c);
  }
#endif

  //  Formal error in a network packet generates a hole card.
  //
  //  There should be a separate path for crypto failures,
  //  to prevent timing attacks, but isn't right now.  To deal
  //  with a crypto failure, just drop the packet.
  //
  if ( (c3__exit == why) && (c3__hear == u2h(u2t(ovo))) ) {
    u2_lo_punt(2, u2_ckb_flop(u2k(tan)));

    bov = u2nc(u2k(u2h(ovo)), u2nc(c3__hole, u2k(u2t(u2t(ovo)))));
    u2z(why);
  }
  else {
    bov = u2nc(u2k(u2h(ovo)), u2nt(c3__crud, why, u2k(tan)));
    u2_hevn_at(lad) = u2_nul;
  }
  // u2_lo_show("data", u2k(u2t(u2t(ovo))));

  u2z(ovo);

  gon = u2_lo_soft(rec_u, 0, u2_reck_poke, u2k(bov));
  if ( u2_blip == u2h(gon) ) {
    _raft_sure(rec_u, bov, u2k(u2h(u2t(gon))), u2k(u2t(u2t(gon))));

    u2z(gon);
  }
  else {
    u2z(gon);
    {
      u2_noun vab = u2nc(u2k(u2h(bov)),
                         u2nc(c3__warn, u2_ci_tape("crude crash!")));
      u2_noun nog = u2_lo_soft(rec_u, 0, u2_reck_poke, u2k(vab));

      if ( u2_blip == u2h(nog) ) {
        _raft_sure(rec_u, vab, u2k(u2h(u2t(nog))), u2k(u2t(u2t(nog))));
        u2z(nog);
      }
      else {
        u2z(nog);
        u2z(vab);

        uL(fprintf(uH, "crude: all delivery failed!\n"));
      }
    }
  }
}

/* _raft_punk(): insert and apply an input ovum (unprotected).
*/
static void
_raft_punk(u2_reck* rec_u, u2_noun ovo)
{
  // c3_c* txt_c = u2_cr_string(u2h(u2t(ovo)));
  c3_w sec_w;
  // static c3_w num_w;
  u2_noun gon;

  // uL(fprintf(uH, "punk: %s: %d\n", u2_cr_string(u2h(u2t(ovo))), num_w++));

  //  XX this is wrong - the timer should be on the original hose.
  //
  if ( (c3__term == u2h(u2t(u2h(ovo)))) ||
       (c3__batz == u2h(u2t(u2h(ovo)))) ) {
    sec_w = 0;
  } else sec_w = 60;

  //  Control alarm loops.
  //
  if ( c3__wake != u2h(u2t(ovo)) ) {
    u2_Host.beh_u.run_w = 0;
  }

  gon = u2_lo_soft(rec_u, sec_w, u2_reck_poke, u2k(ovo));

  if ( u2_blip != u2h(gon) ) {
    u2_noun why = u2k(u2h(gon));
    u2_noun tan = u2k(u2t(gon));

    u2z(gon);
    _raft_lame(rec_u, ovo, why, tan);
  }
  else {
    u2_noun vir = u2k(u2h(u2t(gon)));
    u2_noun cor = u2k(u2t(u2t(gon)));
    u2_noun nug;

    u2z(gon);
    nug = u2_reck_nick(rec_u, vir, cor);

    if ( u2_blip != u2h(nug) ) {
      u2_noun why = u2k(u2h(nug));
      u2_noun tan = u2k(u2t(nug));

      u2z(nug);
      _raft_lame(rec_u, ovo, why, tan);
    }
    else {
      vir = u2k(u2h(u2t(nug)));
      cor = u2k(u2t(u2t(nug)));

      u2z(nug);
      _raft_sure(rec_u, ovo, vir, cor);
    }
  }
  // uL(fprintf(uH, "punk oot %s\n", txt_c));
}

static void
_raft_comm(u2_reck* rec_u, c3_w bid_w)
{
  u2_cart* egg_u;

  u2_lo_open();

  egg_u = rec_u->ova.egg_u;
  while ( egg_u ) {
    if ( egg_u->ent_w <= bid_w ) {
      egg_u->did = u2_yes;
      egg_u->cit = u2_yes;
    } else break;
    egg_u = egg_u->nex_u;
  }
  u2_lo_shut(u2_yes);
}

static void
_raft_comm_cb(uv_timer_t* tim_u, c3_i sas_i)
{
  u2_raft* raf_u = tim_u->data;

  _raft_comm(u2A, raf_u->ent_w);
}

static c3_w
_raft_push(u2_raft* raf_u, c3_w* bob_w, c3_w len_w)
{
  c3_assert(raf_u->typ_e == u2_raty_lead);
  c3_assert(0 != bob_w && 0 < len_w);

  if ( 0 == u2_Host.ops_u.rop_u.por_s ) {
    raf_u->ent_w = u2_sist_pack(u2A, c3__ov, bob_w, len_w);
    raf_u->lat_w = raf_u->tem_w;  //  XX

    if ( !uv_is_active((uv_handle_t*)&raf_u->tim_u) ) {
      uv_timer_start(&raf_u->tim_u, _raft_comm_cb, 0, 0);
    }

    return raf_u->ent_w;
  }
  else {
    uL(fprintf(uH, "raft: multi-instance push\n"));
    c3_assert(0);
  }
}

/* _raft_kick_all(): kick a list of events, transferring.
*/
static void
_raft_kick_all(u2_reck* rec_u, u2_noun vir)
{
  while ( u2_nul != vir ) {
    u2_noun ovo = u2k(u2h(vir));
    u2_noun nex = u2k(u2t(vir));
    u2z(vir); vir = nex;

    u2_reck_kick(rec_u, ovo);
  }
}

/* u2_raft_work(): work in rec_u.
*/
void
u2_raft_work(u2_reck* rec_u)
{
  if ( u2R->typ_e != u2_raty_lead ) {
    c3_assert(rec_u->ova.egg_u == 0);
    if ( u2_nul != rec_u->roe ) {
      uL(fprintf(uH, "raft: dropping roe!!\n"));
      u2z(rec_u->roe);
      rec_u->roe = u2_nul;
    }
  }
  else {
    u2_cart* egg_u;
    u2_noun  ova;
    u2_noun  vir;
    u2_noun  nex;

    //  Apply effects from just-committed events, and delete finished events.
    //
    while ( rec_u->ova.egg_u ) {
      egg_u = rec_u->ova.egg_u;

      if ( u2_yes == egg_u->did ) {
        vir = egg_u->vir;

        if ( egg_u == rec_u->ova.geg_u ) {
          c3_assert(egg_u->nex_u == 0);
          rec_u->ova.geg_u = rec_u->ova.egg_u = 0;
          free(egg_u);
        }
        else {
          c3_assert(egg_u->nex_u != 0);
          rec_u->ova.egg_u = egg_u->nex_u;
          free(egg_u);
        }

        if ( u2_yes == egg_u->cit ) {
          _raft_kick_all(rec_u, vir);
        }
        else {
          //  We poked an event, but Raft failed to persist it.
          //  TODO: gracefully recover.
          uL(fprintf(uH, "vere: event executed but not persisted\n"));
          c3_assert(0);
        }
      }
      else break;
    }

    //  Poke pending events, leaving the poked events and errors on rec_u->roe.
    //
    {
      if ( 0 == u2R->lug_u.len_d ) {
        return;
      }
      ova = u2_ckb_flop(rec_u->roe);
      rec_u->roe = u2_nul;

      while ( u2_nul != ova ) {
        _raft_punk(rec_u, u2k(u2t(u2h(ova))));
        c3_assert(u2_nul == u2h(u2h(ova)));

        nex = u2k(u2t(ova));
        u2z(ova); ova = nex;
      }
    }

    //  Cartify, jam, and encrypt this batch of events. Take a number, Raft will
    //  be with you shortly.
    {
      c3_w    bid_w;
      c3_w    len_w;
      c3_w*   bob_w;
      u2_noun ron;
      u2_noun ovo;

      ova = u2_ckb_flop(rec_u->roe);
      rec_u->roe = u2_nul;

      while ( u2_nul != ova ) {
        ovo = u2k(u2t(u2h(ova)));
        vir = u2k(u2h(u2h(ova)));
        nex = u2k(u2t(ova));
        u2z(ova); ova = nex;

        if ( u2_nul != ovo ) {
          egg_u = malloc(sizeof(*egg_u));
          egg_u->nex_u = 0;
          egg_u->cit = u2_no;
          egg_u->did = u2_no;
          egg_u->vir = vir;

          ron = u2_cke_jam(u2nc(u2k(rec_u->now), ovo));
          c3_assert(rec_u->key);
          ron = u2_dc("en:crya", u2k(rec_u->key), ron);

          len_w = u2_cr_met(5, ron);
          bob_w = malloc(len_w * 4L);
          u2_cr_words(0, len_w, bob_w, ron);
          u2z(ron);

          bid_w = _raft_push(u2R, bob_w, len_w);
          egg_u->ent_w = bid_w;

          if ( 0 == rec_u->ova.geg_u ) {
            c3_assert(0 == rec_u->ova.egg_u);
            rec_u->ova.geg_u = rec_u->ova.egg_u = egg_u;
          }
          else {
            c3_assert(0 == rec_u->ova.geg_u->nex_u);
            rec_u->ova.geg_u->nex_u = egg_u;
            rec_u->ova.geg_u = egg_u;
          }
        }
        else {
          _raft_kick_all(rec_u, vir);
        }
      }
    }
  }
}
