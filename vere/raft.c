/* vere/raft.c
**
*/
#include <uv.h>
#include <sys/stat.h>

#include "all.h"
#include "vere/vere.h"


/* u3_rent: Log entry wire format.
*/
typedef struct {
  c3_w             tem_w;                               //  Log entry term
  c3_w             typ_w;                               //  Entry type, %ra|%ov
  c3_w             len_w;                               //  Word length of blob
  c3_w*            bob_w;                               //  Blob
} u3_rent;

/* u3_rmsg: Raft RPC wire format.
*/
typedef struct _u3_rmsg {
  c3_w             ver_w;                               //  version, mug('a')...
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
      c3_w         nam_w;                               //  Name word length
      c3_c*        nam_c;                               //  Requestor name
      union {
        struct {
          c3_d     cit_d;                               //  Leader commitIndex
          c3_d     ent_d;                               //  Number of entries
          u3_rent* ent_u;                               //  Entries
        } apen;
      };
    } rest;
  };
} u3_rmsg;


static ssize_t _raft_rmsg_read(const u3_rbuf* buf_u, u3_rmsg* msg_u);
static void _raft_rmsg_send(u3_rcon* ron_u, const u3_rmsg* msg_u);
static void _raft_rmsg_free(u3_rmsg* msg_u);
static void _raft_conn_dead(u3_rcon* ron_u);
static u3_noun _raft_remove_run(u3_rcon* ron_u);
static void _raft_send_rasp(u3_rcon* ron_u, c3_t suc_t);
static void _raft_rreq_free(u3_rreq* req_u);
static void _raft_time_cb(uv_timer_t* tim_u);

static void
_raft_rnam_free(u3_rnam* nam_u)
{
  if ( nam_u ) {
    c3_assert(0 == nam_u->ron_u);
    c3_assert(0 == nam_u->nex_u);
    free(nam_u->str_c);
    free(nam_u->nam_c);
    free(nam_u->por_c);
  }
  free(nam_u);
}

/* _raft_readname(): parse a raft host:port peer name.
*/
static u3_rnam*
_raft_readname(const c3_c* str_c, c3_w siz_w)
{
  u3_rnam* nam_u = c3_calloc(sizeof(*nam_u));
  c3_c*    col_c;
  c3_w     nam_w;

  nam_u->str_c = c3_malloc(siz_w + 1);
  strncpy(nam_u->str_c, str_c, siz_w + 1);
  nam_u->str_c[siz_w] = '\0';

  if ( 0 == (col_c = strchr(nam_u->str_c, ':')) ) {
    uL(fprintf(uH, "raft: invalid name %s\n", nam_u->str_c));
    _raft_rnam_free(nam_u);
    nam_u = 0;
  }
  else {
    nam_w = col_c - nam_u->str_c + 1;
    nam_u->nam_c = c3_malloc(nam_w);
    strncpy(nam_u->nam_c, nam_u->str_c, nam_w + 1);
    *(nam_u->nam_c + nam_w) = 0;
    nam_u->por_c = strdup(col_c + 1);
  }
  return nam_u;
}

/* u3_raft_readopt(): parse a string into a list of raft peers.
*/
u3_rnam*
u3_raft_readopt(const c3_c* arg_c, c3_c* our_c, c3_s oup_s)
{
  u3_rnam* nam_u;
  u3_rnam* nex_u;
  c3_c*    com_c;

  if ( 0 == (com_c = strchr(arg_c, ',')) ) {
    nam_u = _raft_readname(arg_c, strlen(arg_c));
    nex_u = 0;
  }
  else {
    nam_u = _raft_readname(arg_c, com_c - arg_c);
    nex_u = u3_raft_readopt(com_c + 1, our_c, oup_s);
  }

  if ( nam_u ) {
    c3_c* end_c;
    c3_w  por_w = strtoul(nam_u->por_c, &end_c, 10);

    if ( '\0' == *nam_u->por_c || '\0' != *end_c || por_w >= 65536 ) {
      uL(fprintf(uH, "raft: invalid port %s\n", nam_u->por_c));
      _raft_rnam_free(nam_u);
      _raft_rnam_free(nex_u);
      nam_u = 0;
    }
    else {
      if ( oup_s == por_w && 0 == strcmp(our_c, nam_u->nam_c) ) {
        _raft_rnam_free(nam_u);
        nam_u = nex_u;
      }
      else nam_u->nex_u = nex_u;
    }
  }
  else _raft_rnam_free(nex_u);
  return nam_u;
}

/* _raft_alloc(): libuv-style allocator for raft.
*/
static void
_raft_alloc(uv_handle_t* had_u, 
            size_t len_i,
            uv_buf_t* buf
            )
{
  void* ptr_v = c3_malloc(len_i);
  *buf = uv_buf_init(ptr_v, len_i);
}


/* _raft_election_rand(): election timeout.
*/
static c3_w
_raft_election_rand()
{
  c3_w ret = (1.0 + (float) rand() / RAND_MAX) * 150;
  //uL(fprintf(uH, "raft: timeout %d\n", ret));
  return ret;
}

/* _raft_promote(): actions on raft leader election.
*/
static void
_raft_promote(u3_raft* raf_u)
{
  if ( u3_raty_lead == raf_u->typ_e ) {
    uL(fprintf(uH, "raft: double promote; ignoring\n"));
  }
  else {
    c3_i sas_i;

    if ( 1 == raf_u->pop_w ) {
      //  uL(fprintf(uH, "raft:      -> lead\n"));
      raf_u->typ_e = u3_raty_lead;
      //  TODO boot in multiuser mode
      u3_sist_boot();
      if ( c3n == u3_Host.ops_u.bat ) {
        u3_lo_lead();
        u3_raft_work();
      }
    }
    else {
      c3_assert(u3_raty_cand == raf_u->typ_e);
      uL(fprintf(uH, "raft: cand -> lead\n"));
      raf_u->typ_e = u3_raty_lead;

      sas_i = uv_timer_stop(&raf_u->tim_u);
      c3_assert(0 == sas_i);
      sas_i = uv_timer_start(&raf_u->tim_u, _raft_time_cb, 50, 50);
      c3_assert(0 == sas_i);
    }
  }
}

/* _raft_demote(): demote to follower.
*/
static void
_raft_demote(u3_raft* raf_u)
{
  u3_raty typ_e = raf_u->typ_e;

  raf_u->vog_c = 0;
  u3_sist_nil("vote");
  raf_u->vot_w = 0;
  raf_u->typ_e = u3_raty_foll;

  if ( u3_raty_lead == typ_e ) {
    c3_i sas_i;

    uL(fprintf(uH, "raft: lead -> foll\n"));
    sas_i = uv_timer_stop(&raf_u->tim_u);
    c3_assert(0 == sas_i);
    sas_i = uv_timer_start(&raf_u->tim_u, _raft_time_cb,
                           _raft_election_rand(), 0);
    c3_assert(0 == sas_i);
    //  TODO dump not-yet-committed events
  }
  else {
    c3_assert(u3_raty_cand == typ_e);
    uL(fprintf(uH, "raft: cand -> foll\n"));
  }
}

/* _raft_note_term(): note a term from the network, demoting if it is newer.
*/
static void
_raft_note_term(u3_raft* raf_u, c3_w tem_w)
{
  if ( raf_u->tem_w < tem_w ) {
    uL(fprintf(uH, "raft: got term from network: %d\n", tem_w));
    raf_u->tem_w = tem_w;
    u3_sist_put("term", (c3_y*)&raf_u->tem_w, sizeof(c3_w));
    c3_assert(raf_u->typ_e != u3_raty_none);
    if ( raf_u->typ_e == u3_raty_foll ) {
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
static void  //  TODO indicate whether conn died
_raft_rest_name(u3_rcon* ron_u, const c3_c* nam_c)
{
  if ( 0 != ron_u->nam_u ) {
    if ( 0 != strcmp(ron_u->nam_u->str_c, nam_c) ) {
      uL(fprintf(uH, "raft: names disagree o:%s n:%s\n",
                     ron_u->nam_u->str_c, nam_c));
      _raft_conn_dead(ron_u);
    }
  }
  else {
    u3_raft* raf_u = ron_u->raf_u;
    u3_rnam* nam_u = raf_u->nam_u;

    while ( nam_u ) {
      if ( 0 == strcmp(nam_u->str_c, nam_c) ) {
        if ( nam_u->ron_u ) {
          c3_assert(nam_u->ron_u != ron_u);
          //uL(fprintf(uH, "raft: closing old conn %p to %s (%p)\n",
          //               nam_u->ron_u, nam_u->str_c, ron_u));
          _raft_conn_dead(nam_u->ron_u);
        }
        uL(fprintf(uH, "raft: incoming conn from %s\n", nam_u->str_c));
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
_raft_do_rest(u3_rcon* ron_u, const u3_rmsg* msg_u)
{
  u3_raft* raf_u = ron_u->raf_u;

  if ( u3_raty_cand == raf_u->typ_e || u3_raty_foll == raf_u->typ_e ) {
    c3_i sas_i;

    sas_i = uv_timer_stop(&raf_u->tim_u);
    c3_assert(0 == sas_i);
    sas_i = uv_timer_start(&raf_u->tim_u, _raft_time_cb,
                           _raft_election_rand(), 0);
    c3_assert(0 == sas_i);
  }

  _raft_rest_name(ron_u, msg_u->rest.nam_c);
  _raft_note_term(raf_u, msg_u->tem_w);
}

/* _raft_do_apen(): Handle incoming AppendEntries.
*/
static void
_raft_do_apen(u3_rcon* ron_u, const u3_rmsg* msg_u)
{
  c3_assert(c3__apen == msg_u->typ_w);
  _raft_do_rest(ron_u, msg_u);
  /* TODO respond */
}

/* _raft_apen_done(): process AppendEntries response.
*/
static void
_raft_apen_done(u3_rreq* req_u, c3_w suc_w)
{
  c3_assert(c3__apen == req_u->msg_u->typ_w);
  /* TODO */
}

/* _raft_do_revo(): Handle incoming RequestVote.
*/
static void
_raft_do_revo(u3_rcon* ron_u, const u3_rmsg* msg_u)
{
  u3_raft* raf_u = ron_u->raf_u;

  c3_assert(c3__revo == msg_u->typ_w);
  _raft_do_rest(ron_u, msg_u);

  c3_assert(0 != ron_u->nam_u);
  if ( msg_u->tem_w >= raf_u->tem_w                     &&
       (0 == raf_u->vog_c                             ||
        0 == strcmp(raf_u->vog_c, ron_u->nam_u->str_c)) &&
       (raf_u->lat_w < msg_u->rest.lat_w              ||
        (raf_u->lat_w == msg_u->rest.lat_w          &&
         raf_u->ent_d <= msg_u->rest.lai_d)) )
  {
    raf_u->vog_c = ron_u->nam_u->str_c;
    u3_sist_put("vote", (c3_y*)raf_u->vog_c, strlen(raf_u->vog_c));
    uL(fprintf(uH, "raft: granting vote to %s\n", raf_u->vog_c));
    _raft_send_rasp(ron_u, 1);
  }
  else _raft_send_rasp(ron_u, 0);
}

/* _raft_revo_done(): process RequestVote response.
*/
static void
_raft_revo_done(u3_rreq* req_u, c3_w suc_w)
{
  u3_rcon* ron_u = req_u->ron_u;
  u3_raft* raf_u = ron_u->raf_u;

  c3_assert(c3__revo == req_u->msg_u->typ_w);
  if ( suc_w && req_u->msg_u->tem_w == raf_u->tem_w ) {
    if ( c3n == ron_u->nam_u->vog ) {
      ron_u->nam_u->vog = c3y;
      raf_u->vot_w++;
    }
    else {
      uL(fprintf(uH, "XX raft: duplicate response for %s [tem:%d]\n",
                     ron_u->nam_u->str_c, raf_u->tem_w));
    }
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
_raft_do_rasp(u3_rcon* ron_u, u3_rmsg* msg_u)
{
  u3_raft* raf_u = ron_u->raf_u;

  c3_assert(c3__rasp == msg_u->typ_w);
  if ( 0 == ron_u->nam_u ) {
    uL(fprintf(uH, "raft: invalid connection from unknown host\n"));
    _raft_conn_dead(ron_u);
  }
  else {
    u3_rreq* req_u = ron_u->out_u;

    if ( !req_u ) {
      uL(fprintf(uH, "raft: response with no request from %s\n",
                     ron_u->nam_u->str_c));
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

      _raft_note_term(raf_u, msg_u->tem_w);

      ron_u->out_u = req_u->nex_u;
      if ( 0 == req_u->nex_u ) {
        c3_assert(req_u == ron_u->tou_u);
        ron_u->tou_u = 0;
      }
      _raft_rreq_free(req_u);
    }
  }
}

/* _raft_rmsg_read(): read a u3_rmsg from a buffer.
**
** Returns <0 on parse failure.
** Returns 0 on partial data.
** Returns bytes read on successful read.
**
** If successful, caller must eventually call _raft_free_rmsg() on msg_u.
*/
static ssize_t
_raft_rmsg_read(const u3_rbuf* buf_u, u3_rmsg* msg_u)
{
  ssize_t red_i = 0;
  c3_d    ben_d;

  if ( buf_u->len_w < sizeof(c3_w) + sizeof(c3_d) ) {
    return 0;
  }
  memcpy(&msg_u->ver_w, buf_u->buf_y + red_i, sizeof(c3_w));
  red_i += sizeof(c3_w);
  if ( msg_u->ver_w != u3r_mug('a') ) {
    uL(fprintf(uH, "raft: versions don't match: %x %x\n",
                   msg_u->ver_w, u3r_mug('a')));
    return -1;
  }

  memcpy(&msg_u->len_d, buf_u->buf_y + red_i, sizeof(c3_d));
  red_i += sizeof(c3_d);

  if ( msg_u->len_d < 4 ) {
    uL(fprintf(uH, "raft: length too short (a) %" PRIu64 "\n", msg_u->len_d));
    return -1;
  }

  ben_d = 4ULL * msg_u->len_d;

  if ( buf_u->len_w < ben_d ) {
    return 0;
  }

  if ( ben_d < red_i + 2 * sizeof(c3_w) ) {
    uL(fprintf(uH, "raft: length too short (b) %" PRIu64 "\n", msg_u->len_d));
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
        uL(fprintf(uH, "raft: length too short (c) %" PRIu64 "\n", msg_u->len_d));
        return -1;
      }
      memcpy(&msg_u->rasp.suc_w, buf_u->buf_y + red_i, sizeof(c3_w));
      red_i += sizeof(c3_w);
      break;
    }
    case c3__apen: case c3__revo: {
      if ( ben_d < red_i + sizeof(c3_d) + 2 * sizeof(c3_w) ) {
        uL(fprintf(uH, "raft: length too short (d) %" PRIu64 "\n", msg_u->len_d));
        return -1;
      }
      memcpy(&msg_u->rest.lai_d, buf_u->buf_y + red_i, sizeof(c3_d));
      red_i += sizeof(c3_d);
      memcpy(&msg_u->rest.lat_w, buf_u->buf_y + red_i, sizeof(c3_w));
      red_i += sizeof(c3_w);
      memcpy(&msg_u->rest.nam_w, buf_u->buf_y + red_i, sizeof(c3_w));
      red_i += sizeof(c3_w);

      if ( ben_d < red_i + 4 * msg_u->rest.nam_w ) {
        uL(fprintf(uH, "raft: length too short (e) %" PRIu64 "\n", msg_u->len_d));
        return -1;
      }
      msg_u->rest.nam_c = c3_malloc(4 * msg_u->rest.nam_w);
      strncpy(msg_u->rest.nam_c, (const char*)(buf_u->buf_y + red_i), 4 * msg_u->rest.nam_w + 1);
      red_i += 4 * msg_u->rest.nam_w;
      break;
    }
  }

  if ( c3__apen == msg_u->typ_w ) {
    if ( ben_d < red_i + 2 * sizeof(c3_d) ) {
      uL(fprintf(uH, "raft: length too short (f) %" PRIu64 "\n", msg_u->len_d));
      red_i = -1;
      goto fail;
    }
    memcpy(&msg_u->rest.apen.cit_d, buf_u->buf_y + red_i, sizeof(c3_d));
    red_i += sizeof(c3_d);
    memcpy(&msg_u->rest.apen.ent_d, buf_u->buf_y + red_i, sizeof(c3_d));
    red_i += sizeof(c3_d);

    msg_u->rest.apen.ent_u = c3_calloc(
        msg_u->rest.apen.ent_d * sizeof(u3_rent));
    {
      c3_d     i_d;
      u3_rent* ent_u = msg_u->rest.apen.ent_u;

      for ( i_d = 0; i_d < msg_u->rest.apen.ent_d; i_d++ ) {
        if ( ben_d < red_i + 3 * sizeof(c3_w) ) {
          uL(fprintf(uH, "raft: length too short (g) %" PRIu64 "\n", msg_u->len_d));
          red_i = -1;
          goto fail;
        }
        memcpy(&ent_u[i_d].tem_w, buf_u->buf_y + red_i, sizeof(c3_w));
        red_i += sizeof(c3_w);
        memcpy(&ent_u[i_d].typ_w, buf_u->buf_y + red_i, sizeof(c3_w));
        red_i += sizeof(c3_w);
        memcpy(&ent_u[i_d].len_w, buf_u->buf_y + red_i, sizeof(c3_w));
        red_i += sizeof(c3_w);
        if ( ben_d < red_i + 4 * ent_u[i_d].len_w ) {
          uL(fprintf(uH, "raft: length too short (h) %" PRIu64 "\n", msg_u->len_d));
          red_i = -1;
          goto fail;
        }
        ent_u[i_d].bob_w = c3_malloc(4 * ent_u[i_d].len_w);
        memcpy(ent_u[i_d].bob_w, buf_u->buf_y + red_i, 4 * ent_u[i_d].len_w);
        red_i += 4 * ent_u[i_d].len_w;
      }
    }
  }

  if ( red_i != ben_d ) {
    uL(fprintf(uH, "raft: sizes don't match r:%zd w:%" PRIu64 "\n", red_i, ben_d));
    red_i = -1;
    goto fail;
  }

out:
  return red_i;
fail:
  _raft_rmsg_free(msg_u);
  goto out;
}

/* _raft_rbuf_grow(): append data to the buffer, reallocating if needed.
**
** Returns new buffer location, as realloc.
*/
static u3_rbuf*
_raft_rbuf_grow(u3_rbuf* buf_u, const c3_y* buf_y, size_t siz_i)
{
  if ( 0 == buf_u ) {
    buf_u = c3_malloc(sizeof(*buf_u) + siz_i);
    buf_u->len_w = 0;
    buf_u->cap_w = siz_i;
  }

  if ( buf_u->cap_w < buf_u->len_w + siz_i ) {
    c3_w cap_w = c3_max(2 * buf_u->cap_w, buf_u->len_w + siz_i);

    buf_u = realloc(buf_u, sizeof(*buf_u) + cap_w);
    buf_u->cap_w = cap_w;
  }

  memcpy(buf_u->buf_y + buf_u->len_w, buf_y, siz_i);
  buf_u->len_w += siz_i;
  return buf_u;
}

/* _raft_bytes_send():
*/
static void
_raft_bytes_send(u3_rcon* ron_u, const void* ptr_v, size_t siz_i)
{
  ron_u->wri_u = _raft_rbuf_grow(ron_u->wri_u, ptr_v, siz_i);
}

/* _raft_rmsg_send(): send a u3_rmsg over the wire.
*/
static void
_raft_rmsg_send(u3_rcon* ron_u, const u3_rmsg* msg_u)
{
  c3_d len_d = sizeof(c3_d) + 3 * sizeof(c3_w);

  _raft_bytes_send(ron_u, &msg_u->ver_w, sizeof(c3_w));
  _raft_bytes_send(ron_u, &msg_u->len_d, sizeof(c3_d));
  _raft_bytes_send(ron_u, &msg_u->tem_w, sizeof(c3_w));
  _raft_bytes_send(ron_u, &msg_u->typ_w, sizeof(c3_w));
  switch ( msg_u->typ_w ) {
    default: {
      uL(fprintf(uH, "raft: send: unknown message type\n"));
      c3_assert(0);
    }
    case c3__rasp: {
      len_d += sizeof(c3_w);
      _raft_bytes_send(ron_u, &msg_u->rasp.suc_w, sizeof(c3_w));
      break;
    }
    case c3__apen: case c3__revo: {
      len_d += sizeof(c3_d) + 2 * sizeof(c3_w) + 4 * msg_u->rest.nam_w;
      _raft_bytes_send(ron_u, &msg_u->rest.lai_d, sizeof(c3_d));
      _raft_bytes_send(ron_u, &msg_u->rest.lat_w, sizeof(c3_w));
      _raft_bytes_send(ron_u, &msg_u->rest.nam_w, sizeof(c3_w));
      _raft_bytes_send(ron_u, msg_u->rest.nam_c, 4 * msg_u->rest.nam_w);
      break;
    }
  }
  if ( c3__apen == msg_u->typ_w ) {
    c3_d     i_d;
    u3_rent* ent_u = msg_u->rest.apen.ent_u;

    len_d += 2 * sizeof(c3_d);
    _raft_bytes_send(ron_u, &msg_u->rest.apen.cit_d, sizeof(c3_d));
    _raft_bytes_send(ron_u, &msg_u->rest.apen.ent_d, sizeof(c3_d));
    for ( i_d = 0; i_d < msg_u->rest.apen.ent_d; i_d++ ) {
      len_d += 3 * sizeof(c3_w) + ent_u[i_d].len_w;
      _raft_bytes_send(ron_u, &ent_u[i_d].tem_w, sizeof(c3_w));
      _raft_bytes_send(ron_u, &ent_u[i_d].typ_w, sizeof(c3_w));
      _raft_bytes_send(ron_u, &ent_u[i_d].len_w, sizeof(c3_w));
      _raft_bytes_send(ron_u, ent_u[i_d].bob_w, ent_u[i_d].len_w);
    }
  }

  //uL(fprintf(uH, "raft: sent %" PRIu64 " (%" PRIu64 ") [%x]\n",
  //               len_d, msg_u->len_d, msg_u->typ_w));
  c3_assert(len_d == 4 * msg_u->len_d);
}

/* _raft_rmsg_free(): free a u3_rmsg's resources (but not the msg itself).
*/
static void
_raft_rmsg_free(u3_rmsg* msg_u) {
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

/* An unusual lameness in libuv.
*/
struct _u3_write_t {
  uv_write_t wri_u;
  c3_y*      buf_y;
};

/* _raft_write_cb(): generic write callback.
*/
static void
_raft_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  struct _u3_write_t* req_u = (struct _u3_write_t*)wri_u;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "raft: write_cb: error\n"));
    _raft_conn_dead((u3_rcon*)wri_u->handle);
  }
  free(req_u->buf_y);
  free(req_u);
}

/* _raft_conn_work(): read and write requests and responses.
*/
static void
_raft_conn_work(u3_rcon* ron_u)
{
  c3_assert(c3y == ron_u->liv);
  if ( c3y == ron_u->red ) {
    c3_assert(ron_u->red_u);
    ron_u->red = c3n;
    while (1) {
      u3_rmsg msg_u;
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
          _raft_rmsg_free(&msg_u);
        }
      }
    }
  }

  if ( ron_u->wri_u && ron_u->wri_u->len_w > 0 ) {
    uv_buf_t            buf_u;
    struct _u3_write_t* req_u = c3_malloc(sizeof(*req_u));


    req_u->buf_y = c3_malloc(ron_u->wri_u->len_w);
    memcpy(req_u->buf_y, ron_u->wri_u->buf_y, ron_u->wri_u->len_w);
    buf_u.base = (char*)req_u->buf_y;
    buf_u.len = ron_u->wri_u->len_w;

    c3_w ret_w;
    if ( 0 != (ret_w = uv_write((uv_write_t*)req_u,
                                (uv_stream_t*)&ron_u->wax_u,
                                &buf_u,
                                1,
                                _raft_write_cb)) )
    {
      uL(fprintf(uH, "raft: conn_work (write): %s\n",
                     uv_strerror(ret_w)));
      free(req_u->buf_y);
      free(req_u);
    }
    else {
      ron_u->wri_u->len_w = 0;
    }
  }
}

/* _raft_conn_read_cb(): generic connection read callback.
*/
static void
_raft_conn_read_cb(uv_stream_t* tcp_u,
                   ssize_t      siz_i,
                   const uv_buf_t *     buf_u)
{
  u3_rcon* ron_u = (u3_rcon*)tcp_u;

  u3_lo_open();
  {
    if ( siz_i < 0 ) {
      uL(fprintf(uH, "raft: read ERROR"));
      _raft_conn_dead(ron_u);
    }
    else if ( siz_i == 0 ) {
      //  do nothing
    }
    else {
      if ( c3y == ron_u->liv ) {
        ron_u->red_u = _raft_rbuf_grow(ron_u->red_u, (c3_y*)buf_u->base, siz_i);
        ron_u->red = c3y;
        _raft_conn_work(ron_u);
      }
      else uL(fprintf(uH, "XX raft: read on dead conn %p\n", ron_u));
    }
  }
  free(buf_u->base);
  u3_lo_shut(c3n);
}

/* _raft_conn_new(): allocate a new raft connection.
*/
static u3_rcon*
_raft_conn_new(u3_raft* raf_u)
{
  u3_rcon* ron_u = c3_malloc(sizeof(*ron_u));

  uv_tcp_init(u3L, &ron_u->wax_u);
  ron_u->red_u = 0;
  ron_u->out_u = ron_u->tou_u = 0;
  ron_u->red_u = 0;
  ron_u->red = c3n;
  ron_u->wri_u = 0;
  ron_u->nam_u = 0;
  ron_u->raf_u = raf_u;
  ron_u->nex_u = 0;
  ron_u->liv = c3n;

  return ron_u;
}

/* _raft_remove_run(): remove a connection from the list of unknowns.
*/
static u3_noun
_raft_remove_run(u3_rcon* ron_u)
{
  u3_raft* raf_u = ron_u->raf_u;
  u3_noun  suc = c3n;

  if ( raf_u->run_u == ron_u ) {
    raf_u->run_u = ron_u->nex_u;
    suc = c3y;
  }
  else {
    u3_rcon* pre_u = raf_u->run_u;

    while ( pre_u ) {
      if ( pre_u->nex_u == ron_u ) {
        pre_u->nex_u = ron_u->nex_u;
        suc = c3y;
        break;
      }
      else pre_u = pre_u->nex_u;
    }
  }

  return suc;
}

static u3_rreq*
_raft_rreq_new(u3_rcon* ron_u)
{
  u3_rreq* req_u = c3_malloc(sizeof(*req_u));

  req_u->msg_u = c3_malloc(sizeof(*req_u->msg_u));
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
_raft_rreq_free(u3_rreq* req_u)
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
  u3_rcon* ron_u = (void*)had_u;
  u3_raft* raf_u = ron_u->raf_u;

  //uL(fprintf(uH, "raft: conn_free %p\n", ron_u));

  //  Unlink references.
  if ( ron_u->nam_u ) {
    c3_assert(c3n == _raft_remove_run(ron_u));
    if ( ron_u->nam_u->ron_u == ron_u ) {
      ron_u->nam_u->ron_u = 0;
    }
  }
  else {
    u3_noun suc = _raft_remove_run(ron_u);
    c3_assert(c3y == suc);
    //  Slow, expensive debug assert.
    {
      u3_rnam* nam_u = raf_u->nam_u;

      while ( nam_u ) {
        c3_assert(nam_u->ron_u != ron_u);
        nam_u = nam_u->nex_u;
      }
    }
  }

  //  Free requests.
  {
    u3_rreq* req_u = ron_u->out_u;

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
  free(ron_u->wri_u);
  free(ron_u);
}

/* _raft_conn_dead(): kill a connection.
*/
static void
_raft_conn_dead(u3_rcon* ron_u)
{
  if ( c3n == ron_u->liv ) {
    //uL(fprintf(uH, "raft: conn already dead %p\n", ron_u));
    return;
  }
  else {
    uL(fprintf(uH, "raft: conn_dead %p\n", ron_u));
    ron_u->liv = c3n;
  }

  uv_read_stop((uv_stream_t*)&ron_u->wax_u);
  uv_close((uv_handle_t*)&ron_u->wax_u, _raft_conn_free);
}

/* _raft_listen_cb(): generic listen callback.
*/
static void
_raft_listen_cb(uv_stream_t* str_u, c3_i sas_i)
{
  u3_raft* raf_u = (u3_raft*)str_u;

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "raft: listen_cb: error\n"));
  }
  else {
    u3_rcon* ron_u = _raft_conn_new(raf_u);

    if ( 0 != uv_accept((uv_stream_t*)&raf_u->wax_u,
                        (uv_stream_t*)&ron_u->wax_u) )
    {
      uL(fprintf(uH, "raft: accept: error\n"));

      uv_close((uv_handle_t*)&ron_u->wax_u, 0);
      free(ron_u);
    }
    else {
      ron_u->liv = c3y;

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
  u3_rcon* ron_u = con_u->data;
  free(con_u);

  if ( 0 != sas_i ) {
    uL(fprintf(uH, "raft: connect_cb: %s\n",
                   uv_strerror(sas_i)));
    uv_close((uv_handle_t*)&ron_u->wax_u, _raft_conn_free);
  }
  else {
    c3_assert(ron_u->nam_u);
    uL(fprintf(uH, "raft: connected to %s\n", ron_u->nam_u->str_c));
    ron_u->liv = c3y;

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
  uv_connect_t*    con_u = c3_malloc(sizeof(*con_u));
  u3_rcon*         ron_u = raq_u->data;

  //uL(fprintf(uH, "getaddrinfo_cb %s\n", ron_u->nam_u->nam_c));

  con_u->data = ron_u;
  for ( res_u = add_u; res_u; res_u = res_u->ai_next ) {
    if ( 0 != uv_tcp_connect(con_u,
                             &ron_u->wax_u,
                             (const struct sockaddr*)res_u->ai_addr,
                             _raft_connect_cb) )
    {
      uL(fprintf(uH, "raft: getaddrinfo_cb: %s\n",
                     uv_strerror(sas_i)));
      uv_close((uv_handle_t*)&ron_u->wax_u, 0);
      continue;
    }
    else {
#if 0
      c3_c  add_c[17] = {'\0'};

      uv_ip4_name((struct sockaddr_in*)res_u->ai_addr, add_c, 16);
      uL(fprintf(uH, "raft: conn %s\n", add_c));
#endif
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
_raft_conn_all(u3_raft* raf_u, void (*con_f)(u3_rcon* ron_u))
{
  u3_rnam* nam_u = raf_u->nam_u;
  u3_rcon* ron_u;

  while ( nam_u ) {
    if ( 0 == nam_u->ron_u || c3n == nam_u->ron_u->liv ) {
      struct addrinfo   hit_u;
      uv_getaddrinfo_t* raq_u = c3_malloc(sizeof(*raq_u));

      ron_u = _raft_conn_new(raf_u);

      //uL(fprintf(uH, "raft: new conn to %s:%s %p\n",
      //               nam_u->nam_c, nam_u->por_c, ron_u));

      memset(&hit_u, 0, sizeof(hit_u));
      hit_u.ai_family = AF_INET;
      hit_u.ai_socktype = SOCK_STREAM;
      hit_u.ai_protocol = IPPROTO_TCP;

      raq_u->data = ron_u;

      int ret;
      if ( 0 != (ret = uv_getaddrinfo(u3L,
                               raq_u,
                               _raft_getaddrinfo_cb,
                               nam_u->nam_c,
                               nam_u->por_c,
                                      &hit_u) ))
      {
        uL(fprintf(uH, "raft: getaddrinfo: %s\n",
                       uv_strerror(ret)));

        uv_close((uv_handle_t*)&ron_u->wax_u, 0);
        free(raq_u);
        free(ron_u);
        c3_assert(0);
      }
      else {
        ron_u->nam_u = nam_u;
        nam_u->ron_u = ron_u;
      }

      con_f(nam_u->ron_u);
    }
    else {
      //uL(fprintf(uH, "raft: existing connection %p for %s\n",
      //               nam_u->ron_u, nam_u->str_c));
      con_f(nam_u->ron_u);
      if ( c3y == nam_u->ron_u->liv ) {
        _raft_conn_work(nam_u->ron_u);
      }
    }
    nam_u = nam_u->nex_u;
  }
}

/* _raft_write_base(): Populate the base fields of a u3_rmsg.
**
** Should not be called directly.
*/
static void
_raft_write_base(u3_rcon* ron_u, u3_rmsg* msg_u)
{
  u3_raft* raf_u = ron_u->raf_u;

  msg_u->ver_w = u3r_mug('a');
  msg_u->tem_w = raf_u->tem_w;
  msg_u->len_d = 5;
}

/* _raft_write_rest(): Write fields for an RPC request to msg_u.
**
** Should not be called directly.
*/
static void
_raft_write_rest(u3_rcon* ron_u, c3_d lai_d, c3_w lat_w, u3_rmsg* msg_u)
{
  u3_raft* raf_u = ron_u->raf_u;

  c3_assert(ron_u->nam_u);
  _raft_write_base(ron_u, msg_u);
  msg_u->rest.lai_d = lai_d;
  msg_u->rest.lat_w = lat_w;
  msg_u->rest.nam_w = 1 + strlen(raf_u->str_c) / 4;
  msg_u->rest.nam_c = c3_calloc(4 * msg_u->rest.nam_w);
  strncpy(msg_u->rest.nam_c, raf_u->str_c, 4 * msg_u->rest.nam_w + 1);
  msg_u->len_d += 4 + msg_u->rest.nam_w;
}

/* _raft_write_apen(): Write fields for an AppendEntries request.
*/
static void
_raft_write_apen(u3_rcon* ron_u,
                 c3_d lai_d, c3_w lat_w,
                 c3_d cit_d, c3_d ent_d, u3_rent* ent_u,
                 u3_rmsg* msg_u)
{
  _raft_write_rest(ron_u, lai_d, lat_w, msg_u);
  msg_u->typ_w = c3__apen;
  msg_u->rest.apen.cit_d = cit_d;
  msg_u->rest.apen.ent_d = ent_d;
  msg_u->len_d += 4;

  msg_u->rest.apen.ent_u = ent_u;
  {
    c3_d i_d;

    for ( i_d = 0; i_d < ent_d; i_d++ ) {
      msg_u->len_d += 3 + ent_u[i_d].len_w;
    }
  }
}

/* _raft_write_revo(): Write fields for a RequestVote request.
*/
static void
_raft_write_revo(u3_rcon* ron_u, u3_rmsg* msg_u)
{
  u3_raft* raf_u = ron_u->raf_u;

  _raft_write_rest(ron_u, raf_u->ent_d, raf_u->lat_w, msg_u);
  msg_u->typ_w = c3__revo;
}

/* _raft_send_rasp(): Send a rasp (raft response) to a peer.
*/
static void
_raft_send_rasp(u3_rcon* ron_u, c3_t suc_t)
{
  u3_rmsg msg_u;

  _raft_write_base(ron_u, &msg_u);
  msg_u.typ_w = c3__rasp;
  msg_u.rasp.suc_w = suc_t;
  msg_u.len_d += 1;
  _raft_rmsg_send(ron_u, &msg_u);
}

/* _raft_send_beat(): send a heartbeat (empty AppendEntries) to a peer.
**
** Creates a new request.
*/
static void
_raft_send_beat(u3_rcon* ron_u)
{
  u3_rreq*    req_u = _raft_rreq_new(ron_u);
  u3_rmsg*    msg_u = req_u->msg_u;

  c3_log_every(50, "raft: beat 50\n");

  _raft_write_apen(ron_u, 0, 0, 0, 0, 0, msg_u);
  _raft_rmsg_send(ron_u, msg_u);
}

/* _raft_send_revo(): send a RequestVote to a peer.
**
** Creates a new request.
*/
static void
_raft_send_revo(u3_rcon* ron_u)
{
  u3_rreq* req_u = _raft_rreq_new(ron_u);
  u3_rmsg* msg_u = req_u->msg_u;

  _raft_write_revo(ron_u, msg_u);
  _raft_rmsg_send(ron_u, msg_u);
}

/* _raft_start_election(): bump term, vote for self, solicit votes from peers.
*/
static void
_raft_start_election(u3_raft* raf_u)
{
  c3_i sas_i;

  c3_assert(0 == uv_is_active((uv_handle_t*)&raf_u->tim_u));
  sas_i = uv_timer_start(&raf_u->tim_u, _raft_time_cb,
                         _raft_election_rand(), 0);
  c3_assert(sas_i == 0);

  raf_u->tem_w++;
  u3_sist_put("term", (c3_y*)&raf_u->tem_w, sizeof(c3_w));
  uL(fprintf(uH, "raft: starting election [tem:%d]\n", raf_u->tem_w));

  {
    u3_rnam* nam_u;

    for ( nam_u = raf_u->nam_u; nam_u; nam_u = nam_u->nex_u ) {
      nam_u->vog = c3n;
    }
  }
  raf_u->vot_w = 1;
  raf_u->vog_c = raf_u->str_c;
  u3_sist_put("vote", (c3_y*)raf_u->vog_c, strlen(raf_u->vog_c));

  _raft_conn_all(raf_u, _raft_send_revo);
}

/* _raft_heartbeat(): send a heartbeat to all peers.
*/
static void
_raft_heartbeat(u3_raft* raf_u)
{
  _raft_conn_all(raf_u, _raft_send_beat);
}

/* _raft_time_cb(): generic timer callback.
**
** Called on election timeouts for non-leaders, and at heartbeat interval for
** leaders.
*/
static void
_raft_time_cb(uv_timer_t* tim_u)
{
  u3_raft* raf_u = tim_u->data;
  //uL(fprintf(uH, "raft: time\n"));

  switch ( raf_u->typ_e ) {
    default: {
      uL(fprintf(uH, "raft: time_cb: unknown server state\n"));
      c3_assert(0);
    }
    case u3_raty_foll: {
      uL(fprintf(uH, "raft: foll -> cand\n"));
      raf_u->typ_e = u3_raty_cand;
      // continue to cand
    }
    case u3_raty_cand: {
      _raft_start_election(raf_u);
      break;
    }
    case u3_raty_lead: {
      _raft_heartbeat(raf_u);
      break;
    }
  }
}

/* _raft_foll_init(): begin, follower mode.
*/
static void
_raft_foll_init(u3_raft* raf_u)
{
  uL(fprintf(uH, "raft: none -> foll\n"));
  raf_u->typ_e = u3_raty_foll;

  //  Initialize and count peers.
  {
    u3_rnam* nam_u = u3_raft_readopt(u3_Host.ops_u.raf_c,
                                     u3_Host.ops_u.nam_c,
                                     u3_Host.ops_u.rop_s);

    if ( 0 == nam_u ) {
      uL(fprintf(uH, "raft: couldn't parse arg '%s'\n", u3_Host.ops_u.raf_c));
      u3_lo_bail();
    }

    raf_u->pop_w = 1; raf_u->nam_u = nam_u;
    while ( nam_u ) {
      raf_u->pop_w++; nam_u = nam_u->nex_u;
    }
  }

  //  Set our name.
  {
    c3_i wri_i, siz_i;

    siz_i = strlen(u3_Host.ops_u.nam_c) + strlen(":65536") + 1;
    raf_u->str_c = c3_malloc(siz_i);
    wri_i = snprintf(raf_u->str_c, siz_i, "%s:%d",
                     u3_Host.ops_u.nam_c, u3_Host.ops_u.rop_s);
    c3_assert(wri_i < siz_i);
  }

  //  Load persisted settings.
  {
    c3_w  tem_w = 0;
    c3_c* vog_c = 0;
    c3_i  ret_i;

    if ( (ret_i = u3_sist_has("term")) >= 0 ) {
      c3_assert(sizeof(c3_w) == ret_i);
      u3_sist_get("term", (c3_y*)&tem_w);
      uL(fprintf(uH, "raft: term from sist: %u\n", tem_w));
    }
    if ( (ret_i = u3_sist_has("vote")) >= 0 ) {
      c3_assert(ret_i > 0);
      vog_c = c3_malloc(ret_i);
      u3_sist_get("vote", (c3_y*)vog_c);
      uL(fprintf(uH, "raft: vote from sist: %s\n", vog_c));
    }

    raf_u->tem_w = tem_w;
    if ( vog_c ) {
      if ( 0 == strcmp(vog_c, raf_u->str_c) ) {
        raf_u->vog_c = raf_u->str_c;
        raf_u->vot_w = 1;
        raf_u->typ_e = u3_raty_cand;
      }
      else {
        u3_rnam* nam_u;

        for ( nam_u = raf_u->nam_u; nam_u; nam_u = nam_u->nex_u ) {
          if ( 0 == strcmp(vog_c, nam_u->str_c) ) {
            raf_u->vog_c = nam_u->str_c;
            break;
          }
        }
        if ( 0 == nam_u ) {
          uL(fprintf(uH, "raft: discarding unknown vote %s\n", vog_c));
        }
      }
      free(vog_c);
    }
  }

  //  Bind the listener.
  {
    struct sockaddr_in add_u; 
    c3_w ret_w;
    if (0 != (ret_w = uv_ip4_addr("0.0.0.0", u3_Host.ops_u.rop_s, &add_u ))){
      uL(fprintf(uH, "raft: init: %s\n", uv_strerror(ret_w)));
      c3_assert(0);
    }

    if ( 0 != (ret_w = uv_tcp_init(u3L, &raf_u->wax_u)) ) {
      uL(fprintf(uH, "raft: init: %s\n", uv_strerror(ret_w)));
      c3_assert(0);
    }
    if ( 0 != (ret_w = uv_tcp_bind(&raf_u->wax_u, (struct sockaddr *) & add_u, 0)) ) {
      uL(fprintf(uH, "raft: bind: %s\n", uv_strerror(ret_w)));
      c3_assert(0);
    }
    if ( 0 != (ret_w = uv_listen((uv_stream_t*)&raf_u->wax_u, 16, _raft_listen_cb)) ) {
      uL(fprintf(uH, "raft: listen: %s\n", uv_strerror(ret_w)));
      c3_assert(0);
    }
    else {
      uL(fprintf(uH, "raft: on TCP %d\n", u3_Host.ops_u.rop_s));
    }
  }

  //  Start the initial election timeout.
  uv_timer_start(&raf_u->tim_u, _raft_time_cb, _raft_election_rand(), 0);
}

/* _raft_lone_init(): begin, single-instance mode.
*/
static void
_raft_lone_init(u3_raft* raf_u)
{
  //  uL(fprintf(uH, "raft: single-instance mode\n"));
  raf_u->pop_w = 1;
  _raft_promote(raf_u);
}

/* u3_raft_init(): start Raft process.
*/
void
u3_raft_init()
{
  u3_raft* raf_u = u3Z;

  //  Initialize timer -- used in both single and multi-instance mode,
  //  for different things.
  uv_timer_init(u3L, &raf_u->tim_u);
  raf_u->tim_u.data = raf_u;

  if ( 0 == u3_Host.ops_u.raf_c ) {
    _raft_lone_init(raf_u);
  }
  else {
    _raft_foll_init(raf_u);
  }
}

/* _raft_sure(): apply and save an input ovum and its result.
*/
static u3_noun
_raft_sure(u3_noun ovo, u3_noun vir, u3_noun cor)
{
  //  Whatever worked, save it.  (XX - should be concurrent with execute.)
  //  We'd like more events that don't change the state but need work here.
  {
    u3_noun ret;

    u3r_mug(cor);
    u3r_mug(u3A->roc);

    //  XX review this, and confirm it's actually an optimization
    //  Seems like it could be very expensive in some cases
    //
    if ( c3n == u3r_sing(cor, u3A->roc) ) {
      ret = u3nc(vir, ovo);

      u3z(u3A->roc);
      u3A->roc = cor;
    }
    else {
      u3z(ovo);

      //  we return ~ in place of the event ovum to skip persistence
      //
      ret = u3nc(vir, u3_nul);

      u3z(cor);
    }
    return ret;
  }
}

/* _raft_lame(): handle an application failure.
*/
static u3_weak
_raft_lame(u3_noun ovo, u3_noun why, u3_noun tan)
{
  u3_noun bov, gon, ret;

  u3_noun wir, tag, cad;
  u3x_trel(ovo, &wir, &tag, &cad);

#if 0
  {
    c3_c* tag_c = u3r_string(tag);
    uL(fprintf(uH, "lame: %s\n", tag_c));
    u3_lo_show("data", u3k(u3t(u3t(ovo))));
    free(tag_c);
  }
#endif

  //  Formal error in a network packet generates a %hole card.
  //
  //    There should be a separate path for crypto failures,
  //    to prevent timing attacks, but isn't right now.  To deal
  //    with a crypto failure, just drop the packet.
  //
  if ( (c3__exit == why) && (c3__hear == tag) ) {
    bov = u3nt(u3k(wir), c3__hole, u3k(cad));
  }
  //  All other errors are replaced with [%crud why trace]
  //
  else {
    bov = u3nq(u3k(wir), c3__crud, u3k(why), u3k(tan));
  }

  //  poke with replacement event, on the same wire
  //
  gon = u3m_soft(0, u3v_poke, u3k(bov));

  if ( u3_blip == u3h(gon) ) {
    u3_noun hed, tal;
    u3x_trel(gon, 0, &hed, &tal);

    ret = _raft_sure(u3k(bov), u3k(hed), u3k(tal));
  }
  else {
    //  XX this will always fail, nothing in arvo handles %warn
    //
#if 0
    u3_noun vab = u3nt(u3k(u3h(bov)), c3__warn,
                       u3i_tape("crude crash!"));
    u3_noun nog = u3m_soft(0, u3v_poke, u3k(vab));

    if ( u3_blip == u3h(nog) ) {
      ret = _raft_sure(u3k(vab), u3k(u3h(u3t(nog))), u3k(u3t(u3t(nog))));
    }
    else {
#endif

      ret = u3_none;

      uL(fprintf(uH, "crude: all delivery failed!\n"));

      {
        c3_c* tag_c = u3r_string(tag);
        uL(fprintf(uH, "event: %s\n", tag_c));
        u3_lo_punt(2, u3kb_flop(u3k(tan)));
        free(tag_c);
      }

      uL(fprintf(uH, "crude: punted\n"));
      // c3_assert(!"crud");

#if 0
    }

    u3z(vab); u3z(nog);
#endif
  }

  u3z(ovo); u3z(why); u3z(tan);
  u3z(bov); u3z(gon);

  return ret;
}

/* _raft_punk(): insert and apply an input ovum (unprotected).
*/
static u3_weak
_raft_punk(u3_noun ovo)
{
#ifdef U3_EVENT_TIME_DEBUG
  c3_c* txt_c = u3r_string(u3h(u3t(ovo)));
#endif
  c3_w sec_w;
  //  static c3_w num_w;
  u3_noun gon;

  //  uL(fprintf(uH, "punk: %s: %d\n", u3r_string(u3h(u3t(ovo))), num_w++));

  //  XX this is wrong - the timer should be on the original hose.
  //
  if (c3__term == u3h(u3t(u3h(ovo)))) {
    sec_w = 0;
  } else sec_w = 600;

#ifdef U3_EVENT_TIME_DEBUG
  struct timeval b4, f2, d0;
  gettimeofday(&b4, 0);
  if( c3__belt != u3h(u3t(ovo)) ){
    uL(fprintf(uH, "%%soft %s\n", txt_c));
  }
#endif

  // TODO: Embed event number here?
  u3t_event_trace("Running", 'b');
  gon = u3m_soft(sec_w, u3v_poke, u3k(ovo));
  u3t_event_trace("Running", 'e');

#ifdef U3_EVENT_TIME_DEBUG
  c3_w ms_w;
  c3_w clr_w;

  gettimeofday(&f2, 0);
  timersub(&f2, &b4, &d0);
  ms_w = (d0.tv_sec * 1000) + (d0.tv_usec / 1000);
  clr_w = ms_w > 1000 ? 1 : ms_w < 100 ? 2 : 3; //  red, green, yellow
  if(c3__belt != u3h(u3t(ovo)) || clr_w != 2){
    uL(fprintf(uH, "\x1b[3%dm%%punk %s %4d.%02dms\x1b[0m\n",
                       clr_w, txt_c, ms_w, (int) (d0.tv_usec % 1000) / 10));
  }
  free(txt_c);
#endif

  {
    u3_noun hed, tal;
    u3x_cell(gon, &hed, &tal);

    u3k(hed); u3k(tal);
    u3z(gon);

    if ( u3_blip != hed ) {
      return _raft_lame(ovo, hed, tal);
    }
    else {
      u3_noun vir, cor;
      u3x_cell(tal, &vir, &cor);

      u3k(vir); u3k(cor);
      u3z(tal);

      return _raft_sure(ovo, vir, cor);
    }
  }
}

/* _raft_push(): save an event
*/
static c3_d
_raft_push(u3_raft* raf_u, c3_w* bob_w, c3_w len_w)
{
  c3_assert(raf_u->typ_e == u3_raty_lead);
  c3_assert(0 != bob_w && 0 < len_w);

  if ( 1 == raf_u->pop_w ) {
    c3_assert(u3_raty_lead == raf_u->typ_e);
    u3t_event_trace("Recording", 'b');
    raf_u->ent_d = u3_sist_pack(raf_u->tem_w, c3__ov, bob_w, len_w);
    u3t_event_trace("Recording", 'e');
    raf_u->lat_w = raf_u->tem_w;  //  XX

    u3p(u3v_cart) egg_p;

    egg_p = u3A->ova.egg_p;
    while ( egg_p ) {
      u3v_cart* egg_u = u3to(u3v_cart, egg_p);

      if ( egg_u->ent_d <= raf_u->ent_d ) {
        egg_u->cit = c3y;
      } else break;

      egg_p = egg_u->nex_p;
    }

    return raf_u->ent_d;
  }
  else {
    //  TODO
    uL(fprintf(uH, "raft: multi-instance push\n"));
    c3_assert(0);
  }
}


/* _raft_kick(): kick a list of effects, transferring.
*/
static void
_raft_kick(u3_noun vir)
{
  while ( u3_nul != vir ) {
    u3_noun ovo = u3k(u3h(vir));
    u3_noun nex = u3k(u3t(vir));
    u3z(vir); vir = nex;

    u3_reck_kick(ovo);
  }
}

/* _raft_spac(): print n spaces.
*/
void _raft_spac(FILE* fil_u,  c3_w n)
{
  for (; n > 0; n--)
    (fprintf(fil_u," "));
}

/* _raft_print_memory: print memory amount.
**
**  Helper for _raft_prof(), just an un-captioned u3a_print_memory().
*/
void
_raft_print_memory(FILE* fil_u, c3_w wor_w)
{
  c3_w byt_w = (wor_w * 4);
  c3_w gib_w = (byt_w / 1000000000);
  c3_w mib_w = (byt_w % 1000000000) / 1000000;
  c3_w kib_w = (byt_w % 1000000) / 1000;
  c3_w bib_w = (byt_w % 1000);

  if ( gib_w ) {
    (fprintf(fil_u, "GB/%d.%03d.%03d.%03d\r\n",
        gib_w, mib_w, kib_w, bib_w));
  }
  else if ( mib_w ) {
    (fprintf(fil_u, "MB/%d.%03d.%03d\r\n", mib_w, kib_w, bib_w));
  }
  else if ( kib_w ) {
    (fprintf(fil_u, "KB/%d.%03d\r\n", kib_w, bib_w));
  }
  else {
    (fprintf(fil_u, "B/%d\r\n", bib_w));
  }
}

#if 0
/*  _raft_prof_noun(): get memory usage, in words, of noun. RETAIN.
*/
c3_w
_raft_prof_noun(u3p(u3h_root) hax, u3_noun non, c3_t dud)
{
  return 0;
  /*
  u3_weak got = u3h_git(hax, dud ? non & 0x7fffffff : non);

  if (u3_none != got) {
    return 1;
  }
  else {
    c3_w res;

    if (!(non & 0x80000000)) {
      res = 1;
    }
    if (_(u3ud(non))) {
      res = 3 + 3 + u3r_met(5, non);
    }
    else {
      res = 3 + 2
            + _raft_prof_noun(hax, u3h(non), dud)
            + _raft_prof_noun(hax, u3t(non), dud);
    }

    u3h_put(hax, dud ? non & 0x7fffffff : non, res);

    return res;
  }
  */
}
#endif

/* _raft_prof(): print memory profile. RETAIN.
*/
c3_w
_raft_prof(FILE* fil_u, c3_w den, u3_noun mas)
{
  c3_w tot_w = 0;
  u3_noun h_mas, t_mas;

  if ( c3n == u3r_cell(mas, &h_mas, &t_mas) ) {
    _raft_spac(fil_u, den);
    fprintf(fil_u, "mistyped mass\r\n");
    return tot_w;
  }
  else if ( _(u3du(h_mas)) ) {
    _raft_spac(fil_u, den);
    fprintf(fil_u, "mistyped mass head\r\n");
    {
      c3_c* lab_c = u3m_pretty(h_mas);
      fprintf(fil_u, "h_mas: %s", lab_c);
      free(lab_c);
    }
    return tot_w;
  }
  else {
    _raft_spac(fil_u, den);

    {
      c3_c* lab_c = u3m_pretty(h_mas);
      fprintf(fil_u, "%s: ", lab_c);
      free(lab_c);
    }

    u3_noun it_mas, tt_mas;

    if ( c3n == u3r_cell(t_mas, &it_mas, &tt_mas) ) {
      fprintf(fil_u, "mistyped mass tail\r\n");
      return tot_w;
    }
    else if ( c3y == it_mas ) {
      tot_w += u3a_mark_noun(tt_mas);
      _raft_print_memory(fil_u, tot_w);

#if 1
      /* The basic issue here is that tt_mas is included in
       * u3A->sac, so they can't both be roots in the normal
       * sense. When we mark u3A->sac later on, we want tt_mas
       * to appear unmarked, but its children should be already
       * marked.
      */
      if ( _(u3a_is_dog(tt_mas)) ) {
        u3a_box* box_u = u3a_botox(u3a_to_ptr(tt_mas));
#ifdef U3_MEMORY_DEBUG
        if ( 1 == box_u->eus_w ) {
          box_u->eus_w = 0xffffffff;
        }
        else {
          box_u->eus_w -= 1;
        }
#else
        if ( -1 == (c3_w)box_u->use_w ) {
          box_u->use_w = 0x80000000;
        }
        else {
          box_u->use_w += 1;
        }
#endif
      }
#endif

      return tot_w;
    }
    else if ( c3n == it_mas ) {
      fprintf(fil_u, "\r\n");

      while ( _(u3du(tt_mas)) ) {
        tot_w += _raft_prof(fil_u, den+2, u3h(tt_mas));
        tt_mas = u3t(tt_mas);
      }

      _raft_spac(fil_u, den);
      fprintf(fil_u, "--");
      _raft_print_memory(fil_u, tot_w);

      return tot_w;

    }
    else {
      _raft_spac(fil_u, den);
      fprintf(fil_u, "mistyped (strange) mass tail\r\n");
      return tot_w;
    }
  }
}

/* _raft_grab(): garbage collect, checking for profiling. RETAIN.
*/
static void
_raft_grab(u3_noun rus)
{
  if ( u3_nul == u3A->sac) {
    if ( u3C.wag_w & (u3o_debug_ram | u3o_check_corrupt) ) {
      u3m_grab(rus, u3_none);
    }
  }
  else {
    c3_w usr_w = 0, man_w = 0, ova_w = 0, sac_w = 0;

    FILE* fil_u;

#ifdef U3_MEMORY_LOG
    {
      c3_c* wen_c = u3r_string(u3A->wen);

      c3_c nam_c[2048];
      snprintf(nam_c, 2048, "%s/.urb/put/mass", u3_Host.dir_c);

      struct stat st;
      if ( -1 == stat(nam_c, &st) ) {
        mkdir(nam_c, 0700);
      }

      c3_c man_c[2048];
      snprintf(man_c, 2048, "%s/%s.txt", nam_c, wen_c);

      fil_u = fopen(man_c, "w");
      fprintf(fil_u, "%s\r\n", wen_c);

      free(wen_c);
    }
#else
    {
      fil_u = stderr;
    }
#endif

    c3_assert( u3R == &(u3H->rod_u) );

    fprintf(fil_u, "\r\n");
    usr_w = _raft_prof(fil_u, 0, u3A->sac);
    u3a_print_memory(fil_u, "total userspace", usr_w);

    man_w = u3m_mark(fil_u);

    ova_w = u3a_mark_noun(rus);
    u3a_print_memory(fil_u, "event & effects", ova_w);

    sac_w = u3a_mark_noun(u3A->sac);
    u3a_print_memory(fil_u, "space profile", sac_w);

    u3a_print_memory(fil_u, "total marked", usr_w + man_w + ova_w + sac_w);

    u3a_print_memory(fil_u, "sweep", u3a_sweep());

#ifdef U3_MEMORY_LOG
    {
      fclose(fil_u);
    }
#endif

    // u3h_free(u3R->cax.har_p);
    // u3R->cax.har_p = u3h_new();

    u3z(u3A->sac);
    u3A->sac = u3_nul;

    //  restore prompt
    //
    uL(fprintf(uH, "\n"));
  }
}

/* _raft_crop(): Delete finished events.
*/
static void
_raft_crop(void)
{
  while ( u3A->ova.egg_p ) {
    u3p(u3v_cart) egg_p = u3A->ova.egg_p;
    u3v_cart*     egg_u = u3to(u3v_cart, u3A->ova.egg_p);

    if ( c3y == egg_u->did ) {
      if ( egg_p == u3A->ova.geg_p ) {
        c3_assert(egg_u->nex_p == 0);
        u3A->ova.geg_p = u3A->ova.egg_p = 0;
      }
      else {
        c3_assert(egg_u->nex_p != 0);
        u3A->ova.egg_p = egg_u->nex_p;
      }
      egg_u->cit = c3y;
      u3a_free(egg_u);
    }
    else break;
  }
}

/* _raft_pop_roe(): pop the next [~ event] off the queue.
**
**  effects are no longer stored on u3A->roe; the head of
**  each pair is always null.
*/
static u3_weak
_raft_pop_roe(void)
{
  if ( u3_nul == u3A->roe ) {
    return u3_none;
  }

  u3_noun ovo;

  {
    u3_noun ova = u3kb_flop(u3A->roe);
    u3A->roe    = u3qb_flop(u3t(ova));
    ovo         = u3k(u3h(ova));
    u3z(ova);
  }

  return ovo;
}

/* _raft_poke(): poke Arvo with the next queued event.
*/
static u3_weak
_raft_poke(void)
{
  u3_weak rus;

  //  defer event processing until storage is initialized
  //
  if ( 0 == u3Z->lug_u.len_d ) {
    return u3_none;
  }

  if ( u3_none != (rus = _raft_pop_roe()) ) {
    u3_noun ovo, vir;

    u3_term_ef_blit(0, u3nc(u3nc(c3__bee, u3k(rus)), u3_nul));

    u3x_cell(rus, &vir, &ovo);
    c3_assert( u3_nul == vir );
    u3k(ovo);
    u3z(rus);

    rus = _raft_punk(ovo);

    u3_term_ef_blit(0, u3nc(u3nc(c3__bee, u3_nul), u3_nul));
  }

  return rus;
}

/* _raft_pump(): Cartify, jam, and save an ovum.
*/
static void
_raft_pump(u3_noun ovo)
{
  u3v_cart*     egg_u = u3a_malloc(sizeof(*egg_u));
  u3p(u3v_cart) egg_p = u3of(u3v_cart, egg_u);
  u3_noun       ron;
  c3_d          bid_d;
  c3_w          len_w;
  c3_w*         bob_w;

  egg_u->nex_p = 0;
  egg_u->cit = c3n;
  egg_u->did = c3n;
  egg_u->vir = 0;

  ron = u3ke_jam(u3nc(u3k(u3A->now), ovo));
  c3_assert(u3A->key);
  ron = u3dc("en:crub:crypto", u3k(u3A->key), ron);

  len_w = u3r_met(5, ron);
  bob_w = c3_malloc(len_w * 4L);
  u3r_words(0, len_w, bob_w, ron);
  u3z(ron);

  bid_d = _raft_push(u3Z, bob_w, len_w);
  egg_u->ent_d = bid_d;

  if ( 0 == u3A->ova.geg_p ) {
    c3_assert(0 == u3A->ova.egg_p);
    u3A->ova.geg_p = u3A->ova.egg_p = egg_p;
  }
  else {
    c3_assert(0 == u3to(u3v_cart, u3A->ova.geg_p)->nex_p);
    u3to(u3v_cart, u3A->ova.geg_p)->nex_p = egg_p;
    u3A->ova.geg_p = egg_p;
  }

  egg_u->did = c3y;
}

/* u3_raft_chip(): chip one event off for processing.
*/
void
u3_raft_chip(void)
{
  if ( (u3C.wag_w & u3o_trace) && (u3_Host.tra_u.con_w >= 100000) ) {
    u3t_trace_close();
    u3t_trace_open();
  }

  u3_weak rus = _raft_poke();

  _raft_crop();

  if ( u3_none != rus ) {
    u3_noun ovo, vir;
    u3x_cell(rus, &vir, &ovo);

    if ( u3_nul != ovo ) {
      _raft_pump(u3k(ovo));
    }

    _raft_kick(u3k(vir));
    _raft_grab(rus);

    u3z(rus);
  }

  if ( 0 == (u3A->ent_d % 1000ULL) ) {
    u3m_reclaim();
  }
}

/* u3_raft_play(): synchronously process events.
*/
void
u3_raft_play(void)
{
  c3_assert( u3Z->typ_e == u3_raty_lead );

  u3_raft_chip();

  if ( u3_nul != u3A->roe ) {
    u3_raft_play();
  }
}

/* _raft_work_cb(): callback to recurse into u3_raft_work().
*/
static void
_raft_work_cb(uv_timer_t* tim_u)
{
  u3_raft_work();
}

/* u3_raft_work(): asynchronously process events.
*/
void
u3_raft_work(void)
{
  c3_assert( u3Z->typ_e == u3_raty_lead );

  u3_raft_chip();

  if ( u3_nul != u3A->roe ) {
    uv_timer_start(&u3Z->tim_u, _raft_work_cb, 0, 0);
  }
}
