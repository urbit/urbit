/* vere/newt.c
**
**  implements noun blob messages with trivial framing.
**
**  a message is a 64-bit little-endian byte count, followed
**  by the indicated number of bytes.  the bytes are the
**  the +jam of of a noun.
**
**  the implementation is relatively inefficient and could
**  lose a few copies, mallocs, etc.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <sigsegv.h>
#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>

#include "all.h"
#include "vere/vere.h"

/* _newt_mess_head(): await next msg header.
*/
static void
_newt_mess_head(u3_mess* mes_u)
{
  mes_u->sat_e = u3_mess_head;
  mes_u->hed_u.has_y = 0;
}

/* _newt_mess_tail(): await msg body.
*/
static void
_newt_mess_tail(u3_mess* mes_u, c3_d len_d)
{
  u3_meat* met_u = c3_malloc(len_d + sizeof(*met_u));
  met_u->nex_u   = 0;
  met_u->len_d   = len_d;

  mes_u->sat_e = u3_mess_tail;
  mes_u->tal_u.has_d = 0;
  mes_u->tal_u.met_u = met_u;
}

/* _newt_meat_plan(): enqueue complete msg.
*/
static void
_newt_meat_plan(u3_moat* mot_u, u3_meat* met_u)
{
  if ( mot_u->ent_u ) {
    mot_u->ent_u->nex_u = met_u;
    mot_u->ent_u = met_u;
  }
  else {
    mot_u->ent_u = mot_u->ext_u = met_u;
  }
}

/* _newt_meat_poke(): deliver completed msg.
*/
static void
_newt_meat_poke(u3_moat* mot_u, u3_meat* met_u)
{
  u3_noun mat = u3i_bytes((c3_w)met_u->len_d, met_u->hun_y);
  mot_u->pok_f(mot_u->ptr_v, mat);
  c3_free(met_u);
}

/* _newt_meat_next_sync(): deliver completed msgs, synchronously.
*/
static void
_newt_meat_next_sync(u3_moat* mot_u)
{
  u3_meat* met_u = mot_u->ext_u;

  while ( met_u ) {
    u3_meat* nex_u = met_u->nex_u;
    _newt_meat_poke(mot_u, met_u);
    met_u = nex_u;
  }

  mot_u->ent_u = mot_u->ext_u = 0;
}

static void
_newt_meat_next_cb(uv_timer_t* tim_u);

/* _newt_meat_next(): deliver completed msgs, asynchronously.
*/
static void
_newt_meat_next(u3_moat* mot_u)
{
  u3_meat* met_u = mot_u->ext_u;

  if ( met_u ) {
    mot_u->ext_u = met_u->nex_u;

    if ( mot_u->ext_u ) {
      uv_timer_start(&mot_u->tim_u, _newt_meat_next_cb, 0, 0);
    }
    else {
      mot_u->ent_u = 0;
    }

    _newt_meat_poke(mot_u, met_u);
  }
}

/* _newt_meat_next_cb(): handle next msg after timer.
*/
static void
_newt_meat_next_cb(uv_timer_t* tim_u)
{
  u3_moat* mot_u = tim_u->data;
  _newt_meat_next(mot_u);
}

/* u3_newt_decode(): decode a (partial) length-prefixed byte buffer
*/
void
u3_newt_decode(u3_moat* mot_u, c3_y* buf_y, c3_d len_d)
{
  u3_mess* mes_u = &mot_u->mes_u;

  while ( len_d ) {
    switch( mes_u->sat_e ) {

      //  read up to 8 length bytes as needed
      //
      case u3_mess_head: {
        c3_y* len_y = mes_u->hed_u.len_y;
        c3_y  has_y = mes_u->hed_u.has_y;
        c3_y  ned_y = 8 - has_y;
        c3_y  cop_y = c3_min(ned_y, len_d);

        memcpy(len_y + has_y, buf_y, cop_y);
        buf_y += cop_y;
        len_d -= cop_y;
        ned_y -= cop_y;

        //  moar bytes needed, yield
        //
        if ( ned_y ) {
          mes_u->hed_u.has_y = (has_y + cop_y);
        }
        //  length known, allocate message
        //
        else {
          c3_d met_d = (((c3_d)len_y[0]) <<  0)
                     | (((c3_d)len_y[1]) <<  8)
                     | (((c3_d)len_y[2]) << 16)
                     | (((c3_d)len_y[3]) << 24)
                     | (((c3_d)len_y[4]) << 32)
                     | (((c3_d)len_y[5]) << 40)
                     | (((c3_d)len_y[6]) << 48)
                     | (((c3_d)len_y[7]) << 56);

          //  must be non-zero, only 32 bits supported
          //
          c3_assert( met_d );
          c3_assert( 0xFFFFFFFFULL > met_d );

          //  await body
          //
          _newt_mess_tail(mes_u, met_d);
        }
      } break;

      case u3_mess_tail: {
        u3_meat* met_u = mes_u->tal_u.met_u;
        c3_d     has_d = mes_u->tal_u.has_d;
        c3_d     ned_d = met_u->len_d - has_d;
        c3_d     cop_d = c3_min(ned_d, len_d);

        memcpy(met_u->hun_y + has_d, buf_y, cop_d);
        buf_y += cop_d;
        len_d -= cop_d;
        ned_d -= cop_d;

        //  moar bytes needed, yield
        //
        if ( ned_d ) {
          mes_u->tal_u.has_d = (has_d + cop_d);
        }
        //  message completed, enqueue and await next header
        //
        else {
          _newt_meat_plan(mot_u, met_u);
          _newt_mess_head(mes_u);
        }
      } break;
    }
  }
}

/* _newt_read(): handle async read result.
*/
static c3_o
_newt_read(u3_moat*        mot_u,
           ssize_t         len_i,
           const uv_buf_t* buf_u)
{
  if ( 0 > len_i ) {
    c3_free(buf_u->base);
    uv_read_stop((uv_stream_t*)&mot_u->pyp_u);
    fprintf(stderr, "newt: read failed %s\r\n", uv_strerror(len_i));
    mot_u->bal_f(mot_u->ptr_v, uv_strerror(len_i));
    return c3n;
  }
  //  EAGAIN/EWOULDBLOCK
  //
  else if ( 0 == len_i ) {
    c3_free(buf_u->base);
    return c3n;
  }
  else {
    u3_newt_decode(mot_u, (c3_y*)buf_u->base, (c3_d)len_i);
    c3_free(buf_u->base);
    return c3y;
  }
}

/* _newt_read_sync_cb(): async read callback, sync msg delivery.
*/
static void
_newt_read_sync_cb(uv_stream_t*    str_u,
                   ssize_t         len_i,
                   const uv_buf_t* buf_u)
{
  u3_moat* mot_u = (void *)str_u;

  if ( c3y == _newt_read(mot_u, len_i, buf_u) ) {
    _newt_meat_next_sync(mot_u);
  }
}

/* _newt_read_cb(): async read callback, async msg delivery.
*/
static void
_newt_read_cb(uv_stream_t*    str_u,
              ssize_t         len_i,
              const uv_buf_t* buf_u)
{
  u3_moat* mot_u = (void *)str_u;

  if ( c3y == _newt_read(mot_u, len_i, buf_u) ) {
    _newt_meat_next(mot_u);
  }
}

/* _newt_alloc(): libuv-style allocator.
*/
static void
_newt_alloc(uv_handle_t* had_u,
            size_t len_i,
            uv_buf_t* buf_u)
{
  //  XX pick an appropriate size
  //
  void* ptr_v = c3_malloc(len_i);

  *buf_u = uv_buf_init(ptr_v, len_i);
}

static void
_newt_read_init(u3_moat* mot_u, uv_read_cb read_cb_f)
{
  //  zero-initialize completed msg queue
  //
  mot_u->ent_u = mot_u->ext_u = 0;

  //  store pointer for queue timer callback
  //
  mot_u->tim_u.data = mot_u;

  //  await next msg header
  //
  _newt_mess_head(&mot_u->mes_u);

  {
    c3_i sas_i;

    if ( 0 != (sas_i = uv_read_start((uv_stream_t*)&mot_u->pyp_u,
                                     _newt_alloc,
                                     read_cb_f)) )
    {
      fprintf(stderr, "newt: read failed %s\r\n", uv_strerror(sas_i));
      mot_u->bal_f(mot_u->ptr_v, uv_strerror(sas_i));
    }
  }
}

/* u3_newt_read_sync(): start reading; multiple msgs synchronous.
*/
void
u3_newt_read_sync(u3_moat* mot_u)
{
  _newt_read_init(mot_u, _newt_read_sync_cb);
}

/* u3_newt_read(): start reading; each msg asynchronous.
*/
void
u3_newt_read(u3_moat* mot_u)
{
  _newt_read_init(mot_u, _newt_read_cb);
}

/* n_req: write request for newt
*/
typedef struct _n_req {
  uv_write_t wri_u;
  u3_mojo*   moj_u;
  c3_y       buf_y[0];
} n_req;

/* _newt_write_cb(): generic write callback.
*/
static void
_newt_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  n_req*   req_u = (n_req*)wri_u;
  u3_mojo* moj_u = req_u->moj_u;

  c3_free(req_u);

  if ( 0 != sas_i ) {
    fprintf(stderr, "newt: write failed %s\r\n", uv_strerror(sas_i));
    moj_u->bal_f(moj_u->ptr_v, uv_strerror(sas_i));
  }
}

/* u3_newt_write(): write atom to stream; free atom.
*/
void
u3_newt_write(u3_mojo* moj_u, u3_atom mat)
{
  c3_w   len_w = u3r_met(3, mat);
  n_req* req_u = c3_malloc(8 + len_w + sizeof(*req_u));
  req_u->moj_u = moj_u;

  //  write header; c3_d is futureproofing
  //
  req_u->buf_y[0] = ((len_w >>  0) & 0xff);
  req_u->buf_y[1] = ((len_w >>  8) & 0xff);
  req_u->buf_y[2] = ((len_w >> 16) & 0xff);
  req_u->buf_y[3] = ((len_w >> 24) & 0xff);
  req_u->buf_y[4] = req_u->buf_y[5] = req_u->buf_y[6] = req_u->buf_y[7] = 0;

  //  write payload
  //
  u3r_bytes(0, len_w, req_u->buf_y + 8, mat);
  u3z(mat);

  {
    uv_buf_t buf_u = uv_buf_init((c3_c*)req_u->buf_y, 8 + len_w);
    c3_i     sas_i;

    if ( 0 != (sas_i = uv_write(&req_u->wri_u,
                                (uv_stream_t*)&moj_u->pyp_u,
                                &buf_u, 1,
                                _newt_write_cb)) )
    {
      c3_free(req_u);
      fprintf(stderr, "newt: write failed %s\r\n", uv_strerror(sas_i));
      moj_u->bal_f(moj_u->ptr_v, uv_strerror(sas_i));
    }
  }
}
