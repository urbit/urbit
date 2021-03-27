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

static void
_newt_meat_next_cb(uv_timer_t* tim_u);

/* _newt_meat_poke(): deliver completed msg.
*/
static void
_newt_meat_poke(u3_moat* mot_u)
{
  u3_meat* met_u = mot_u->ext_u;

  if ( met_u ) {
    uv_timer_start(&mot_u->tim_u, _newt_meat_next_cb, 0, 0);

    if ( c3y == mot_u->pok_f(mot_u->ptr_v, met_u->len_d, met_u->hun_y) ) {
      mot_u->ext_u = met_u->nex_u;

      if ( !mot_u->ext_u ) {
        mot_u->ent_u = 0;
      }

      c3_free(met_u);
    }
  }
}

/* _newt_meat_next_cb(): handle next msg after timer.
*/
static void
_newt_meat_next_cb(uv_timer_t* tim_u)
{
  u3_moat* mot_u = tim_u->data;
  _newt_meat_poke(mot_u);
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

/* _newt_read_cb(): stream input callback.
*/
static void
_newt_read_cb(uv_stream_t*    str_u,
              ssize_t         len_i,
              const uv_buf_t* buf_u)
{
  u3_moat* mot_u = (void *)str_u;

  if ( 0 > len_i ) {
    c3_free(buf_u->base);
    uv_read_stop((uv_stream_t*)&mot_u->pyp_u);

    if ( UV_EOF != len_i ) {
      fprintf(stderr, "newt: read failed %s\r\n", uv_strerror(len_i));
    }

    mot_u->bal_f(mot_u->ptr_v, len_i, uv_strerror(len_i));
  }
  //  EAGAIN/EWOULDBLOCK
  //
  else if ( 0 == len_i ) {
    c3_free(buf_u->base);
  }
  else {
    u3_newt_decode(mot_u, (c3_y*)buf_u->base, (c3_d)len_i);
    c3_free(buf_u->base);

    _newt_meat_poke(mot_u);
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

/* u3_newt_read(): start stream reading.
*/
void
u3_newt_read(u3_moat* mot_u)
{
  //  zero-initialize completed msg queue
  //
  mot_u->ent_u = mot_u->ext_u = 0;

  //  store pointer for libuv handle callback
  //
  mot_u->pyp_u.data = mot_u;
  mot_u->tim_u.data = mot_u;

  //  await next msg header
  //
  _newt_mess_head(&mot_u->mes_u);

  {
    c3_i sas_i;

    if ( 0 != (sas_i = uv_read_start((uv_stream_t*)&mot_u->pyp_u,
                                     _newt_alloc,
                                     _newt_read_cb)) )
    {
      fprintf(stderr, "newt: read failed %s\r\n", uv_strerror(sas_i));
      mot_u->bal_f(mot_u->ptr_v, sas_i, uv_strerror(sas_i));
    }
  }
}

/* _moat_stop_cb(): finalize stop/close input stream..
*/
static void
_moat_stop_cb(uv_handle_t* han_u)
{
  u3_moat* mot_u = han_u->data;
  mot_u->bal_f(mot_u->ptr_v, -1, "");
}

/* u3_newt_moat_stop(); newt stop/close input stream.
*/
void
u3_newt_moat_stop(u3_moat* mot_u, u3_moor_bail bal_f)
{
  mot_u->pyp_u.data = mot_u;

  if ( bal_f ) {
    mot_u->bal_f = bal_f;
  }

  uv_close((uv_handle_t*)&mot_u->pyp_u, _moat_stop_cb);
  uv_close((uv_handle_t*)&mot_u->tim_u, 0);

  //  dispose in-process message
  //
  if ( u3_mess_tail == mot_u->mes_u.sat_e ) {
    c3_free(mot_u->mes_u.tal_u.met_u);
    _newt_mess_head(&mot_u->mes_u);
  }

  //  dispose pending messages
  {
    u3_meat* met_u = mot_u->ext_u;
    u3_meat* nex_u;

    while ( met_u ) {
      nex_u = met_u->nex_u;
      c3_free(met_u);
      met_u = nex_u;
    }

    mot_u->ent_u = mot_u->ext_u = 0;
  }
}

/* u3_newt_moat_info(); print status info.
*/
void
u3_newt_moat_info(u3_moat* mot_u)
{
  u3_meat* met_u = mot_u->ext_u;
  c3_w     len_w = 0;

    while ( met_u ) {
      len_w++;
      met_u = met_u->nex_u;
    }

  if ( len_w ) {
    u3l_log("    newt: %u inbound ipc messages pending\n", len_w);
  }
}

/* n_req: write request for newt
*/
typedef struct _n_req {
  uv_write_t wri_u;
  u3_mojo*   moj_u;
  c3_y*      buf_y;
  c3_y       len_y[8];
} n_req;

/* _newt_write_cb(): generic write callback.
*/
static void
_newt_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  n_req*   req_u = (n_req*)wri_u;
  u3_mojo* moj_u = req_u->moj_u;

  c3_free(req_u->buf_y);
  c3_free(req_u);

  if ( 0 != sas_i ) {
    if ( UV_ECANCELED == sas_i ) {
      fprintf(stderr, "newt: write canceled\r\n");
    }
    else {
      fprintf(stderr, "newt: write failed %s\r\n", uv_strerror(sas_i));
      moj_u->bal_f(moj_u->ptr_v, sas_i, uv_strerror(sas_i));
    }
  }
}

/* _mojo_stop_cb(): finalize stop/close output stream..
*/
static void
_mojo_stop_cb(uv_handle_t* han_u)
{
  u3_mojo* moj_u = han_u->data;
  moj_u->bal_f(moj_u->ptr_v, -1, "");
}

/* u3_newt_mojo_stop(); newt stop/close output stream.
*/
void
u3_newt_mojo_stop(u3_mojo* moj_u, u3_moor_bail bal_f)
{
  moj_u->pyp_u.data = moj_u;

  if ( bal_f ) {
    moj_u->bal_f = bal_f;
  }

  uv_close((uv_handle_t*)&moj_u->pyp_u, _mojo_stop_cb);
}

/* u3_newt_send(): write buffer to stream.
*/
void
u3_newt_send(u3_mojo* moj_u, c3_d len_d, c3_y* byt_y)
{
  n_req* req_u = c3_malloc(sizeof(*req_u));
  req_u->moj_u = moj_u;
  req_u->buf_y = byt_y;

  //  write header
  //
  req_u->len_y[0] = ( len_d        & 0xff);
  req_u->len_y[1] = ((len_d >>  8) & 0xff);
  req_u->len_y[2] = ((len_d >> 16) & 0xff);
  req_u->len_y[3] = ((len_d >> 24) & 0xff);
  req_u->len_y[4] = ((len_d >> 32) & 0xff);
  req_u->len_y[5] = ((len_d >> 40) & 0xff);
  req_u->len_y[6] = ((len_d >> 48) & 0xff);
  req_u->len_y[7] = ((len_d >> 56) & 0xff);

  {
    uv_buf_t buf_u[2] = {
      uv_buf_init((c3_c*)req_u->len_y, 8),
      uv_buf_init((c3_c*)req_u->buf_y, len_d)
    };

    c3_i     sas_i;

    if ( 0 != (sas_i = uv_write(&req_u->wri_u,
                                (uv_stream_t*)&moj_u->pyp_u,
                                buf_u, 2,
                                _newt_write_cb)) )
    {
      c3_free(req_u);
      fprintf(stderr, "newt: write failed %s\r\n", uv_strerror(sas_i));
      moj_u->bal_f(moj_u->ptr_v, sas_i, uv_strerror(sas_i));
    }
  }
}
