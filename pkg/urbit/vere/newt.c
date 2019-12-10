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
#include <ncurses/curses.h>
#include <termios.h>
#include <ncurses/term.h>

#include "all.h"
#include "vere/vere.h"

/* _newt_gain_meat(): add a block to an existing message
*/
static void
_newt_gain_meat(u3_moat* mot_u)
{
  c3_assert( 0 != mot_u->mes_u );

  //  create block
  //
  u3_meat* met_u = c3_malloc(mot_u->len_d + (c3_d) sizeof(u3_meat));
  met_u->nex_u = 0;
  met_u->len_d = mot_u->len_d;
  memcpy(met_u->hun_y, mot_u->rag_y, mot_u->len_d);

  //  enqueue block
  //
  if ( !mot_u->mes_u->meq_u ) {
    mot_u->mes_u->meq_u = mot_u->mes_u->qem_u = met_u;
  }
  else {
    mot_u->mes_u->qem_u->nex_u = met_u;
    mot_u->mes_u->qem_u = met_u;
  }
  mot_u->mes_u->has_d += met_u->len_d;

  //  free consumed stray bytes
  //
  c3_free(mot_u->rag_y);
  mot_u->len_d = 0;
  mot_u->rag_y = 0;
}

/* _newt_gain_mess(): begin parsing a new message
*/
static void
_newt_gain_mess(u3_moat* mot_u)
{
  c3_assert( 8ULL <= mot_u->len_d );
  c3_assert( 0 == mot_u->mes_u );

  c3_d nel_d = 0ULL;

  nel_d |= ((c3_d) mot_u->rag_y[0]) << 0ULL;
  nel_d |= ((c3_d) mot_u->rag_y[1]) << 8ULL;
  nel_d |= ((c3_d) mot_u->rag_y[2]) << 16ULL;
  nel_d |= ((c3_d) mot_u->rag_y[3]) << 24ULL;
  nel_d |= ((c3_d) mot_u->rag_y[4]) << 32ULL;
  nel_d |= ((c3_d) mot_u->rag_y[5]) << 40ULL;
  nel_d |= ((c3_d) mot_u->rag_y[6]) << 48ULL;
  nel_d |= ((c3_d) mot_u->rag_y[7]) << 56ULL;

  c3_assert( 0ULL != nel_d );

  //  very likely to be a bad write, we can't jam anything this big
  //
  if ( 0xFFFFFFFFULL < nel_d ) {
    u3l_log("newt: %d warn: large read %" PRIu64 "\r\n",
            getpid(),
            nel_d);
  }

  mot_u->len_d -= 8ULL;

  mot_u->mes_u = c3_malloc(sizeof(u3_mess));
  mot_u->mes_u->len_d = nel_d;
  mot_u->mes_u->has_d = 0;
  mot_u->mes_u->meq_u = mot_u->mes_u->qem_u = 0;

  if ( 0ULL == mot_u->len_d ) {
    c3_free(mot_u->rag_y);
    mot_u->rag_y = 0;
  }
  else {
    //  remove consumed length from stray bytes
    //
    c3_y* buf_y = c3_malloc(mot_u->len_d);
    memcpy(buf_y, mot_u->rag_y + 8, mot_u->len_d);

    c3_free(mot_u->rag_y);
    mot_u->rag_y = buf_y;
  }
}

/* _newt_poke_mess(): pass message to [mot_u] callback
*/
static void
_newt_poke_mess(u3_moat* mot_u)
{
  c3_assert( 0 != mot_u->mes_u );
  c3_assert( mot_u->mes_u->has_d >= mot_u->mes_u->len_d );

  c3_d     len_d = mot_u->mes_u->len_d;
  c3_y*    buf_y = c3_malloc(len_d);
  c3_d     pat_d = 0;
  u3_meat* met_u;

  //  we should have just cleared this
  //
  c3_assert(!mot_u->rag_y);
  c3_assert(!mot_u->len_d);

  //  collect queue blocks, cleaning them up; return any spare meat
  //  to the rag.
  //
  {
    met_u = mot_u->mes_u->meq_u;

    while ( met_u && (pat_d < len_d) ) {
      u3_meat* nex_u = met_u->nex_u;
      c3_d     end_d = (pat_d + met_u->len_d);
      c3_d     eat_d;
      c3_d     rem_d;

      eat_d = c3_min(len_d, end_d) - pat_d;
      memcpy(buf_y + pat_d, met_u->hun_y, eat_d);
      pat_d += eat_d;

      rem_d = (met_u->len_d - eat_d);
      if ( rem_d ) {
        mot_u->rag_y = c3_malloc(rem_d);
        memcpy(mot_u->rag_y, met_u->hun_y + eat_d, rem_d);
        mot_u->len_d = rem_d;

        //  one: unless we got a bad length, this has to be the last
        //  block in the message.
        //
        //  two: bad data on a newt channel can cause us to assert.
        //  that's actually the right thing for a private channel.
        ///
        c3_assert(0 == nex_u);
      }

      c3_free(met_u);
      met_u = nex_u;
    }

    c3_assert(pat_d == len_d);

    //  clear the message
    //
    c3_free(mot_u->mes_u);
    mot_u->mes_u = 0;
  }

  //  build and send the object
  //
  {
    u3_noun mat = u3i_bytes((c3_w)len_d, buf_y);
    mot_u->pok_f(mot_u->vod_p, mat);
  }
}

/* u3_newt_decode(): decode a (partial) length-prefixed byte buffer
*/
void
u3_newt_decode(u3_moat* mot_u, c3_y* buf_y, c3_w len_w)
{
  //  grow read buffer by `len_d` bytes
  //
  if ( mot_u->rag_y ) {
    //  XX check SIZE_MAX?
    //
    c3_d nel_d = mot_u->len_d + len_w;

    mot_u->rag_y = c3_realloc(mot_u->rag_y, nel_d);
    memcpy(mot_u->rag_y + mot_u->len_d, buf_y, len_w);

    mot_u->len_d = nel_d;
    c3_free(buf_y);
  }
  else {
    mot_u->rag_y = buf_y;
    mot_u->len_d = (c3_d)len_w;
  }

  //  process stray bytes, trying to create a new message
  //  or add a block to an existing one.
  //
  while ( mot_u->rag_y ) {
    //  no message
    //
    if ( !mot_u->mes_u ) {
      //  but enough stray bytes to start one
      //
      if ( 8ULL <= mot_u->len_d ) {
        _newt_gain_mess(mot_u);
      }
      else {
        break;
      }
    }
    else {
      //  there is a live message, add a block to the queue.
      //
      _newt_gain_meat(mot_u);

      //  check for message completions
      //
      if ( mot_u->mes_u->has_d >= mot_u->mes_u->len_d ) {
        _newt_poke_mess(mot_u);
      }
    }
  }
}

/* _raft_alloc(): libuv-style allocator for raft.
*/
static void
_newt_alloc(uv_handle_t* had_u,
            size_t len_i,
            uv_buf_t* buf_u)
{
  void* ptr_v = c3_malloc(len_i);

  *buf_u = uv_buf_init(ptr_v, len_i);
}

/* _newt_read_cb(): stream input callback.
*/
void
_newt_read_cb(uv_stream_t*    str_u,
              ssize_t         len_i,
              const uv_buf_t* buf_u)
{
  u3_moat* mot_u = (void *)str_u;

  if ( 0 > len_i ) {
    c3_free(buf_u->base);
    uv_read_stop(str_u);
    mot_u->bal_f(mot_u->vod_p, uv_strerror(len_i));
  }
  //  EAGAIN/EWOULDBLOCK
  //
  else if ( 0 == len_i ) {
    c3_free(buf_u->base);
  }
  else {
    u3_newt_decode(mot_u, (c3_y*)buf_u->base, (c3_w)len_i);
  }
}

/* u3_newt_read(): start stream reading.
*/
void
u3_newt_read(u3_moat* mot_u)
{
  c3_i err_i;

  mot_u->mes_u = 0;
  mot_u->len_d = 0;
  mot_u->rag_y = 0;

  err_i = uv_read_start((uv_stream_t*) &mot_u->pyp_u,
                        _newt_alloc,
                        _newt_read_cb);

  if ( err_i != 0 ) {
    mot_u->bal_f(mot_u, uv_strerror(err_i));
  }
}

/* u3_write_t: write request for newt
*/
typedef struct _u3_write_t {
  uv_write_t wri_u;
  u3_mojo*   moj_u;
  void*      vod_p;
  c3_y*      buf_y;
} u3_write_t;

/* _newt_write_cb(): generic write callback.
*/
static void
_newt_write_cb(uv_write_t* wri_u, c3_i sas_i)
{
  u3_write_t* req_u = (struct _u3_write_t*)wri_u;
  void*       vod_p = req_u->vod_p;
  u3_mojo*    moj_u = req_u->moj_u;

  free(req_u->buf_y);
  free(req_u);

  if ( 0 != sas_i ) {
    u3l_log("newt: bad write %d\r\n", sas_i);
    moj_u->bal_f(vod_p, uv_strerror(sas_i));
  }
}

/* u3_newt_encode(): encode an atom to a length-prefixed byte buffer
*/
c3_y*
u3_newt_encode(u3_atom mat, c3_w* len_w)
{
  c3_w  met_w = u3r_met(3, mat);
  c3_y* buf_y;

  *len_w = 8 + met_w;
  buf_y  = c3_malloc(*len_w);

  //  write header; c3_d is futureproofing
  //
  buf_y[0] = ((met_w >> 0) & 0xff);
  buf_y[1] = ((met_w >> 8) & 0xff);
  buf_y[2] = ((met_w >> 16) & 0xff);
  buf_y[3] = ((met_w >> 24) & 0xff);
  buf_y[4] = buf_y[5] = buf_y[6] = buf_y[7] = 0;

  u3r_bytes(0, met_w, buf_y + 8, mat);
  u3z(mat);

  return buf_y;
}

/* u3_newt_write(): write atom to stream; free atom.
*/
void
u3_newt_write(u3_mojo* moj_u,
              u3_atom  mat,
              void*    vod_p)
{
  u3_write_t* req_u = c3_malloc(sizeof(*req_u));
  c3_w        len_w;
  c3_y*       buf_y = u3_newt_encode(mat, &len_w);
  uv_buf_t    buf_u;
  c3_i        err_i;

  req_u->moj_u = moj_u;
  req_u->buf_y = buf_y;
  buf_u = uv_buf_init((c3_c*)buf_y, len_w);

  if ( 0 != (err_i = uv_write((uv_write_t*)req_u,
                              (uv_stream_t*)&moj_u->pyp_u,
                              &buf_u, 1,
                              _newt_write_cb)) )
  {
    moj_u->bal_f(moj_u, uv_strerror(err_i));
  }
}
