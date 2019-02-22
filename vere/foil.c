/* vere/foil.c
**
**  This file is in the public domain.
*/

#include "all.h"

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <dirent.h>
#include <stdint.h>
#include <uv.h>
#include <termios.h>
#include <term.h>
#include <errno.h>
#include <libgen.h>
#include <ftw.h>

#include "vere/vere.h"

    /*  assumptions:
    **    all measurements are in chubs (double-words, c3_d, uint64_t).
    **    little-endian addressing is ASSUMED.
    **
    **  framing:
    **    the last two chubs of a frame:
    **
    **      {
    **        64-bit frame length
    **        {
    **          (high 32 bits) mug of frame
    **          (low 32 bits) mug of current address
    **        }
    **      }
    **
    **    we can scan for one of these frames with very low probability
    **    of a false positive.  we always write to and read from the end
    **    of a file.  a frame position points to its end.
    **
    **  protocol:
    **    once the callback is called, all results are fully fsynced.
    **    all callbacks are optional and can be passed 0.
    */

/* _foil_fail(): fail with error.
*/
static void
_foil_fail(const c3_c* why_c, c3_i err_i)
{
  if ( err_i ) {
    fprintf(stderr, "%s: error: %s\r\n", why_c, uv_strerror(err_i));
    c3_assert(0);
  } else {
    fprintf(stderr, "%s: file error\r\n", why_c);
  }
  exit(1);
}

/* _foil_close(): close file, blockingly.
*/
static void
_foil_close(uv_file fil_f)
{
  c3_i    err_i;
  uv_fs_t ruq_u;

  if ( 0 != (err_i = uv_fs_close(u3L, &ruq_u, fil_f, 0)) ) {
    _foil_fail("uv_fs_close", err_i);
  }
}

/* _foil_path(): allocate path.
*/
static c3_c*
_foil_path(u3_dire*    dir_u,
           const c3_c* nam_c)
{
  c3_w  len_w = strlen(dir_u->pax_c);
  c3_c* pax_c;

  pax_c = c3_malloc(1 + len_w + 1 + strlen(nam_c));
  strcpy(pax_c, dir_u->pax_c);
  pax_c[len_w] = '/';
  strcpy(pax_c + len_w + 1, nam_c);

  return pax_c;
}

/* u3_foil_folder(): load directory, blockingly.  null if nonexistent.
*/
u3_dire*
u3_foil_folder(const c3_c* pax_c)
{
  u3_dire*    dir_u;
  uv_fs_t     ruq_u;
  uv_dirent_t den_u;
  c3_i        err_i;

  /*  open directory, synchronously
  */
  {
    err_i = uv_fs_scandir(u3L, &ruq_u, pax_c, 0, 0);

    if ( err_i < 0 ) {
      if ( UV_ENOENT != err_i ) {
        _foil_fail(pax_c, err_i);
        return 0;
      }
      else {
        if ( 0 != (err_i = uv_fs_mkdir(u3L, &ruq_u, pax_c, 0700, 0)) ) {
          _foil_fail(pax_c, err_i);
          return 0;
        }
        else {
          uv_fs_req_cleanup(&ruq_u);
          return u3_foil_folder(pax_c);
        }
      }
    }
    dir_u = c3_malloc(sizeof *dir_u);
    dir_u->all_u = 0;
    dir_u->pax_c = c3_malloc(1 + strlen(pax_c));
    strcpy(dir_u->pax_c, pax_c);
  }

  /*  create entries for all files
  */
  while ( UV_EOF != uv_fs_scandir_next(&ruq_u, &den_u) ) {
    if ( UV_DIRENT_FILE == den_u.type ) {
      u3_dent* det_u = c3_malloc(sizeof(*det_u));

      det_u->nam_c = c3_malloc(1 + strlen(den_u.name));
      strcpy(det_u->nam_c, den_u.name);

      det_u->nex_u = dir_u->all_u;
      dir_u->all_u = det_u;
    }
  }

  /*  clean up request
  */
  {
    uv_fs_req_cleanup(&ruq_u);
  }

  /* open directory file for reading, to fsync
  */
  {
    if ( 0 > (err_i = uv_fs_open(u3L,
                                 &ruq_u,
                                 pax_c,
                                 O_RDONLY,
                                 0600,
                                 0)) )
    {
      _foil_fail("open directory", err_i);
      return 0;
    }
    dir_u->fil_u = ruq_u.result;

    uv_fs_req_cleanup(&ruq_u);
  }
  return dir_u;
}

/* u3_foil_create(): create a new, empty, open file, not syncing.
*/
  struct _foil_create_request {
    uv_fs_t  ruq_u;
    void     (*fun_f)(void*, u3_foil*);
    void*    vod_p;
    u3_dire* dir_u;
    c3_c*    nam_c;
    c3_c*    pax_c;
  };

  static void
  _foil_create_cb(uv_fs_t* ruq_u)
  {
    struct _foil_create_request* req_u = (void *)ruq_u;
    u3_foil*                     fol_u;

    fol_u = c3_malloc(sizeof(*fol_u));
    fol_u->fil_u = ruq_u->result;
    fol_u->dir_u = req_u->dir_u;
    fol_u->nam_c = req_u->nam_c;
    fol_u->end_d = 0;

    req_u->fun_f(req_u->vod_p, fol_u);

    c3_free(req_u->pax_c);
    uv_fs_req_cleanup(ruq_u);
    c3_free(req_u);
  }
void
u3_foil_create(void      (*fun_f)(void*,    //  context pointer
                                  u3_foil*),//  file object
               void*       vod_p,           //  context pointer
               u3_dire*    dir_u,           //  directory
               const c3_c* nam_c)           //  name of new file
{
  c3_c* pax_c;
  c3_i  err_i;

  /* construct full path
  */
  pax_c = _foil_path(dir_u, nam_c);

  /* perform create
  */
  {
    struct _foil_create_request* req_u;

    req_u = c3_malloc(sizeof(*req_u));

    req_u->fun_f = fun_f;
    req_u->vod_p = vod_p;
    req_u->dir_u = dir_u;
    req_u->nam_c = c3_malloc(1 + strlen(nam_c));
    strcpy(req_u->nam_c, nam_c);
    req_u->pax_c = pax_c;

    if ( 0 != (err_i = uv_fs_open(u3L,
                                  &req_u->ruq_u,
                                  pax_c,
                                  O_CREAT | O_WRONLY,
                                  0600,
                                  _foil_create_cb)) )
    {
      _foil_fail("uv_fs_open", err_i);
    }
  }
}

/* u3_foil_absorb(): open logfile, truncating to last good frame; blocking.
*/
u3_foil*
u3_foil_absorb(u3_dire* dir_u,              //  directory
               c3_c*    nam_c)              //  filename
{
  u3_foil* fol_u;
  uv_fs_t  ruq_u;
  c3_i     err_i;

  /* open file and create wrapper
  */
  {
    c3_c* pax_c = _foil_path(dir_u, nam_c);

    if ( 0 > (err_i = uv_fs_open(u3L,
                                 &ruq_u,
                                 pax_c,
                                 O_RDWR | O_CREAT,
                                 0600,
                                 0)) )
    {
      _foil_fail(pax_c, err_i);
      c3_free(pax_c);
      return 0;
    }
    c3_free(pax_c);

    fol_u = c3_malloc(sizeof(*fol_u));
    fol_u->dir_u = dir_u;
    fol_u->fil_u = ruq_u.result;
    fol_u->nam_c = c3_malloc(1 + strlen(nam_c));
    strcpy(fol_u->nam_c, nam_c);

    uv_fs_req_cleanup(&ruq_u);
  }

  /* measure file
  */
  {
    if ( 0 != (err_i = uv_fs_fstat(u3L, &ruq_u, fol_u->fil_u, 0)) ) {
      _foil_fail("uv_fs_fstat", err_i);
      return 0;
    }
    if ( 0 != (7 & ruq_u.statbuf.st_size) ) {
      _foil_fail("logfile size corrupt", 0);
      return 0;
    }
    fol_u->end_d = (ruq_u.statbuf.st_size >> 3ULL);
  }

  /* XX: scan for good frame.
  */
  return fol_u;
}

/* u3_foil_delete(): delete a file; free descriptor.
*/
  struct _foil_delete_request {
    uv_fs_t  ruq_u;
    void     (*fun_f)(void*);
    void*    vod_p;
    u3_foil* fol_u;
    c3_c*    pax_c;
  };

  static void
  _foil_delete_cb(uv_fs_t* ruq_u)
  {
    struct _foil_delete_request* req_u = (void *)ruq_u;

    if ( req_u->fun_f ) {
      req_u->fun_f(req_u->vod_p);
    }

    c3_free(req_u->pax_c);
    c3_free(req_u->fol_u->nam_c);
    c3_free(req_u->fol_u);

    uv_fs_req_cleanup(ruq_u);
    c3_free(req_u);
  }
void
u3_foil_delete(void   (*fun_f)(void*),      //  context pointer
               void*    vod_p,              //  context pointer
               u3_foil* fol_u)              //  file to delete
{
  c3_i  err_i;
  c3_c* pax_c;

  /* construct full path
  */
  pax_c = _foil_path(fol_u->dir_u, fol_u->nam_c);

  /* perform delete
  */
  {
    struct _foil_delete_request* req_u;

    req_u = c3_malloc(sizeof(*req_u));

    req_u->fun_f = fun_f;
    req_u->vod_p = vod_p;
    req_u->fol_u = fol_u;
    req_u->pax_c = pax_c;

    if ( 0 != (err_i = uv_fs_unlink(u3L,
                                    &req_u->ruq_u,
                                    pax_c,
                                    _foil_delete_cb)) )
    {
      _foil_fail("uv_fs_unlink", err_i);
    }
  }
}

/* u3_foil_append(): write a frame at the end of a file, freeing the buffer.
*/
  struct _foil_append_request {
    uv_fs_t  ruq_u;
    void     (*fun_f)(void*);
    void*    vod_p;
    u3_foil* fol_u;
    c3_d*    fam_d;
    c3_d*    buf_d;
  };
  static void
  _foil_append_cb_2(uv_fs_t* ruq_u)
  {
    struct _foil_append_request* req_u = (void*) ruq_u;

    req_u->fun_f(req_u->vod_p);
    uv_fs_req_cleanup(ruq_u);
    c3_free(req_u);
  }
  static void
  _foil_append_cb_1(uv_fs_t* ruq_u)
  {
    struct _foil_append_request* req_u = (void*) ruq_u;

    uv_fs_req_cleanup(ruq_u);
    c3_free(req_u->buf_d);

    uv_fs_fsync(u3L, &req_u->ruq_u,
                     req_u->fol_u->fil_u,
                     _foil_append_cb_2);
  }
void
u3_foil_append(void   (*fun_f)(void*),      //  context pointer
               void*    vod_p,              //  context pointer
               u3_foil* fol_u,              //  file
               c3_d*    buf_d,              //  buffer to write from
               c3_d     len_d)              //  length in chubs
{
  c3_d                         pos_d = fol_u->end_d;
  struct _foil_append_request* req_u;
  c3_i                         err_i;

  /* set up request
  */
  {
    req_u = c3_malloc(sizeof(*req_u));
    req_u->fun_f = fun_f;
    req_u->vod_p = vod_p;
    req_u->fol_u = fol_u;
    req_u->buf_d = buf_d;
    req_u->fam_d = c3_malloc(16);
  }

  /* framing
  */
  {
    c3_w top_w, bot_w;

    fol_u->end_d = pos_d + len_d + 2;

    /*  XX: assumes "little-endian won", 32-bit frame length.
    */
    top_w = u3r_mug_words((c3_w *)(void *) buf_d, (2 * len_d));
    bot_w = (req_u->fol_u->end_d & 0xffffffff);
    bot_w = u3r_mug_words(&bot_w, 1);

    req_u->fam_d[0] = len_d;
    req_u->fam_d[1] = ((c3_d)top_w) << 32ULL | ((c3_d) bot_w);
  }

  /* do it
  */
  {
    uv_buf_t buf_u[2];

    buf_u[0] = uv_buf_init((void *)buf_d, (len_d * 8));
    buf_u[1] = uv_buf_init((void *)req_u->fam_d, 16);

    if ( 0 != (err_i = uv_fs_write(u3L,
                                   &req_u->ruq_u,
                                   fol_u->fil_u,
                                   buf_u,
                                   2,
                                   (8ULL * pos_d),
                                   _foil_append_cb_1)) )
    {
      _foil_fail("uv_fs_write", err_i);
    }
  }
}

/* u3_foil_reveal(): read the frame before a position, blocking.
*/
c3_d*
u3_foil_reveal(u3_foil* fol_u,              //  file from
               c3_d*    sop_d,              //  end position/prev end
               c3_d*    len_d)              //  length return
{
  c3_d    pos_d = *sop_d;
  c3_d    fam_d[2];
  c3_l    mug_l;
  uv_fs_t ruq_u;
  c3_i    err_i;

  c3_assert(pos_d >= 2);
  c3_assert(pos_d <= fol_u->end_d);

  /* read frame data
  */
  {
    uv_buf_t buf_u = uv_buf_init((void *)fam_d, 16);

    fam_d[0] = fam_d[1] = 0;
    if ( 0 > (err_i = uv_fs_read(u3L,
                                 &ruq_u,
                                 fol_u->fil_u,
                                 &buf_u, 1,
                                 (8ULL * (pos_d - 2ULL)),
                                 0)) )
    {
      _foil_fail("uv_fs_read", err_i);
      return 0;
    }
    uv_fs_req_cleanup(&ruq_u);
  }

  /* validate frame
  */
  {
    c3_w top_w, bot_w;
    c3_l chk_l;

    *len_d = fam_d[0];
    if ( *len_d > (pos_d - 2ULL) ) {
      _foil_fail("corrupt frame a", 0);
      return 0;
    }

    top_w = fam_d[1] >> 32ULL;
    mug_l = top_w;

    bot_w = fam_d[1] & 0xffffffff;
    chk_l = (pos_d & 0xffffffff);
    chk_l = u3r_mug_words(&chk_l, 1);
    if ( bot_w != chk_l ) {
      _foil_fail("corrupt frame b", 0);
    }
  }

  /* read frame
  */
  {
    c3_d*    buf_d = c3_malloc(8 * *len_d);
    uv_buf_t buf_u = uv_buf_init((void *)buf_d, 8 * *len_d);
    c3_l     gum_l;

    if ( 0 > (err_i = uv_fs_read(u3L,
                                 &ruq_u,
                                 fol_u->fil_u,
                                 &buf_u, 1,
                                 (8ULL * (pos_d - (*len_d + 2ULL))),
                                 0) ) )
    {
      _foil_fail("uv_fs_read", err_i);
      return 0;
    }
    uv_fs_req_cleanup(&ruq_u);

    gum_l = u3r_mug_words((c3_w *)(void *) buf_d, (2 * *len_d));
    if ( mug_l != gum_l ) {
      _foil_fail("corrupt frame c", 0);
      return 0;
    }
    *sop_d = (pos_d - (*len_d + 2ULL));
    return buf_d;
  }
}

/* u3_foil_invent(): create a new file with one frame, freeing buffer; sync.
*/
  struct _foil_invent_request {
    uv_fs_t        ruq_u;
    void           (*fun_f)(void*, u3_foil*);
    u3_foil*       fol_u;
    void*          vod_p;
    c3_d*          buf_d;
    c3_d           len_d;
    c3_d           num_d;
#if 0
    struct timeval bef_u;
#endif
  };
  static void
  _foil_invent_cb_2a(void* req_p)
  {
    struct _foil_invent_request* req_u = req_p;

    if ( 1 == req_u->num_d ) {
#if 0
    {
      struct timeval aft_u, gap_u;
      c3_w   mls_w;

      gettimeofday(&aft_u, 0);
      timersub(&aft_u, &req_u->bef_u, &gap_u);
      mls_w = (gap_u.tv_sec * 1000) + (gap_u.tv_usec / 1000);

      fprintf(stderr, "invent ms: %d\r\n", mls_w);
    }
#endif
      req_u->fun_f(req_u->vod_p, req_u->fol_u);
      _foil_close(req_u->fol_u->fil_u);

      c3_free(req_u);
    }
    else {
      req_u->num_d++;
    }
  }

  static void
  _foil_invent_cb_2b(uv_fs_t* ruq_u)
  {
    struct _foil_invent_request* req_u = (void *)ruq_u;

    uv_fs_req_cleanup(ruq_u);
    _foil_invent_cb_2a(req_u);
  }

  static void
  _foil_invent_cb_1(void*    req_p,
                    u3_foil* fol_u)
  {
    struct _foil_invent_request* req_u = req_p;

    req_u->fol_u = fol_u;

    /* fsync the parent directory, since we just created a file.
    */
    uv_fs_fsync(u3L, &req_u->ruq_u,
                     req_u->fol_u->dir_u->fil_u,
                     _foil_invent_cb_2b);

    u3_foil_append(_foil_invent_cb_2a,
                   req_u,
                   fol_u,
                   req_u->buf_d,
                   req_u->len_d);
  }
void
u3_foil_invent(void   (*fun_f)(void*,       //  context pointer
                               u3_foil*),   //  new file
               void*    vod_p,              //  context pointer
               u3_dire* dir_u,              //  directory
               c3_c*    nam_c,              //  filename
               c3_d*    buf_d,              //  buffer (to free)
               c3_d     len_d)              //  length
{
  struct _foil_invent_request* req_u;

  req_u = malloc(sizeof(*req_u));
  req_u->fun_f = fun_f;
  req_u->fol_u = 0;
  req_u->vod_p = vod_p;
  req_u->buf_d = buf_d;
  req_u->len_d = len_d;
  req_u->num_d = 0;
#if 0
  gettimeofday(&req_u->bef_u, 0);
#endif

  u3_foil_create(_foil_invent_cb_1, req_u, dir_u, nam_c);
}
