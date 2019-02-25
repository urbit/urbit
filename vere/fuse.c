/* v/fuse.c
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

/* helper routines adapted from FUSE example code
*/
  struct _fusedr_buf {
    c3_c* buf_c;
    c3_z  siz_z;
  };

  static void
  _fusedr_buf_add(fuse_req_t          req_u,
                  struct _fusedr_buf* buf_u,
                  const c3_c*         nam_c,
                  fuse_ino_t          ino_i)
  {
    struct stat sat_u;
    c3_z        old_z = buf_u->siz_z;

    buf_u->siz_z += fuse_add_direntry(req_u, NULL, 0, nam_c, NULL, 0);
    buf_u->buf_c = (c3_c*) realloc(buf_u->buf_c, buf_u->siz_z);

    memset(&sat_u, 0, sizeof(sat_u));
    sat_u.st_ino = ino_i;

    fuse_add_direntry(req_u,
                      buf_u->buf_c + old_z,
                      buf_u->siz_z - old_z,
                      nam_c,
                      &sat_u,
                      buf_u->siz_z);
  }

  static void
  _fuse_buf_reply(fuse_req_t          req_u,
                  c3_c*               buf_c,
                  c3_z                siz_z,
                  c3_f                off_f,
                  c3_z                max_z)
  {
    if ( off_f < siz_z ) {
      fuse_reply_buf(req_u,
                            buf_c + off_f,
                            c3_min(siz_z - off_f, max_z));
    } else {
      fuse_reply_buf(req_u, NULL, 0);
    }
  }

static void
_inode_init(void)
{
  u3_fuse* fus_u = &u3_Host.fus_u;

  memset(fus_u, 0, sizeof(u3_fuse));
  {
    fus_u->rot_u.ino_i = FUSE_ROOT_ID;
    fus_u->rot_u.val_u = 0;
    fus_u->rot_u.nam_c = strdup("/");
    fus_u->rot_u.pax_c = 0;
    fus_u->rot_u.val_u = 0;
    fus_u->rot_u.ref_w = 0x1fffffff;
    fus_u->rot_u.par_u = 0;
    fus_u->rot_u.kid_u = 0;
    fus_u->rot_u.nex_u = 0;
  }
  {
    fus_u->ion_u.ino_i = FUSE_ROOT_ID + 1;
    fus_u->ion_u.len_w = 64;
    fus_u->ion_u.nod_u = calloc(64, sizeof(struct fnod *));
    fus_u->ion_u.nod_u[FUSE_ROOT_ID] = &fus_u->rot_u;
  }
}

/* _inode_get(): look up an inode.
*/
static u3_fnod*
_inode_get(fuse_ino_t ino_i)
{
  u3_fuse* fus_u = &u3_Host.fus_u;

  c3_assert(ino_i < fus_u->ion_u.ino_i);
  return fus_u->ion_u.nod_u[ino_i];
}

/* _inode_new(): create an inode.
*/
static u3_fnod*
_inode_new(void)
{
  u3_fuse* fus_u = &u3_Host.fus_u;
  u3_fnod* nod_u = calloc(1, sizeof(u3_fnod));

  nod_u->ino_i = fus_u->ion_u.ino_i;
  fus_u->ion_u.nod_u[nod_u->ino_i] = nod_u;
  fus_u->ion_u.ino_i++;

  if ( fus_u->ion_u.len_w == fus_u->ion_u.ino_i ) {
    fus_u->ion_u.len_w *= 2;
    fus_u->ion_u.nod_u = realloc(fus_u->ion_u.nod_u,
                                 (fus_u->ion_u.len_w *
                                  sizeof(struct fnod *)));
  }
  return nod_u;
}

/* _inode_make(): set up an inode.
*/
static u3_fnod*
_inode_make(u3_fnod* par_u, c3_c* nam_c)
{
  u3_fnod* nod_u = _inode_new();

  nod_u->nam_c = nam_c;
  nod_u->par_u = par_u;
  nod_u->nex_u = par_u->kid_u;
  par_u->kid_u = nod_u;

  return nod_u;
}

/* _inode_stat(): fill stat buffer from inode; return c3y if available
*/
static c3_o
_inode_stat(u3_fnod* nod_u, struct stat* buf_u)
{
  memset(buf_u, 0, sizeof(struct stat));

  switch ( nod_u->typ_e ) {
    case u3_fuse_type_unknown: return c3n;
    case u3_fuse_type_file: {
      if ( 0 == nod_u->val_u ) {
        return c3n;
      }
      else {
        buf_u->st_mode = S_IFREG | 0444;
        buf_u->st_nlink = 1;
        buf_u->st_size = nod_u->val_u->siz_z;

        return c3y;
      }
    }
    case u3_fuse_type_directory: {
      buf_u->st_mode = S_IFDIR | 0555;
      buf_u->st_nlink = 2;

      return c3y;
    }
  }
}

/* _inode_path(): map inode path to noun.
*/
static u3_noun
_inode_path(u3_fnod* nod_u, u3_noun end)
{
  if ( nod_u->par_u == 0 ) {
    return end;
  }
  else {
    end = u3nc(u3i_string(nod_u->nam_c), end);

    return _inode_path(nod_u->par_u, end);
  }
}

/* _inode_load_arch(): load urbit's own "inode".
*/
static u3_weak
_inode_load_arch(u3_noun hap)
{
  if ( u3_nul == u3A->own ) {
    return u3_none;
  }
  else {
    u3_noun our = u3dc("scot", 'p', u3k(u3h(u3A->own)));
    u3_noun pax = u3nc(c3__cy, u3nq(our, c3__home, u3k(u3A->wen), hap));
    u3_noun val = u3v_peek(pax);

    if ( u3_nul == val ) {
      return u3_none;
    } else {
      u3_noun ret = u3k(u3t(val));

      u3z(val);
      return ret;
    }
  }
}

/* _inode_load_data(): load urbit file contents.
*/
static u3_weak
_inode_load_data(u3_noun hap)
{
  if ( u3_nul == u3A->own ) {
    return u3_none;
  }
  else {
    u3_noun our = u3dc("scot", 'p', u3k(u3h(u3A->own)));
    u3_noun pax = u3nc(c3__cx, u3nq(our, c3__home, u3k(u3A->wen), hap));
    u3_noun val = u3v_peek(pax);

    if ( u3_nul == val ) {
      return u3_none;
    } else {
      u3_noun ret = u3k(u3t(val));

      u3z(val);
      if ( c3n == u3ud(ret) ) {
        uL(fprintf(uH, "inode_load_data: not an atom\n"));

        u3z(ret);
        ret = 0;
      }
      return ret;
    }
  }
}

/* _inode_fill_directory(): fill directory inode.
*/
static u3_fdir*
_inode_fill_directory(u3_fnod* par_u, u3_noun kiz)
{
  u3_fdir* dir_u;

  dir_u = malloc(sizeof(u3_fdir));
  dir_u->num_w = 0;

  {
    u3_noun zik = kiz;

    while ( u3_nul != zik ) {
      u3_noun  ph_zik = u3h(u3h(zik));
      c3_c*    nam_c  = u3r_string(ph_zik);
      u3_fent* fen_u  = calloc(1, sizeof(u3_fent));

      fen_u->nod_u = _inode_make(par_u, nam_c);
      fen_u->nex_u = dir_u->fen_u;
      dir_u->fen_u = fen_u;

      dir_u->num_w += 1;
      zik = u3t(zik);
    }
    u3z(kiz);
  }
  return dir_u;
}

/* _inode_fill_file(): fill file inode.
*/
static u3_fval*
_inode_fill_file(u3_atom dat)
{
  u3_fval* val_u = calloc(1, sizeof(u3_fval));

  val_u->siz_z = u3r_met(3, dat);
  val_u->buf_y = malloc(val_u->siz_z);

  u3r_bytes(0, val_u->siz_z, val_u->buf_y, dat);
  u3z(dat);

  return val_u;
}

/* _inode_load(): load inode value.
*/
static c3_o
_inode_load(u3_fnod* nod_u)
{
  if ( u3_nul == u3A->own ) {
    return c3n;
  }
  else {
    if ( nod_u->typ_e != u3_fuse_type_unknown ) {
      return c3y;
    }
    else {
      u3_noun hap = _inode_path(nod_u, u3_nul);
      u3_weak ark = _inode_load_arch(u3k(hap));
      c3_o    ret;

      if ( u3_none == ark ) {
        ret = c3n;
      }
      else {
        if ( u3_nul == u3h(ark) ) {
          nod_u->typ_e = u3_fuse_type_directory;
          nod_u->dir_u = _inode_fill_directory
            (nod_u, u3qdb_tap(u3t(ark)));
        }
        else {
          u3_noun dat;

          nod_u->typ_e = u3_fuse_type_file;
          dat = _inode_load_data(u3k(hap));
          nod_u->val_u = _inode_fill_file(dat);
        }
        ret = c3y;
        u3z(ark);
      }
      u3z(hap);
      return ret;
    }
  }
}

  /**
   * Initialize filesystem
   *
   * Called before any other filesystem method
   *
   * There's no reply to this function
   *
   * @param userdata the user data passed to fuse_lowlevel_new()
   */
static void
_fuse_ll_init(void*                  usr_v,
              struct fuse_conn_info* con_u)
{
  uL(fprintf(uH, "ll_init\n"));
  {
    _inode_init();
  }
}

  /**
   * Look up a directory entry by name and get its attributes.
   *
   * Valid replies:
   *   fuse_reply_entry
   *   fuse_reply_err
   *
   * @param req request handle
   * @param parent inode number of the parent directory
   * @param name the name to look up
   */
static void
_fuse_ll_lookup(fuse_req_t  req_u,
                fuse_ino_t  pno_i,
                const c3_c* nam_c)
{
  uL(fprintf(uH, "ll_lookup %ld %s\n", pno_i, nam_c));
  {
    u3_fnod* par_u = _inode_get(pno_i);
    u3_fnod* nod_u;

    //  Find, then make.
    {
      for ( nod_u = par_u->kid_u; nod_u; nod_u = nod_u->nex_u ) {
        if ( !strcmp(nam_c, nod_u->nam_c) ) {
          break;
        }
      }

      if ( !nod_u ) {
        nod_u = _inode_make(par_u, strdup(nam_c));
      }
    }

    if ( c3n == _inode_load(nod_u) ) {
      fuse_reply_err(req_u, ENOENT);
    }
    else {
      struct fuse_entry_param ent_u;

      memset(&ent_u, 0, sizeof(ent_u));
      if ( c3n == _inode_stat(nod_u, &ent_u.attr) ) {
        fuse_reply_err(req_u, ENOENT);
      }
      ent_u.ino = nod_u->ino_i;
      ent_u.generation = 1;
      ent_u.attr_timeout = 1.0;
      ent_u.entry_timeout = 1.0;

      fuse_reply_entry(req_u, &ent_u);
    }
  }
}

  /**
   * Get file attributes
   *
   * Valid replies:
   *   fuse_reply_attr
   *   fuse_reply_err
   *
   * @param req request handle
   * @param ino the inode number
   * @param fi for future use, currently always NULL
   */
static void
_fuse_ll_getattr(fuse_req_t             req_u,
                 fuse_ino_t             ino_i,
                 struct fuse_file_info* ffi_u)
{
  uL(fprintf(uH, "ll_getattr %ld\n", ino_i));
  {
    u3_fnod* nod_u = _inode_get(ino_i);

    if ( c3n == _inode_load(nod_u) ) {
      fuse_reply_err(req_u, ENOENT);
    }
    else {
      struct stat buf_u;

      if ( c3n == _inode_stat(nod_u, &buf_u) ) {
        fuse_reply_err(req_u, ENOENT);
      }
      else {
        fuse_reply_attr(req_u, &buf_u, 1.0);
      }
    }
  }
}

  /**
   * Read directory
   *
   * Send a buffer filled using fuse_add_direntry(), with size not
   * exceeding the requested size.  Send an empty buffer on end of
   * stream.
   *
   * fi->fh will contain the value set by the opendir method, or
   * will be undefined if the opendir method didn't set any value.
   *
   * Valid replies:
   *   fuse_reply_buf
   *   fuse_reply_err
   *
   * @param req request handle
   * @param ino the inode number
   * @param size maximum number of bytes to send
   * @param off offset to continue reading the directory stream
   * @param fi file information
   */
static void
_fuse_ll_readdir(fuse_req_t             req_u,
                 fuse_ino_t             ino_i,
                 c3_z                   max_z,
                 c3_f                   off_f,
                 struct fuse_file_info* ffi_u)
{
  uL(fprintf(uH, "ll_readdir %ld %ld %" PRIu64 "\n", ino_i, max_z, off_f));
  {
    u3_fnod* nod_u = _inode_get(ino_i);

    if ( c3n == _inode_load(nod_u) ) {
      fuse_reply_err(req_u, ENOENT);
    }
    else if ( u3_fuse_type_directory != nod_u->typ_e ) {
      fuse_reply_err(req_u, ENOTDIR);
    }
    else {
      struct _fusedr_buf buf_u;

      memset(&buf_u, 0, sizeof(buf_u));
      _fusedr_buf_add(req_u, &buf_u, ".", ino_i);
      _fusedr_buf_add(req_u, &buf_u, "..", nod_u->par_u
                                           ? nod_u->par_u->ino_i
                                           : ino_i);
      {
        u3_fent* fen_u;

        for ( fen_u = nod_u->dir_u->fen_u; fen_u; fen_u = fen_u->nex_u ) {
          _fusedr_buf_add(req_u,
                          &buf_u,
                          fen_u->nod_u->nam_c,
                          fen_u->nod_u->ino_i);
        }
      }
      _fuse_buf_reply(req_u, buf_u.buf_c, buf_u.siz_z, off_f, max_z);
    }
  }
}

  /**
   * Open a file
   *
   * Open flags (with the exception of O_CREAT, O_EXCL, O_NOCTTY and
   * O_TRUNC) are available in fi->flags.
   *
   * Filesystem may store an arbitrary file handle (pointer, index,
   * etc) in fi->fh, and use this in other all other file operations
   * (read, write, flush, release, fsync).
   *
   * Filesystem may also implement stateless file I/O and not store
   * anything in fi->fh.
   *
   * There are also some flags (direct_io, keep_cache) which the
   * filesystem may set in fi, to change the way the file is opened.
   * See fuse_file_info structure in <fuse_common.h> for more details.
   *
   * Valid replies:
   *   fuse_reply_open
   *   fuse_reply_err
   *
   * @param req request handle
   * @param ino the inode number
   * @param fi file information
   */
static void
_fuse_ll_open(fuse_req_t             req_u,
              fuse_ino_t             ino_i,
              struct fuse_file_info* ffi_u)
{
  uL(fprintf(uH, "ll_open %ld\n", ino_i));
  {
    u3_fnod* nod_u = _inode_get(ino_i);

    if ( c3n == _inode_load(nod_u) ) {
      fuse_reply_err(req_u, ENOENT);
    }
    else if ( u3_fuse_type_file != nod_u->typ_e ) {
      fuse_reply_err(req_u, ENOTDIR);
    }
    else if ( (ffi_u->flags & 3) != O_RDONLY ) {
      fuse_reply_err(req_u, EACCES);
    }
    else {
      fuse_reply_open(req_u, ffi_u);
    }
  }
}

  /**
   * Read data
   *
   * Read should send exactly the number of bytes requested except
   * on EOF or error, otherwise the rest of the data will be
   * substituted with zeroes.  An exception to this is when the file
   * has been opened in 'direct_io' mode, in which case the return
   * value of the read system call will reflect the return value of
   * this operation.
   *
   * fi->fh will contain the value set by the open method, or will
   * be undefined if the open method didn't set any value.
   *
   * Valid replies:
   *   fuse_reply_buf
   *   fuse_reply_err
   *
   * @param req request handle
   * @param ino the inode number
   * @param size number of bytes to read
   * @param off offset to read from
   * @param fi file information
   */
static void
_fuse_ll_read(fuse_req_t             req_u,
              fuse_ino_t             ino_i,
              c3_z                   max_z,
              c3_f                   off_f,
              struct fuse_file_info* ffi_u)
{
  uL(fprintf(uH, "ll_read %ld %ld %" PRIu64 "\n", ino_i, max_z, off_f));
  {
    u3_fnod* nod_u = _inode_get(ino_i);

    if ( c3n == _inode_load(nod_u) ) {
      fuse_reply_err(req_u, ENOENT);
    } else {
      _fuse_buf_reply(req_u,
                      (c3_c*)(nod_u->val_u->buf_y),
                      nod_u->val_u->siz_z,
                      off_f,
                      max_z);
    }
  }
}

static struct fuse_lowlevel_ops fuse_api = {
  .init      = _fuse_ll_init,
  .lookup    = _fuse_ll_lookup,
  .getattr   = _fuse_ll_getattr,
  .readdir   = _fuse_ll_readdir,
  .open	     = _fuse_ll_open,
  .read	     = _fuse_ll_read,
};

/* _fuse_poll_cb():
*/
static void
_fuse_poll_cb(uv_poll_t* wax_u,
              c3_i       sas_i,
              c3_i       evt_i)
{
  u3_fuse* fus_u = &u3_Host.fus_u;

  uL(fprintf(uH, "fuse_poll_cb\n"));
  {
    c3_z  buf_z = fuse_chan_bufsize(fus_u->cha_u);
    c3_y* buf_y = malloc(buf_z + 1);
    c3_i  res_i = fuse_chan_recv(&fus_u->cha_u, (c3_c *)buf_y, buf_z);

    if ( res_i < 0 ) {
			if ( (res_i != -EINTR) && (res_i != -EAGAIN) ) {
        uL(fprintf(uH, "fuse_poll_cb: error: %s\n", strerror(res_i)));
        c3_assert(0);
      }
    }
    else {
      uL(fprintf(uH, "fuse_session_process\n"));

      fuse_session_process(fus_u->sez_u,
                           (c3_c *)buf_y,
                           buf_z,
                           fus_u->cha_u);

      uv_poll_start(&fus_u->wax_u, UV_READABLE, _fuse_poll_cb);
    }
  }
}

/* u3_fuse_io_init(): initialize FUSE.
*/
void
u3_fuse_io_init(void)
{
  u3_fuse* fus_u = &u3_Host.fus_u;

#if 0
  fus_u->mnt_c = malloc(strlen(u3_Host.dir_c) + 16);
  strcpy(fus_u->mnt_c, u3_Host.dir_c);
  strncat(fus_u->mnt_c, "/.urb/fun", 14);
#else
  fus_u->mnt_c = strdup("/Users/cyarvin/urbit");
#endif
  mkdir(fus_u->mnt_c, 0755);

  uL(fprintf(uH, "fuse: mounting: %s\n", fus_u->mnt_c));

  if ( !(fus_u->cha_u = fuse_mount(fus_u->mnt_c, 0)) ) {
    uL(fprintf(uH, "fuse: could not mount %s\n", fus_u->mnt_c));
  }

  if ( !(fus_u->sez_u=fuse_lowlevel_new(0, &fuse_api, sizeof(fuse_api), 0)) ) {
    uL(fprintf(uH, "fuse: could not create session\n"));
  }
  fuse_session_add_chan(fus_u->sez_u, fus_u->cha_u);

  {
    c3_i fid_i = fuse_chan_fd(fus_u->cha_u);
    c3_i err_i;

    uL(fprintf(uH, "fuse: fd: %d (loop %p)\n", fid_i, u3L));
    if ( (err_i = uv_poll_init(u3L, &fus_u->wax_u, fid_i)) < 0 ) {
      uL(fprintf(uH, "fuse: poll_init failed: %s\n", uv_strerror(err_i)));
    }
    uv_poll_start(&fus_u->wax_u, UV_READABLE, _fuse_poll_cb);
  }
}


/* u3_fuse_io_exit(): shut down FUSE.
*/
void
u3_fuse_io_exit(void)
{
  u3_fuse* fus_u = &u3_Host.fus_u;

  uv_poll_stop(&fus_u->wax_u);

  fuse_session_remove_chan(fus_u->cha_u);
  fuse_session_destroy(fus_u->sez_u);
  fuse_unmount(fus_u->mnt_c, fus_u->cha_u);

  uL(fprintf(uH, "fuse: unmounted: %s\n", fus_u->mnt_c));

}
