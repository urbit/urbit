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
#include <ncurses/term.h>
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
    u3l_log("%s: error: %s\r\n", why_c, uv_strerror(err_i));
    c3_assert(0);
  } else {
    u3l_log("%s: file error\r\n", why_c);
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
