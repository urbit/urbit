/* vere/unix.c
**
*/
#include "all.h"
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <dirent.h>
#include <uv.h>
#include <termios.h>
#include <term.h>
#include <errno.h>
#include <libgen.h>
#include <ftw.h>

#include "vere/vere.h"

/* _unix_down(): descend path.
*/
static c3_c*
_unix_down(c3_c* pax_c, c3_c* sub_c)
{
  c3_w pax_w = strlen(pax_c);
  c3_w sub_w = strlen(sub_c);
  c3_c* don_c = c3_malloc(pax_w + sub_w + 2);

  strncpy(don_c, pax_c, pax_w);
  don_c[pax_w] = '/';
  strncpy(don_c + pax_w + 1, sub_c, sub_w);
  don_c[pax_w + 1 + sub_w] = '\0';

  return don_c;
}

/* _unix_string_to_path(): convert c string to u3_noun path
 *
 * c string must begin with the pier path plus mountpoint
*/
static u3_noun
_unix_string_to_path_helper(c3_c* pax_c) {
  c3_assert(pax_c[-1] == '/');
  c3_c* end_w = strchr(pax_c, '/');
  if ( !end_w ) {
    end_w = strrchr(pax_c, '.');
    if ( !end_w ) {
      return u3nc(u3i_string(pax_c), u3_nul);
    }
    else {
      return u3nt(u3i_bytes(end_w - pax_c, (c3_y*) pax_c),
                  u3i_string(end_w + 1),
                  u3_nul);
    }
  }
  else {
    return u3nc(u3i_bytes(end_w - pax_c, (c3_y*) pax_c),
                _unix_string_to_path_helper(end_w + 1));
  }
}
static u3_noun
_unix_string_to_path(u3_pier *pir_u, c3_c* pax_c) {
  pax_c += strlen(pir_u->pax_c) + 1;
  c3_c* pox_c = strchr(pax_c, '/');
  if ( !pox_c ) {
    pox_c = strchr(pax_c, '.');
    if ( !pox_c ) {
      return u3_nul;
    }
    else {
      return u3nc(u3i_string(pox_c + 1), u3_nul);
    }
  }
  else {
    return _unix_string_to_path_helper(pox_c + 1);
  }
}

/* _unix_rm_r_cb(): callback to delete individual files/directories
*/
static c3_i
_unix_rm_r_cb(const c3_c* pax_c,
              const struct stat* buf_u,
              c3_i typeflag,
              struct FTW* ftw_u)
{
  switch ( typeflag ) {
    default:
      uL(fprintf(uH, "bad file type in rm_r: %s\r\n", pax_c));
      break;
    case FTW_F:
      if ( 0 != unlink(pax_c) && ENOENT != errno ) {
        uL(fprintf(uH, "error unlinking (in rm_r) %s: %s\n",
                   pax_c, strerror(errno)));
        c3_assert(0);
      }
      break;
    case FTW_D:
      uL(fprintf(uH, "shouldn't have gotten pure directory: %s\r\n", pax_c));
      break;
    case FTW_DNR:
      uL(fprintf(uH, "couldn't read directory: %s\r\n", pax_c));
      break;
    case FTW_NS:
      uL(fprintf(uH, "couldn't stat path: %s\r\n", pax_c));
      break;
    case FTW_DP:
      if ( 0 != rmdir(pax_c) && ENOENT != errno ) {
        uL(fprintf(uH, "error rmdiring %s: %s\n", pax_c, strerror(errno)));
        c3_assert(0);
      }
      break;
    case FTW_SL:
      uL(fprintf(uH, "got symbolic link: %s\r\n", pax_c));
      break;
    case FTW_SLN:
      uL(fprintf(uH, "got nonexistent symbolic link: %s\r\n", pax_c));
      break;
  }

  return 0;
}

/* _unix_rm_r(): rm -r directory
*/
static void
_unix_rm_r(c3_c* pax_c)
{
  if ( 0 > nftw(pax_c, _unix_rm_r_cb, 100, FTW_DEPTH | FTW_PHYS )
       && ENOENT != errno) {
    uL(fprintf(uH, "rm_r error on %s: %s\r\n", pax_c, strerror(errno)));
  }
}

/* _unix_mkdir(): mkdir, asserting.
*/
static void
_unix_mkdir(c3_c* pax_c)
{
  if ( 0 != mkdir(pax_c, 0755) && EEXIST != errno) {
    uL(fprintf(uH, "error mkdiring %s: %s\n", pax_c, strerror(errno)));
    c3_assert(0);
  }
}

/* _unix_write_file_hard(): write to a file, overwriting what's there
*/
static c3_w
_unix_write_file_hard(c3_c* pax_c, u3_noun mim)
{
  c3_i  fid_i = open(pax_c, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  c3_w  len_w, rit_w, siz_w, mug_w = 0;
  c3_y* dat_y;

  u3_noun dat = u3t(u3t(mim));

  if ( fid_i < 0 ) {
    uL(fprintf(uH, "error opening %s for writing: %s\r\n",
               pax_c, strerror(errno)));
    u3z(mim);
    return 0;
  }

  siz_w = u3h(u3t(mim));
  len_w = u3r_met(3, dat);
  dat_y = c3_calloc(siz_w);

  u3r_bytes(0, len_w, dat_y, dat);
  u3z(mim);

  rit_w = write(fid_i, dat_y, siz_w);

  if ( rit_w != siz_w ) {
    uL(fprintf(uH, "error writing %s: %s\r\n",
               pax_c, strerror(errno)));
    mug_w = 0;
  }
  else {
    mug_w = u3r_mug_bytes(dat_y, len_w);
  }

  close(fid_i);
  free(dat_y);

  return mug_w;
}

/* _unix_write_file_soft(): write to a file, not overwriting if it's changed
*/
static void
_unix_write_file_soft(u3_ufil* fil_u, u3_noun mim)
{
  struct stat buf_u;
  c3_i  fid_i = open(fil_u->pax_c, O_RDONLY, 0644);
  c3_ws len_ws, red_ws;
  c3_w  old_w;
  c3_y* old_y;

  if ( fid_i < 0 || fstat(fid_i, &buf_u) < 0 ) {
    if ( ENOENT == errno ) {
      goto _unix_write_file_soft_go;
    }
    else {
      uL(fprintf(uH, "error opening file (soft) %s: %s\r\n",
                 fil_u->pax_c, strerror(errno)));
      u3z(mim);
      return;
    }
  }

  len_ws = buf_u.st_size;
  old_y = c3_malloc(len_ws);

  red_ws = read(fid_i, old_y, len_ws);

  if ( close(fid_i) < 0 ) {
    uL(fprintf(uH, "error closing file (soft) %s: %s\r\n",
               fil_u->pax_c, strerror(errno)));
  }

  if ( len_ws != red_ws ) {
    if ( red_ws < 0 ) {
      uL(fprintf(uH, "error reading file (soft) %s: %s\r\n",
                 fil_u->pax_c, strerror(errno)));
    }
    else {
      uL(fprintf(uH, "wrong # of bytes read in file %s: %d %d\r\n",
                 fil_u->pax_c, len_ws, red_ws));
    }
    free(old_y);
    u3z(mim);
    return;
  }

  old_w = u3r_mug_bytes(old_y, len_ws);

  if ( old_w != fil_u->gum_w ) {
    fil_u->gum_w = u3r_mug(u3t(u3t(mim))); // XXX this might fail with
    free(old_y);                           //     trailing zeros
    u3z(mim);
    return;
  }

  free(old_y);

_unix_write_file_soft_go:
  fil_u->gum_w = _unix_write_file_hard(fil_u->pax_c, mim);
}

static void
_unix_watch_dir(u3_udir* dir_u, u3_udir* par_u, c3_c* pax_c);
static void
_unix_watch_file(u3_pier *pir_u, u3_ufil* fil_u, u3_udir* par_u, c3_c* pax_c);

/* _unix_get_mount_point(): retrieve or create mount point
*/
static u3_umon*
_unix_get_mount_point(u3_pier *pir_u, u3_noun mon)
{
  if ( c3n == u3ud(mon) ) {
    c3_assert(!"mount point must be an atom");
    u3z(mon);
    return NULL;
  }

  c3_c* nam_c = u3r_string(mon);
  u3_umon* mon_u;

  for ( mon_u = pir_u->unx_u->mon_u;
        mon_u && 0 != strcmp(nam_c, mon_u->nam_c);
        mon_u = mon_u->nex_u )
  {
  }

  if ( !mon_u ) {
    mon_u = c3_malloc(sizeof(u3_umon));
    mon_u->nam_c = nam_c;
    mon_u->dir_u.dir = c3y;
    mon_u->dir_u.dry = c3n;
    mon_u->dir_u.pax_c = strdup(pir_u->pax_c);
    mon_u->dir_u.par_u = NULL;
    mon_u->dir_u.nex_u = NULL;
    mon_u->dir_u.kid_u = NULL;
    mon_u->nex_u = pir_u->unx_u->mon_u;
    pir_u->unx_u->mon_u = mon_u;

  }
  else {
    free(nam_c);
  }

  u3z(mon);

  return mon_u;
}

/* _unix_scan_mount_point(): scan unix for already-existing mount point
*/
static void
_unix_scan_mount_point(u3_pier *pir_u, u3_umon* mon_u)
{
  DIR* rid_u = opendir(mon_u->dir_u.pax_c);
  if ( !rid_u ) {
    uL(fprintf(uH, "error opening pier directory: %s: %s\r\n",
               mon_u->dir_u.pax_c, strerror(errno)));
    return;
  }

  c3_w len_w = strlen(mon_u->nam_c);

  while ( 1 ) {
    struct dirent  ent_u;
    struct dirent* out_u;
    c3_w err_w;

    if ( 0 != (err_w = readdir_r(rid_u, &ent_u, &out_u)) ) {
      uL(fprintf(uH, "erroring loading pier directory %s: %s\r\n",
                 mon_u->dir_u.pax_c, strerror(errno)));
      c3_assert(0);
    }
    else if ( !out_u ) {
      break;
    }
    else if ( '.' == out_u->d_name[0] ) { // unnecessary, but consistency
      continue;
    }
    else if ( 0 != strncmp(mon_u->nam_c, out_u->d_name, len_w) ) {
      continue;
    }
    else {
      c3_c* pax_c = _unix_down(mon_u->dir_u.pax_c, out_u->d_name);

      struct stat buf_u;

      if ( 0 != stat(pax_c, &buf_u) ) {
        uL(fprintf(uH, "can't stat pier directory %s: %s\r\n",
                   mon_u->dir_u.pax_c, strerror(errno)));
        free(pax_c);
        continue;
      }
      if ( S_ISDIR(buf_u.st_mode) ) {
        if ( out_u->d_name[len_w] != '\0' ) {
          free(pax_c);
          continue;
        }
        else {
          u3_udir* dir_u = c3_malloc(sizeof(u3_udir));
          _unix_watch_dir(dir_u, &mon_u->dir_u, pax_c);
        }
      }
      else {
        if ( '.' != out_u->d_name[len_w]
             || '\0' == out_u->d_name[len_w + 1]
             || '~' == out_u->d_name[strlen(out_u->d_name) - 1]
             || ('#' == out_u->d_name[0] &&
                 '#' == out_u->d_name[strlen(out_u->d_name) - 1])
	     ) {
          free(pax_c);
          continue;
        }
        else {
          u3_ufil* fil_u = c3_malloc(sizeof(u3_ufil));
          _unix_watch_file(pir_u, fil_u, &mon_u->dir_u, pax_c);
        }
      }

      free(pax_c);
    }
  }
}

static u3_noun _unix_free_node(u3_pier *pir_u, u3_unod* nod_u);

/* _unix_free_file(): free file, unlinking it
*/
static void
_unix_free_file(u3_ufil *fil_u)
{
  if ( 0 != unlink(fil_u->pax_c) && ENOENT != errno ) {
    uL(fprintf(uH, "error unlinking %s: %s\n", fil_u->pax_c, strerror(errno)));
    c3_assert(0);
  }

  free(fil_u->pax_c);
  free(fil_u);
}

/* _unix_free_dir(): free directory, deleting everything within
*/
static void
_unix_free_dir(u3_udir *dir_u)
{
  _unix_rm_r(dir_u->pax_c);

  if ( dir_u->kid_u ) {
    fprintf(stderr, "don't kill me, i've got a family %s\r\n", dir_u->pax_c);
  }
  else {
    // fprintf(stderr, "i'm a lone, lonely loner %s\r\n", dir_u->pax_c);
  }
  free(dir_u->pax_c);
  free(dir_u); // XXX this might be too early, how do we
               //     know we've freed all the children?
               //     i suspect we should do this only if
               //     our kid list is empty
}

/* _unix_free_node(): free node, deleting everything within
 *
 * also deletes from parent list if in it
*/
static u3_noun
_unix_free_node(u3_pier *pir_u, u3_unod* nod_u)
{
  u3_noun can;
  if ( nod_u->par_u ) {
    u3_unod* don_u = nod_u->par_u->kid_u;
  
    if ( !don_u ) {
    }
    else if ( nod_u == don_u ) {
      nod_u->par_u->kid_u = nod_u->par_u->kid_u->nex_u;
    }
    else {
      for ( ; don_u->nex_u && nod_u != don_u->nex_u; don_u = don_u->nex_u ) {
      }
      if ( don_u->nex_u ) {
        don_u->nex_u = don_u->nex_u->nex_u;
      }
    }
  }

  if ( c3y == nod_u->dir ) {
    can = u3_nul;
    u3_unod* nud_u = ((u3_udir*) nod_u)->kid_u;
    while ( nud_u ) {
      u3_unod* nex_u = nud_u->nex_u;
      can = u3kb_weld(_unix_free_node(pir_u, nud_u), can);
      nud_u = nex_u;
    }
    _unix_free_dir((u3_udir *)nod_u);
  }
  else {
    can = u3nc(u3nc(_unix_string_to_path(pir_u, nod_u->pax_c), u3_nul),
               u3_nul);
    _unix_free_file((u3_ufil *)nod_u);
  }

  return can;
}

/* _unix_free_mount_point(): free mount point
 *
 * this process needs to happen in a very careful order.  in particular,
 * we must recurse before we get to the callback, so that libuv does all
 * the child directories before it does us.
 *
 * tread carefully
*/
static void
_unix_free_mount_point(u3_pier *pir_u, u3_umon* mon_u)
{
  u3_unod* nod_u;
  for ( nod_u = mon_u->dir_u.kid_u; nod_u; ) {
    u3_unod* nex_u = nod_u->nex_u;
    u3z(_unix_free_node(pir_u, nod_u));
    nod_u = nex_u;
  }

  free(mon_u->dir_u.pax_c);
  free(mon_u->nam_c);
  free(mon_u);
}

/* _unix_delete_mount_point(): remove mount point from list and free
*/
static void
_unix_delete_mount_point(u3_pier *pir_u, u3_noun mon)
{
  if ( c3n == u3ud(mon) ) {
    c3_assert(!"mount point must be an atom");
    u3z(mon);
    return;
  }

  c3_c* nam_c = u3r_string(mon);
  u3_umon* mon_u;
  u3_umon* tem_u;

  mon_u = pir_u->unx_u->mon_u;
  if ( !mon_u ) {
    uL(fprintf(uH, "mount point already gone: %s\r\n", nam_c));
    goto _delete_mount_point_out;
  }
  if ( 0 == strcmp(nam_c, mon_u->nam_c) ) {
    pir_u->unx_u->mon_u = mon_u->nex_u;
    _unix_free_mount_point(pir_u, mon_u);
    goto _delete_mount_point_out;
  }

  for ( ;
        mon_u->nex_u && 0 != strcmp(nam_c, mon_u->nex_u->nam_c);
        mon_u = mon_u->nex_u )
  {
  }

  if ( !mon_u->nex_u ) {
    uL(fprintf(uH, "mount point already gone: %s\r\n", nam_c));
    goto _delete_mount_point_out;
  }

  tem_u = mon_u->nex_u;
  mon_u->nex_u = mon_u->nex_u->nex_u;
  _unix_free_mount_point(pir_u, tem_u);

_delete_mount_point_out:
  free(nam_c);
  u3z(mon);
}

/* _unix_commit_mount_point: commit from mount point
*/
static void
_unix_commit_mount_point(u3_pier *pir_u, u3_noun mon)
{
  pir_u->unx_u->dyr = c3y;
  u3z(mon);
  u3_unix_ef_look(pir_u, c3n);
  return;
}

/* _unix_watch_file(): initialize file
*/
static void
_unix_watch_file(u3_pier *pir_u, u3_ufil* fil_u, u3_udir* par_u, c3_c* pax_c)
{
  // initialize fil_u

  fil_u->dir = c3n;
  fil_u->dry = c3n;
  fil_u->pax_c = c3_malloc(1 + strlen(pax_c));
  strcpy(fil_u->pax_c, pax_c);
  fil_u->par_u = par_u;
  fil_u->nex_u = NULL;
  fil_u->mug_w = 0;
  fil_u->gum_w = 0;

  if ( par_u ) {
    fil_u->nex_u = par_u->kid_u;
    par_u->kid_u = (u3_unod*) fil_u;
  }
}

/* _unix_watch_dir(): initialize directory
*/
static void
_unix_watch_dir(u3_udir* dir_u, u3_udir* par_u, c3_c* pax_c)
{
  // initialize dir_u

  dir_u->dir = c3y;
  dir_u->dry = c3n;
  dir_u->pax_c = c3_malloc(1 + strlen(pax_c));
  strcpy(dir_u->pax_c, pax_c);
  dir_u->par_u = par_u;
  dir_u->nex_u = NULL;
  dir_u->kid_u = NULL;

  if ( par_u ) {
    dir_u->nex_u = par_u->kid_u;
    par_u->kid_u = (u3_unod*) dir_u;
  }
}

/* _unix_create_dir(): create unix directory and watch it
*/
static void
_unix_create_dir(u3_udir* dir_u, u3_udir* par_u, u3_noun nam)
{
  c3_c* nam_c = u3r_string(nam);
  c3_w  nam_w = strlen(nam_c);
  c3_w  pax_w = strlen(par_u->pax_c);
  c3_c* pax_c = c3_malloc(pax_w + 1 + nam_w + 1);

  strncpy(pax_c, par_u->pax_c, pax_w);
  pax_c[pax_w] = '/';
  strncpy(pax_c + pax_w + 1, nam_c, nam_w);
  pax_c[pax_w + 1 + nam_w] = '\0';

  free(nam_c);
  u3z(nam);

  _unix_mkdir(pax_c);
  _unix_watch_dir(dir_u, par_u, pax_c);
}

static u3_noun _unix_update_node(u3_pier *pir_u, u3_unod* nod_u);

/* _unix_update_file(): update file, producing list of changes
 *
 * when scanning through files, if dry, do nothing.  otherwise, mark as
 * dry, then check if file exists.  if not, remove self from node list
 * and add path plus sig to %into event.  otherwise, read the file and
 * get a mug checksum.  if same as mug_w, move on.  otherwise, overwrite
 * mug_w with new mug and add path plus data to %into event.
*/
static u3_noun
_unix_update_file(u3_pier *pir_u, u3_ufil* fil_u)
{
  c3_assert( c3n == fil_u->dir );

  if ( c3y == fil_u->dry ) {
    return u3_nul;
  }

  fil_u->dry = c3n;

  struct stat buf_u;
  c3_i  fid_i = open(fil_u->pax_c, O_RDONLY, 0644);
  c3_ws len_ws, red_ws;
  c3_y* dat_y;

  if ( fid_i < 0 || fstat(fid_i, &buf_u) < 0 ) {
    if ( ENOENT == errno ) {
      return u3nc(u3nc(_unix_string_to_path(pir_u, fil_u->pax_c), u3_nul), u3_nul);
    }
    else {
      uL(fprintf(uH, "error opening file %s: %s\r\n",
                 fil_u->pax_c, strerror(errno)));
      return u3_nul;
    }
  }

  len_ws = buf_u.st_size;
  dat_y = c3_malloc(len_ws);

  red_ws = read(fid_i, dat_y, len_ws);

  if ( close(fid_i) < 0 ) {
    uL(fprintf(uH, "error closing file %s: %s\r\n",
               fil_u->pax_c, strerror(errno)));
  }

  if ( len_ws != red_ws ) {
    if ( red_ws < 0 ) {
      uL(fprintf(uH, "error reading file %s: %s\r\n",
                 fil_u->pax_c, strerror(errno)));
    }
    else {
      uL(fprintf(uH, "wrong # of bytes read in file %s: %d %d\r\n",
                 fil_u->pax_c, len_ws, red_ws));
    }
    free(dat_y);
    return u3_nul;
  }
  else {
    c3_w mug_w = u3r_mug_bytes(dat_y, len_ws);
    if ( mug_w == fil_u->mug_w ) {
      free(dat_y);
      return u3_nul;
    }
    else if ( mug_w == fil_u->gum_w ) {
      fil_u->mug_w = mug_w;
      free(dat_y);
      return u3_nul;
    }
    else {
      fil_u->mug_w = mug_w;

      u3_noun pax = _unix_string_to_path(pir_u, fil_u->pax_c);
      u3_noun mim = u3nt(c3__text, u3i_string("plain"), u3_nul);
      u3_noun dat = u3nt(mim, len_ws, u3i_bytes(len_ws, dat_y));

      free(dat_y);
      return u3nc(u3nt(pax, u3_nul, dat), u3_nul);
    }
  }
}

/* _unix_update_dir(): update directory, producing list of changes
 *
 * when changing this, consider whether to also change
 * _unix_initial_update_dir()
*/
static u3_noun
_unix_update_dir(u3_pier *pir_u, u3_udir* dir_u)
{
  u3_noun can = u3_nul;

  c3_assert( c3y == dir_u->dir );

  if ( c3y == dir_u->dry ) {
    return u3_nul;
  }

  dir_u->dry = c3n;

  // Check that old nodes are still there

  u3_unod* nod_u = dir_u->kid_u;

  if ( nod_u ) {
    while ( nod_u ) {
      if ( c3y == nod_u->dry ) {
        nod_u = nod_u->nex_u;
      }
      else {
        if ( c3y == nod_u->dir ) {
          DIR* red_u = opendir(nod_u->pax_c);
          if ( 0 == red_u ) {
            u3_unod* nex_u = nod_u->nex_u;
            can = u3kb_weld(_unix_free_node(pir_u, nod_u), can);
            nod_u = nex_u;
          }
          else {
            closedir(red_u);
            nod_u = nod_u->nex_u;
          }
        }
        else {
          struct stat buf_u;
          c3_i  fid_i = open(nod_u->pax_c, O_RDONLY, 0644);

          if ( (fid_i < 0) || (fstat(fid_i, &buf_u) < 0) ) {
            if ( ENOENT != errno ) {
              uL(fprintf(uH, "_unix_update_dir: error opening file %s: %s\r\n",
                         nod_u->pax_c, strerror(errno)));
            }

            u3_unod* nex_u = nod_u->nex_u;
            can = u3kb_weld(_unix_free_node(pir_u, nod_u), can);
            nod_u = nex_u;
          }
          else {
            if ( close(fid_i) < 0 ) {
              uL(fprintf(uH, "_unix_update_dir: error closing file %s: %s\r\n",
                         nod_u->pax_c, strerror(errno)));
            }

            nod_u = nod_u->nex_u;
          }
        }
      }
    }
  }

  // Check for new nodes

  DIR* rid_u = opendir(dir_u->pax_c);
  if ( !rid_u ) {
    uL(fprintf(uH, "error opening directory %s: %s\r\n",
               dir_u->pax_c, strerror(errno)));
    c3_assert(0);
  }

  while ( 1 ) {
    struct dirent  ent_u;
    struct dirent* out_u;
    c3_w err_w;

    if ( (err_w = readdir_r(rid_u, &ent_u, &out_u)) != 0 ) {
      uL(fprintf(uH, "error loading directory %s: %s\r\n",
                 dir_u->pax_c, strerror(err_w)));
      c3_assert(0);
    }
    else if ( !out_u ) {
      break;
    }
    else if ( '.' == out_u->d_name[0] ) {
      continue;
    }
    else {
      c3_c* pax_c = _unix_down(dir_u->pax_c, out_u->d_name);

      struct stat buf_u;

      if ( 0 != stat(pax_c, &buf_u) ) {
        uL(fprintf(uH, "can't stat %s: %s\r\n", pax_c, strerror(errno)));
        free(pax_c);
        continue;
      }
      else {
        u3_unod* nod_u;
        for ( nod_u = dir_u->kid_u; nod_u; nod_u = nod_u->nex_u ) {
          if ( 0 == strcmp(pax_c, nod_u->pax_c) ) {
            if ( S_ISDIR(buf_u.st_mode) ) {
              if ( c3n == nod_u->dir ) {
                uL(fprintf(uH, "not a directory: %s\r\n", nod_u->pax_c));
                c3_assert(0);
              }
            }
            else {
              if ( c3y == nod_u->dir ) {
                uL(fprintf(uH, "not a file: %s\r\n", nod_u->pax_c));
                c3_assert(0);
              }
            }
            break;
          }
        }

        if ( !nod_u ) {
          if ( !S_ISDIR(buf_u.st_mode) ) {
            if ( !strchr(out_u->d_name,'.')
                 || '~' == out_u->d_name[strlen(out_u->d_name) - 1]
                 || ('#' == out_u->d_name[0] &&
                     '#' == out_u->d_name[strlen(out_u->d_name) - 1])
               ) {
              free(pax_c);
              continue;
            }

            u3_ufil* fil_u = c3_malloc(sizeof(u3_ufil));
            _unix_watch_file(pir_u, fil_u, dir_u, pax_c);
          }
          else {
            u3_udir* dis_u = c3_malloc(sizeof(u3_udir));
            _unix_watch_dir(dis_u, dir_u, pax_c);
            can = u3kb_weld(_unix_update_dir(pir_u, dis_u), can); // XXX unnecessary?
          }
        }
      }

      free(pax_c);
    }
  }

  if ( closedir(rid_u) < 0 ) {
    uL(fprintf(uH, "error closing directory %s: %s\r\n",
               dir_u->pax_c, strerror(errno)));
  }

  if ( !dir_u->kid_u ) {
    return u3kb_weld(_unix_free_node(pir_u, (u3_unod*) dir_u), can);
  }

  // get change list

  for ( nod_u = dir_u->kid_u; nod_u; nod_u = nod_u->nex_u ) {
    can = u3kb_weld(_unix_update_node(pir_u, nod_u), can);
  }

  return can;
}

/* _unix_update_node(): update node, producing list of changes
*/
static u3_noun
_unix_update_node(u3_pier *pir_u, u3_unod* nod_u)
{
  if ( c3y == nod_u->dir ) {
    return _unix_update_dir(pir_u, (void*)nod_u);
  }
  else {
    return _unix_update_file(pir_u, (void*)nod_u);
  }
}

/* _unix_update_mount(): update mount point
*/
static void
_unix_update_mount(u3_pier *pir_u, u3_umon* mon_u, u3_noun all)
{
  if ( c3n == mon_u->dir_u.dry ) {
    u3_noun  can = u3_nul;
    u3_unod* nod_u;
    for ( nod_u = mon_u->dir_u.kid_u; nod_u; nod_u = nod_u->nex_u ) {
      can = u3kb_weld(_unix_update_node(pir_u, nod_u), can);
    }

    u3_pier_work(pir_u,
             u3nq(u3_blip, c3__sync, u3k(u3A->sen), u3_nul),
             u3nq(c3__into, u3i_string(mon_u->nam_c), all, can));
  }
}

/* _unix_initial_update_file(): read file, but don't watch
** XX deduplicate with _unix_update_file()
*/
static u3_noun
_unix_initial_update_file(c3_c* pax_c, c3_c* bas_c)
{
  struct stat buf_u;
  c3_i  fid_i = open(pax_c, O_RDONLY, 0644);
  c3_ws len_ws, red_ws;
  c3_y* dat_y;

  if ( fid_i < 0 || fstat(fid_i, &buf_u) < 0 ) {
    if ( ENOENT == errno ) {
      return u3_nul;
    }
    else {
      uL(fprintf(uH, "error opening initial file %s: %s\r\n",
                 pax_c, strerror(errno)));
      return u3_nul;
    }
  }

  len_ws = buf_u.st_size;
  dat_y = c3_malloc(len_ws);

  red_ws = read(fid_i, dat_y, len_ws);

  if ( close(fid_i) < 0 ) {
    uL(fprintf(uH, "error closing initial file %s: %s\r\n",
               pax_c, strerror(errno)));
  }

  if ( len_ws != red_ws ) {
    if ( red_ws < 0 ) {
      uL(fprintf(uH, "error reading initial file %s: %s\r\n",
                 pax_c, strerror(errno)));
    }
    else {
      uL(fprintf(uH, "wrong # of bytes read in initial file %s: %d %d\r\n",
                 pax_c, len_ws, red_ws));
    }
    free(dat_y);
    return u3_nul;
  }
  else {
    u3_noun pax = _unix_string_to_path_helper(pax_c
                   + strlen(bas_c)
                   + 1); /* XX slightly less VERY BAD than before*/
    u3_noun mim = u3nt(c3__text, u3i_string("plain"), u3_nul);
    u3_noun dat = u3nt(mim, len_ws, u3i_bytes(len_ws, dat_y));

    free(dat_y);
    return u3nc(u3nt(pax, u3_nul, dat), u3_nul);
  }
}

/* _unix_initial_update_dir(): read directory, but don't watch
** XX deduplicate with _unix_update_dir()
*/
static u3_noun
_unix_initial_update_dir(c3_c* pax_c, c3_c* bas_c)
{
  u3_noun can = u3_nul;

  DIR* rid_u = opendir(pax_c);
  if ( !rid_u ) {
    uL(fprintf(uH, "error opening initial directory: %s: %s\r\n",
               pax_c, strerror(errno)));
    return u3_nul;
  }

  while ( 1 ) {
    struct dirent  ent_u;
    struct dirent* out_u;
    c3_w err_w;

    if ( 0 != (err_w = readdir_r(rid_u, &ent_u, &out_u)) ) {
      uL(fprintf(uH, "error loading initial directory %s: %s\r\n",
                 pax_c, strerror(errno)));
      c3_assert(0);
    }
    else if ( !out_u ) {
      break;
    }
    else if ( '.' == out_u->d_name[0] ) {
      continue;
    }
    else {
      c3_c* pox_c = _unix_down(pax_c, out_u->d_name);

      struct stat buf_u;

      if ( 0 != stat(pox_c, &buf_u) ) {
        uL(fprintf(uH, "initial can't stat %s: %s\r\n",
                   pox_c, strerror(errno)));
        free(pox_c);
        continue;
      }
      else {
        if ( S_ISDIR(buf_u.st_mode) ) {
          can = u3kb_weld(_unix_initial_update_dir(pox_c, bas_c), can);
        }
        else {
          can = u3kb_weld(_unix_initial_update_file(pox_c, bas_c), can);
        }
        free(pox_c);
      }
    }
  }

  if ( closedir(rid_u) < 0 ) {
    uL(fprintf(uH, "error closing initial directory %s: %s\r\n",
               pax_c, strerror(errno)));
  }

  return can;
}

/* u3_unix_initial_into_card(): create initial filesystem sync card.
*/
u3_noun
u3_unix_initial_into_card(c3_c* arv_c)
{
  u3_noun can = _unix_initial_update_dir(arv_c, arv_c);

  return u3nc(u3nt(u3_blip, c3__sync, u3_nul),
              u3nq(c3__into, u3_nul, c3y, can));
}

/* _unix_sign_cb: signal callback.
*/
static void
_unix_sign_cb(uv_signal_t* sil_u, c3_i num_i)
{
  {
    switch ( num_i ) {
      default: fprintf(stderr, "\r\nmysterious signal %d\r\n", num_i); break;
      case SIGTERM:
        u3_pier_exit();
        break;
      case SIGINT:
        fprintf(stderr, "\r\ninterrupt\r\n");
        u3_term_ef_ctlc();
        break;
      case SIGWINCH: u3_term_ef_winc(); break;
    }
  }
}

/* _unix_sync_file(): sync file to unix
*/
static void
_unix_sync_file(u3_pier *pir_u, u3_udir* par_u, u3_noun nam, u3_noun ext, u3_noun mim)
{
  c3_assert( par_u );
  c3_assert( c3y == par_u->dir );

  // form file path

  c3_c* nam_c = u3r_string(nam);
  c3_c* ext_c = u3r_string(ext);
  c3_w  par_w = strlen(par_u->pax_c);
  c3_w  nam_w = strlen(nam_c);
  c3_w  ext_w = strlen(ext_c);
  c3_c* pax_c = c3_malloc(par_w + 1 + nam_w + 1 + ext_w + 1);

  strncpy(pax_c, par_u->pax_c, par_w);
  pax_c[par_w] = '/';
  strncpy(pax_c + par_w + 1, nam_c, nam_w);
  pax_c[par_w + 1 + nam_w] = '.';
  strncpy(pax_c + par_w + 1 + nam_w + 1, ext_c, ext_w);
  pax_c[par_w + 1 + nam_w + 1 + ext_w] = '\0';

  free(nam_c); free(ext_c);
  u3z(nam); u3z(ext);

  // check whether we already know about this file

  u3_unod* nod_u;
  for ( nod_u = par_u->kid_u;
        ( nod_u &&
          ( c3y == nod_u->dir ||
            0 != strcmp(nod_u->pax_c, pax_c) ) );
        nod_u = nod_u->nex_u )
  { }

  // apply change

  if ( u3_nul == mim ) {
    if ( nod_u ) {
      u3z(_unix_free_node(pir_u, nod_u));
    }
  }
  else {

    if ( !nod_u ) {
      c3_w gum_w = _unix_write_file_hard(pax_c, u3k(u3t(mim)));
      u3_ufil* fil_u = c3_malloc(sizeof(u3_ufil));
      _unix_watch_file(pir_u, fil_u, par_u, pax_c);
      fil_u->gum_w = gum_w;
      goto _unix_sync_file_out;
    }
    else {
      _unix_write_file_soft((u3_ufil*) nod_u, u3k(u3t(mim)));
    }
  }

  free(pax_c);

_unix_sync_file_out:
  u3z(mim);
}

/* _unix_sync_change(): sync single change to unix
*/
static void
_unix_sync_change(u3_pier *pir_u, u3_udir* dir_u, u3_noun pax, u3_noun mim)
{
  c3_assert( c3y == dir_u->dir );

  if ( c3n == u3du(pax) ) {
    if ( u3_nul == pax ) {
      uL(fprintf(uH,"can't sync out file as top-level, strange\r\n"));
    }
    else {
      uL(fprintf(uH,"sync out: bad path\r\n"));
    }
    u3z(pax); u3z(mim);
    return;
  }
  else if ( c3n == u3du(u3t(pax)) ) {
    uL(fprintf(uH,"can't sync out file as top-level, strangely\r\n"));
    u3z(pax); u3z(mim);
  }
  else {
    u3_noun i_pax = u3h(pax);
    u3_noun t_pax = u3t(pax);
    u3_noun it_pax = u3h(t_pax);
    u3_noun tt_pax = u3t(t_pax);

    if ( u3_nul == tt_pax ) {
      _unix_sync_file(pir_u, dir_u, u3k(i_pax), u3k(it_pax), mim);
    }
    else {
      c3_c* nam_c = u3r_string(i_pax);
      c3_w pax_w = strlen(dir_u->pax_c);
      u3_unod* nod_u;

      for ( nod_u = dir_u->kid_u;
            ( nod_u &&
              ( c3n == nod_u->dir ||
                0 != strcmp(nod_u->pax_c + pax_w + 1, nam_c) ) );
            nod_u = nod_u->nex_u )
      { }

      if ( !nod_u ) {
        nod_u = c3_malloc(sizeof(u3_udir));
        _unix_create_dir((u3_udir*) nod_u, dir_u, u3k(i_pax));
      }

      if ( c3n == nod_u->dir ) {
        uL(fprintf(uH,
           "weird, we got a file when we weren't expecting to\r\n"));
        c3_assert(0);
      }

      _unix_sync_change(pir_u, (u3_udir*) nod_u, u3k(t_pax), mim);
    }
  }
  u3z(pax);
}

/* _unix_sync_ergo(): sync list of changes to unix
*/
static void
_unix_sync_ergo(u3_pier *pir_u, u3_umon* mon_u, u3_noun can)
{
  u3_noun nac = can;
  u3_noun nam = u3i_string(mon_u->nam_c);

  while ( u3_nul != nac) {
    _unix_sync_change(pir_u, &mon_u->dir_u,
                      u3nc(u3k(nam), u3k(u3h(u3h(nac)))),
                      u3k(u3t(u3h(nac))));
    nac = u3t(nac);
  }

  u3z(nam);
  u3z(can);
}

/* u3_unix_ef_dirk(): commit mount point
*/
void
u3_unix_ef_dirk(u3_pier *pir_u, u3_noun mon)
{
  _unix_commit_mount_point(pir_u, mon);
}

/* u3_unix_ef_ergo(): update filesystem from urbit
*/
void
u3_unix_ef_ergo(u3_pier *pir_u, u3_noun mon, u3_noun can)
{
  u3_umon* mon_u = _unix_get_mount_point(pir_u, mon);

  _unix_sync_ergo(pir_u, mon_u, can);
}

/* u3_unix_ef_ogre(): delete mount point
*/
void
u3_unix_ef_ogre(u3_pier *pir_u, u3_noun mon)
{
  _unix_delete_mount_point(pir_u, mon);
}

/* u3_unix_ef_hill(): enumerate mount points
*/
void
u3_unix_ef_hill(u3_pier *pir_u, u3_noun hil)
{
  u3_noun mon;
  for ( mon = hil; c3y == u3du(mon); mon = u3t(mon) ) {
    u3_umon* mon_u = _unix_get_mount_point(pir_u, u3k(u3h(mon)));
    _unix_scan_mount_point(pir_u, mon_u);
  }
  u3z(hil);
  pir_u->unx_u->dyr = c3y;
  u3_unix_ef_look(pir_u, c3y);
}

/* u3_unix_io_init(): initialize unix sync.
*/
void
u3_unix_io_init(u3_pier *pir_u)
{
  u3_unix* unx_u = pir_u->unx_u;

  unx_u->mon_u = NULL;

  unx_u->alm = c3n;
  unx_u->dyr = c3n;
}

/* u3_unix_acquire(): acquire a lockfile, killing anything that holds it.
*/
static void
u3_unix_acquire(c3_c* pax_c)
{
  c3_c* paf_c = _unix_down(pax_c, ".vere.lock");
  c3_w pid_w;
  FILE* loq_u;

  if ( NULL != (loq_u = fopen(paf_c, "r")) ) {
    if ( 1 != fscanf(loq_u, "%" SCNu32, &pid_w) ) {
      uL(fprintf(uH, "lockfile %s is corrupt!\n", paf_c));
      kill(getpid(), SIGTERM);
      sleep(1); c3_assert(0);
    }
    else if (pid_w != getpid()) {
      c3_w i_w;

      if ( -1 != kill(pid_w, SIGTERM) ) {
        uL(fprintf(uH, "unix: stopping process %d, live in %s...\n",
                        pid_w, pax_c));

        for ( i_w = 0; i_w < 16; i_w++ ) {
          sleep(1);
          if ( -1 == kill(pid_w, SIGTERM) ) {
            break;
          }
        }
        if ( 16 == i_w ) {
          for ( i_w = 0; i_w < 16; i_w++ ) {
            if ( -1 == kill(pid_w, SIGKILL) ) {
              break;
            }
            sleep(1);
          }
        }
        if ( 16 == i_w ) {
          uL(fprintf(uH, "process %d seems unkillable!\n", pid_w));
          c3_assert(0);
        }
        uL(fprintf(uH, "unix: stopped old process %u\n", pid_w));
      }
    }
    fclose(loq_u);
    unlink(paf_c);
  }

  loq_u = fopen(paf_c, "w");
  fprintf(loq_u, "%u\n", getpid());

  {
    c3_i fid_i = fileno(loq_u);
#if defined(U3_OS_linux)
    fdatasync(fid_i);
#elif defined(U3_OS_osx)
    fcntl(fid_i, F_FULLFSYNC);
#elif defined(U3_OS_bsd)
    fsync(fid_i);
#else
#   error "port: datasync"
#endif
  }
  fclose(loq_u);
  free(paf_c);
}

/* u3_unix_release(): release a lockfile.
*/
static void
u3_unix_release(c3_c* pax_c)
{
  c3_c* paf_c = _unix_down(pax_c, ".vere.lock");

  unlink(paf_c);
  free(paf_c);
}

/* u3_unix_ef_hold()
*/
void
u3_unix_ef_hold(void)
{
  u3_usig* sig_u;

  for ( sig_u = u3_Host.sig_u; sig_u; sig_u = sig_u->nex_u ) {
    uv_signal_stop(&sig_u->sil_u);
  }
}

/* u3_unix_ef_bake(): initial effects for new process.
*/
void
u3_unix_ef_bake(u3_pier *pir_u)
{
  u3_pier_work(pir_u,
               u3nt(u3_blip, c3__boat, u3_nul),
               u3nc(c3__boat, u3_nul));
}

/* u3_unix_ef_move()
*/
void
u3_unix_ef_move(void)
{
  u3_usig* sig_u;

  for ( sig_u = u3_Host.sig_u; sig_u; sig_u = sig_u->nex_u ) {
    uv_signal_start(&sig_u->sil_u, _unix_sign_cb, sig_u->num_i);
  }
}

/* u3_unix_ef_look(): update the root.
*/
void
u3_unix_ef_look(u3_pier *pir_u, u3_noun all)
{
  if ( c3y == pir_u->unx_u->dyr ) {
    pir_u->unx_u->dyr = c3n;
    u3_umon* mon_u;
  
    for ( mon_u = pir_u->unx_u->mon_u; mon_u; mon_u = mon_u->nex_u ) {
      _unix_update_mount(pir_u, mon_u, all);
    }
  }
}

/* u3_unix_io_talk(): start listening for fs events.
*/
void
u3_unix_io_talk(u3_pier *pir_u)
{
  u3_unix_acquire(pir_u->pax_c);
  u3_unix_ef_move();
}

/* u3_unix_io_exit(): terminate unix I/O.
*/
void
u3_unix_io_exit(u3_pier *pir_u)
{
  u3_unix_release(pir_u->pax_c);
}
